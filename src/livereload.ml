open Lwt
open Websocket
open Websocket_cohttp_lwt

type msg =
  { command: string
  ; path: string
  ; liveCSS: bool
  }

let hello =
{|{
  "command": "hello",
  "protocols": ["http://livereload.com/protocols/official-7"],
  "serverName": "ocaml-livereload"
}|}

let reload path = Printf.sprintf
{|{
  "command": "reload",
  "path": "%s",
  "liveCss": true
}|} path

module ByConn = CCMap.Make(struct
    type t = Cohttp.Connection.t
    let compare = Cohttp.Connection.compare
  end)

let make_handler
    (next : Conduit_lwt_unix.flow * Cohttp.Connection.t -> Cohttp_lwt_unix.Request.t -> Cohttp_lwt.Body.t  -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
  : ((string -> unit Lwt.t) *
     (Conduit_lwt_unix.flow * Cohttp.Connection.t -> Cohttp_lwt_unix.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t))
  =
  let conn_update_handlers = Lwt_mvar.create ByConn.empty in
  (fun path ->
     Lwt_mvar.take conn_update_handlers
     >>= fun handlers ->
     Lwt_mvar.put conn_update_handlers handlers
     >>= fun () ->
     Lwt.return @@
     (handlers |> ByConn.iter (fun c handler ->
          Printf.eprintf "[livereload] Telling %s about update to %s\n%!"
            (Cohttp.Connection.to_string c) path;
          handler @@ Some (Frame.create ~content:(reload path) ())
        ));
  ),
  (fun conn req body ->
     let open Frame in
     let uri = Cohttp.Request.uri req in
     match Uri.path uri with
     | "/livereload.js" ->
       Lwt_io.eprintf "[livereload] /livereload.js\n%!"
       >>= fun () ->
       Cohttp_lwt_unix.Server.respond_string
         ~headers: (Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/javascript")
         ~status:`OK
         ~body: [%blob "../static/livereload.js"]
         ()
     | "/livereload" ->
       Lwt_io.eprintf "[livereload] /livereload\n%!"
       >>= fun () ->
       Cohttp_lwt.Body.drain_body body
       >>= fun () ->
       Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
         fun f ->
           match f.opcode with
           | Opcode.Close ->
             Printf.eprintf "[RECV] CLOSE\n%!";
             Lwt.async (fun () ->
                 Lwt_io.eprintf "[livereload] Removing client %s \n%!"
                   (snd conn |> Cohttp.Connection.to_string) >>= fun () ->
                 Lwt_mvar.take conn_update_handlers
                 >>= fun handlers ->
                 let updated = ByConn.remove (snd conn) handlers in
                 Lwt_mvar.put conn_update_handlers updated)

           | _ ->
             Printf.eprintf "[RECV] %s\n%!" f.content
       )
       >>= fun (resp, body, frames_out_fn) ->
       Lwt_io.eprintf "[livereload] Adding client %s \n%!"
         (snd conn |> Cohttp.Connection.to_string) >>= fun () ->
       Lwt_mvar.take conn_update_handlers
       >>= fun handlers ->
       let updated = (ByConn.add (snd conn) frames_out_fn handlers) in
       Lwt_mvar.put conn_update_handlers updated
       >>= fun () ->
       Lwt.wrap1 frames_out_fn @@
       Some (Frame.create ~content:hello ())
       >>= fun () ->
       Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
     | _ -> next conn req body)


let make_watcher
    (paths : (string * string) list)
    (change_cb : string -> unit Lwt.t)
  : unit Lwt.t =
  Lwt_inotify.create () >>= fun inotify ->
  paths
  |> CCList.map (fun (fs_path, server_base) ->
      Lwt_inotify.add_watch inotify fs_path [Inotify.S_Attrib; Inotify.S_Modify]
      >>= fun _ -> Lwt.return ()
    )
  |> Lwt.join >>= fun () ->
  let rec go () =
    Lwt_inotify.read inotify >>= fun event ->
    Lwt_io.eprintf "%s%!" (Inotify.string_of_event event) >>= fun () ->
    go ()
  in go ()
