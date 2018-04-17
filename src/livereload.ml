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

type conn =
  { ready : bool
  ; frames_out_fn : (Websocket_cohttp_lwt.Frame.t option -> unit)
  }

type handler = Conduit_lwt_unix.flow * Cohttp.Connection.t -> Cohttp_lwt_unix.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

let make_raw_handler
    ?(debug=false)
    (next : handler)
  : ((string -> unit Lwt.t) * handler)
  =
  let debug_log (msg : string) = if debug then print_endline msg else () in
  let debug_log_lwt (msg : string) = if debug then Lwt_io.eprint msg else Lwt.return () in
  let conn_update_handlers = Lwt_mvar.create ByConn.empty in
  (fun path ->
     Lwt_mvar.take conn_update_handlers
     >>= fun handlers ->
     Lwt_mvar.put conn_update_handlers handlers
     >>= fun () ->
     Lwt.return @@
     (handlers |> ByConn.iter (fun c handler ->
          debug_log (Printf.sprintf "[livereload] Telling %s about update to %s\n%!"
                       (Cohttp.Connection.to_string c) path);
          if handler.ready then
            handler.frames_out_fn @@ Some (Frame.create ~content:(reload path) ())
          else ()
        ));
  ),
  (fun conn req body ->
     let open Frame in
     let uri = Cohttp.Request.uri req in
     match Uri.path uri with
     | "/livereload.js" ->
       debug_log_lwt (Printf.sprintf "[livereload] /livereload.js\n%!")
       >>= fun () ->
       Cohttp_lwt_unix.Server.respond_string
         ~headers: (Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/javascript")
         ~status:`OK
         ~body: [%blob "../static/livereload.js"]
         ()
     | "/livereload" ->
       debug_log_lwt (Printf.sprintf "[livereload] /livereload\n%!")
       >>= fun () ->
       Cohttp_lwt.Body.drain_body body
       >>= fun () ->
       Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
         fun f ->
           match f.opcode with
           | Opcode.Close ->
             Lwt.async (fun () ->
                 debug_log_lwt (Printf.sprintf "[livereload] Removing client %s \n%!" (snd conn |> Cohttp.Connection.to_string))
                 >>= fun () ->
                 Lwt_mvar.take conn_update_handlers
                 >>= fun handlers ->
                 let updated = ByConn.remove (snd conn) handlers in
                 Lwt_mvar.put conn_update_handlers updated)

           | _ ->
             if CCString.find "hello" f.content <> -1 then
               Lwt.async (fun () ->
                   debug_log_lwt (Printf.sprintf "[livereload] Client %s is ready \n%!"
                                    (snd conn |> Cohttp.Connection.to_string))
                   >>= fun () ->
                   Lwt_mvar.take conn_update_handlers
                   >>= fun handlers ->
                   let updated = (ByConn.update (snd conn) (function Some c -> Some { c with ready = true } | _ -> None) handlers) in
                   Lwt_mvar.put conn_update_handlers updated
                 )
             else
               ())

       >>= fun (resp, body, frames_out_fn) ->
       debug_log_lwt (Printf.sprintf "[livereload] Adding client %s \n%!"
                        (snd conn |> Cohttp.Connection.to_string))
       >>= fun () ->
       Lwt_mvar.take conn_update_handlers
       >>= fun handlers ->
       let updated = (ByConn.add (snd conn) { ready = false; frames_out_fn = frames_out_fn } handlers) in
       Lwt_mvar.put conn_update_handlers updated
       >>= fun () ->
       Lwt.wrap1 frames_out_fn @@ Some (Frame.create ~content:hello ())
       >>= fun () ->
       Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
     | _ -> next conn req body)


let make_handler
    ?(debug=false)
    (path_configs : Types.path_config list)
    (next : handler)
    : handler
    = let send_update_fn, handler = make_raw_handler ~debug next in
    Lwt.async (fun () -> Backend.make_watcher ~debug path_configs send_update_fn);
    Lwt.async (fun () ->
        Lwt_unix.sleep 2.0 >>= fun _ ->
        send_update_fn "/"
      );
    handler

