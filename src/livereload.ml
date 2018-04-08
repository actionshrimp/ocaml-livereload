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

let alert =
{|{
  "command": "alert",
  "message": "HEY!"
}|}

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t)
    (next : Conduit_lwt_unix.flow * Cohttp.Connection.t -> Cohttp_lwt_unix.Request.t -> Cohttp_lwt.Body.t  -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
  : ((Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
  =
  let open Frame in
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/livereload.js" ->
    Cohttp_lwt_unix.Server.respond_string
      ~headers: (Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/javascript")
    ~status:`OK
    ~body: [%blob "static/livereload.js"]
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
                Printf.eprintf "[RECV] CLOSE\n%!"
            | _ ->
                Printf.eprintf "[RECV] %s\n%!" f.content
    )
    >>= fun (resp, body, frames_out_fn) ->
    (* send a message to the client every second *)
    Lwt_io.eprintf "[SEND] hello\n%!"
    >>= fun () ->
    Lwt.wrap1 frames_out_fn @@
    Some (Frame.create ~content:hello ())
    >>= fun () ->
    Lwt.wrap1 frames_out_fn @@
    Some (Frame.create ~content:alert ())
    >>= fun () ->
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ -> next conn req body
