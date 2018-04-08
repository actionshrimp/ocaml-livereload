open Lwt
open Websocket
open Websocket_cohttp_lwt

type msg =
  { command: string
  ; path: string
  ; liveCSS: bool
  }

(* let livereload_js = *)
(*   CCIO.(with_in (Filename.concat "day09.txt") read_lines_l) *)

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  Lwt_io.eprintf
        "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  let uri = Cohttp.Request.uri req in
  let _ = Lwt_io.eprintf "[PATH] %s\n%!" (Uri.path uri) in
  match Uri.path uri with
  | "/livereload.js" ->
    Cohttp_lwt_unix.Server.respond_string
    ~status:`OK
    ~body: [%blob "static/livereload.js"]
    ()
  | "/ws" ->
    Lwt_io.eprintf "[PATH] /ws\n%!"
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
    let _ =
        let num_ref = ref 10 in
        let rec go () =
            if !num_ref > 0 then
                let msg = Printf.sprintf "-> Ping %d" !num_ref in
                Lwt_io.eprintf "[SEND] %s\n%!" msg
                >>= fun () ->
                Lwt.wrap1 frames_out_fn @@
                    Some (Frame.create ~content:msg ())
                >>= fun () ->
                Lwt.return (num_ref := !num_ref - 1)
                >>= fun () ->
                Lwt_unix.sleep 1.
                >>= go
            else
                Lwt_io.eprintf "[INFO] Test done\n%!"
                >>= Lwt.return
        in
        go ()
    in
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ ->
    Lwt_io.eprintf "[PATH] Catch-all\n%!"
    >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string
        ~status:`Not_found
        ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
        ()
