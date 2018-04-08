open Lwt
open Websocket
open Websocket_cohttp_lwt

let port = 7777

let index = Printf.sprintf {|
<html>
<head>
    <meta charset="utf-8">
    <script src="http://localhost:%d/livereload.js"></script>
    <link rel="stylesheet" href="/main.css">
</head>
<body>
    <div id='msg'></div>
</body>
</html> |} port

let cssmain color = Printf.sprintf "body { background-color: %s }" color

let make_handler () =
  let css_content = Lwt_mvar.create (cssmain "red") in
  let next = fun conn req body ->
        let uri = Cohttp.Request.uri req in
        match Uri.path uri with
        | "/main.css" ->
          Lwt_mvar.take css_content
          >>= fun content ->
          Lwt_mvar.put css_content content
          >>= fun () ->
          Cohttp_lwt_unix.Server.respond_string
            ~headers: (Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "text/css")
            ~status:`OK
            ~body: content
            ()
        | "/" ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`OK
            ~body: index
            ()
        | _ ->
          Lwt_io.eprintf "[PATH] Catch-all\n%!"
          >>= fun () ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`Not_found
            ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
            ()
  in
  let send_update_fn, handler = Livereload.make_handler next in
  let _ =
    let rec go (c : string) =
      Lwt_io.eprintf "[SERV] Switching color to %s\n%!" c
      >>= fun () ->
      Lwt_mvar.take css_content
      >>= fun content ->
      Lwt_mvar.put css_content (cssmain c)
      >>= fun () ->
      send_update_fn "/main.css"
      >>= fun () ->
      Lwt_unix.sleep 3.
      >>= fun () ->
      go (if c = "red" then "green" else "red")
    in
    Lwt.async (fun () -> (go "green"))
  in
  fun conn req body ->
    Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
    >>= fun _ ->
    let uri = Cohttp.Request.uri req in
    let _ = Lwt_io.eprintf "[PATH] %s\n%!" (Uri.path uri) in
    handler conn req body

let start_server host port () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:(make_handler ()) ~conn_closed ())

(* main *)
let () =
    Lwt_main.run (start_server "localhost" port ())
