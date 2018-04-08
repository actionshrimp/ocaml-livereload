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

let cssmain = "body { background-color: red }"

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  let uri = Cohttp.Request.uri req in
  let _ = Lwt_io.eprintf "[PATH] %s\n%!" (Uri.path uri) in

  Livereload.handler conn req body
    (fun conn req body ->
       let uri = Cohttp.Request.uri req in
       match Uri.path uri with
       | "/main.css" ->
         Cohttp_lwt_unix.Server.respond_string
           ~headers: (Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "text/css")
           ~status:`OK
           ~body: cssmain
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
    )

let start_server host port () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:handler ~conn_closed ())

(* main *)
let () =
    Lwt_main.run (start_server "localhost" port ())
