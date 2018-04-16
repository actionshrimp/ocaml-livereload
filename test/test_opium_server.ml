open Opium.Std


let port = 3000

let index = Printf.sprintf {|
<html>
<head>
    <meta charset="utf-8">
    <script src="http://localhost:%d/livereload.js"></script>
    <link rel="stylesheet" href="/static/main.css">
</head>
<body>
    <div id='msg'></div>
    <script src="/static/main.js"></script>
</body>
</html> |} port

let hello = get "/" (fun _ -> `String index |> respond')

let lr_handler = Livereload.make_handler [{ local_dir = "test/static"; server_path = "/static" }]

let app =
  let static = Middleware.static ~local_path:"test/static" ~uri_prefix:"/static" in
  App.empty
  |> middleware static
  |> hello

let rock_app = App.to_rock app

(* From Opium.App *)
let run_unix ?ssl t ~port =
  let open Rock in
  let open Lwt in
  let middlewares = t |> App.middlewares |> List.map Middleware.filter in
  let handler = App.handler t in
  let mode =
    ssl
    |> CCOpt.map (fun (c, k) -> `TLS (c, k, `No_password, `Port port))
    |> CCOpt.get_or ~default:(`TCP (`Port port))
  in
  Cohttp_lwt_unix.Server.create ~mode (
    Cohttp_lwt_unix.Server.make ~callback:(lr_handler (fun conn req body ->
      let req = Request.create ~body req in
      let handler = Filter.apply_all middlewares handler in
      handler req >>= fun { Response.code; headers; body; _ } ->
      Cohttp_lwt_unix.Server.respond ~headers ~body ~status:code ()
    )) ()
  )
  |> Lwt_main.run

let _ = run_unix rock_app ~port:port
