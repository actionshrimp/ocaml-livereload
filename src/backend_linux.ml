open Lwt
open Types

let make_watcher
    ?(debug=false)
    (path_configs : Types.path_config list)
    (change_cb : string -> unit Lwt.t)
  : unit Lwt.t =
  let debug_log_lwt (msg : string) = if debug then Lwt_io.eprint msg else Lwt.return () in
  Lwt_inotify.create () >>= fun inotify ->
  path_configs
  |> CCList.map (fun p ->
      Lwt_inotify.add_watch inotify p.local_dir [Inotify.S_Attrib; Inotify.S_Modify]
      >>= fun _ -> Lwt.return ()
    )
  |> Lwt.join >>= fun () ->
  let rec go () =
    Lwt_inotify.read inotify >>= fun event ->
    debug_log_lwt (Printf.sprintf "[livereload] %s%!" (Inotify.string_of_event event)) >>= fun () ->
    let watch, _, _, fname_opt = event in
    let i = (Inotify.int_of_watch watch) - 1 in
    begin
      match (CCList.get_at_idx i path_configs, fname_opt) with
      | (Some {server_path}, Some fname) ->
        let server_path = server_path ^ "/" ^ fname in
        debug_log_lwt (Printf.sprintf "[livereload (inotify)] %s%!" server_path) >>= fun _ ->
        change_cb server_path
      | _ -> Lwt.return ()
    end;
    >>= fun _ -> go ()
  in go ()
