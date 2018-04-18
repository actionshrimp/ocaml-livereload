open Lwt
open Types

let watcher_ref = ref None

let make_watcher
    ?(debug = false)
    (path_configs : Types.path_config list)
    (change_cb : string -> unit Lwt.t)
  : unit Lwt.t =
  let debug_log_lwt (msg : string) = if debug then Lwt_io.eprint msg else Lwt.return () in

  let create_flags = Fsevents.CreateFlags.detailed_interactive in
  let watcher = Fsevents_lwt.create 0. create_flags (CCList.map (fun { local_dir } -> local_dir ) path_configs) in
  let () = watcher_ref := Some watcher in (* Don't GC me! *)
  let run_loop_mode = Cf.RunLoop.Mode.Default in

  Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents_lwt.schedule_with_run_loop watcher runloop run_loop_mode;
      if not (Fsevents_lwt.start watcher)
      then prerr_endline "Failed to start FSEvents stream"
    ) >>= fun _rl ->

  Fsevents_lwt.stream watcher
  |> Lwt_stream.iter_s (fun event ->
      (* TODO: handle path mapping back to server path *)
      let local_path = event.Fsevents_lwt.path in
      debug_log_lwt (Printf.sprintf "[livereload (osx-fsevents)] %s%!" local_path) >>= fun _ ->
      change_cb local_path >>= fun _ ->
      Lwt.return ())
