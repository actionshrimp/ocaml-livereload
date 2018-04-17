open Lwt
open Types


let make_watcher
    ?(debug = false)
    (path_configs : Types.path_config list)
    (change_cb : string -> unit Lwt.t)
  : unit Lwt.t =
  let debug_log_lwt (msg : string) = if debug then Lwt_io.eprint msg else Lwt.return () in

  let create_flags = Fsevents.CreateFlags.detailed_interactive in
  let watcher = Fsevents_lwt.create 0. create_flags (CCList.map (fun { local_dir } -> local_dir ) path_configs) in
  let event_stream = Fsevents_lwt.event_stream watcher in
  let stream = Fsevents_lwt.stream watcher in
  let run_loop_mode = Cf.RunLoop.Mode.Default in

  let cb_f event =
          (* TODO: handle path mapping back to server path *)
          let local_path = event.Fsevents_lwt.path in
          debug_log_lwt (Printf.sprintf "[livereload (osx-fsevents)] %s%!" local_path) >>= fun _ ->
          change_cb local_path >>= fun _ ->
          Lwt.return ()

  in

  Lwt.async (fun () -> Lwt_stream.iter_s cb_f stream);

  Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents.schedule_with_run_loop event_stream runloop run_loop_mode;
      if not (Fsevents.start event_stream)
      then prerr_endline "Failed to start FSEvents stream"
    ); >>= fun _ -> Lwt.return ();
