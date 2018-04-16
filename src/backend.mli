val make_watcher : ?debug : bool -> Types.path_config list -> (string -> unit Lwt.t) -> unit Lwt.t
