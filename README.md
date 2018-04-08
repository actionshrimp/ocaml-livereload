# ocaml-livereload

Livereload server in ocaml that can live in your webserver process.

## TODO

- [ ] Work out the best way to hook this into e.g. an [opium](https://github.com/rgrinberg/opium) middleware or to live alongside opium in the http server.
- [ ] Improve file watching API to handle single files and make sure path mapping works correctly
- [ ] Wire together file watcher and reload handler into a single entrypoint

See `test/test_files_server.ml` for an example of wiring with file watcher.
