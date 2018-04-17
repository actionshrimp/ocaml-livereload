# ocaml-livereload

Livereload server in ocaml that can live in your webserver process. File watching uses `inotify` on linux and `osx-fsevents` on macos.

## Setup

- Create a livereload handler for your server process by calling `Livereload.make_handler` along with config about which files to watch.
- Hook up the handler so it's called as part of your server callback.
- Add a script tag to your markup to load the `livereload.js` script served by the created handler into the browser.

## Examples

See:
- `test/test_files_server.ml` for an example of wiring with file watcher.
- `test/test_opium_server.ml` for an example of wiring with an opium app, and opium static middleware. This requires calling through to some lower level opium funcitons at the minute as it requires information about the underlying connection for the websocket to work properly, which is not exposed by Opium currently.

Build / run the examples with e.g. `jbuilder exec -- test/test_files_server.exe`
