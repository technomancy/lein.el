# lein.el

A no-startup-delay eshell replacement for the `lein` bash script from
[Leiningen](http://leiningen.org).

## Usage

Load it, then launch <kbd>M-x eshell</kbd>. The first `lein` invocation
will launch a Leiningen subprocess via nrepl, and subsequent
invocations will be sent to that instead of starting a new one.

## Gotchas

Plugins for each project will be loaded repeatedly, so there's a
chance for conflicts to arise.

## License

Copyright © 2013 Phil Hagelberg

Distributed under the GNU General Public License; see C-h t to view.
