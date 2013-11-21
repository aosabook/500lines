Author: Kresten Krab Thorup
Project: Torrent Client
Requirements: Erlang

This directory holds a simple torrent client in Erlang. The client only works
for single-file torrents that use a http tracker.

In building this, the major challenge is to keep the project under 500
lines, since we're building it almost from first principles.  Right
now it is ~542 lines (excluding blank or comments lines).

The easiest way to get Erlang, is to download it from www.erlang-solutions.org.
Don't use the Erlang distributed with Ubuntu.

compile:
  ./rebar get-deps
  ./rebar compile

run:
  erl -pa ebin -pa deps/*/ebin
  1> torrent_client:download("../path/to.torrent")

