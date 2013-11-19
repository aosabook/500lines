Author: Kresten Krab Thorup
Project: Torrent Client
Requirements: Erlang

This directory holds a simple torrent client in Erlang.

In building this, the major challenge is to keep the project under 500
lines, since we're building it almost from first principles.  Right
now it is ~690 lines (of which ~140 are blank or comments); I'll
rework the code and the requirements to fit.

The easiest way to get Erlang, is to download it from www.erlang-solutions.org.
Don't use the Erlang distributed with Ubuntu.

compile:
  ./rebar get-deps
  ./rebar compile

run:
  erl -pa ebin -pa deps/gproc/ebin
  1> torrent_file:download("../path/to.torrent")

