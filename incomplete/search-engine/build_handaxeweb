#!/bin/sh
if ! [ -x /usr/bin/lua ] ; then
   echo "You need Lua installed at /usr/bin/lua to build Handaxeweb" >&2
   exit 1
fi

set -ve
./handaxeweb.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb2.lua

# test new version
lua handaxeweb2.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb3.lua

# try building it with itself:
lua handaxeweb3.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb4.lua

# verify output is the same:
diff handaxeweb3.lua handaxeweb4.lua

# okay, we’ll accept it
cp handaxeweb4.lua handaxeweb.lua

./handaxeweb.lua build_handaxeweb 0 < handaxeweb.md > build_handaxeweb.new
cp build_handaxeweb.new build_handaxeweb
