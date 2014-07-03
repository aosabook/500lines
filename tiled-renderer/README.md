A Tiled Renderer
================

Author: Pierre-Antoine LaFayette <pierre@alumni.utoronto.ca>

Requirements
------------

* A modern browser that supports WebGL and a [compatible GPU](http://www.khronos.org/webgl/wiki/BlacklistsAndWhitelists)
* Because of the [WebGL security policy](http://www.khronos.org/webgl/security/)
on loading images from file:/// URLs, if you want to run locally, you have two
options:
    1. Run a local server from the command line by changing to the
tiled-renderer directory and executing: `python3 -m http.server`. This is the
recommended approach. Use `python -m SimpleHTTPServer` on Python 2.x.
    2. Temporarily turn off the same origin policy on your browser. On Chrome
this can be done with the command line flag `--disable-web-security`. Please
remember to remove the flag for normal browsing as this is a security risk.
* If the demo still fails to load, you may need to disable ad blocking
extensions in your browser as they may interfere with the loading of the
required scripts.

Abstract
--------

This project will demonstrate how modern browsers use backing stores to
implement features such as smooth scrolling and zooming. The renderer is a tile
based renderer written in JavaScript using WebGL. The renderer uses the GPU for
rasterization through render-to-texture. In this chapter, we'll discuss the
benefits of a backing store, tiling, GPU vs CPU rasterization, zooming, and
progressive rendering.

Usage
-----

There is a live version running at:
[http://tiled-renderer.appspot.com](http://tiled-renderer.appspot.com)

On a Mac:

```
cd ~/500lines/tiled-renderer
python3 -m http.server &
open http://localhost:8000
```
