A Tiled Renderer
================

Author: Pierre-Antoine LaFayette (<pierre@alumni.utoronto.ca>)

Requirements
------------

* A modern browser that supports WebGL and a compatible GPU
* Because of the WebGL [security policy](http://www.khronos.org/webgl/security/) on loading images from file:/// URLs, if you want to run locally, you may need to temporarily turn off the same origin policy on your browser. On Chrome this can be done with the command line flag *--disable-web-security*. Please remember to remove the flag for normal browsing.

Abstract
--------

This project will demonstrate how modern browsers use backing stores to implement features such as smooth scrolling and zooming. The renderer is a tile based renderer written in JavaScript using WebGL. The renderer uses the GPU for rasterization through render-to-texture. In this chapter, we'll discuss the benefits of a backing store, tiling, GPU vs CPU rasterization, zooming, and progressive rendering.

Usage
-----

There is a live version running at: [http://tiled-renderer.appspot.com](http://tiled-renderer.appspot.com)

To run locally, open index.html in your browser.
