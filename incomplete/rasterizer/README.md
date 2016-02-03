# `tiny_gfx`: A tiny rasterizer

A *rasterizer* is a piece of software that converts arbitrary shapes
into *raster images*, which is just a funny name for rectangular grids
of pixels. A rasterizer, in some way or another, is at the heart of
pretty much every modern display technology today, from computer
displays to e-ink displays to printers both 2D and 3D (the graphics in
laser shows are the most notable exception).

In this chapter, I will teach you a little about how rasterizers work,
by describing `tiny_gfx`, a simple rasterizer in pure Python. Along
the way we will pick up some techniques that show up repeatedly in
computer graphics code. Having a bit of mathematical background on
linear algebra will help, but I hope the presentation will be
self-contained.

`tiny_gfx` is not practical in many ways. Besides being slow (see
below), the main shortcoming in `tiny_gfx` is that shapes are all of a
single solid color. Still, for 500 lines of code, `tiny_gfx` has a
relatively large set of features:

- alpha blending for semi-transparent colors
- polygonal shapes (possibly concave, but not self-intersecting)
  (TODO: right now only convex polygons are supported)
- circles and ellipses
- transformations of shapes
- boolean operations on shapes (union, intersection, subtraction)
- antialiasing
- empty space skipping and fast rasterization of runs for general
  shapes

Maybe most interestingly, shapes in `tiny_gfx` are extensible. New
shapes are easily added, and they compose well with the other parts of
the code.

A description of the current version of the code is in `doc/README.md`.

## A performance caveat

Rasterizers are so central to display technology that their
performance can make or break a piece of software, and these days the
fastest rasterizers are all based in hardware. Your videogame graphics
card (and even the cheapest smartphones these days) is rasterizing
polygons in highly parallel processors; 192 cores is a typical
number. It should be no surprise, then, that the rasterizer we will
see here is slow: if CPU-intensive tasks in Python run around 50 times
slower than heavily-optimized, low-level code, and if a graphics
driver has around 200 cores at its disposal, a slowdown of 10,000
times should not be surprising. In reality, `tiny_gfx` is closer to
1,000,000 times slower than the special-purpose graphics rasterizer
from the laptop in which I'm developing it.
