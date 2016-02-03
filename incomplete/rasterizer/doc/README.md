# Overview

By far, the most important class in this project is `Shape`, in
`shape.py`.


## `shape.py`

`Shape` is the main class of `tiny_gfx`. The class itself contains a
very basic constructor, and the main code for the scanline rasterizer
itself, in `Shape.draw`.

Concrete subclasses of `Shape` need to implement two methods:
`contains` and `signed_distance_bound`. Both methods take a point in
the plane. `contains` should return true if the shape contains the
point (no surprise there). `signed_distance_bound` is the main method
used by `tiny_gfx` to *accelerate* the rendering. The idea is that
`signed_distance_bound` should return a real value that describes a
*distance certificate*. If this value (call it `r`) is negative, then the shape is
promising that not only is the point is not in the shape, but no
points inside a ball of radius `-r` around the point is in the shape as
well. If the value is positive, then the shape is
promising that not only is the point in the shape, but a ball of
radius `r` is entirely contained in the shape as well. If the shape
cannot guarantee anything either way, it should return zero. In other
words, `signed_distance_bound` is used by `Shape.draw` to skip large
numbers of checks against `contains`, both inside and outside the
shape. It's ok if `signed_distance_bound` is conservative. In fact,
always returning zero would be correct behavior. It would just be
much slower.

Finally, concrete subclasses of `Shape` must have a `self.bound` field
that stores an `AABox` object that represents an axis-aligned bounding
box of the object. The bounding box does not need to be exact, but it
must be *conservative*: `self.contains(p)` must imply
`self.bounds.contains(p)`.  The smaller the area of this box, the more
efficient rasterization will be.

`Shape.draw` works by traversing candidate image pixels inside the
shape bounding box in row order. Anti-aliasing is performed by
jittered sampling (see Figure 5.16
[here](http://www.cs.utah.edu/~shirley/papers/thesis/ch5b.pdf)). Note
that if `Shape.draw` knows that the pixel is entirely inside the image
(because `r` from above is greater than the length of a pixel
diagonal), then no anti-aliasing tests are needed. This allows the
default setting for super-sampling to be to take 36 samples inside a
pixel with no large loss in performance. By the same token, if `r` is
a negative number, then `Shape.draw` skips those pixels with no
further checks. Anti-aliasing is performed by counting the number of
points that pass the `contains` call, and updating the image pixel.



## `geometry.py`

This file contains geometry classes generally needed for the
rasterizer.

* `Vector`, a 2D vector with your basic operator overloading and
  methods. In this code we use this class to store both points and
  vectors. There are reasons why this is a bad idea, but for sake of
  simplicity and brevity, we do it.

* `AABox`, a 2D axis-aligned bounding box

* `HalfPlane`, a class that models one-half of the 2D plane by a
  linear equation
  
* `Transform`, a class for 2D affine transformations of `Vector`s

* utility functions to build transforms (which should perhaps be
  `classmethod`s of Transform, except that using them leads to long,
  unreadable lines for things that should have short names)
  

## `color.py`

* `Color`: Self-contained RGBA class in plain old dumb RGBA.

## `csg.py`

This file contains classes used for Boolean operations with
shapes. (CSG stands for Constructive Solid Geometry, the three-letter
acronym used in graphics for the idea). The base class is `CSG`,
and there's a class for each supported Boolean operation: `Union`,
`Intersection` and `Subtraction`.

If the `signed_distance_bound` and `contains` ideas above makes sense,
then the code for the three classes should be self-explanatory.


## `ellipse.py`

`Ellipse` is the most complicated `Shape` available, and is presented
as an example of the generality of the approach used here. The
internal representation for an ellipse is the implicit equation form,
which defines the ellipse as the set of points for which a certain
function is less than 0. In this case, the function is a quadratic
polynomial in x and y.

`Ellipse.contains` simply evaluates the implicit equation.

The code for `Ellipse.signed_distance_bound` is actually relatively
clever and non-trivial. This flavor of geometric insights is prevalent
in graphics, so I wanted to give an actual example of it in the
code. However, it takes a diagram to show why it works, so I don't
really expect you to understand it without comments.

I have to draw a diagram to explain how it
works, so if you run into trouble on that one, send me a message and
I'll move it to the top of my priority queue.

There's more description of the code for `Ellipse` under `rasterizer/ellipse.md`.

## `image.py`

`PPMImage`: Simple, self-contained class that stores a two-dimensional array of
`Color` pixels, and writes them as a
[PPM file](http://netpbm.sourceforge.net/doc/ppm.html).


## `poly.py`

`ConvexPoly` represents a convex polygon by a set of
half-planes. `Shape.contains` simply tests all half-planes
corresponding to each edge of the polygon, and
`Shape.signed_distance_bound` takes the most conservative value across
all of the shape half-planes. This actually gives values of 0 for
points on the "infinite line" spanned by polygon edges, but that's
fine because the result needs only be conservative.


## `scene.py`

`Scene` stores a hierarchical scene graph, where each node is either a
`Shape` or a `Scene` itself. In addition, each `Scene` object carries
an affine transform that's applied to every element under it. By
having different scenes holding the same object lists with different
transformations, it becomes easy to express scenes with repeated
elements.

