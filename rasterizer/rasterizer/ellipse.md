# The ellipse class

The ellipse class is the most complicated shape of the system. I
include it to show how complex shapes can be incorporated in the
tiny_gfx. This section will require more mathematical background than
the other sections, but not more than calculus.

We use an *implicit equation* for the ellipse. This is a function of
the input coordinates x and y, which will return a positive value if
the point lies outside the shape. Any ellipse can be represented by
the following polynomial in x and y:

v(x, y) = a x^2 + b y^2 + c x y + d x + e y + f

Most of the class will 

## Finding the center of the ellipse

We will need the center of the ellipse to compute the signed distance
bound below. By a happy accident, the gradient of the implicit ellipse
function is an affine transformation of the input vector, and the
center of the ellipse is the only point in the function where the
gradient is zero. To find the input vector whose gradient is zero,
then, we invert the transformation and multiply by Vector(0, 0).

In a real system, you wouldn't solve a system Ax=b by explicitly
finding the inverse: it is slower than necessary and numerically
unstable. We use the inverse here for convenience (we already need the
inverse of a transformation in order to transform the ellipse, see
below) and to fit in the 500 line budget.

## Finding the bounding box of the ellipse

Our shape class needs a bounding box. Take the partial with respect to
x to find where the ellipse boundary is vertical, df/dx(x, y) = 0, and
v(x, y) = 0, so that the point is on the boundary. The solutions will
give the leftmost and rightmost points of the ellipse. solving the
system of equations df/dy(x, y) and v(x, y) = 0 gives the top and
bottom.

## Transforming an ellipse

Say we're given an affine transformation T(x, y) = M . (x, y) = (x', y') that we
need to apply to the ellipse to get a new ellipse. This means that the
implicit equation for the new ellipse needs to be written as a
function of x' and y', but our current equation is a polynomial in x
and y. To get the new equation, we need to write x and y themselves as
functions of x' and y'

T(x, y) = (x', y')

## Bounding the distance to an ellipse

The idea is that the distance from a point to an ellipse is not
exactly trivial to compute, and requires numerical optimization. We
don't want to waste lines of code on a numerical optimizer and an
ugly(ier?) function. Instead, we can get a good bound quickly by:

* computing the intersection between the point `p` and the ellipse
center (call this intersection `i`)
* finding a line that goes through `i` in the direction of the normal
 of the ellipse (call this line `n`)
* project the `p` onto the `l` (call it `s`)
* return the distance between `l` and `s`

The proof that this is a lower bound of the distance to an ellipse is
easy, but it takes a diagram to make it obvious. I'll make one soon.
