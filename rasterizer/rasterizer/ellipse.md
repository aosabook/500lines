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

The boundary of the ellipse is exactly the set of points where v(x,y)
= 0. When we evaluate a point x,y and get a value greater than zero,
we consider that point to be outside the ellipse. Most of the ellipse
class will involve manipulating this implicit equation.


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

Our shape class needs a bounding box. The partial derivative measures
the rate of change of the function with respect a variable.  When the
implicit equation of the ellipse is zero, *and* the rate of change of
the implicit function with respect to x is zero, then ellipse boundary
is vertical. So we solve the system of equations df/dx(x, y) = 0, v(x,
y) = 0, and the two solutions are going to be touching the top and
bottom of the bounding box exactly. Solving the system of equations
df/dy(x, y) = 0, v(x, y) = 0 gives the left and right points.


## Transforming an ellipse

Say we're given an affine transformation T(x, y) = M . (x, y) = (x',
y') that we need to apply to the ellipse to get a new ellipse. The
straightforward way is to transform query points every time we want to
test them against the new ellipse. That is, if we translated an
ellipse by (10,0), then we need to transform the query points by the
*inverse* of T. To see this, think of the translated ellipse
above. Its new center is at (10,0), which means that query points at
(10,0) need to be transformed to (0,0) on the old coordinate
system. This approach works, but it only gives us the `contains`
primitive. Worse yet, affine transformations do not preserve distances
to shapes, so we cannot hope to make a query to
`signed_distance_bound` and then invert that result somehow. Instead,
we will use the inverse transformation to compute entirely new
coefficients. We write (x, y) = M^-1(x', y'), which gives us an
expression of the new coordinates in terms of the old ones, and then
recreate the quadratic polynomial. This is tedious but
straightforward algebra that goes sort of like this:

x = m00 x' + m01 y' + m02
y = m10 x' + m11 y' + m12
(where m00, m01, etc. are the coefficients of the inverse matrix)

f(x, y) = ax^2 + by^2 + cxy + dx + ey + f
f(x, y) = a(m00 x' + m01 y' + m02)^2 + b(m10 x' + m11 y' + m12)^2
        + ...

Then you collect all terms that are quadratic in x' and call then a',
all the terms quadratic in y' and call them b', etc. You end up with

f'(x', y') = a'x'^2 + b'y^2 + c'x'y' + d'x' + e'y' + f'

In the code, the variables a', b', c', etc. are denoted respectively
as aa, bb, cc, etc.

## Bounding the distance to an ellipse

The idea is that the distance from a point to an ellipse is not
exactly trivial to compute, and requires numerical optimization. We
don't want to waste lines of code on a numerical optimizer and an
ugly(ier?) function. Instead, we can get a good bound quickly by:

* computing the intersection between the point `p` and the ellipse
center (call this intersection `i`)
* finding a line that goes through `i` in the direction of the normal
 of the ellipse (call this line `n`)
* project the `p` onto the `i` (call it `s`)
* return the distance between `i` and `s`

Referring to the [diagram](../doc/ellipse_1.svg), `p` is outside the ellipse,
`t` is the line tangent to `i`, and `e` is the closest point in the
ellipse to `p`. Notice that the distance between `s` and `i` is equal
to the distance between `p` and `s'`, since they form parallel
lines. Now take the line from `p` to `e`. Notice that it intersects
the line from `i` to `s'`, at a point we will call `e'`. Now notice
that `e'`, `s'` and `p` form a right triangle, of which the line from
`p` to `e'` is the hypotenuse (and it's *shorter* than the original
line from `p` to `i`). This shortened line, in turn, is yet shorter than the leg
`p` to `s'`. But length of this leg is precisely the distance from `s`
to `i`, and so we know that it's a lower bound to the distance from
`e` to `p`, and we are done.

If `p` is inside the ellipse, then the problem is simpler. We first
look for a convex polygon that inscribes the ellipse and contains
`p`. This is simple: we simply shoot rays in the horizontal and
vertical directions from `p` and collect the intersection points of
those rays and the ellipse. Because ellipses are convex, a polygon
that connects those intersections will be entirely inside the
ellipse. Then, we simply return the signed distance bound for
that polygon.
