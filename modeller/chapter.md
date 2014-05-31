## Intro
3D modelling software is widely used as a component of Computer Assisted Design (CAD) software. For example, AutoCAD, Maya, Blender, and others all
include a 3D modelling component to their software.

## Modeller structure

### Setting the Scene
The first architectural challenge we encounter when designing a 3d modeller is the representation of the objects in the scene.
We would like to design the scene so that it can store all types of objects that we want to include, and so that it can be easily extended to
store new types of objects.

We use a base class to represent an object that can be placed in the scene, called a `Node`. This base class allows
us to reason about the scene abstractly. The scene object doesn't need to know about the details of every type of object it displays,
it only needs to know that it contains a list of nodes. Each type of `Node` defines its own behaviour for rendering itself and for any other necessary
interactions.
In this project,`Sphere` and a `Cube` available. More shapes can be added easily by extending the Node class again.

The abstract Node class contains all of the logic common to all nodes. In this project, most of the code is common.
The Primitive class contains the code to render a primitive. It requires a call list name for rendering. An OpenGL Call List is a series of
OpenGL calls that are bundled together and named. The calls can be dispatched with `glClassList(LIST_NAME)`.
Finally, the concrete primitives (`Sphere` and `Cube`) define the call list required to render them. They could also specialize any of the other `Node` behaviour, if necessary.

Using a class structure like this means that the `Node` class is easily extensible. As an example of the extensibility, consider adding a `Node` type that combines multiple
primitived, like a figure for a character. We can easily extend the `Node` class for this situation by creating a new class `class Figure(Node)` which will override some of
the functionality of the `Node` manage a list of sub-nodes.

By making the `Node` class extensible in this way, we are able to add new types of shapes to the scene without changing any of the other code around scene
manipulation and rendering.

### Rendering
Now that we have an abstract representation of the objects in the scene, we would like to draw the scene to the screen.

#### OpenGL
OpenGL is a graphical application programming interface for cross-platform development. It's the standard API for developing graphics applications across platforms.
OpenGL is two major variants. They are "Legacy OpenGL" and "Modern OpenGL".

Rendering in OpenGL is based on polygons defined by vertices and normals. For example, to render one side of a cube, one would specify the 4 vertices and the normal of the side.

Legacy OpenGL provides a "fixed function pipeline". By setting global variables, the programmer can enable and disable automatic features such
as lighting, coloring, face culling, etc. OpenGL then automatically renders the scene with the enabled functionality. This functionality is deprecated.

Modern OpenGL, on the other hand, features a programmable rendering pipeline where the programmer writes small programs called "shaders" that
run on dedicated graphics hardware (GPUs). Most features of Modern OpenGL were introduced in version 2.1. The programmable
pipeline of Modern OpenGL has replaced Legacy OpenGL. In this programmable pipeline, the programmer must
write the code to calculate anything that needs to be rendered. For example, the programmer is responsible for calculating the color of each
polygon. The shader program will use the positions of the lights in the scene, the direction they're facing, and other parameters to calculate
the final color of each polygon in the scene. (This is a simplified description, but it gives and idea of the differences between the variants).

In this project, despite the fact that it is deprecated, we use Legacy OpenGL. The fixed functionality provided by Legacy OpenGL is very useful for keeping
code size small. It reduces the amount of linear algebra knowledge required, and it simplifies the code we will write. Most production
software has moved on to using Modern OpenGL.

##### OpenGL's State Machine
Legacy OpenGL can be considered a State Machine. The API to enable/disable functionality modifies the current state of the OpenGL machine.
When a polygon render call is made, the current state of the machine is used.
OpenGL also stores two matrices.  These are called the "ModelView" matrix and the "Projection" matrix.
The ModelView matrix determines the transformation of the current polygon within the scene. The Projection matrix is used to project that polygon onto the screen.
The matrices are manipulated with `glMultMatrix`, which multiplies the current matrix by the matrix parameter.
OpenGL also maintains a stack of matrices. The programmer can choose to push and pop from this stack. This facilitates traversing a scene graph for rendering purposes, as we will discuss later.

Most of the setup and interfacing with OpenGL is found in the viewer.py file.

### Linear algebra (TODO)
#### Matrix arithmetic (TODO: is this necessary?)
 * what is a matrix?
 * what is a projection matrix?
 * what is a ModelView matrix?
 * The 4th vector element to distinguish points from vectors

The purposes of the ModelView and Projection matrices can be understood with some basic linear algebra. Detailed explanations can be found here: (TODO!!!).

TODO: Should we put some linear algebra here? This topic is covered in every OpenGL tutorial, but it will be unfamiliar to many people. I'm not sure if it's best to redirect to another tutorial,
or have an explanation here.


#### Traversing the Scene
We leverage the data structure of the scene for rendering. The render function of the scene traverses the list of `Node` in the scene and
calls the `render` function for each `Node`. We make sure of the aforementioned OpenGL Push and Pop Matrix functions for `Node` rendering.
For each `Node`, the steps to render the `Node` are:

```
Push the current OpenGL ModelView Matrix onto the stack
Apply the transformation to OpenGL
Draw the node
Pop the OpenGL ModelView matrix
```

The `PushMatrix` and `PopMatrix` functions in OpenGL provide us access to a stack object for saving the status of the matrix.
This suits us perfectly, as we're traversing a graph of nodes.
The OpenGL matrix stack is used to store the matrix state of each `Node` when it is rendered.

Manipulating the ModelView matrix allows us to have a single render list for each type of primitive. For example, the render list for the Cube primitive draws a cube at the origin with sides of length 1.
By setting the OpenGL matrix, we can change the size and location the rendered cube.

Again, the Matrix stack functionality of OpenGL allows us to extend the `Node` class to contain nested nodes. If there are a nested nodes, we simply
push onto the stack before we render a nested node.

Thus, using the scene traversal and OpenGL Matrix Stack allows us to implemented the `Node` class in an extensible way, and allows each `Node`'s render code
to be independent from its location in the scene.


### User Interaction
Now that we're able to render the scene, we want to be able to add new Nodes, and to adjust the Nodes in the scene.
We encapsulate the user interaction code into its own class: `Interaction`. The `Viewer` class, which drives the scene and the rendering, owns the instance of `Interaction`.


#### Callbacks
The `Interaction` class maintains a very simple callback system in the form of a dictionary, `callbacks`, to call in certain situations.
The viewer class registers callbacks on the `Interaction` instance by calling `register_callback`.
```
def register_callback(self, name, func):
    self.callbacks[name].append(func)
```
When user interface code needs to trigger an event on the scene, the `Interaction` class calls all of the saved callbacks it has:

```
def trigger(self, name, *args, **kwargs):
    for func in self.callbacks[name]:
        func(*args, **kwargs)
```

This simple callback system provides all of the functionality we need for this project. In a production 3d modeller, however, user interface objects are often created and destroyed dynamically.
In the case where user interface objects are created and destroyed, we would need a more sophisticated event listening system, where objects can both register and un-register callbacks for events.

#### GLUT
GLUT is the OpenGL Utility Toolkit. Is is bundled with OpenGL and it provides a simple windowing API and user interface callbacks. The basic functionality it
offers is sufficient for our purposes in this project. If we wanted a more full featured library for window management and user interaction, we would consider using
a full featured game engine like PyGame. GLUT allows us to register callbacks for user input using the following functions:

```
    glutMouseFunc(self.handle_mouse_button)
    glutMotionFunc(self.handle_mouse_move)
    glutKeyboardFunc(self.handle_keystroke)
    glutSpecialFunc(self.handle_keystroke)
```

#### Moving the Camera
There are two camera controls available in this project. In this project, we accomplish camera motion by transforming the scene. In other words, the
camera is at a fixed location and the camera controls actually move the scene instead of moving the camera. The camera is placed at `[0, 0, -15]` and
faces the origin. We could alternatively change the perspective matrix to move the camera instead of the scene.
This design decision has very little impact on the rest of the project. We move the scene instead of the camera because it is the standard practise.
There are two types of interaction with the scene: rotation and translation.

##### Rotation via a Trackball
We accomplish rotation of the scene by using a Trackball algorithm. The trackball is an intuitive interface for manipulating the scene in 3 dimensions.
Conceptually, a trackball interface functions as if the scene was inside a transparent globe. Placing a hand on the surface of the globe and pushing it rotates the globe. Similarly, clicking the right mouse button and moving it on the screen rotates the scene.
You can find out more about the theory of the trackball at the [OpenGL Wiki](http://www.opengl.org/wiki/Object_Mouse_Trackball).
In this project, we use a trackball implementation provided as part of [Glumpy](https://code.google.com/p/glumpy/source/browse/glumpy/trackball.py). It's available in Appendix ??? (TODO: this?).

We interact with the trackball using the `drag_to` function with the starting and ending x and y as parameters.

```
self.trackball.drag_to(self.mouse_loc[0], self.mouse_loc[1], -dx, -dy)
```
The resulting rotation matrix is retrieved as `trackball.matrix` in the viewer when the scene is rendered.

Rotations are traditionally represented in one of two ways. The first is a rotation value around each axis. You could store this as a 3-tuple of floating point numbers.
The other common representation for rotations is a quaternion. Using quaternions has numerous benefits over per-axis rotation. In particular, they are more numerically stable. Using quaternions avoids some tricky problems like [Gimbal Lock](http://en.wikipedia.org/wiki/Gimbal_lock).
The unfortunate downside of quaternions is that they are less intuitive to work with and harder to understand. If you are brave and would like to learn more about quaternions, you can refer to [this explanation](http://3dgep.com/?p=1815).

The trackball implementation avoids Gimbal Lock by using quaternions internally to store the rotation of the scene. Luckily, we do not need to work with quaternions directly, because the matrix member on the trackball
converts the rotation to a matrix.
We do not need to concern ourselves with this detail, because the trackball library provides a method to get the matrix representation of a rotation.

##### Translation
Scene translation is much simpler than scene rotation. Scene translations are provided with the mouse wheel and the left mouse button. The left mouse
button translates the scene in the x and y coordinates. Scrolling the mouse wheel translates the scene in the z coordinate
(towards or away from the camera). The `Interaction` class stores the current camera location, and modifies it with:
```
def translate(self, x, y, z):
    self.camera_loc[0] += x
    self.camera_loc[1] += y
    self.camera_loc[2] += z
```
The viewer retrieves the `Interaction` camera location during rendering to use in a `glTranslated` call.

#### Picking
In this project, we implement a very simple ray-based picking [algorithm](http://www.opengl-tutorial.org/miscellaneous/clicking-on-objects/picking-with-custom-ray-obb-function/). Each node stores an Axis-Aligned Bounding Box which is an approximation of the
space it occupies. When the user clicks in the window, we use the current projection matrix to generate a ray that represents the mouse click, as if the mouse pointer shoots a ray into the scene.
```
# viewer.py, line 138
def get_ray(self, x, y):
    """ Generate a ray beginning at the near plane, in the direction that the x, y coordinates are facing
        Consumes: x, y coordinates of mouse on screen
        Return: start, direction of the ray """
    self.init_view()

    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

    # get two points on the line.
    start = numpy.array(gluUnProject(x, y, 0.001))
    end = numpy.array(gluUnProject(x, y, 0.999))

    # convert those points into a ray
    direction = end - start
    direction = direction / norm(direction)

    return (start, direction)
```
To determine which Node was clicked on, we traverse the scene to test whether the ray intersects with each Node's Bounding Box. We choose the Node with the intersection closest to the ray origin and store it as the selected node.
```
# scene.py, line 30
def pick(self, start, direction, mat):
    """ Execute selection.
        Consume: start, direction describing a Ray
                 mat              is the inverse of the current modelview matrix for the scene """
    if self.selected_node is not None:
        self.selected_node.select(False)
        self.selected_node = None

    # Keep track of the closest hit.
    mindist, closest_node = sys.maxint, None
    for node in self.node_list:
        hit, distance = node.pick(start, direction, mat)
        if hit and distance < mindist:
            mindist, closest_node = distance, node

    # If we hit something, keep track of it.
    if closest_node is not None:
        closest_node.select()
        closest_node.depth = mindist
        closest_node.selected_loc = start + direction * mindist
        self.selected_node = closest_node
```

The Ray-AABB selection approach is very simple to understand and implement. However, the results are wrong in certain situations. For example, in the `Sphere` primitive, the sphere itself only touches
the AABB in the centre of each of its planes. However if the user clicks on the corner of the Sphere's AABB, the collision will be detected with the Sphere, even if the user intended to click
past the Sphere onto something behind it.

To address this limitation, a production modeller would use a more sophisticated selection algorithm. These algorithms are usually based on Ray-Object intersection, but there are many improvements that
can be made over the AABB implementation. For example, the picking algorithm could do an exact intersection test with each type of Node. Doing exact intersection means that each type of Node must have its own
implementation of Ray intersection. Intersection with arbitrary objects is much more complex than AABB intersection, so there is also a performance penalty for using exact intersection. The performance
penalty can be offset by using increasingly sophisticated algorithms for collision detection. Often, these will involve partitioning the scene, and only testing for intersection in partitions that are hit by the ray.

#### Transforming Nodes
A selected node can be moved, resized, or colorized.

##### Color
Colorization is accomplished with a very simplistic list of possible colors. The user can cycle through the colors with the arrow keys. The selected
color is passed to OpenGL with `glColor` when the Node is rendered.

##### Scale
Each Node stores a current matrix that stores its scale. A matrix that scales by parameters `x`, `y` and `z` in those respective directions is:

![Scale Matrix](scale.png?raw=true)

The function `scaling` returns such a matrix, given a list representing the `x`, `y`, and `z` scaling factors.
When the user modifies the scale of a Node, the resulting scaling matrix is multiplied into the current scaling matrix for the Node.

```
# node.py, line 35
def scale(self, up):
    s =  1.1 if up else 0.9
    self.scalemat = numpy.dot(self.scalemat, scaling([s, s, s]))
    self.aabb.scale(s)
```

##### Translation
In order to translate a node, we use the same ray calculation from picking. We pass the ray that represents the current mouse location in to the scene's
`move` function. The new location of the Node should be on the ray.
In order to determine where on the ray to place the Node, we need to know the Node's distance from the camera. Since we stored the Node's location and distance
from the camera when it was selected (in the `pick` function), we can use that data here.
We find the point that is the same distance from the camera along the target ray and we calculate the vector difference between the new and old locations.
We then translate the `Node` by the resulting vector.

```
# scene.py, line 52
def move(self, start, direction, inv_modelview):
    """ Move the selected node, if there is one.
        Consume:  start, direction  describes the Ray to move to
                  mat               is the modelview matrix for the scene """
    if self.selected_node is None: return

    # Find the current depth and location of the selected node
    node = self.selected_node
    depth = node.depth
    oldloc = node.selected_loc

    # The new location of the node is the same depth along the new ray
    newloc = (start + direction * depth)

    # transform the translation with the modelview matrix
    translation = newloc - oldloc
    pre_tran = numpy.array([translation[0], translation[1], translation[2], 0])
    translation = inv_modelview.dot(pre_tran)

    # translate the node and track its location
    node.translate(translation[0], translation[1], translation[2])
    node.selected_loc = newloc
```

As with scale, each node stores a matrix which represents its translation. A translation matrix looks like:

![Translation Matrix](translate.png?raw=true)

When the node is translated, we construct a new translation matrix for the
current translation, and multiply it into the Node's translation matrix.

```
# node.py, line 26
def translate(self, x, y, z):
    self.translation = numpy.dot(self.translation, translation([x, y, z]))
```

#### Placing Nodes
Node placement uses techniques from both picking and translation. We use the same ray calculation for the current mouse location to determine where to place the node.
To place a new node, we calculate the generate a ray that represents the mouse cursor in the scene.
We create a new node which is originally at the origin, and we translate it to a point on the ray, a fixed distance from the camera.

```
def place(self, shape, start, direction, inv_modelview):
    """ Place a new node.
        Consume:  shape             the shape to add
                  start, direction  describes the Ray to move to
                  inv_modelview     is the inverse modelview matrix for the scene """
    new_node = None
    if shape == 'sphere': new_node = Sphere()
    elif shape == 'cube': new_node = Cube()

    self.add_node(new_node)

    # place the node at the cursor in camera-space
    translation = (start + direction * self.PLACE_DEPTH)

    # convert the translation to world-space
    pre_tran = numpy.array([translation[0], translation[1], translation[2], 1])
    translation = inv_modelview.dot(pre_tran)

    new_node.translate(translation[0], translation[1], translation[2])
```
