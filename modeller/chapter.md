## Intro
3D modelling software is widely used as a component of Computer Assisted Design (CAD) software. For example, AutoCAD, Maya, Blender, and others all
include a 3D modelling component to their software.

## Modeller structure

### Setting the Scene
The first architectural challenge we encounter when designind a 3d modeller is the representation of the objects in the scene.
We would like to design the scene so that it can store all types of objects that we want to include, and so that it can be easily extended to
store new types of objects.
We use a base class to represent an object that can be placed in the scene, called a "node". Using the general concept of a node allows
us to reason about the scene in an abstract manner. The scene object doesn't need to know about the details of every type of object it displays.
It only needs to know that it contains a list of nodes. Each type of node then defines its own behaviour for rendering itself and for any other necessary
interactions.
In this project, the nodes are all primitive shapes. We have a Sphere and a Cube available. More shapes can be added easily by extending the Node class again.

The abstract Node class contains all of the logic common to all nodes. In this project, most of the code is common.
The Primitive class contains the code to render a primitive. It requires a call list name for rendering. An OpenGL Call List is a series of
OpenGL calls that are bundled together and named. The calls can be dispatched with `glClassList(LIST_NAME)`.
Finally, the concrete primitives (Sphere and Cube) define the call list required to render them. They could also specialize any of the other Node behaviour, if necessary.

Using a class structure like this means that the Node class is easily extensible. As an example of the extensibility, consider adding a new Node type
containing multiple primitives, say a snow figure. It would have 3 white Spheres, and a few small colored Spheres to add features. We can easily
extend the node class for this situation by creating a new class `class SnowFigure(Node)` which will override some of the functionality of the Node.

It would contain multiple sub-nodes that represent the figure.  Most call will simply dispatch to all of the sub-nodes: `render`, `translate`, `scale`.
To disable changing the color, we simply make `rotate_color` a no-op. Finally, `pick` remains unchanged, since it is used for selecting the entire
figure.

By making the Node class extensible in this way, we are able to add new types of shapes to the scene without changing any of the other code around scene
manipulation and rendering.

### Rendering
Now that we have an abstract representation of the objects in the scene, we would like to draw the scene to the screen.

#### OpenGL
OpenGL is a graphical application programming interface for cross-platform development. It's the standard API for developing graphics applications across platforms.
OpenGL is two major variants. They are "Legacy OpenGL" and "Modern OpenGL".

Rendering in OpenGL is based on polygons defined by vertices and normals. For example, to render one side of a cube, one would specify the 4 vertices and the normal of the side.

Legacy OpenGL provides a "fixed function pipeline". By setting global variables, the programmer can enable and disable automatic features such
as lighting, coloring, face culling, etc. OpenGL then automatically renders the scene with the enabled functionality.

Modern OpenGL, on the other hand, features a programmable rendering pipeline where the programmer writes small programs called "shaders" that
run on dedicated graphics hardware (GPUs). Most features of Modern OpenGL were introduced in version 2.1. The programmable
pipeline of Modern OpenGL has replaced Legacy OpenGL, which is considered deprecated. In this programmable pipeline, the programmer must
write the code to calculate anything that needs to be rendered. For example, the programmer is responsible for calculating the color of each
polygon. The shader program will use the positions of the lights in the scene, the direction they're facing, and other parameters to calculate
the final color of each polygon in the scene. (This is a simplified description, but it gives and idea of the differences between the variants).

In this project, despite the fact that it is deprecated, we use Legacy OpenGL. The fixed functionality provided by Legacy OpenGL is very useful for keeping
code size small. It reduces the amount of linear algebra knowledge required, and it simplifies the code we will write. Most production
software has moved on to using Modern OpenGL.

Legacy OpenGL can be considered a State Machine. The API to enable/disable functionality modifies the current state of the OpenGL machine.
When a polygon render call is made, the current state of the machine is used.
OpenGL also stores two matrices.  These are called the "ModelView" matrix and the "Projection" matrix.
The ModelView matrix determines the transformation of the current polyon within the scene. The Projection matrix is used to project that polygon onto the screen.
The matrices are manipulated with "glMultMatrix", which multiplies the current matrix by the matrix parameter.
OpenGL also maintains a stack of matrices. The programmer can choose to push and pop from this stack. This facilitates traversing a scene graph for rendering purposes, as we will discuss later.

Most of the setup and interfacting with OpenGL is found in the viewer.py file.

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
We leverage the data structure of the scene for rendering. The render function of the scene traverses the list of Nodes in the scene and
calls the `render` function for each node. We make sure of the aforementioned OpenGL Push and Pop Matrix functions for node rendering.
For each node, the steps to render the node are:

```
Push the current OpenGL ModelView Matrix onto the stack
Apply the transformation to OpenGL
Draw the node
Pop the OpenGL ModelView matrix
```

The `PushMatrix` and `PopMatrix` functions in OpenGL provide us access to a stack object for saving the status of the matrix.
This suits us perfectly, as we're traversing a graph of nodes.
The OpenGL matrix stack is used to store the matrix state of each node when it is rendered.

This allows us to have a single render list for each node. For example, the render list for the Cube primitive draws a size 1 cube at the origin.
By setting the OpenGL matrix, we can change the size and location the rendered cube.

Again, the Matrix stack functionality of OpenGL allows us to extend the Node class to contain nested nodes. If there are a nested nodes, we simply
push onto the stack before we render a nested node.

Thus, using the scene traversal and OpenGL Matrix Stack allows us to implemented the Node class in an extensible way, and allows each Node's render code
to be independent from its location in the scene.


### User Interaction
How that we're able to render the scene, we want to be able to add new Nodes, and to adjust the Nodes in the scene.

#### GLUT
GLUT is the OpenGL Utility Toolkit. Is is bundled with OpenGL and it provides a simple windowing API and user interface callbacks. The basic functionality it
offers is sufficient for our purposes in this project. If we wanted a more full featured library for window management and user interaction, we would consider using
a full featured game engine like PyGame.

GLUT is very limited in its ability to provide
* What are some limitations of glut? Some reasons you might use PyGame?

#### Moving the Camera
There are two camera controls available in this project. In this project, we accomplish camera motion by tranforming the scene. In other words, the
camera is at a fixed location and the camera controls actually move the scene instead of moving the camera. The camera is placed at `[0, 0, -15]` and
faces the origin. In a different implementation, we could change the perspective matrix to effectively move the camera instead of the scene.
This design decision has very little impact on the rest of the project. We move the scene instead of the camera because it is the standard practise.
There are two types of interaction with the scene: rotation and translation.

##### Rotation via a Trackball
We accomplish rotation of the scene by using a Trackball algorithm. The trackball is an intuitive interface for manipulating the scene in 3 dimensions.
Conceptually, a trackball interface functions as if the scene was inside a transparent globe. Placing a hand on the surface of the globe and pushing it rotates the globe. Similarly, clicking the right mouse button and moving it on the screen rotates the scene.
You can find out more about the theory of the trackball in [http://www.opengl.org/wiki/Object_Mouse_Trackball].
In this project, we use a trackball implementation provided as part of Glumpy. It's available in Appendix ??? (TODO: this?).

Rotations are traditionally represented in one of two ways. The first is a rotation value around each axis. You could store this as a 3-tuple of floating point numbers.
The other common representation for rotations is a quaternion. Using quaternions has numerous benefits over per-axis rotation. In particular, they are more numerically stable. Using quaternions avoids some tricky problems like [http://en.wikipedia.org/wiki/Gimbal_lock](Gimbal Lock).
The unfortunate downside of quaternions is that they are much less intuitive and much harder to understand. If you would like to learn more about quaternions, you can check out [??? TODO]

The trackball implementation avoids Gimbal Lock by using quaternions internally to store the rotation of the scene.
We do not need to concern ourselves with this detail, because the trackball library provides a method to get the matrix representation of a rotation.
We use this method to set the new rotation on the scene whenever the user interacts with the trackball.

##### Translation
Scene translation is much simpler than scene rotation. Scene translations are provided with the mouse wheel and the left mouse button. The left mouse
button translates the scene in the x and y coordinates. Scrolling the mouse wheel translates the scene in the z coordinate
(towards or away from the camera).
The `Interaction` class stores the current state of translation for the scene, which the viewer uses in a `glTranslate` call during rendering.

#### Picking
Several techniques can be used for selecting Nodes in a modeller. One simple technique is to render each nodes in a unique color into a hidden buffer, then query the scene for the color of the pixel under the cursor.
This technique is very accurate, but there is a high performance cost to pay for it. Rendering the scene requires many round trips of reading and writing to video memory, which is an expensive operation. Therefore, most production
modellers favour a selection algorithm that leverages the scene data structure.

In this project, we implement a very simple ray-based picking algorithm. Each node stores an Axis-Aligned Bounding Box which is an approximation of the
space it occupies. When the user clicks in the window, we use the current projection matrix to generate a ray that represents the mouse click, as if the mouse pointer shoots a ray into the scene.
To determine which Node was clicked on, we test whether the ray intersects with each Node's Bounding Box. We choose the Node with the intersection closest to the ray origin.
The Ray-AABB selection approach is very simple to understand and implement. However, it sometimes gives the wrong answer. Think about the Sphere primitive. The Sphere itself only touches
the AABB in the centre of each of its planes. However if the user clicks on the corner of the Sphere's AABB, the collision will be detected with the Sphere, even if the user intended to click
past the Sphere onto something behind it.

To address this limitation, a production modeller will use a more sophisticated selection algorithm. These algorithms are usually based on Ray-Object intersection, but there are many improvements that
can be made over the AABB implementation. For example, the picking algorithm could do an exact intersection test with each type of Node. Doing exact intersection means that each type of Node must have its own
implementation of Ray intersection. Intersection with arbitrary objects is much more complex than AABB intersection, so there is also a performance penalty to be paid for using exact intersection. The performance
penalty can be assuages by using increasingly sophisticated algorithms for collision detection. Often, these will involve partitioning the scene into Cubes, and only testing for intersection in cubes that the ray
actually hits.

Considering the complexity, accuracy and speed tradeoffs, the Ray-AABB picking algorithm is the one best suited to this small 3d modeller project.

#### Transformating Nodes
A selected node can be moved, resized, or colorized.

##### Color
Colorization is accomplished with a very simplistic list of possible colors. The user can cycle through the colors with the arrow keys. The selected
color is passed to OpenGL with `glColor` when the Node is rendered.

##### Scale
Each Node stores a current matrix that stores its scale. A matrix that scales by parameters `x`, `y` and `z` in those respective directions is:

![Scale Matrix](scale.png?raw=true)

When the user modifies the scale of a Node, a scaling matrix for the modification is constructed and multiplied into the current scaling matrix for
the Node.

##### Translation
In order to translate a node, we use the same ray calculation from picking. We pass the ray that represents the current mouse location in to the scene's
`move` function. The new location of the Node should be on the ray.
In order to determine where on the ray to place the Node, we need to know the Node's distance from the camera. Since the Node's location and distance
from the camera are available when it's selected, we store that information at selection time so that we can use it here.
With the distance from the camera, we can place the Node at that distance along the ray. We then calculate the vector difference between the new and old
locations, and translate the node by that difference.

As with scale, each node stores a matrix which represents its translation. When the node is translated, we construct a new translation matrix for the
current translation, and multiply it into the Node's translation matrix.

#### Placing Nodes
Node placement uses some of the concepts from picking. In particular, we make use of the ray calculation.
To place a new node, we calculate the generate a ray that represents the mouse cursor in the scene.
We create a new node which is originally at the origin, and we translate it to a point on the ray, a fixed distance from the camera.
Placing the new Node at a fixed distance from the camera rather is rather limiting.

Usually, a production graphics environment would provide a more sophisticated placement algorithm, in the form of 'ghosting'. Ghosting is a tentative
placement of the object, where the user can modify the object's parameters like size and position before finalizing the placement. Ghosting is indicated
by making the tentative placement render as partially transparent, to differentiate it.

This project does not implement ghosting.
