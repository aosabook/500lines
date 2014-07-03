# Storyline:
* Why do modellers exist? It's so that humans can design real world (or virtual world) objects. ( building, bridges, 3D printing, video games, Film monsters, etc).
* Humans write software to assist in the designing of these things.
* What does that software need to do to be helpful?
* Three things:
    - It needs to have a way to represent the thing that's being designed. (The scene)
    - It needs to have a way to display the thing. Now this is a challenge because the object that we're designing exists in 3 dimensions, but we only have
      2 dimensions to work with on a computer screen. So we need to think about how we perceive 3 dimensional objects with our eyes. How does light interact 
      with objects in the world, and how do our eyes perceive them.
    - It also needs a way for the designer to interact with the object. Being able to view something is all well and good, but if you're designing something, you want to be able to
      add to it, remove from it, move things around, etc.
* Of course, each domain will have it's own additional important features. An architecture CAD tool may need physics simulations to test weather climate stresses on the building. A 
  3D printing tool might need to check whether the object is actually printable, an electrical CAD tool might need to simulate the physics of electrons running across copper, and a film 
  special effects suite might need a way to build photorealistic renderings from the design. All tools need a way to save and load designs from disk so that designers can collaborate, share, 
  and save their work.
* But at its core, all of these tools need at least the 3 features we discussed.
* So, the simplest possible CAD tool is just a 3d modeller that stores a design, displays it to the screen, and allows the designer to interact with it.

* With that in mind, let's see how we can represent a 3D design, display it, and interact with it, in 500 lines of Python.
  


## Intro
Humans are constantly creating new things. In modern times, we have written software to assist in the design and creation process. 
Many designers, engineers and creators make use of Computer Assisted Design (CAD) software.  These tools allow creators to design buildings, bridges, video game art, 
film monsters, 3D printable objects, and many other things on a computer before building a physical version of the design. 

At their core, CAD tools must offer three pieces of functionality. 
Firstly, they must have a data structure to represent the object that's being designed. For a designer to effectively use a piece of software, the softwarw must understand
the types of objects being desinged. The software must have a semantic model of the design.
Secondly, the CAD tool must offer some way to display the design onto the user's screen.  The user is designing a physical object with 3 dimensions, but the computer sceen has only 2 dimensions. 
The CAD tool must model how we perceive objects, and draw them to the screen in a way that the user can understand all 3 dimensions of the object.
Thirdly, the CAD tool must offer the designer a way to interact with the object being designed. The designer must be able to add and modify the design in order to produce the desired result.

Each domain specific CAD tool will offer many additional features for the specific requirements of the domain. An architecture CAD tool would offer physics simulations to test weather and climate stresses on the building, 
a 3D printing tool would have features that check whether the object is actually valid to print, and electrical CAD tool would simulate the physics of electricity running through copper, and a film special effects suite would
include features to accurate simulation of pyrokinetics with photorealistic renderings of the design. 
Additionally, all tools would need a way to save and load designs from disk that that designers can collaborate, share, and save their work.

However, at their core, call CAD tools must include the three features discussed above: a data structure to represent the design, the ability to display it to the screen, and a method to interact with the design.

With that in mind, let's explore how we can represent a 3D design, display it to the screen, and interact with it, in 500 lines of Python.

<!--- TODO: figure out how to split up code -->

## Representing the Design: The Scene
The first of the core features of a 3D modeller is a data structure to represent the design in memory. The class that represents this is often called `Design` or `Scene`. 
In this project, we call it a `Scene`. The `Scene` contains the data that represents the design that the user is working on. The objects contained in the `Scene` are referred to as `Node`s.
The `Scene` has a list of `Node`.

```
TODO: Scene init code
```

### Scene Nodes
We use the `Node` base class to represent an object that can be placed in the scene. This base class allows
us to reason about the scene abstractly. The `Scene` class doesn't need to know about the details of the objects it displays,
it only needs to know that they are `Node`s. Each type of `Node` defines its own behaviour for rendering itself and for any other necessary
interactions.

In this project,`Sphere` and a `Cube` available. More shapes can be added easily by extending the Node class again.

```
class Node(object):
    """ Base class for scene elements """
    def __init__(self):
        self.location = [0, 0, 0]
        self.color_index = random.randint(color.MIN_COLOR, color.MAX_COLOR)
        self.aabb = AABB([0.0, 0.0, 0.0], [0.5, 0.5, 0.5])
        self.translation = numpy.identity(4)
        self.scalemat = numpy.identity(4)
        self.selected = False

class Primitive(Node):
    def __init__(self):
        super(Primitive, self).__init__()
        self.call_list = None

class Sphere(Primitive):
    """ Sphere primitive """
    def __init__(self):
        super(Sphere, self).__init__()
        self.call_list = G_OBJ_SPHERE


class Cube(Primitive):
    """ Cube primitive """
    def __init__(self):
        super(Cube, self).__init__()
        self.call_list = G_OBJ_CUBE
```

The `Node` keeps track of important data about itself: translation matrix, scale matrix, color, location, etc. We will see more about Axis Aligned Bounding Boxes (AABBs) when we discuss
selection below.

The abstract Node class contains all of the logic common to all nodes. The sub classes of `Node` override specific functionality if needed. They are also required to
provide a `render` function.

Using a class structure like this means that the `Node` class is easily extensible. As an example of the extensibility, consider adding a `Node` type that combines multiple
primitives, like a figure for a character. We can easily extend the `Node` class for this situation by creating a new class `class Figure(Node)` which will override some of
the functionality of the `Node` manage a list of sub-nodes.

By making the `Node` class extensible in this way, we are able to add new types of shapes to the scene without changing any of the other code around scene
manipulation and rendering. Using `Node` concept to abstract away the fact that one `Scene` object may have many children is known as the Composite Design Pattern.

### Linear algebra (TODO)
#### Matrix arithmetic (TODO: is this necessary?)
 * what is a matrix?
 * what is a projection matrix?
 * what is a ModelView matrix?
 * The 4th vector element to distinguish points from vectors

The purposes of the ModelView and Projection matrices can be understood with some basic linear algebra. Detailed explanations can be found here: (TODO!!!).

TODO: Should we put some linear algebra here? This topic is covered in every OpenGL tutorial, but it will be unfamiliar to many people. I'm not sure if it's best to redirect to another tutorial,
or have an explanation here.

## Displaying the Design: Rendering
Now that we have an abstract representation of the objects in the scene, the second key feature for a 3D modeller is displaying the design to the screen.
The process of displaying the design to the scene is called rendering. In this project, we use the Open Graphics Library (OpenGL) to communicate with the graphics drivers to 
render the `Scene`.

<!--- TODO: should this get moved to the end of the chapter? -->
### OpenGL
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

#### OpenGL's State Machine
Legacy OpenGL is a State Machine. The API to enable/disable functionality modifies the current state of the OpenGL machine.
When a polygon render call is made, the current state of the machine is used.
OpenGL also stores two matrices.  These are called the "ModelView" matrix and the "Projection" matrix.
The ModelView matrix determines the transformation of the current polygon within the scene. The Projection matrix is used to project that polygon onto the screen.
The matrices are manipulated with `glMultMatrix`, which multiplies the current matrix by the matrix parameter.
OpenGL also maintains a stack of matrices. The programmer can choose to push and pop from this stack. This facilitates traversing a scene graph for rendering purposes, as we will discuss later.

Most of the setup and interfacing with OpenGL is found in the viewer.py file.

### Rendering the Scene: Viewer
With a basic understanding of OpenGL, we examine how to render the `Scene` to the screen. The `Viewer` class created the gui window and handles initializing OpenGL.
The function `init_interface` creates the window that the modeller will be rendered into and specifices the function to be called when the scene needs to rendered. 
The `init_opengl` function sets up the OpenGL state needed for the project. It sets
the matrices, enables backface culling, and registers a light to illuminate the scene, and tells OpenGL that we would like objects to be colored. The `init_scene` function creates the `Scene` objects and places some initial
nodes to get the user started. Finally, `init_interaction` registers callbacks for user interaction, as we'll discuss later.

```
class Viewer(object):
    def __init__(self):
        """ Initialize the viewer. """
        self.init_interface()
        self.init_opengl()
        self.init_scene()
        self.init_interaction()
        init_primitives()

    def init_interface(self):
        """ initialize the window and register the render function """
        glutInit()
        glutInitWindowSize(640, 480)
        glutCreateWindow("3D Modeller")
        glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)
        glutDisplayFunc(self.render)

    def init_opengl(self):
        """ initialize the opengl settings to render the scene """
        self.inverseModelView = numpy.identity(4)
        self.modelView = numpy.identity(4)

        glEnable(GL_CULL_FACE)
        glCullFace(GL_BACK)
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LESS)

        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, 1, 0))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, -1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable(GL_COLOR_MATERIAL)
        glClearColor(0.4, 0.4, 0.4, 0.0)

    def init_scene(self):
        """ initialize the scene object and initial scene """
        self.scene = Scene()
        self.initial_scene()

    def initial_scene(self):
        cube_node = Cube()
        cube_node.translate(2, 0, 2)
        cube_node.color_index = 2
        self.scene.add_node(cube_node)

        sphere_node = Sphere()
        sphere_node.translate(-2, 0, 2)
        sphere_node.color_index = 3
        self.scene.add_node(sphere_node)

        sphere_node_2 = Sphere()
        sphere_node_2.translate(-2, 0, -2)
        sphere_node_2.color_index = 1
        self.scene.add_node(sphere_node_2)

    def init_interaction(self):
        """ init user interaction and callbacks """
        self.interaction = Interaction()
        self.interaction.register_callback('pick', self.pick)
        self.interaction.register_callback('move', self.move)
        self.interaction.register_callback('place', self.place)
        self.interaction.register_callback('rotate_color', self.rotate_color)
        self.interaction.register_callback('scale', self.scale)

    def main_loop(self):
        glutMainLoop()
```

The `render` function is called when the `Scene` needs to be drawn to the screen. Drawing to the screen is necessary whenever anything in the scene has changed, or when the perspective has changed. 
The `render` function handles all of the OpenGL setup that's necessary each time the `Scene` is rendered. It initializes the projection matrix via `init_view` and initializes the modelview matrix with the
global transformation. It tells the scene to render itself, and then renders the unit grid. Finally, it calls `glFlush` to signal to the GPU driver that we are ready for the buffer to be flushed and displayed to the screen.

```
    def render(self):
        """ The render pass for the scene """
        self.init_view()

        # Enable lighting and color
        glEnable(GL_LIGHTING)

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        # Load the modelview matrix from the current state of the trackball
        glMatrixMode(GL_MODELVIEW)
        glPushMatrix()
        glLoadIdentity()
        loc = self.interaction.translation
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.interaction.trackball.matrix)

        # store the inverse of the current modelview.
        currentModelView = numpy.array(glGetFloatv(GL_MODELVIEW_MATRIX))
        self.modelView = numpy.transpose(currentModelView)
        self.inverseModelView = inv(numpy.transpose(currentModelView))

        # render the scene. This will call the render function for each object in the scene
        self.scene.render()

        # draw the grid
        glDisable(GL_LIGHTING)
        glCallList(G_OBJ_PLANE)
        glPopMatrix()

        # flush the buffers so that the scene can be drawn
        glFlush()

    def init_view(self):
        """ initialize the projection matrix """
        xSize, ySize = glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT)
        aspect_ratio = float(xSize) / float(ySize)

        # load the projection matrix. Always the same
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()

        glViewport(0, 0, xSize, ySize)
        gluPerspective(70, aspect_ratio, 0.1, 1000.0)
        glTranslated(0, 0, -15)

```

To render the `Scene`, we will leverage its data structure. The render function of the scene traverses the list of `Node` in the scene and
calls the `render` function for each `Node`.
```

# scene.py, line 21
def render(self):
    """ Render the scene. This function simply calls the render function for each node. """
    for node in self.node_list:
        node.render()
```
An OpenGL Call List is a series of OpenGL calls that are bundled together and named. The calls can be dispatched with `glCallList(LIST_NAME)`. The rendering function uses `glCallList` after setting up the matrices.
Each primitive (`Sphere` and `Cube`) defines the call list required to render it.  For example, the render list for the Cube primitive draws a cube at the origin with sides of length 1.

The `PushMatrix` and `PopMatrix` functions in OpenGL provide access to a stack object for saving the status of the matrix.  We use OpenGL matrix stack to store the matrix state of each `Node` when it is rendered.
Manipulating the ModelView matrix allows us to have a single render list for each type of primitive.  By setting the OpenGL matrix, we can change the size and location the rendered `Primitive`.
```
# node.py, line 63
def render(self):
    glPushMatrix()
    glMultMatrixf(numpy.transpose(self.translation))
    glMultMatrixf(self.scalemat)
    cur_color = color.COLORS[self.color_index]
    glColor3f(cur_color[0], cur_color[1], cur_color[2])
    if self.selected:  # emit light if the node is selected
        glMaterialfv(GL_FRONT, GL_EMISSION, [0.3, 0.3, 0.3])
    glCallList(self.call_list)
    if self.selected:
        glMaterialfv(GL_FRONT, GL_EMISSION, [0.0, 0.0, 0.0])

    glPopMatrix()

```
Again, the Matrix stack functionality of OpenGL allows us to extend the `Node` class to contain nested nodes. If there are a nested nodes, we simply
push onto the stack before we render a nested node.

Thus, using the scene traversal and OpenGL Matrix Stack allows us to implement the `Node` class in an extensible way, and allows each `Node`'s render code
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
A selected node can be moved, resized, or colorized. For color and scale, the scene passes along the function to the selected `Node`, if it exists. For example:
```
# scene.py, line 95
def rotate_color(self, forwards):
    """ Rotate the color of the currently selected node """
    if self.selected_node is None: return
    self.selected_node.rotate_color(forwards)

```
For translation, the scene needs to do more of the work.
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
## Further Exploration
For further insight into real-world 3D modelling software, a few Open Source projects are interesting.

[Blender](http://www.blender.org/) is an Open Source full featured 3D animation suite. It provides a full 3D pipeline for building special effects in video, or for game creation. The modeller is a small part of this
project, and it is a good example of integrating a modeller into a large software suite.

[OpenSCAD](http://www.openscad.org/) is an Open Source 3D modelling tool. It is not interactive, rather it reads a script file that specifies how to generate the scene. This gives the designer "full control over the modelling process".
