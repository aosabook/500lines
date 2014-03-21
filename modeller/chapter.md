# A Small 3D Modeller

## Intro

## Background
CAD, or Computer Assisted Design, tools have a rich history in computing. CAD tools have
reached across all domains of design and development, including electrical engineering, industrial
design, architecture, entertainment.

<-- FIXME: How to tie into 3D modelling??? -->
Some CAD tools include a modelling env

## Intro
 * mention Blender, and how it's a source of inspiration for this project
 * talk about how a modeller could be used

## Matrix arithmetic
 * what is a matrix?
 * what is a projection matrix?
 * what is a modelview matrix?

## What is OpenGL
 * OpenGL is designed by XYZ committee. It's a standard protocol for communicating between user level programs
 and graphics hardware. There are two variants of OpenGL, commonly referred to as "Modern OpenGL" and "Legacy OpenGL".

 Modern OpenGL features a programmable rendering pipeline where the programmer can write small programs called "shaders" that
 run on dedicated graphics hardware (GPU). Most features of Modern OpenGL were introduced in version 2.1. The programmable
 pipeline of Modern OpenGL has replaced Legacy OpenGL, which is considered deprecated.

 Legacy OpenGL offers a fixed function pipeline without programmability. It provides an API for many functions which
 would be written in shaders in Modern OpenGL.

## Modeller structure
 * Conceptual overview of what a scene is.
    * What's in the scene? What is a node?
    * What might a node represent in reality, other than a primitive? E.g. Triangle Mesh, Hierarchical Structure, etc.
 * Scene representation. What does the Scene need to know?

 * Viewer. What does it do? How does it interact with OpenGL. Be careful that this isn't an OpenGL tutorial

 * Interaction. How does user interaction flow through the program?

## Modeller structure

### Setting the Scene
The first question we face when we begin architecting a 3d modeller is how are we going to represent the objects on screen, the scene? (rework)
As in a film or stage production, we'll call this the "scene".
We would like to design the scene so that it can store all manner of objects that we want to include, and so that it can be easily extended to
store new types of objects.
Suppose we use a base class to represent any object that can be placed in the scene, call it a "node". Using the general concept of a node allows
us to reason about the scene in an abstract manner. The scene object doesn't need to know about the details of every type of object it displays.
It only needs to know that it contains a list of nodes.
In this example, the nodes are all primitive shapes. We have a Sphere and a Cube available. More shapes can be added easily by extending the Node class again.

 * Since each derived class can render and manipulate itself, the scene doesn't need to know how to do any of that
 * This structure is easily extensible to be a hierarchical scene, where each node can have a list of children. A hierarchical scene opens up new possibilities such
 as grouping primitives together into a joint shape. Suppose, for example, we have a House object. If we want to place a house in the scene, we don't want to place each wall,
 window, and door separately. Instead, we want to place the house as a hole. To represent the house in a hierarchical scene, we could have a HouseNode which has a list of child nodes
 that represent the parts. That way, we can manipulate the whole house from the top level without having to manipulate each part.



### Drawing
Now that we have an abstract representation of the objects in the scene, we would like to draw the scene to the screen.

#### OpenGL
OpenGL is a graphical application programming interface for cross-platform development.

* Talk about how Legacy OpenGL is basically a state machine. The current matrix state can be push/poped, and operations get applied according
to the current state of the machine.

#### Traversing the Scene
There are a few challenging things about rending a scene.
We would like to leverage the data structure that we are using for the scene to render it.
Since the data structure for the scene contains a list of nodes, the render function needs to traverse the list. If the scene
was a hierarchical, then each node would traverse its child nodes as part of the render pass. The basic set of steps to render a node is:
```
Push the current OpenGL state onto the stack
Apply the transformation to OpenGL
Draw the node
Pop the OpenGL state
```
The `PushMatrix` and `PopMatrix` functions in OpenGL provide us access to a stack object for saving the status of the matrix.
This suits us perfectly, as we're traversing a graph of nodes. The OpenGL matrix stack is used to store the matrix state of each node when it is rendered.
This allows us to have a single render list for each node. For example, the render list for the Cube primitive draws a size 1 cube at the origin.
By setting the OpenGL matrix, we can change the size and location the rendered cube.

### User Interaction
How that we're able to render the scene, we want to be able to add new Nodes, and to adjust the Nodes in the scene.
