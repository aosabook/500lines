Modeller
=========

This project is a very small and limited 3d modeller.


Prerequisites
-------------

* Python 2.7
* A Unix environment
* A virtualenv with:
    * pip install -I pyopengl
    * pip install -I numpy
    

Running
------------

viewer.py is the driver file. 

    $ python viewer.py

Usage
-----

Left click selects and drags objects on screen.

Middle click moves the scene.

Right click rotates the screen.

'C' places a cube at the mouse cursor

'S' places a sphere at the mouse cursor


Code Structure
-------------

Viewer is the main driver class. It dispatches actions to the scene and kicks off rendering.

Interaction handles user input. It maintains the state for the current mouse position, the trackball, and the pressed buttons.

Scene represents the conceptual scene. It contains a list of nodes, which in this case are all primitives but could theoretically be more complex.

Node contains the implementation of the node class, as well as the sphere and cube primitives. A node knows is transformation and has an AABB to be used for collisions.

AABB is a representation of an Axis Aligned Bounding Box. It's currently only used for selection, but it could also be used to calculate collision between nodes.

Transformation builds up the matrices for some common transformations. It's rather uninteresting.

Primitive builds up OpenGL call lists for the primitives used. It's over 100 lines of uninteresting setup code. 

