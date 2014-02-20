
Modeller
=========

This project is a very small and limited 3d modeller.


Prerequisites
-------------

Blech!

* Python 2.7
* A Unix environment
* A virtualenv with:
    * pip install -I pyopengl
    * pip install -I numpy
    * pip install -I PIL
    * pip install -I PyDispatcher
    * pip install -I https://pypi.python.org/packages/source/O/OpenGLContext/OpenGLContext-2.2.0a3.tar.gz#md5=b5bdedbdae5215e7acff3b087c8220d3
    * pip install -I https://pypi.python.org/packages/source/P/PyVRML97/PyVRML97-2.3.0a3.tar.gz#md5=56cd4dd382cfb5a4ca5fdb88ae8f1733
    

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

Transformation builds up the matrices for some common transformations. It's boring and not worth printing in the chapter.

Primitive builds up OpenGL call lists for the primitives used. It's over 100 lines of uninteresting setup code. 

