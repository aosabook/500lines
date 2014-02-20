
Modeller
=========

This project is a very small and limited 3d modeller.


Prerequisites
-------------

Blech!

* Python 2.7
* A Unix environment
* A virtualenv with:
    pip install -I pyopengl
    pip install -I numpy
    pip install -I PIL
    pip install -I PyDispatcher
    pip install -I https://pypi.python.org/packages/source/O/OpenGLContext/OpenGLContext-2.2.0a3.tar.gz#md5=b5bdedbdae5215e7acff3b087c8220d3
    pip install -I https://pypi.python.org/packages/source/P/PyVRML97/PyVRML97-2.3.0a3.tar.gz#md5=56cd4dd382cfb5a4ca5fdb88ae8f1733
    

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
