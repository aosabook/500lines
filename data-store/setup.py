#!/usr/bin/env python

from distutils.core import setup

with open('requirements.txt') as f:
    requirements = [
        req.strip()
        for req in f
        if req and not req.startswith('#')]

setup(
    name='dbdb',
    version='0.1',
    description='DogBed DataBase',
    author='Taavi Burns',
    author_email='taavi.burns@points.com',
    url='https://github.com/aosabook/500lines/tree/master/data-store',
    packages=['dbdb'],
    requries=requirements,
)
