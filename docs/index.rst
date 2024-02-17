.. PyCrysFML08 documentation master file, created by
   sphinx-quickstart on Sat Feb 10 16:35:28 2024.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _CrysFML08: https://code.ill.fr/rodriguez-carvajal/CrysFML2008

Welcome to PyCrysFML08's documentation!
=======================================

PyCrysFML08 is a Python wrapper for the Fortran library `CrysFML08`_, a library which provides plenty of procedures for crystallographic and diffraction calculations. PyCrysFML08 allows the use of the main types, subroutines and functions of `CrysFML08`_, from a Python environment.

`CrysFML08`_ is organised in modules. Each module is dedicated to a specific functionality: input / output, symmetry, optimisation, mathematics, atoms, structure factors, diffraction patterns, etc. Each module contains a number of derived types and classes and a number of procedures, which can be functions or subroutines. Fortran derived types and classes are treated in Python as dictionaries, while both Fortran functions and subroutines are treated in Python as functions.

.. toctree::
   :caption: CONTENTS
   :maxdepth: 1

   ./pages/types.rst

