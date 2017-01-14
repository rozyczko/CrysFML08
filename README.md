This project intends to be an upgrade of the full CrysFML library existing in SVN repository. 
Contrary to CrysFML that is mostly written in Fortran 95, the new project is totally based in 
the latest standard of Fortran (currently Fortran 2008) and cannot be compiled with a pure 
Fortran 95 compiler. 
The CrysFML2008 use from the beginning the submodule and object-oriented capabilities
of Fortran2008. In the Src directory there are the main modules in *.f90 files of the same
name as the corresponding module. A subdirectory with the name of the module (without the
prefix CFML) contains all submodules of the ancestor module. For the moment only gfortran and
intel fortran (ifort) are supported for the three platforms: Windows, Linux and MacOS.
Presently the project is not operational but will be progressively updated up to contain the
whole set of procedures (or equivalent) as the existing CrysFML in the SVN repository: 
https://forge.epn-campus.eu/projects/crysfml/repository 