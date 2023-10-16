@echo off

ifort ..\..\src\forpy_mod.F90 /fpp /libs:dll /threads /c
ifort ..\..\src\powder_mod.f90 /I%CRYSFML08_INSTALL%\include /fpp /libs:dll /threads /c
link powder_mod.obj forpy_mod.obj /out:"powder_mod.dll" /libpath:%CRYSFML08_INSTALL%\lib /dll %LIBPYTHON% libCrysFML08.a /NODEFAULTLIB:libcmt.lib
move powder_mod.dll powder_mod.pyd
del *.obj *.mod *.exp *.lib
