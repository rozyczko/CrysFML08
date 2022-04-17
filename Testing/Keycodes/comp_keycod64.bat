@echo off
rem
rem Intel Compilation
rem
   ifort keycod.f90 /c /Ox /nologo /I"%CRYSFML%"\ifort64\libc08  /warn
   ifort /exe:keycod keycod.obj "%CRYSFML%"\ifort64\libc08\crysfml.lib
rem
   del *.obj *.mod  *.map *.bak > nul
