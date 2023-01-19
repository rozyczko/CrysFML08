@echo off
   ifort sp_precision.f    /c 
   ifort lpkbls.f          /c /O2 /nologo 
   ifort odr.f             /c /O2 /nologo 
   ifort ODR_wrapper.f90   /c /O2 /nologo /I%CRYSFML08%\ifort64\LibC
rem
rem Library
rem
   lib /out:odr_sp.lib *.obj
rem
   move *.mod %CRYSFML08%\ifort64\ODR_sp\.
   move *.lib %CRYSFML08%\ifort64\ODR_sp\.
rem
del *.obj *.mod *.o *.map *.bak > nul