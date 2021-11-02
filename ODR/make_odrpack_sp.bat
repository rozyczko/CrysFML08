@echo off
   ifort sp_precision.f    /c 
   ifort lpkbls.f          /c /O2 /nologo 
   ifort odr.f             /c /O2 /nologo 
rem
rem Library
rem
   lib /out:odr_sp.lib *.obj
rem
   move *.mod %CRYSFML%\ifort64\ODR_sp\.
   move *.lib %CRYSFML%\ifort64\ODR_sp\.
rem
del *.obj *.mod *.o *.map *.bak > nul