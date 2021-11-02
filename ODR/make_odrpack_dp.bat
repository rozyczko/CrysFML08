@echo off
   ifort dp_precision.f    /c 
   ifort lpkbls.f          /c /O2 /nologo 
   ifort odr.f             /c /O2 /nologo 
rem
rem Library
rem
   lib /out:odr_dp.lib *.obj
rem
   move *.mod %CRYSFML%\ifort64\ODR_dp\.
   move *.lib %CRYSFML%\ifort64\ODR_dp\.
del *.obj *.mod *.o *.map *.bak > nul