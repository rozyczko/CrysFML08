@echo off
rem
rem Intel Compilation
rem
   (set OPT0=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB)
   ifort /c keycod.f90  %OPT0% /I"%CRYSFML08%"\ifort64\libc 
   ifort /exe:keycod keycod.obj "%CRYSFML08%"\ifort64\libc\crysfml.lib /link /stack:300000000
rem
   del *.obj *.mod  *.map *.bak > nul
