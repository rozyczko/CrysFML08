@echo off
rem --------------------------------------------
rem ---- Compilation for TOF_fit_LM Program ----
rem --------------------------------------------
rem > INIT 
   (set _DEBUG=N)
   (set _COMP=ifort)
   if [%TARGET_ARCH%]==[] (set TARGET_ARCH=ia32)
rem
rem > Arguments ----
:LOOP
    if [%1]==[debug]  (set _DEBUG=Y)
    if [%1]==[ifort]  (set _COMP=ifort)
    if [%1]==[gfortran32] (
       (set _COMP=gfortran)
       (set _VER=m32)
    )
    if [%1]==[gfortran64] (
       (set _COMP=gfortran)
       (set _VER=m64)
    )   
    shift
    if not [%1]==[] goto LOOP
rem
rem > Compilers
rem
   if [%_COMP%]==[ifort] (
      if [%_DEBUG%]==[Y] (
         if [%TARGET_ARCH%]==[ia32] (set DIRECTORY=ifort_debug) else (set DIRECTORY=ifort64_debug)
         (set OPT0=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB)
         (set OPT1=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB)
      ) else (
         if [%TARGET_ARCH%]==[ia32] (set DIRECTORY=ifort) else (set DIRECTORY=ifort64)
         (set OPT0=/Od)
         (set OPT1=/O2)
      )
      (set OPT2=/fpp /Qopt-report:0)
   )
rem   
   if [%_COMP%]==[gfortran] (
      if [%_DEBUG%]==[Y] (
         if [%_VER%]==[m32] (set DIRECTORY=gfortran_debug) else (set DIRECTORY=gfortran64_debug)
         (set OPT0=-O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
         (set OPT1=-O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
      ) else (
         if [%_VER%]==[m32] (set DIRECTORY=gfortran) else (set DIRECTORY=gfortran64)
         (set OPT0=-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
         (set OPT1=-O3 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
      )
      (set OPT2=)
   )
rem
rem > Compilation
rem > Go to the proper directory
cd ..\..\TOF-fit
   if [%_COMP%]==[ifort] (
      ifort /c TOF_module_LM.f90   /nologo %OPT1% /I%CRYSFML08%\%DIRECTORY%\LibC  /I%CRYSFML08%\%DIRECTORY%\ODR_sp
      ifort /c TOF_fitting_LM.f90  /nologo %OPT1% /I%CRYSFML08%\%DIRECTORY%\LibC  /I%CRYSFML08%\%DIRECTORY%\ODR_sp
      ifort /exe:TOF_fit_LM *.obj  %CRYSFML08%\%DIRECTORY%\LibC\crysfml.lib  %CRYSFML08%\%DIRECTORY%\ODR_sp\odr_sp.lib  /link /stack:300000000 
   )
rem   
   if [%_COMP%]==[gfortran] (
      gfortran -c TOF_module_LM.f90  %OPT1% -I%CRYSFML08%\%DIRECTORY%\LibC -I%CRYSFML08%\%DIRECTORY%\ODR_sp
      gfortran -c TOF_fitting_LM.f90 %OPT1% -I%CRYSFML08%\%DIRECTORY%\LibC -I%CRYSFML08%\%DIRECTORY%\ODR_sp
      gfortran -o TOF_fit_LM.exe *.o -L%CRYSFML08%\%DIRECTORY%\LibC -lcrysfml
   )
rem   
   if exist %FULLPROF% copy TOF_fit_LM.exe %FULLPROF%\TOF_fit_LM.exe
   if exist %PROGCFML% copy TOF_fit_LM.exe %PROGCFML%\DistFPS_64b\TOF_fit_LM.exe    
   del *.obj *.mod *.o *.map *.bak > nul
cd ..\Programs_FP\Windows