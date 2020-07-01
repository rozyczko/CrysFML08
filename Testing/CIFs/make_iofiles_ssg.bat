@echo off
rem ****
rem ****---- Compilation for IO_Files_SSG Program ----****
rem ****
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
         (set OPT0=-g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
         (set OPT1=-g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
      ) else (
         if [%_VER%]==[m32] (set DIRECTORY=gfortran) else (set DIRECTORY=gfortran64)
         (set OPT0=-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
         (set OPT1=-O3 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
      )
      (set OPT2=)
   )
rem
rem > Compilation
   if [%_COMP%]==[ifort] (
      ifort /c io_files_ssg.f90   /nologo %OPT1% /I%CRYSFML%\%DIRECTORY%\LibC
      ifort /exe:io_files_ssg *.obj  %CRYSFML%\%DIRECTORY%\LibC\crysfml.lib /link /stack:300000000
      copy io_files_ssg.exe %FULLPROF%\.      
   )
rem   
   if [%_COMP%]==[gfortran] (
      gfortran -c io_files_ssg.f90           %OPT1% -I%CRYSFML%\%DIRECTORY%\LibC
      gfortran -o io_files_ssg.exe *.o -L%CRYSFML%\%DIRECTORY%\LibC -lcrysfml
      copy io_files_ssg_gf.exe %FULLPROF%\.      
   )
rem   
   del *.obj *.mod *.o *.map *.bak > nul
   
