@echo off
rem ****
rem ****---- Compilation for MagRef Program ----****
rem ****
rem **** Author: JRC
rem **** Revision: June-2009
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_MAGREF: Make MagRef Compilation
   echo    Syntax: make_magref [gfortran/ifort/ifortd]
   goto END
rem
:CONT
   if x%1 == xgfortran  goto GFOR
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd    goto IFORTD
   goto END
rem
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c magref.f90 /O2 /nologo /I. /I %CRYSFML%\ifort_release\include
   link /subsystem:console /stack:102400000 /out:MagRef.exe *.obj %CRYSFML%\ifort_release\lib\libCrysFML08.a
   goto END
:IFORTD
   ifort /c magref.f90  /debug:full /check /traceback /nologo  /I. /I %CRYSFML%\ifort_debug\include
rem   ifort  *.obj /exe:MagPolar3D_if  %CRYSFML%\ifort_debug\LibC\CrysFML.lib /link /stack:102400000
   link /subsystem:console /stack:102400000 /out:MagRef.exe *.obj %CRYSFML%\ifort_debug\lib\libCrysFML08.a
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c -O3  -std=f2003  -funroll-loops  -msse2   magref.f90   -I%CRYSFML%\gfortran_release\include
   gfortran  *.o -o MagRef_gf -O3  -funroll-loops  -msse2  -L %CRYSFML%\gfortran_release\lib -lcrysfml08  -Wl,--heap=0x01000000
   goto END
rem
:END
   del *.obj *.mod *.o *.map *.bak > nul
