@echo off
rem ****
rem ****---- Compilation of GlOpSAnn Program ----****
rem ****
rem **** Author: JRC + JGP
rem **** Revision: Nov-2008
rem ****
rem
   if not x%1 == x goto CONT
   cls
   echo    MAKE_GLOpSAnn: Make GLOpSAnn Compilation
   echo    Syntax: make_GLOpSAnn [f95/lf95/g95/gfortran/ifort]
   goto END
rem
:CONT
cd ..\..\Src
   if x%1 == xgfortran  goto GFOR
   if x%1 == xgfortrand  goto GFORD
   if x%1 == xifort     goto IFORT
   if x%1 == xifortd    goto IFORTD
   goto END
rem ****---- Intel Compiler ----****
:IFORT
   ifort /c GLS_observ.f90            /O2 /nologo /heap-arrays /I%CRYSFML08%\ifort64\LibC
   ifort /c GLS_cost_functions.f90    /O2 /nologo /heap-arrays /I%CRYSFML08%\ifort64\LibC
   ifort /c GLOpSAnn.f90              /O2 /nologo /heap-arrays /I%CRYSFML08%\ifort64\LibC
   ifort /exe:GLOpSAnn *.obj  %CRYSFML08%\ifort64\LibC\CrysFML.lib /link /stack:64000000
rem   ifort  /exe:GLOpSAnn.exe *.obj  C:\CrysFML\ifort64\LibC\CrysFML.lib
   goto END
:IFORTD
   ifort /c GLS_observ.f90           /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /c GLS_cost_functions.f90   /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /c GLOpSAnn.f90             /heap-arrays   /debug=full /traceback /nologo /I%CRYSFML08%\ifort64_debug\LibC
   ifort /exe:GLOpSAnn.exe *.obj  %CRYSFML08%\ifort64_debug\LibC\CrysFML.lib
   goto END
rem
rem **---- GFORTRAN Compiler ----**
:GFOR
   gfortran -c GLS_observ.f90          -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran -c GLS_cost_functions.f90  -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran -c GLOpSAnn.f90            -O3 -funroll-loops  -msse2  -I%CRYSFML08%\gfortran\LibC
   gfortran  *.o -o  GLOpSAnn_gf  -L%CRYSFML08%\gfortran\LibC -lcrysfml  -Wl,--heap=0x01000000
   upx GLOpSAnn_gf.exe
   if exist %FULLPROF%  copy GLOpSAnn_gf.exe %FULLPROF%\.
   goto FIN
:GFORD
   gfortran -c GLS_observ.f90          -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran -c GLS_cost_functions.f90  -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran -c GLOpSAnn.f90            -g -O0 -std=f2008 -Wall -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics   -I%CRYSFML08%\gfortran_debug\LibC
   gfortran  *.o -o  GLOpSAnn_gf  -L%CRYSFML08%\gfortran_debug\LibC -lcrysfml  -Wl,--heap=0x01000000
   upx GLOpSAnn_gf.exe
   if exist %FULLPROF%  copy GLOpSAnn_gf.exe %FULLPROF%\.
   goto FIN
rem
:END
   upx GLOpSAnn.exe
 if exist %FULLPROF%  copy GLOpSAnn.exe %FULLPROF%\nGLOpSAnn.exe
rem if exist %PROGCFML%  move GLOpSAnn.exe %PROGCFML%\DistFPS_64b\.
rem if exist %PROGCFML%  copy ..\Docs\GLOpSAnn.pdf %PROGCFML%\DistFPS\Docs\.
:FIN
   del *.obj *.mod *.o *.map *.bak *.exe > nul
cd ..\Scripts\Windows
