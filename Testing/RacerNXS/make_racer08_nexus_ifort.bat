@echo off
set SRC=..\..\src\console
set SRC_NXS=C:\ILL_Git\Int3D08\fortran\nexus\src
set INCLUDE=/I%CRYSFML08%\ifort64\LibC /I%HDF5_INSTALL%\include\static
rem set INCLUDE=/I%CRYSFML08%\ifort64_debug\LibC /I%HDF5_INSTALL%\include\static
set OPT=/c /O3 /Warn
rem set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn
 ifort %SRC_NXS%\nexus_mod.f90           %OPT% %INCLUDE%
 ifort %SRC%\racer_cfml08_mod.f90        %OPT% %INCLUDE%
 ifort %SRC%\racer_cfml08_files.f90      %OPT% %INCLUDE%
 ifort %SRC%\racer_cfml08.f90            %OPT% %INCLUDE%

 link *.obj /out:racer08_nexus.exe /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64\LibC crysfml.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib
rem link *.obj /out:racer08_nexus.exe /subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\ifort64_debug\LibC crysfml.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib  /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib

copy racer08_nexus.exe %FULLPROF%\.
del *.obj *.mod

rem option /subsystem: console is used to prevent LINK : fatal error LNK1561: entry point must be defined
rem options /NODEFAULTLIB is used to prevent linker errors due to multiple references to the same function.