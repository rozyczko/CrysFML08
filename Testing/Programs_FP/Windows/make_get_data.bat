@echo off
   (set _DEBUG=N)
   (set _COMP=ifort)
   (set SRC_NXS=%CRYSFML08%\HDF5)
   (set SRC=%CRYSFML08%\Testing\Get_Data)
:LOOP
    if [%1]==[debug]  (set _DEBUG=Y)
    if [%1]==[ifort]  (set _COMP=ifort)
    if [%1]==[gfortran] (
       (set _COMP=gfortran)
       (set _VER=m64)
    )
    shift
    if not [%1]==[] goto LOOP

    if [%_COMP%]==[ifort] (
        if [%_DEBUG%]==[Y] (
           (set DIRECTORY=ifort64_debug)
           (set OPT=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn)
           (set OPT2=/c /debug:full /check /check:noarg_temp_created /traceback /nologo /CB /heap-arrays /Warn)
        ) else (
           (set DIRECTORY=ifort64)
           (set OPT=/c /O3 /nologo /nologo /Warn)
           (set OPT2=/c /O3 /nologo /nologo /heap-arrays /Warn)
        )
      (set INCLUDE=/I %CRYSFML08%\%DIRECTORY%\include /I %HDF5_INSTALL%\include\static)
      (set liblink=/subsystem:console /stack:128000000 /libpath:%HDF5_INSTALL%\lib /libpath:%CRYSFML08%\%DIRECTORY%\lib ^
           CrysFML08.lib libhdf5_fortran.lib libhdf5_f90cstub.lib libhdf5.lib libszip.lib libzlib.lib ^
           /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libmmt.lib)

    )

rem Intel Compilation
rem Compiling the source files

    %_COMP% %SRC_NXS%\Nexus_Mod.f90 %OPT% %INCLUDE%
    %_COMP% %SRC%\D2B_read_mod.f90        %OPT% %INCLUDE%
    %_COMP% %SRC%\D2B_data_mod.f90        %OPT% %INCLUDE%
    %_COMP% %SRC%\D2B_int_mod.f90         %OPT% %INCLUDE%
    %_COMP% %SRC%\GetData_Globals.f90     %OPT% %INCLUDE%
    %_COMP% %SRC%\Get_data.f90            %OPT% %INCLUDE%
    link /out:get_data.exe *.obj    %liblink%
:FIN
rem
rem Compress executable
rem
   upx get_data.exe
rem
rem
rem Update FullProf Distribution
rem
   if exist %FULLPROF% copy get_data.exe %FULLPROF%
   if exist %PROGCFML% copy get_data.exe %PROGCFML%\DistFPS_64b\get_data.exe
rem
rem Clean several files
rem
   del *.obj *.o *.mod *.map *.bak *.pdb *.exe > nul
:ENDT
