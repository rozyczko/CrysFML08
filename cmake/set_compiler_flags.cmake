macro(set_compiler_flags)

    # Nullify all the Fortran flags
    set(CMAKE_Fortran_FLAGS "")
    set(CMAKE_Fortran_FLAGS_DEBUG "")
    set(CMAKE_Fortran_FLAGS_RELEASE "")

    get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME_WE)

    message(STATUS "_____ WIN32=${WIN32}, UNIX=${UNIX} _____")

    # Covers Windows
    if(WIN32)
        message(STATUS "_____ Windows _____")

        # Windows + ifort
        if(COMPILER_NAME STREQUAL ifort)
        message(STATUS "_____ Windows + ifort _____")

            # Windows + ifort + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Windows + ifort + Debug _____")

                set(CMAKE_Fortran_FLAGS_DEBUG "/debug:full /traceback /nologo /fpp /Qopt-report=0 /heap-arrays")
                set(OPT_FLAGS0 "/Od /check:noarg_temp_created ")
                set(OPT_FLAGS  "/Od /check:noarg_temp_created ")
                set(OPT_FLAGS1 "/Od /check:noarg_temp_created ")
                set(OPT_FLAGS2 "/Od /check:noarg_temp_created ")
                set(OPT_FLAGS3 "/Od /check:noarg_temp_created ")
                
            # Windows + ifort + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Windows + ifort + Release _____")

                set(CMAKE_Fortran_FLAGS_RELEASE "/Qopt-report=0 /nologo /heap-arrays")
		        if (QPARALLEL)
                    set(OPT_FLAGS0 "/Od")
                    set(OPT_FLAGS  "/O2 /Qparallel")
                    set(OPT_FLAGS1 "/O1")
                    set(OPT_FLAGS2 "/O2 /Qparallel")
                    set(OPT_FLAGS3 "/O3 /Qparallel")
		        else()
                    set(OPT_FLAGS0 "/Od")
                    set(OPT_FLAGS  "/O2")
                    set(OPT_FLAGS1 "/O1")
                    set(OPT_FLAGS2 "/O2")
                    set(OPT_FLAGS3 "/O3")
		        endif()
                
            endif()
        
        # Windows + ifx
        elseif(COMPILER_NAME STREQUAL ifx)
            message(STATUS "_____ Windows + ifx _____")
                
            set(OPT_FLAGS2 "/fpp /Qopt-report:0")

            # Windows + ifx + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Windows + ifx + Debug _____")

                set(OPT_FLAGS "/nologo")
                set(OPT_FLAGS0 "/Od")
                set(OPT_FLAGS1 "/debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn")
                set(CMAKE_Fortran_FLAGS_DEBUG "/heap-arrays")
                
            # Windows + ifx + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Windows + ifx + Release _____")

                set(OPT_FLAGS "/nologo")
                set(OPT_FLAGS0 "/Od")
                set(OPT_FLAGS1 "/O2")
                set(CMAKE_Fortran_FLAGS_RELEASE "/heap-arrays")
                
            endif()
                        
        # Windows + gfortran
        elseif(COMPILER_NAME STREQUAL gfortran)
            message(STATUS "_____ Windows + gfortran _____")
                
            if(ARCH32)
                set(OPT_FLAGSC "-m32")
            else()
                set(OPT_FLAGSC "-m64")
            endif()
            
            set(OPT_FLAGS2 "")
            
            # Windows + gfortran + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Windows + gfortran + Debug _____")

                set(OPT_FLAGS0 "-O0 -std=f2008 -Wall -Wno-maybe-uninitialized -Wno-conversion -Wno-character-truncation -fbacktrace -fdec-math -ffree-line-length-0 -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                set(OPT_FLAGS1 "-O0 -std=f2008 -Wall -Wno-maybe-uninitialized -Wno-conversion -Wno-character-truncation -fbacktrace -fdec-math -ffree-line-length-0 -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                
            # Windows + gfortran + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Windows + gfortran + Release _____")

                set(OPT_FLAGS0 "-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                set(OPT_FLAGS1 "-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                
            endif()
                        
        endif()
        
    # Covers Linux and macOS
    # Flags are minimal here and should be updated (notably on optimization)
    elseif(UNIX)
        message(STATUS "_____ Unix _____")

        # Unix + ifort
        if(COMPILER_NAME STREQUAL ifort)
        message(STATUS "_____ Unix + ifort _____")

            # Unix + ifort + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Unix + ifort + Debug _____")

                set(CMAKE_Fortran_FLAGS_DEBUG "-g -warn -cpp -ffree-line-length-none -fPIC -heap-arrays")
                #set(OPT_FLAGS "-g" )
		        #set(OPT_FLAGS0 "-g" )
                #set(OPT_FLAGS1 "-g")
                
            # Unix + ifort + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Unix + ifort + Release _____")

		        set(CMAKE_Fortran_FLAGS_RELEASE "-warn -cpp -ffree-line-length-none -fPIC -heap-arrays")
                set(OPT_FLAGS0 "-O0")
                set(OPT_FLAGS1 "-O2")
                
            endif()

        # Unix + ifx
        elseif(COMPILER_NAME STREQUAL ifx)
            message(STATUS "_____ Unix + ifx _____")
                
            set(OPT_FLAGS2 "")

            # Unix + ifx + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Unix + ifx + Debug _____")

                set(CMAKE_Fortran_FLAGS_DEBUG "-fpp -heap-arrays")
                set(OPT_FLAGS0 "-O0 -Warn")
                set(OPT_FLAGS1 "-g  -Warn")
                
            # Unix + ifx + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Unix + ifx + Release _____")

                set(CMAKE_Fortran_FLAGS_RELEASE "-fpp -heap-arrays")
                set(OPT_FLAGS0 "-O0")
                set(OPT_FLAGS1 "-O2")
                
            endif()
                        
        # Unix + gfortran
        elseif(COMPILER_NAME STREQUAL gfortran)
            message(STATUS "_____ Unix + gfortran _____")
                
            if(ARCH32)
                set(OPT_FLAGSC "-m32")
            else()
                set(OPT_FLAGSC "-m64")
            endif()
            
            # Unix + gfortran + Debug
            if(CMAKE_BUILD_TYPE STREQUAL Debug)
                message(STATUS "_____ Unix + gfortran + Debug _____")

                set(CMAKE_Fortran_FLAGS_DEBUG "-cpp -std=f2008 -Wall -Wno-maybe-uninitialized -Wno-conversion -Wno-character-truncation -fbacktrace -fdec-math -ffree-line-length-0 -fPIC -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                set(OPT_FLAGS0 "-O0")
                set(OPT_FLAGS1 "-O0")
                
            # Unix + gfortran + Release
            elseif(CMAKE_BUILD_TYPE STREQUAL Release)
                message(STATUS "_____ Unix + gfortran + Release _____")

                set(CMAKE_Fortran_FLAGS_RELEASE "-cpp -std=f2008 -ffree-line-length-0 -fPIC -fdec-math -fall-intrinsics -fno-stack-arrays -fmax-stack-var-size=8")
                set(OPT_FLAGS0 "-O0")
                set(OPT_FLAGS1 "-O0")
                
            endif()
            
        endif()
        
    endif()

endmacro()
