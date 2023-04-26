#################################
# Commad line arguments section
#################################

cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)

# By default the built is in CONSOLE mode and 64 bits. If 64 bits have to be used one should do -D ARCH32=OFF
option(MINGW_HDF_TRICK "Help building with MinGW" OFF)
option(ARCH32 "32 or 64 bit architecture" OFF)
option(HEAP_ARRAYS "Put arrays in heap instead of stack (only for Windows ifort)" OFF)
option(PYTHON_API "Build Python API" OFF)
option(QPARALLEL "qparallel option for Ifort on Windows" ON)

if(DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} CACHE STRING "The Fortran compiler. One of 'ifort' or 'gfortran'.")
else()
    set(CMAKE_Fortran_COMPILER ifort CACHE STRING "The Fortran compiler. One of 'ifort' or 'gfortran'.")
endif()
message(STATUS "Setting compiler to ${CMAKE_Fortran_COMPILER}")

if(DEFINED CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "The build type. One of 'Release' or 'Debug'.")
else()
    set(CMAKE_BUILD_TYPE Release CACHE STRING "The build type. One of 'Release' or 'Debug'.")
endif()
message(STATUS "Setting build type to ${CMAKE_BUILD_TYPE}")

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME_WE)

#set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if (DEFINED CMAKE_INSTALL_PREFIX)
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE PATH "The installation path.")
else()
    set(CMAKE_INSTALL_PREFIX ${CMAKE_HOME_DIRECTORY}/${COMPILER_NAME} CACHE PATH "The installation path.")
endif()
message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")

if (DEFINED CRYSFML_PREFIX)
    set(CRYSFML_PREFIX ${CRYSFML_PREFIX})
else()
    set(CRYSFML_PREFIX LibC)
endif()

set (CRYSFML_PREFIX ${CMAKE_INSTALL_PREFIX}/${CRYSFML_PREFIX})
message(STATUS "CrysFML installation path set to ${CRYSFML_PREFIX}")

#################################
# Project section
#################################

# The project is declared. It is a Fortran project.
project(libcrysfml Fortran)

# Set the a verbose make.
set(CMAKE_VERBOSE_MAKEFILE ON)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake)


# Includes some modules and build the project.
include(add_prefix)
include(add_suffix)

include(set_compiler_flags)
set_compiler_flags()
# The subdirectories of the project.
add_subdirectory(Src)


if (PYTHON_API)
  set(PYTHON_API_PREFIX ${CMAKE_INSTALL_PREFIX}/Python_API)

  if (NOT PYTHON_INTERPRETER_PATH OR NOT PYTHON_LIBRARY_PATH)
    message(STATUS "Python not in path")
    set(Python3_USE_STATIC_LIBS, TRUE)
    find_package(Python3 COMPONENTS Interpreter Development)
    set(PYTHON_INTERPRETER_PATH ${Python3_EXECUTABLE})
    set(PYTHON_LIBRARY_PATH Python3::Python)
    message(STATUS "Using Python interpreter found with PYTHON_INTERPRETER_PATH variable: " ${PYTHON_INTERPRETER_PATH})
    message(STATUS "Linking Python extension with found PYTHON_LIBRARY_PATH variable: " ${PYTHON_LIBRARY_PATH})
else()
    message(STATUS "Using Python interpreter provided with PYTHON_INTERPRETER_PATH variable: " ${PYTHON_INTERPRETER_PATH})
    message(STATUS "Linking Python extension with provided PYTHON_LIBRARY_PATH variable: " ${PYTHON_LIBRARY_PATH})
endif()

  execute_process(COMMAND "${CMAKE_COMMAND}" "-E" "env"  CRYSFML08=${CMAKE_HOME_DIRECTORY} ${Python3_EXECUTABLE} ${CMAKE_HOME_DIRECTORY}/Testing/Python_API/api_generator.py)
  
  add_subdirectory(API/Src)
  #add_subdirectory(Testing/Powder/Test_1/src)
  add_subdirectory(Testing/Powder/Test_2/fortran/src)
  
endif()
