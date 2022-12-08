# Command line arguments section
#################################
cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

#################################
# Project section
#################################
# The project is declared. It is a Fortran project.
project(Python_API_test1 Fortran)
set(LIBRARY_NAME powder_mod)

add_library(${LIBRARY_NAME} SHARED
  forpy_mod.F90
  powder_mod.f90)

# Forpy must be preprocessed
set_source_files_properties(forpy_mod.f90 PROPERTIES COMPILE_FLAG "-cpp")

#################################
# Dependencies section
#################################
# Add a dependency to crysfml_common to keep sure that the crysfml_common library will be built first.
# See https://gitlab.kitware.com/cmake/cmake/-/issues/19226 for linking
add_dependencies(${LIBRARY_NAME} crysfml)

if(WIN32 OR MSYS)
  add_library(Python3_LIB STATIC IMPORTED)
  set_property(TARGET Python3_LIB PROPERTY IMPORTED_LOCATION ${PYTHON_LIBRARY_PATH})
  target_link_libraries(${LIBRARY_NAME} Python3_LIB)
endif()

# Set -Wl,-undefined,dynamic_lookup to OTHER_LDFLAGS
# Set include and linking
include_directories(${CRYSFML_MODULE_DIRECTORY})
target_link_libraries(${LIBRARY_NAME} crysfml)
# Fix a problem on Mac OS X when building shared libraries
if(APPLE)
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined dynamic_lookup")
endif()

# Set library extension
set_target_properties(${LIBRARY_NAME} PROPERTIES PREFIX "" OUTPUT_NAME ${LIBRARY_NAME})
set(LIBRARY_FILENAME "powder_mod.so")
if(APPLE)
    set_target_properties(${LIBRARY_NAME} PROPERTIES SUFFIX ".so")
    set(LIBRARY_FILENAME "powder_mod.so")
elseif(WIN32 OR MSYS)
    set_target_properties(${LIBRARY_NAME} PROPERTIES SUFFIX ".pyd")
    set(LIBRARY_FILENAME "powder_mod.pyd")
endif()

