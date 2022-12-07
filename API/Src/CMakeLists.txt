#################################
# Command line arguments section
#################################
cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

#################################
# Project section
#################################
# The project is declared. It is a Fortran project.
project(Python_API Fortran)
set(LIBRARY_NAME crysfml_api)

add_library(${LIBRARY_NAME} SHARED
  Extensions/forpy_mod.F90
  Extensions/wraps.f90
  Extensions/api_init.f90
  Extensions/py_cfml_atoms.f90 
  Extensions/py_cfml_eos.f90 
  Extensions/py_cfml_extincorr.f90
  Extensions/py_cfml_geom.f90
  Extensions/py_cfml_gspacegroups.f90
  #Extensions/py_cfml_keycodes.f90
  Extensions/py_cfml_magnetic_database.f90
  Extensions/py_cfml_maths.f90
  Extensions/py_cfml_profiles.f90
  Extensions/py_cfml_random.f90
  Extensions/py_cfml_reflections.f90
  Extensions/py_cfml_scattering_tables.f90
  Extensions/py_cfml_superspace_database.f90
  Extensions/py_cfml_sxtal_geom.f90
  Extensions/py_cfml_symmetry_tables.f90)
  
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
set(LIBRARY_FILENAME "crysfml_api.so")
if(APPLE)
    set_target_properties(${LIBRARY_NAME} PROPERTIES SUFFIX ".so")
    set(LIBRARY_FILENAME "crysfml_api.so")
elseif(WIN32 OR MSYS)
    set_target_properties(${LIBRARY_NAME} PROPERTIES SUFFIX ".pyd")
    set(LIBRARY_FILENAME "crysfml_api.pyd")
endif()

# #################################
# # Documentation
# #################################
# find_package(Doxygen)
# if (DOXYGEN_FOUND)
# 	set( DOXYGEN_OUTPUT_DIRECTORY doxygen )
# 	set( DOXYGEN_COLLABORATION_GRAPH YES )
# 	set( DOXYGEN_EXTRACT_ALL YES )
# 	set( DOXYGEN_CLASS_DIAGRAMS YES )
# 	set( DOXYGEN_HIDE_UNDOC_RELATIONS NO )
# 	set( DOXYGEN_HAVE_DOT YES )
# 	set( DOXYGEN_CLASS_GRAPH YES )
# 	set( DOXYGEN_CALL_GRAPH YES )
# 	set( DOXYGEN_CALLER_GRAPH YES )
# 	set( DOXYGEN_COLLABORATION_GRAPH YES )
# 	set( DOXYGEN_BUILTIN_STL_SUPPORT YES )
# 	set( DOXYGEN_EXTRACT_PRIVATE YES )
# 	set( DOXYGEN_EXTRACT_PACKAGE YES )
# 	set( DOXYGEN_EXTRACT_STATIC YES )
# 	set( DOXYGEN_EXTRACT_LOCALMETHODS YES )
# 	set( DOXYGEN_UML_LOOK YES )
# 	set( DOXYGEN_UML_LIMIT_NUM_FIELDS 50 )
# 	set( DOXYGEN_TEMPLATE_RELATIONS YES )
# 	set( DOXYGEN_DOT_GRAPH_MAX_NODES 100 )
# 	set( DOXYGEN_MAX_DOT_GRAPH_DEPTH 0 )
# 	set( DOXYGEN_DOT_TRANSPARENT YES )
# 	set( DOXYGEN_EXCLUDE_PATTERNS *.f90 )
# 	doxygen_add_docs( doxygen ${CMAKE_CURRENT_SOURCE_DIR} )
# else (DOXYGEN_FOUND)
#   message("Doxygen is not installed - no documentation will be generated")
# endif (DOXYGEN_FOUND)


# #################################
# # Tests
# #################################
# # Add tests
# add_test(NAME "Python_API_Tests" COMMAND ${PYTHON_INTERPRETER_PATH} -m unittest discover ${CMAKE_CURRENT_BINARY_DIR}/../Tests)
# if(PYSETUP)
#     add_custom_target(Python_Files ALL
#         COMMAND
#         ${CMAKE_COMMAND} -E copy_directory
#         ${CMAKE_CURRENT_SOURCE_DIR}/../Tests
#         ${CMAKE_CURRENT_BINARY_DIR}/../Tests

#         COMMAND
#         ${CMAKE_COMMAND} -E copy_directory
#         ${CMAKE_CURRENT_SOURCE_DIR}
#         ${CMAKE_CURRENT_BINARY_DIR}/../Tests/CFML_api
#         )
# else()
#     add_custom_target(Python_Files ALL
#         COMMAND
#         ${CMAKE_COMMAND} -E copy_directory
#         ${CMAKE_CURRENT_SOURCE_DIR}/../Tests
#         ${CMAKE_CURRENT_BINARY_DIR}/../Tests

#         COMMAND
#         ${CMAKE_COMMAND} -E copy_directory
#         ${CMAKE_CURRENT_SOURCE_DIR}
#         ${CMAKE_CURRENT_BINARY_DIR}/../Tests/CFML_api

#         COMMAND
#         ${CMAKE_COMMAND} -E copy
#         ${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_FILENAME}
#         ${CMAKE_CURRENT_BINARY_DIR}/../Tests/CFML_api/${LIBRARY_FILENAME}
#         )
# endif()

#add_dependencies(Python_Files ${LIBRARY_NAME})

#################################
# Install section
#################################
# The rules for installing the library.
install(DIRECTORY "" DESTINATION ${PYTHON_API_PREFIX}/CFML_api FILES_MATCHING PATTERN "*.py" PATTERN "Extensions" EXCLUDE)
if(WIN32 OR MSYS)
    install(TARGETS ${LIBRARY_NAME} RUNTIME DESTINATION ${PYTHON_API_PREFIX}/CFML_api)
else()
    install(TARGETS ${LIBRARY_NAME} LIBRARY DESTINATION ${PYTHON_API_PREFIX}/CFML_api)
endif()

# # Installing example files
# install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Examples" DESTINATION ${PYTHON_API_PREFIX} FILES_MATCHING PATTERN "*.py")
# install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Examples/Data" DESTINATION ${PYTHON_API_PREFIX}/Examples FILES_MATCHING PATTERN "*.cfl" PATTERN "*.cif")
# #
# # # Installing tests files
# install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Tests" DESTINATION ${PYTHON_API_PREFIX} FILES_MATCHING PATTERN "*.py")
# #
# # # Installing documentation files
# install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/doxygen/html" DESTINATION ${PYTHON_API_PREFIX}/Doc OPTIONAL)
