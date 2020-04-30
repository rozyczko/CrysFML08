# Set source files and compiler flags for each module

# CFML_GlobalDeps
# This module is OS and compiler dependent.
if(WIN32)
   # Windows
    if(${COMPILER_NAME} STREQUAL ifort)
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Windows_IFOR.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Windows_GFOR.f90)
    endif()
elseif(APPLE)
    # MacOS
    if(${COMPILER_NAME} STREQUAL ifort)
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_MacOS_IFOR.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_MacOS_GFOR.f90)
    endif()
elseif(UNIX)   
    # Unix
    if(${COMPILER_NAME} STREQUAL ifort)      
        # Intel Fortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Linux_IFOR.f90)
    else()
        # GFortran compiler
        set(GLOBAL_DEPS_SRC CFML_GlobalDeps_Linux_GFOR.f90)
    endif()
endif()
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${GLOBAL_DEPS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${GLOBAL_DEPS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Messages
file(GLOB SUBMOD_MESSAGES_SRC CFML_Messages/*.f90)
set(MESSAGES_SRC CFML_Messages.f90 
                 CFML_Messages/Err_Message.f90
                 CFML_Messages/Info_Message.f90
                 CFML_Messages/Print_Message.f90 
                 CFML_Messages/Wait_Message.f90
                 CFML_Messages/Write_ScrollMsg.f90)
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${MESSAGES_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${MESSAGES_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# Mathematics: CFML_Maths,CFML_FFT,CFML_Random
file(GLOB SUBMOD_MATHS_SRC CFML_Maths/*.f90)
file(GLOB SUBMOD_FFT_SRC CFML_FFT/*.f90)
file(GLOB SUBMOD_RANDOM_SRC CFML_Random/*.f90)
set(MATHS_SRC CFML_Maths.f90 
              CFML_FFT.f90
              CFML_Random.f90
              CFML_Trigonometry.f90
              ${SUBMOD_MATHS_SRC}
              ${SUBMOD_FFT_SRC}
              ${SUBMOD_RANDOM_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${MATHS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${MATHS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif() 

# CFML_Strings
file(GLOB SUBMOD_STRINGS_SRC CFML_Strings/*.f90)
set(STRINGS_SRC CFML_Strings.f90 
                ${SUBMOD_STRINGS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${STRINGS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${STRINGS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Rational
file(GLOB SUBMOD_RATIONAL_SRC CFML_Rational/*.f90)
set(RATIONAL_SRC CFML_Rational.f90 
                ${SUBMOD_RATIONAL_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${RATIONAL_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${RATIONAL_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Metrics
file(GLOB SUBMOD_METRICS_SRC CFML_Metrics/*.f90)
set(METRICS_SRC CFML_Metrics.f90 
                ${SUBMOD_METRICS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${METRICS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${METRICS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Tables
set(SUBMOD_TABLES_1_SRC CFML_Tables/Del_ScatterT.f90
                        CFML_Tables/Get_ScatterT.f90
                        CFML_Tables/Del_BondsT.f90
                        CFML_Tables/Get_BondsT.f90
                        CFML_Tables/Del_SpgT.f90
                        CFML_Tables/Get_SpgT.f90
                        CFML_Tables/Del_BVST.f90
                        CFML_Tables/Allocating_MagneticDBase.f90
                        CFML_Tables/Read_MagneticDBase.f90
                        CFML_Tables/Allocating_SuperSpaceDBase.f90 
                        CFML_Tables/Read_SSG_DBase.f90)                        
set(SUBMOD_TABLES_2_SRC CFML_Tables/Set_ScatterT.f90
                        CFML_Tables/Set_BondsT.f90
                        CFML_Tables/Get_SpgSymbols.f90
                        CFML_Tables/Set_SpgT.f90
                        CFML_Tables/Set_BVST.f90)
set(TABLES_1_SRC CFML_Scattering_Tables.f90 
                 CFML_Bonds_Tables.f90 
                 ${SUBMOD_TABLES_1_SRC})
set(TABLES_2_SRC CFML_Symmetry_Tables.f90
                 CFML_BVS_Tables.f90
                 CFML_Magnetic_Database.f90
                 CFML_SuperSpace_Database.f90)
set(TABLES_3_SRC ${SUBMOD_TABLES_2_SRC})
                 
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${TABLES_1_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${TABLES_1_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${TABLES_2_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS0} ${OPT_FLAGS2})
    
    set_source_files_properties(${TABLES_3_SRC} 
        PROPERTIES COMPILE_FLAGS "${OPT_FLAGS} ${OPT_FLAGS0}")
else()
    set_source_files_properties(${TABLES_2_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS0})
    
    set_source_files_properties(${TABLES_3_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS0})
endif()

# CFML_gSpaceGroups
file(GLOB SUBMOD_GROUPS_SRC CFML_gSpaceGroups/*.f90)
set(GROUPS_SRC CFML_gSpaceGroups.f90 
               ${SUBMOD_GROUPS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${GROUPS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${GROUPS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Profiles
file(GLOB SUBMOD_PROFILES_SRC CFML_Profiles/P*.f90
                             CFML_Profiles/T*.f90)
set(PROFILES_1_SRC CFML_Profiles.f90 
                   ${SUBMOD_PROFILES_SRC})   
set(PROFILES_2_SRC CFML_Profiles/Init_ProfVal.f90)             
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${PROFILES_1_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${PROFILES_1_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${PROFILES_2_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS0} ${OPT_FLAGS2})
else()
    set_source_files_properties(${PROFILES_2_SRC}
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS0})
endif()

# CFML_DiffPatt
file(GLOB SUBMOD_DIFFPATT_SRC CFML_DiffPatt/*.f90)
set(DIFFPATT_SRC CFML_DiffPatt.f90 
                 ${SUBMOD_DIFFPATT_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${DIFFPATT_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${DIFFPATT_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_ExtintCorr
file(GLOB SUBMOD_EXTINCORR_SRC CFML_ExtinCorr/*.f90)
set(EXTINCORR_SRC CFML_ExtinCorr.f90 
                  ${SUBMOD_EXTINCORR_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${EXTINCORR_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${EXTINCORR_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_EOS
file(GLOB SUBMOD_EOS_SRC CFML_EoS/*.f90)
set(EOS_SRC CFML_EoS.f90 
            ${SUBMOD_EOS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${EOS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${EOS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Atoms
file(GLOB SUBMOD_ATOMS_SRC CFML_Atoms/*.f90)
set(ATOMS_SRC CFML_Atoms.f90 
              ${SUBMOD_ATOMS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${ATOMS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${ATOMS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Reflections
file(GLOB SUBMOD_REFLECTIONS_SRC CFML_Reflections/*.f90)
set(REFLECTIONS_SRC CFML_Reflections.f90 
                 ${SUBMOD_REFLECTIONS_SRC})
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${REFLECTIONS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${REFLECTIONS_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_Propagk
set(PROPAGK_SRC CFML_Propagk.f90)
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${PROPAGK_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${PROPAGK_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

# CFML_IOForm
#file(GLOB SUBMOD_IOFORM_SRC CFML_IOForm/*.f90)
set(IOFORM_SRC CFML_IOForm.f90 CFML_IOForm/Format_CFL.f90)
if(${COMPILER_NAME} STREQUAL ifort)  
    set_source_files_properties(${IOFORM_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGS} ${OPT_FLAGS1} ${OPT_FLAGS2})
else()
    set_source_files_properties(${IOFORM_SRC} 
        PROPERTIES COMPILE_FLAGS ${OPT_FLAGSC} ${OPT_FLAGS1})
endif()

set(CRYSFML_COMMON_SRC 
    ${GLOBAL_DEPS_SRC}
    ${MESSAGES_SRC}
    ${MATHS_SRC}
    ${STRINGS_SRC}
    ${RATIONAL_SRC}
    ${METRICS_SRC}
    ${TABLES_1_SRC}
    ${TABLES_2_SRC}
    ${TABLES_3_SRC}
    ${GROUPS_SRC}
    ${PROFILE_1_SRC}
    ${PROFILE_2_SRC}
    ${DIFFPATT_SRC}
    ${EXTINCORR_SRC}
    ${EOS_SRC}
    ${ATOMS_SRC}
    ${REFLECTIONS_SRC}
    ${PROPAGK_SRC}
    ${IOFORM_SRC})
    
# Build the library
set(LIBRARY_NAME crysfml)

add_library(${LIBRARY_NAME} STATIC ${CRYSFML_COMMON_SRC})

# The directory where the CrysFML modules files will be stored.
set(CRYSFML_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Src08/crysfml_modules)

# Sets the path where to place the mod files for the crysfml_common library.
set_target_properties(${LIBRARY_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CRYSFML_MODULE_DIRECTORY})

#################################
# Install section
#################################

# The rules for installing the library.
install(TARGETS ${LIBRARY_NAME} ARCHIVE DESTINATION ${CRYSFML_PREFIX})

# The rules for installing the mod files. Take care the "/" is on purpose.
install(DIRECTORY ${CRYSFML_MODULE_DIRECTORY}/ 
        DESTINATION ${CRYSFML_PREFIX}
        FILES_MATCHING
        PATTERN "*.*mod"
        PATTERN CMakeFiles EXCLUDE)