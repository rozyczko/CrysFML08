#!/bin/bash

# ----------------------------------------
# Script to compile  ODR and ODR_wrapper
# Authors: JGP, JRC
# Date: May 2022
# Compilers: ifort, gfortran
# ----------------------------------------

# Checking CrySFML Environment Variable
if [ -z "$CRYSFML08" ]; then
   echo "****"
   echo "**** Please, set the environment variable CRYSFML08 in your .bash_profile"
   echo "****"
   exit
fi
#
# Default Options
#
compiler="ifort"
debug="N"

#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "ifort")
         compiler=$arg
         ;;
      "gfortran")
         compiler=$arg
         ;;
      "debug"*)
         debug="Y"
         ;;
   esac
done

#
# Intel compiler
#
if [ $compiler == "ifort" ]; then
   inc="-I$CRYSFML08/ifort64/LibC"
   if [ $debug == "Y" ]; then
      opt1="-c -g -warn -m64"
   else
      opt1="-c -O3 -m64 -qopt-report=0"
   fi
fi

#
# GFortran compiler
#
if [ $compiler == "gfortran" ]; then
   inc="-I$CRYSFML08/GFortran64/LibC"
   if [ $debug == "Y" ]; then
      opt1="-c -g -m64 -ffree-line-length-none"
   else
      opt1="-c -O3 -m64 -ffree-line-length-none"
   fi
fi
#
# Compilation Process
#
echo " ########################################################"
echo " #### Building ODR and ODR_wrapper             (1.0) ####"
echo " ########################################################"
echo " Compiling ODR library in single precision"
   $compiler $opt1 sp_precision.f
   $compiler $opt1 lpkbls.f
   $compiler $opt1 odr.f
   $compiler $opt1 ODR_wrapper.f90   $inc
#
# Library
#
   ar cr libodr_sp.a *.o
if [ $compiler == "gfortran" ]; then
   mv *.mod $CRYSFML08/GFortran64/ODR_sp/.
   mv *.a $CRYSFML08/GFortran64/ODR_sp/.
else
   mv *.mod $CRYSFML08/ifort64/ODR_sp/.
   mv *.a $CRYSFML08/ifort64/ODR_sp/.
fi
#
rm *.o
