#!/bin/bash
# ----------------------------------------
# Script to compile the program: MHall
# Authors: JGP, JRC
# Date: January 2021
# ----------------------------------------
# General Options
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
   lib="-L$CRYSFML08/ifort64/LibC"
   mode="-static-intel"
   if [ $debug == "Y" ]; then
      opt1="-c -g -warn -arch x86_64 -heap-arrays"
   else
      opt1="-c -O2 -arch x86_64 -qopt-report=0 -heap-arrays"
   fi
fi

#
# GFortran compiler
#
if [ $compiler == "gfortran" ]; then
   inc="-I$CRYSFML/GFortran64/LibC"
   lib="-L$CRYSFML/GFortran64/LibC"
   mode="-static-libgfortran"
   if [ $debug == "Y" ]; then
      opt1="-c -g -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   else
      opt1="-c -O2 -arch x86_64 -ffree-line-length-none -fno-stack-arrays"
   fi
fi

# Compilation Process
#
cd ../../Hall_Symbols
echo " ########################################################"
echo " #### MHall       Program                      (1.0) ####"
echo " #### JRC                              CopyLeft-2020 ####"
echo " ########################################################"
$compiler $opt1 MHall.f90 $inc
$compiler -arch x86_64 *.o -o MHall $lib $mode
#
# Final process
#
rm -rf *.o *.mod *_genmod.f90
upx MHall
if [ ! -d $PROGCFML/DistFPS/MacOS ]; then
   mkdir $PROGCFML/DistFPS/MacOS
fi
cp MHall $PROGCFML/DistFPS/MacOS
#
if [ -d $FULLPROF ]; then
   mv -f MHall $FULLPROF
fi

cd ../Programs_FP/MacOS
# END


