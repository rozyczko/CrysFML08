#!/bin/bash
# ------------------------------
# Script to compile the program: nDataRed
# Authors: JGP, JRC
# Date: January 2021
# ------------------------------
# General Options
COMP="ifort"
DEBUG="N"
ARCH="m64"
#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "m32")
         ARCH=$arg
         ;;
      "m64")
         ARCH=$arg
         ;;
      "debug"*)
         DEBUG="Y"
         ;;
   esac
done
#
# Settings
#
if [ $ARCH == "m32" ]; then
   INC="-I$CRYSFML08/ifort/LibC "
   LIB="-L$CRYSFML08/ifort/LibC"
   LIBSTATIC="-lcrysfml"
   VERS="Linux"
else
   INC="-I$CRYSFML08/ifort64/LibC"
   LIB="-L$CRYSFML08/ifort64/LibC"
   LIBSTATIC="-lcrysfml"
   VERS="Linux64"
fi
if [ $DEBUG == "Y" ]; then
   OPT1="-c -g -heap-arrays -$ARCH"
else
   OPT1="-c -warn -heap-arrays -$ARCH -O2 -qopt-report=0"
fi
#
#
# Compilation Process
#
cd ../../DataRed
echo " ########################################################"
echo " ####  nDataRed Program                        (1.0) ####"
echo " ####  JRC                             CopyLeft-2022 ####"
echo " ########################################################"
$COMP $OPT1 twin_mod.f90                  $INC
$COMP $OPT1 datared_mod.f90               $INC
$COMP $OPT1 datared_rnw_reflections.f90   $INC
$COMP $OPT1 datared_treat_reflections.f90 $INC
$COMP $OPT1 datared.f90                   $INC
$COMP -$ARCH *.o -o nDataRed -static-intel $LIB  $LIBSTATIC
#
# Final process
#
upx nDataRed
rm -rf *.o *.mod
cp nDataRed $FULLPROF/.
mv nDataRed $PROGCFML/DistFPS/MacOS
cd ../Programs_FP/MacOS
#
