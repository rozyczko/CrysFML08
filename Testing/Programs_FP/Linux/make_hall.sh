#!/bin/bash
# ----------------------------------------
# Script to compile the program: MHall
# Authors: JGP, JRC
# Date: January 2021
# ----------------------------------------
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
   INC="-I$CRYSFML08/ifort/include"
   LIB="-L$CRYSFML08/ifort/lib"
   LIBSTATIC="-lCrysFML08"
   VERS="Linux"
else
   INC="-I$CRYSFML08/ifort64/include"
   LIB="-L$CRYSFML08/ifort64/lib"
   LIBSTATIC="-lCrysFML08"
   VERS="Linux64"
fi
if [ $DEBUG == "Y" ]; then
   OPT1="-c -g -$ARCH -heap-arrays"
else
   OPT1="-c -warn -$ARCH -O2 -qopt-report=0 -heap-arrays"
fi
#
#
# Compilation Process
#
cd ../../Hall_Symbols
echo " ########################################################"
echo " #### MHall       Program                      (1.0) ####"
echo " #### JRC                              CopyLeft-2020 ####"
echo " ########################################################"
$COMP $OPT1 MHall.f90 $INC
$COMP -$ARCH *.o -o MHall -static-intel $LIB  $LIBSTATIC
#
# Final process
#
upx MHall
rm -rf *.o *.mod
cp MHall $FULLPROF/.
mv MHall $PROGCFML/DistFPS/$VERS
cd ../Programs_FP/Linux
#

