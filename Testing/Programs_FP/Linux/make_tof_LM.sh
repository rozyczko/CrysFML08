#!/bin/bash
# ------------------------------
# Script to compile the program: TOF_fit_LM
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
   INC="-I$CRYSFML08/ifort/LibC"
   LIB="-L$CRYSFML08/ifort/LibC"
   LIBSTATIC="-lcrysfml"
   VERS="Linux"
else
   INC="-I$CRYSFML08/ifort64/LibC"
   INCODR="-I$CRYSFML08/ifort64/ODR_sp"
   LIB="-L$CRYSFML08/ifort64/LibC"
   LIBODR="-L$CRYSFML08/ifort64/ODR_sp"
   LIBSTATIC="-lcrysfml"
   LIBSTATIC_ODR="-lodr_sp"
   VERS="Linux64"
fi
if [ $DEBUG == "Y" ]; then
   OPT1="-c -g -$ARCH"
else
   OPT1="-c -warn -$ARCH -O2 -qopt-report=0"
fi
#
#
# Compilation Process
#
cd ../../TOF-fit
echo " ########################################################"
echo " #### TOF_fit_LM   Program                     (1.0) ####"
echo " #### JRC                              CopyLeft-2021 ####"
echo " ########################################################"
$COMP $OPT1 TOF_module_LM.f90 $INCODR $INC
$COMP $OPT1 TOF_fitting_LM.f90 $INCODR $INC
$COMP -$ARCH *.o -o TOF_fit_LM -static-intel $LIBODR $LIBSTATIC_ODR $LIB  $LIBSTATIC
#
# Final process
#
upx TOF_fit_LM
rm -rf *.o *.mod
cp TOF_fit_LM $FULLPROF/.
mv TOF_fit_LM $PROGCFML/DistFPS/$VERS/.
cd ../Programs_FP/Linux
#
