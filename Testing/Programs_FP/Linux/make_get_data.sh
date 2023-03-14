#!/bin/bash
# ------------------------------
# Script to compile the program: Get_Data
# Authors: NAK, JGP, JRC
# Date: March 2023
# ------------------------------
# General Options
COMP="ifort"
DEBUG="N"
ARCH="m64"
HDF5_INSTALL=/usr/local/HDF_Group/HDF5/1.14.0
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
   SRC_NXS=$CRYSFML08/HDF5
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
cd ../../Get_Data
echo " ########################################################"
echo " ####  Get_Data Program                        (2.0) ####"
echo " ####  JRC                             CopyLeft-2023 ####"
echo " ########################################################"
$COMP $OPT1 $SRC_NXS/Nexus_Mod.f90    $INC
$COMP $OPT1 D2B_read_mod.f90          $INC
$COMP $OPT1 D2B_read_mod.f90          $INC
$COMP $OPT1 D2B_data_mod.f90          $INC
$COMP $OPT1 D2B_int_mod.f90           $INC
$COMP $OPT1 GetData_Globals.f90       $INC
$COMP $OPT1 Get_data.f90              $INC
$COMP -$ARCH *.o -o get_data -static-intel $LIB  $LIBSTATIC  $HDF5_INSTALL/lib -L  $ZLIB_DIR -l hdf5_fortran -l hdf5_f90cstub -l hdf5  -l z

#
# Final process
#
upx get_data
rm -rf *.o *.mod
cp get_data $FULLPROF/.
mv get_data $PROGCFML/DistFPS/$VERS/.
cd ../Programs_FP/Linux
#
