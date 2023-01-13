echo " Compiling ODR library in single precision"
   ifort sp_precision.f    -c
   ifort lpkbls.f          -c -O2
   ifort odr.f             -c -O2
   ifort ODR_wrapper.f90   -c -O2 -I$CRYSFML/ifort64/LibC
#
# Library
#
   ar cr libodr_sp.a *.o
#
   mv *.mod $CRYSFML/ifort64/ODR_sp/.
   mv *.a $CRYSFML/ifort64/ODR_sp/.
#
rm *.o
