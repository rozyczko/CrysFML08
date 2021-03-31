!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Modulo_Lat
 implicit none
 Contains

    !!----
    !!---- MODULO_LAT
    !!----    Reduces a real vector to another with components in
    !!----    the interval [0,1)
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Modulo_Lat(v) result(u)
       !---- Argument ----!
       real(kind=cp), dimension(:), intent( in) :: v
       real(kind=cp), dimension(1:size(v))      :: u

       u=mod(v+10.0_cp,1.0_cp)

    End Function Modulo_Lat
    !!----
    !!---- LAT_MODULO
    !!----    Reduces a real vector u to another v with components in
    !!----    the interval [0,1) and returns the traslation  L to perform the operation
    !!----    The input vertor is u, the output vector is: v=u+L
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Subroutine Lat_Modulo(u,v,lat)
       !---- Argument ----!
       real(kind=cp), dimension(:),         intent( in) :: u
       real(kind=cp), dimension(1:size(u)), intent(out) :: v
       integer,       dimension(1:size(u)), intent(out) :: Lat
       integer :: i,j
       v=mod(u+10.0_cp,1.0_cp)
       Lat=nint(u-v)
    End Subroutine Lat_Modulo

End Submodule Maths_Modulo_Lat
