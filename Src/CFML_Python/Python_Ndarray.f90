!!----
!!----
!!----
SubModule (CFML_Python)  Ndarray_Wraps

implicit none
    Contains

    !!----
    !!---- TO_NDARRAY
    !!----
    !!---- 03/07/2023
    !!

    Module Function Rational_To_Ndarray_1d(rational_arr,ierror) Result(nd_arr)
        !---- Arguments ----!
        type(rational), dimension(:), intent(in)  :: rational_arr
        integer,                      intent(out) :: ierror
        type(ndarray)                             :: nd_arr

        !---- Local Variables ----!
        real, dimension(:), allocatable :: real_arr

        allocate(real_arr(size(rational_arr)))
        real_arr = real(rational_arr)
        ierror = ndarray_create(nd_arr,real_arr)
    End Function  Rational_To_Ndarray_1d

    Module Function Rational_To_Ndarray_2d(rational_arr,ierror) Result(nd_arr)
        !---- Arguments ----!
        type(rational), dimension(:,:), intent(in)  :: rational_arr
        integer,                        intent(out) :: ierror
        type(ndarray)                               :: nd_arr

        !---- Local Variables ----!
        real, dimension(:,:), allocatable :: real_arr

        allocate(real_arr(size(rational_arr,1),size(rational_arr,2)))
        real_arr = real(rational_arr)
        ierror = ndarray_create(nd_arr,real_arr)
    End Function  Rational_To_Ndarray_2d

    Module Function Rational_To_Ndarray_3d(rational_arr,ierror) Result(nd_arr)
        !---- Arguments ----!
        type(rational), dimension(:,:,:), intent(in)  :: rational_arr
        integer,                          intent(out) :: ierror
        type(ndarray)                                 :: nd_arr

        !---- Local Variables ----!
        real, dimension(:,:,:), allocatable :: real_arr

        allocate(real_arr(size(rational_arr,1),size(rational_arr,2),size(rational_arr,3)))
        real_arr = real(rational_arr)
        ierror = ndarray_create(nd_arr,real_arr)
    End Function  Rational_To_Ndarray_3d

End Submodule Ndarray_Wraps