! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_keycodes

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_keycodes

    implicit none

    contains

    function py_allocate_vecref(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(n,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call allocate_vecref(n)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"allocate_vecref: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,0)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_allocate_vecref

    function py_writeinfo_refparams(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: iunit ! CrysFML type: integer

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(iunit,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call writeinfo_refparams(iunit)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"writeinfo_refparams: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,0)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_writeinfo_refparams

end module py_cfml_keycodes
