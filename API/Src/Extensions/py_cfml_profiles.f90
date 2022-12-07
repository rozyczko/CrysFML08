! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_profiles

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_profiles

    implicit none

    contains

    function py_psvoigtian(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: twoth ! CrysFML type: real
        real(kind=cp) :: twoth0 ! CrysFML type: real
        real(kind=cp) :: eta ! CrysFML type: real
        real(kind=cp) :: gamma ! CrysFML type: real
        real(kind=cp) :: dprdt ! CrysFML type: real
        real(kind=cp) :: dprdg ! CrysFML type: real
        real(kind=cp) :: dprde ! CrysFML type: real
        real(kind=cp) :: psvoigt ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(twoth,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(twoth0,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(eta,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(gamma,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call psvoigtian(twoth,twoth0,eta,gamma,dprdt,dprdg,dprde,psvoigt)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"psvoigtian: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,4)
        if (ierror == 0) ierror = ret%setitem(0,dprdt)
        if (ierror == 0) ierror = ret%setitem(1,dprdg)
        if (ierror == 0) ierror = ret%setitem(2,dprde)
        if (ierror == 0) ierror = ret%setitem(3,psvoigt)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_psvoigtian

    function py_prof_val(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: eta ! CrysFML type: real
        real(kind=cp) :: gamma ! CrysFML type: real
        real(kind=cp) :: asym1 ! CrysFML type: real
        real(kind=cp) :: asym2 ! CrysFML type: real
        real(kind=cp) :: twoth ! CrysFML type: real
        real(kind=cp) :: twoth0 ! CrysFML type: real
        real(kind=cp) :: dprdt ! CrysFML type: real
        real(kind=cp) :: dprdg ! CrysFML type: real
        real(kind=cp) :: dprde ! CrysFML type: real
        real(kind=cp) :: dprds ! CrysFML type: real
        real(kind=cp) :: dprdd ! CrysFML type: real
        real(kind=cp) :: profval ! CrysFML type: real
        logical :: use_asym ! CrysFML type: logical
        logical :: use_hps ! CrysFML type: logical

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
        if (ierror == 0) ierror = cast(eta,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(gamma,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(asym1,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(asym2,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(twoth,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(twoth0,item)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) ierror = cast(use_asym,item)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) ierror = cast(use_hps,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call prof_val(eta,gamma,asym1,asym2,twoth,twoth0,dprdt,dprdg,dprde,dprds,dprdd,profval,use_asym,use_hps)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"prof_val: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,6)
        if (ierror == 0) ierror = ret%setitem(0,dprdt)
        if (ierror == 0) ierror = ret%setitem(1,dprdg)
        if (ierror == 0) ierror = ret%setitem(2,dprde)
        if (ierror == 0) ierror = ret%setitem(3,dprds)
        if (ierror == 0) ierror = ret%setitem(4,dprdd)
        if (ierror == 0) ierror = ret%setitem(5,profval)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_prof_val

    function py_prof_gaussian(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: pos ! CrysFML type: real
        real(kind=cp) :: pos0 ! CrysFML type: real
        real(kind=cp) :: gamma ! CrysFML type: real
        real(kind=cp) :: dgdt ! CrysFML type: real
        real(kind=cp) :: dgdg ! CrysFML type: real
        real(kind=cp) :: gauss ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(pos,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(pos0,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(gamma,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call prof_gaussian(pos,pos0,gamma,dgdt,dgdg,gauss)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"prof_gaussian: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,3)
        if (ierror == 0) ierror = ret%setitem(0,dgdt)
        if (ierror == 0) ierror = ret%setitem(1,dgdg)
        if (ierror == 0) ierror = ret%setitem(2,gauss)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_prof_gaussian

    function py_prof_lorentzian(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: pos ! CrysFML type: real
        real(kind=cp) :: pos0 ! CrysFML type: real
        real(kind=cp) :: gamma ! CrysFML type: real
        real(kind=cp) :: dldt ! CrysFML type: real
        real(kind=cp) :: dldg ! CrysFML type: real
        real(kind=cp) :: lorentz ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(pos,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(pos0,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(gamma,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call prof_lorentzian(pos,pos0,gamma,dldt,dldg,lorentz)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"prof_lorentzian: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,3)
        if (ierror == 0) ierror = ret%setitem(0,dldt)
        if (ierror == 0) ierror = ret%setitem(1,dldg)
        if (ierror == 0) ierror = ret%setitem(2,lorentz)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_prof_lorentzian

end module py_cfml_profiles
