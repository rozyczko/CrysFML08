! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_sxtal_geom

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_sxtal_geom

    implicit none

    contains

    function py_chkin180(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: angle ! CrysFML type: real
        real(kind=cp) :: angle_in ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(angle,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            angle_in = chkin180(angle)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"chkin180: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,angle_in)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_chkin180

    function py_psd_convert(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: mpsd ! CrysFML type: integer
        real(kind=cp) :: gamm ! CrysFML type: real
        real(kind=cp) :: gamp ! CrysFML type: real
        real(kind=cp) :: nup ! CrysFML type: real
        real(kind=cp) :: xobs ! CrysFML type: real
        real(kind=cp) :: zobs ! CrysFML type: real
        real(kind=cp) :: cath ! CrysFML type: real
        real(kind=cp) :: anod ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(mpsd,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(gamm,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(gamp,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nup,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(cath,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(anod,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call psd_convert(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"psd_convert: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,6)
        if (ierror == 0) ierror = ret%setitem(0,gamp)
        if (ierror == 0) ierror = ret%setitem(1,nup)
        if (ierror == 0) ierror = ret%setitem(2,xobs)
        if (ierror == 0) ierror = ret%setitem(3,zobs)
        if (ierror == 0) ierror = ret%setitem(4,cath)
        if (ierror == 0) ierror = ret%setitem(5,anod)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_psd_convert

    function py_d19psd(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: mpsd ! CrysFML type: integer
        real(kind=cp) :: ga ! CrysFML type: real
        real(kind=cp) :: nu ! CrysFML type: real
        real(kind=cp) :: cath ! CrysFML type: real
        real(kind=cp) :: anod ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(mpsd,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(ga,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(nu,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(cath,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(anod,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call d19psd(mpsd,ga,nu,cath,anod)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"d19psd: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,4)
        if (ierror == 0) ierror = ret%setitem(0,ga)
        if (ierror == 0) ierror = ret%setitem(1,nu)
        if (ierror == 0) ierror = ret%setitem(2,cath)
        if (ierror == 0) ierror = ret%setitem(3,anod)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_d19psd

    function py_set_psd(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: dist ! CrysFML type: real
        real(kind=cp) :: cg ! CrysFML type: real
        real(kind=cp) :: ag ! CrysFML type: real
        integer :: nh ! CrysFML type: integer
        integer :: nv ! CrysFML type: integer
        integer :: ip ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(dist,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(cg,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(ag,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nh,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(nv,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(ip,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call set_psd(dist,cg,ag,nh,nv,ip)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"set_psd: "//trim(err_cfml%msg))
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

    end function py_set_psd

end module py_cfml_sxtal_geom
