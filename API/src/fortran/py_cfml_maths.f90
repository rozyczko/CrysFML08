! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_maths

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_maths

    implicit none

    contains

    function py_erfc_deriv(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: x ! CrysFML type: real
        real(kind=cp) :: der ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(x,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            der = erfc_deriv(x)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"erfc_deriv: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,der)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_erfc_deriv

    function py_debye(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        real(kind=cp) :: x ! CrysFML type: real
        real(kind=cp) :: fval ! CrysFML type: real

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
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(x,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fval = debye(n,x)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"debye: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fval)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_debye

    function py_factorial_i(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        integer :: fact ! CrysFML type: integer

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
            fact = factorial_i(n)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"factorial_i: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fact)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_factorial_i

    function py_factorial_r(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        real(kind=cp) :: fact ! CrysFML type: real

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
            fact = factorial_r(n)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"factorial_r: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fact)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_factorial_r

    function py_gcd(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: a ! CrysFML type: integer
        integer :: b ! CrysFML type: integer
        integer :: mcd ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(a,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(b,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            mcd = gcd(a,b)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"gcd: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,mcd)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_gcd

    function py_lcm(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: a ! CrysFML type: integer
        integer :: b ! CrysFML type: integer
        integer :: mcm ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(a,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(b,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            mcm = lcm(a,b)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"lcm: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,mcm)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_lcm

    function py_poly_legendre(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: l ! CrysFML type: integer
        integer :: m ! CrysFML type: integer
        real(kind=cp) :: x ! CrysFML type: real
        real(kind=cp) :: plmx ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(l,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(m,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(x,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            plmx = poly_legendre(l,m,x)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"poly_legendre: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,plmx)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_poly_legendre

    function py_cubic_harm_ang(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: l ! CrysFML type: integer
        integer :: m ! CrysFML type: integer
        real(kind=cp) :: theta ! CrysFML type: real
        real(kind=cp) :: phi ! CrysFML type: real
        real(kind=cp) :: klm ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(l,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(m,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(theta,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(phi,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            klm = cubic_harm_ang(l,m,theta,phi)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"cubic_harm_ang: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,klm)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_cubic_harm_ang

    function py_integral_slater_bessel(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        integer :: l ! CrysFML type: integer
        real(kind=cp) :: z ! CrysFML type: real
        real(kind=cp) :: s ! CrysFML type: real
        real(kind=cp) :: v ! CrysFML type: real

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
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(l,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(z,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(s,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            v = integral_slater_bessel(n,l,z,s)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"integral_slater_bessel: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,v)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_integral_slater_bessel

    function py_real_spher_harm_ang(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: l ! CrysFML type: integer
        integer :: m ! CrysFML type: integer
        integer :: p ! CrysFML type: integer
        real(kind=cp) :: theta ! CrysFML type: real
        real(kind=cp) :: phi ! CrysFML type: real
        real(kind=cp) :: ylmp ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(l,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(m,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(p,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(theta,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(phi,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            ylmp = real_spher_harm_ang(l,m,p,theta,phi)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"real_spher_harm_ang: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,ylmp)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_real_spher_harm_ang

    function py_set_eps_math(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: neweps ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(neweps,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call set_eps_math(neweps)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"set_eps_math: "//trim(err_cfml%msg))
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

    end function py_set_eps_math

end module py_cfml_maths
