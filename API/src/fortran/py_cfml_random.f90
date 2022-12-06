! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_random

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_random

    implicit none

    contains

    function py_random_binomial1(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        real(kind=cp) :: p ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        integer :: ival ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(p,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            ival = random_binomial1(n,p,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_binomial1: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,ival)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_binomial1

    function py_random_binomial2(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: n ! CrysFML type: integer
        real(kind=cp) :: pp ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        integer :: ival ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(pp,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            ival = random_binomial2(n,pp,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_binomial2: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,ival)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_binomial2

    function py_random_chisq(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: ndf ! CrysFML type: integer
        logical :: first ! CrysFML type: logical
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(ndf,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_chisq(ndf,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_chisq: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_chisq

    function py_random_gamma(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: s ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(s,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_gamma(s,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_gamma: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_gamma

    function py_random_gamma1(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: s ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(s,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_gamma1(s,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_gamma1: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_gamma1

    function py_random_gamma2(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: s ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(s,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_gamma2(s,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_gamma2: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_gamma2

    function py_random_neg_binomial(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: sk ! CrysFML type: real
        real(kind=cp) :: p ! CrysFML type: real
        integer :: ival ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(sk,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(p,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            ival = random_neg_binomial(sk,p)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_neg_binomial: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,ival)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_neg_binomial

    function py_random_poisson(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: mt ! CrysFML type: real
        integer :: ival ! CrysFML type: integer

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
        if (ierror == 0) ierror = cast(mt,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            ival = random_poisson(mt)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_poisson: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,ival)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_poisson

    function py_random_t(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        integer :: m ! CrysFML type: integer
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(m,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_t(m)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_t: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_t

    function py_random_von_mises(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: k ! CrysFML type: real
        logical :: first ! CrysFML type: logical
        real(kind=cp) :: fn_val ! CrysFML type: real

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
        if (ierror == 0) ierror = cast(k,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(first,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_von_mises(k,first)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_von_mises: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_von_mises

    function py_random_weibull(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: a ! CrysFML type: real
        real(kind=cp) :: fn_val ! CrysFML type: real

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

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            fn_val = random_weibull(a)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"random_weibull: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,fn_val)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_random_weibull

end module py_cfml_random
