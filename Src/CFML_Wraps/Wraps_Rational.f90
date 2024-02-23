submodule (CFML_Wraps) Wraps_Rational

    implicit none
    contains

    Module Subroutine Unwrap_rational(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(rational), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_rational: Cannot determine fortran type'
        else
            if (fortran_type == 'rational') then
                allocate(rational :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_rational: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_rational','numerator',py_var,for_var%numerator,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_rational','denominator',py_var,for_var%denominator,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_rational: Unwrapping failed'
        end if

    End Subroutine Unwrap_rational

    Module Subroutine Wrap_rational(py_var,for_var,ierror)

        ! Arguments
        type(rational), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('numerator',for_var%numerator)
        if (ierror == 0) ierror = py_var%setitem('denominator',for_var%denominator)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_rational: Wrapping failed'
        end if

    End Subroutine Wrap_rational

end submodule