submodule (CFML_Wraps) Wraps_Profiles

    implicit none
    contains

    Module Subroutine Unwrap_deriv_tof_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(deriv_tof_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_deriv_tof_type: Cannot determine fortran type'
        else
            if (fortran_type == 'deriv_tof_type') then
                allocate(deriv_tof_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_deriv_tof_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','alfa',py_var,for_var%alfa,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','beta',py_var,for_var%beta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','sigma',py_var,for_var%sigma,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','gamm',py_var,for_var%gamm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','eta',py_var,for_var%eta,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_deriv_tof_type','kappa',py_var,for_var%kappa,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_deriv_tof_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_deriv_tof_type

    Module Subroutine Wrap_deriv_tof_type(py_var,for_var,ierror)

        ! Arguments
        type(deriv_tof_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('alfa',for_var%alfa)
        if (ierror == 0) ierror = py_var%setitem('beta',for_var%beta)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (ierror == 0) ierror = py_var%setitem('sigma',for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('gamm',for_var%gamm)
        if (ierror == 0) ierror = py_var%setitem('eta',for_var%eta)
        if (ierror == 0) ierror = py_var%setitem('kappa',for_var%kappa)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_deriv_tof_type: Wrapping failed'
        end if

    End Subroutine Wrap_deriv_tof_type

end submodule