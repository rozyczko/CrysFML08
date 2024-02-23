submodule (CFML_Wraps) Wraps_Optimization

    implicit none
    contains

    Module Subroutine Unwrap_opt_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(opt_conditions_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_opt_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type == 'opt_conditions_type') then
                allocate(opt_conditions_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_opt_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','method',py_var,for_var%method,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','nmeth',py_var,for_var%nmeth,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','npar',py_var,for_var%npar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','mxfun',py_var,for_var%mxfun,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','loops',py_var,for_var%loops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','iquad',py_var,for_var%iquad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','iout',py_var,for_var%iout,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','nflag',py_var,for_var%nflag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','ifun',py_var,for_var%ifun,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','iter',py_var,for_var%iter,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','eps',py_var,for_var%eps,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_opt_conditions_type','acc',py_var,for_var%acc,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_opt_conditions_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_opt_conditions_type

    Module Subroutine Wrap_opt_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(opt_conditions_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('method',for_var%method)
        if (ierror == 0) ierror = py_var%setitem('nmeth',for_var%nmeth)
        if (ierror == 0) ierror = py_var%setitem('npar',for_var%npar)
        if (ierror == 0) ierror = py_var%setitem('mxfun',for_var%mxfun)
        if (ierror == 0) ierror = py_var%setitem('loops',for_var%loops)
        if (ierror == 0) ierror = py_var%setitem('iquad',for_var%iquad)
        if (ierror == 0) ierror = py_var%setitem('iout',for_var%iout)
        if (ierror == 0) ierror = py_var%setitem('nflag',for_var%nflag)
        if (ierror == 0) ierror = py_var%setitem('ifun',for_var%ifun)
        if (ierror == 0) ierror = py_var%setitem('iter',for_var%iter)
        if (ierror == 0) ierror = py_var%setitem('eps',for_var%eps)
        if (ierror == 0) ierror = py_var%setitem('acc',for_var%acc)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_opt_conditions_type: Wrapping failed'
        end if

    End Subroutine Wrap_opt_conditions_type

end submodule