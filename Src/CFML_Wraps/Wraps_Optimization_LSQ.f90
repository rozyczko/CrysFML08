submodule (CFML_Wraps) Wraps_Optimization_LSQ

    implicit none
    contains

    Module Subroutine Unwrap_lsq_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(lsq_conditions_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_lsq_conditions_type: Cannot determine fortran type'
        else
            if (fortran_type == 'lsq_conditions_type') then
                allocate(lsq_conditions_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_lsq_conditions_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','constr',py_var,for_var%constr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','reached',py_var,for_var%reached,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','failed',py_var,for_var%failed,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','corrmax',py_var,for_var%corrmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','nfev',py_var,for_var%nfev,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','njev',py_var,for_var%njev,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','icyc',py_var,for_var%icyc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','npvar',py_var,for_var%npvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','iw',py_var,for_var%iw,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','nprint',py_var,for_var%nprint,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','tol',py_var,for_var%tol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_conditions_type','percent',py_var,for_var%percent,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_lsq_conditions_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_lsq_conditions_type

    Module Subroutine Wrap_lsq_conditions_type(py_var,for_var,ierror)

        ! Arguments
        type(lsq_conditions_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('constr',for_var%constr)
        if (ierror == 0) ierror = py_var%setitem('reached',for_var%reached)
        if (ierror == 0) ierror = py_var%setitem('failed',for_var%failed)
        if (ierror == 0) ierror = py_var%setitem('corrmax',for_var%corrmax)
        if (ierror == 0) ierror = py_var%setitem('nfev',for_var%nfev)
        if (ierror == 0) ierror = py_var%setitem('njev',for_var%njev)
        if (ierror == 0) ierror = py_var%setitem('icyc',for_var%icyc)
        if (ierror == 0) ierror = py_var%setitem('npvar',for_var%npvar)
        if (ierror == 0) ierror = py_var%setitem('iw',for_var%iw)
        if (ierror == 0) ierror = py_var%setitem('nprint',for_var%nprint)
        if (ierror == 0) ierror = py_var%setitem('tol',for_var%tol)
        if (ierror == 0) ierror = py_var%setitem('percent',for_var%percent)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_lsq_conditions_type: Wrapping failed'
        end if

    End Subroutine Wrap_lsq_conditions_type

    Module Subroutine Unwrap_lsq_data_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(lsq_data_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_lsq_data_type: Cannot determine fortran type'
        else
            if (fortran_type == 'lsq_data_type') then
                allocate(lsq_data_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_lsq_data_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','nobs',py_var,for_var%nobs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','iw',py_var,for_var%iw,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_lsq_data_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','y',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_lsq_data_type','y',p_real_1d,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','sw',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_lsq_data_type','sw',p_real_1d,for_var%sw,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_data_type','yc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_lsq_data_type','yc',p_real_1d,for_var%yc,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_lsq_data_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_lsq_data_type

    Module Subroutine Wrap_lsq_data_type(py_var,for_var,ierror)

        ! Arguments
        type(lsq_data_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_x,nd_y,nd_sw,nd_yc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nobs',for_var%nobs)
        if (ierror == 0) ierror = py_var%setitem('iw',for_var%iw)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror == 0) ierror = ndarray_create(nd_y,for_var%y)
        if (ierror == 0) ierror = py_var%setitem('y',nd_y)
        if (ierror == 0) ierror = ndarray_create(nd_sw,for_var%sw)
        if (ierror == 0) ierror = py_var%setitem('sw',nd_sw)
        if (ierror == 0) ierror = ndarray_create(nd_yc,for_var%yc)
        if (ierror == 0) ierror = py_var%setitem('yc',nd_yc)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_lsq_data_type: Wrapping failed'
        end if

    End Subroutine Wrap_lsq_data_type

    Module Subroutine Unwrap_lsq_state_vector_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(lsq_state_vector_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_lsq_state_vector_type: Cannot determine fortran type'
        else
            if (fortran_type == 'lsq_state_vector_type') then
                allocate(lsq_state_vector_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_lsq_state_vector_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','np',py_var,for_var%np,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','code_comp',py_var,for_var%code_comp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','code_max',py_var,for_var%code_max,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','mul',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_lsq_state_vector_type','mul',p_real_1d,for_var%mul,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','pv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_lsq_state_vector_type','pv',p_real_1d,for_var%pv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','spv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_lsq_state_vector_type','spv',p_real_1d,for_var%spv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','dpv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_lsq_state_vector_type','dpv',p_real_1d,for_var%dpv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','code',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_lsq_state_vector_type','code',p_int_1d,for_var%code,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_lsq_state_vector_type','nampar',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_lsq_state_vector_type','nampar',my_list,for_var%nampar,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_lsq_state_vector_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_lsq_state_vector_type

    Module Subroutine Wrap_lsq_state_vector_type(py_var,for_var,ierror)

        ! Arguments
        type(lsq_state_vector_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_nampar
        type(ndarray) :: nd_mul,nd_pv,nd_spv,nd_dpv,nd_code

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('np',for_var%np)
        if (ierror == 0) ierror = py_var%setitem('code_comp',for_var%code_comp)
        if (ierror == 0) ierror = py_var%setitem('code_max',for_var%code_max)
        if (ierror == 0) ierror = ndarray_create(nd_mul,for_var%mul)
        if (ierror == 0) ierror = py_var%setitem('mul',nd_mul)
        if (ierror == 0) ierror = ndarray_create(nd_pv,for_var%pv)
        if (ierror == 0) ierror = py_var%setitem('pv',nd_pv)
        if (ierror == 0) ierror = ndarray_create(nd_spv,for_var%spv)
        if (ierror == 0) ierror = py_var%setitem('spv',nd_spv)
        if (ierror == 0) ierror = ndarray_create(nd_dpv,for_var%dpv)
        if (ierror == 0) ierror = py_var%setitem('dpv',nd_dpv)
        if (ierror == 0) ierror = ndarray_create(nd_code,for_var%code)
        if (ierror == 0) ierror = py_var%setitem('code',nd_code)
        if (ierror == 0) ierror = list_create(li_nampar)
        if (ierror == 0) then
            do i = 1 , size(for_var%nampar)
                if (ierror == 0) ierror = li_nampar%append(for_var%nampar(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('nampar',li_nampar)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_lsq_state_vector_type: Wrapping failed'
        end if

    End Subroutine Wrap_lsq_state_vector_type

end submodule