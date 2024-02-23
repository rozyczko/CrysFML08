submodule (CFML_Wraps) Wraps_Propagation_Vectors

    implicit none
    contains

    Module Subroutine Unwrap_group_k_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(group_k_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(dict) :: dict_g0

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_group_k_type: Cannot determine fortran type'
        else
            if (fortran_type == 'group_k_type') then
                allocate(group_k_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_group_k_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','g0',py_var,dict_g0,ierror)
        if (ierror == 0) call unwrap_group_type('Unwrap_group_k_type','g0',dict_g0,for_var%g0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','ngk',py_var,for_var%ngk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','k_equiv_minusk',py_var,for_var%k_equiv_minusk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','minusk',py_var,for_var%minusk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','extended',py_var,for_var%extended,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','p',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_group_k_type','p',p_int_1d,for_var%p,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','co',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_group_k_type','co',p_int_2d,for_var%co,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','nk',py_var,for_var%nk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','stark',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_group_k_type','stark',p_real_2d,for_var%stark,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_k_type','eqv_k',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_group_k_type','eqv_k',p_real_2d,for_var%eqv_k,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_group_k_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_group_k_type

    Module Subroutine Wrap_group_k_type(py_var,for_var,ierror)

        ! Arguments
        type(group_k_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_g0
        type(ndarray) :: nd_p,nd_co,nd_stark,nd_eqv_k

        ierror = 0
        if (ierror == 0) call wrap_group_type(for_var%g0,di_g0,ierror)
        if (ierror == 0) ierror = py_var%setitem('g0',di_g0)
        if (ierror == 0) ierror = py_var%setitem('ngk',for_var%ngk)
        if (ierror == 0) ierror = py_var%setitem('k_equiv_minusk',for_var%k_equiv_minusk)
        if (ierror == 0) ierror = py_var%setitem('minusk',for_var%minusk)
        if (ierror == 0) ierror = py_var%setitem('extended',for_var%extended)
        if (ierror == 0) ierror = ndarray_create(nd_p,for_var%p)
        if (ierror == 0) ierror = py_var%setitem('p',nd_p)
        if (ierror == 0) ierror = ndarray_create(nd_co,for_var%co)
        if (ierror == 0) ierror = py_var%setitem('co',nd_co)
        if (ierror == 0) ierror = py_var%setitem('nk',for_var%nk)
        if (ierror == 0) ierror = ndarray_create(nd_stark,for_var%stark)
        if (ierror == 0) ierror = py_var%setitem('stark',nd_stark)
        if (ierror == 0) ierror = ndarray_create(nd_eqv_k,for_var%eqv_k)
        if (ierror == 0) ierror = py_var%setitem('eqv_k',nd_eqv_k)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_group_k_type: Wrapping failed'
        end if

    End Subroutine Wrap_group_k_type

end submodule