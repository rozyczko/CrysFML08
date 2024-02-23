submodule (CFML_Wraps) Wraps_Maps

    implicit none
    contains

    Module Subroutine Unwrap_cube_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(cube_info_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_cube_info_type: Cannot determine fortran type'
        else
            if (fortran_type == 'cube_info_type') then
                allocate(cube_info_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_cube_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_cube_info_type','nelem',py_var,for_var%nelem,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cube_info_type','code',py_var,for_var%code,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_cube_info_type','edges',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_cube_info_type','edges',p_int_1d,for_var%edges,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_cube_info_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_cube_info_type

    Module Subroutine Wrap_cube_info_type(py_var,for_var,ierror)

        ! Arguments
        type(cube_info_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_edges

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nelem',for_var%nelem)
        if (ierror == 0) ierror = py_var%setitem('code',for_var%code)
        if (ierror == 0) ierror = ndarray_create(nd_edges,for_var%edges)
        if (ierror == 0) ierror = py_var%setitem('edges',nd_edges)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_cube_info_type: Wrapping failed'
        end if

    End Subroutine Wrap_cube_info_type

end submodule