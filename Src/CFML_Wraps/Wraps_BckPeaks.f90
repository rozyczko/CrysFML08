submodule (CFML_Wraps) Wraps_BckPeaks

    implicit none
    contains

    Module Subroutine Unwrap_pkb_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(pkb_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_pkb_type: Cannot determine fortran type'
        else
            if (fortran_type == 'pkb_type') then
                allocate(pkb_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_pkb_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_pkb_type','np',py_var,for_var%np,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pkb_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_pkb_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pkb_type','y',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_pkb_type','y',p_real_1d,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pkb_type','bkg',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_pkb_type','bkg',p_real_1d,for_var%bkg,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_pkb_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_pkb_type

    Module Subroutine Wrap_pkb_type(py_var,for_var,ierror)

        ! Arguments
        type(pkb_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_x,nd_y,nd_bkg

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('np',for_var%np)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror == 0) ierror = ndarray_create(nd_y,for_var%y)
        if (ierror == 0) ierror = py_var%setitem('y',nd_y)
        if (ierror == 0) ierror = ndarray_create(nd_bkg,for_var%bkg)
        if (ierror == 0) ierror = py_var%setitem('bkg',nd_bkg)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_pkb_type: Wrapping failed'
        end if

    End Subroutine Wrap_pkb_type

    Module Subroutine Unwrap_peak_search_cond_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(peak_search_cond_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_peak_search_cond_type: Cannot determine fortran type'
        else
            if (fortran_type == 'peak_search_cond_type') then
                allocate(peak_search_cond_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_peak_search_cond_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_search_cond_type','peak_threshold',py_var,for_var%peak_threshold,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_search_cond_type','shoulder_threshold',py_var,for_var%shoulder_threshold,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_search_cond_type','bkg_threshold',py_var,for_var%bkg_threshold,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_search_cond_type','kindofpeaks',py_var,for_var%kindofpeaks,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_peak_search_cond_type','iterations',py_var,for_var%iterations,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_peak_search_cond_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_peak_search_cond_type

    Module Subroutine Wrap_peak_search_cond_type(py_var,for_var,ierror)

        ! Arguments
        type(peak_search_cond_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('peak_threshold',for_var%peak_threshold)
        if (ierror == 0) ierror = py_var%setitem('shoulder_threshold',for_var%shoulder_threshold)
        if (ierror == 0) ierror = py_var%setitem('bkg_threshold',for_var%bkg_threshold)
        if (ierror == 0) ierror = py_var%setitem('kindofpeaks',for_var%kindofpeaks)
        if (ierror == 0) ierror = py_var%setitem('iterations',for_var%iterations)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_peak_search_cond_type: Wrapping failed'
        end if

    End Subroutine Wrap_peak_search_cond_type

end submodule