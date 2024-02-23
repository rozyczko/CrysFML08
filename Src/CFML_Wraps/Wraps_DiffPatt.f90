submodule (CFML_Wraps) Wraps_DiffPatt

    implicit none
    contains

    Module Subroutine Unwrap_diffpat_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_diffpat_type: Cannot determine fortran type'
        else
            if (fortran_type == 'diffpat_type') then
                allocate(diffpat_type :: for_var)
            else if (fortran_type == 'diffpat_e_type') then
                allocate(diffpat_e_type :: for_var)
            else if (fortran_type == 'diffpat_g_type') then
                allocate(diffpat_g_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,A%instr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,A%filename,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,A%filepath,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,A%scal,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,A%monitor,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,A%norm_mon,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,A%col_time,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,A%tsample,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,A%tset,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,A%ct_step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,A%al_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,A%al_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,A%al_sigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,A%al_ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,A%al_bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,A%al_istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','ycalc',p_real_1d,A%ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','bgr',p_real_1d,A%bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','istat',p_int_1d,A%istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','nd',p_int_1d,A%nd,ierror)
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_diffpat_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_diffpat_type

    Module Subroutine Unwrap_diffpat_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(diffpat_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'diffpat_type' &
                .and. fortran_type /= 'diffpat_e_type' &
                .and. fortran_type /= 'diffpat_g_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','kindrad',py_var,for_var%kindrad,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','scatvar',py_var,for_var%scatvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmin',py_var,for_var%xmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','xmax',py_var,for_var%xmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymin',py_var,for_var%ymin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','ymax',py_var,for_var%ymax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','step',py_var,for_var%step,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','npts',py_var,for_var%npts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigvar',py_var,for_var%sigvar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','wave',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_diffpat_type','wave',p_real_1d,for_var%wave,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','x',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','x',p_real_1d,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','y',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','y',p_real_1d,for_var%y,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_type','sigma',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_type','sigma',p_real_1d,for_var%sigma,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','instr',py_var,A%instr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filename',py_var,A%filename,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','filepath',py_var,A%filepath,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','scal',py_var,A%scal,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','monitor',py_var,A%monitor,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','norm_mon',py_var,A%norm_mon,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','col_time',py_var,A%col_time,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tsample',py_var,A%tsample,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','tset',py_var,A%tset,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ct_step',py_var,A%ct_step,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_x',py_var,A%al_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_y',py_var,A%al_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_sigma',py_var,A%al_sigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_ycalc',py_var,A%al_ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_bgr',py_var,A%al_bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','al_istat',py_var,A%al_istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','ycalc',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','ycalc',p_real_1d,A%ycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','bgr',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','bgr',p_real_1d,A%bgr,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','istat',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','istat',p_int_1d,A%istat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_e_type','nd',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_diffpat_e_type','nd',p_int_1d,A%nd,ierror)
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_x',py_var,A%legend_x,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','legend_y',py_var,A%legend_y,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gy',py_var,A%gy,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gycalc',py_var,A%gycalc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gsigma',py_var,A%gsigma,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_diffpat_g_type','gbgr',py_var,A%gbgr,ierror)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_diffpat_type_no_alloc: Unwrapping failed'
        end if

    End Subroutine Unwrap_diffpat_type_no_alloc

    Module Subroutine Wrap_diffpat_type(py_var,for_var,ierror)

        ! Arguments
        class(diffpat_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_wave,nd_x,nd_y,nd_sigma,nd_ycalc,nd_bgr,nd_istat,nd_nd

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('title',for_var%title)
        if (ierror == 0) ierror = py_var%setitem('kindrad',for_var%kindrad)
        if (ierror == 0) ierror = py_var%setitem('scatvar',for_var%scatvar)
        if (ierror == 0) ierror = py_var%setitem('xmin',for_var%xmin)
        if (ierror == 0) ierror = py_var%setitem('xmax',for_var%xmax)
        if (ierror == 0) ierror = py_var%setitem('ymin',for_var%ymin)
        if (ierror == 0) ierror = py_var%setitem('ymax',for_var%ymax)
        if (ierror == 0) ierror = py_var%setitem('step',for_var%step)
        if (ierror == 0) ierror = py_var%setitem('npts',for_var%npts)
        if (ierror == 0) ierror = py_var%setitem('sigvar',for_var%sigvar)
        if (ierror == 0) ierror = ndarray_create(nd_wave,for_var%wave)
        if (ierror == 0) ierror = py_var%setitem('wave',nd_wave)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror == 0) ierror = ndarray_create(nd_y,for_var%y)
        if (ierror == 0) ierror = py_var%setitem('y',nd_y)
        if (ierror == 0) ierror = ndarray_create(nd_sigma,for_var%sigma)
        if (ierror == 0) ierror = py_var%setitem('sigma',nd_sigma)
        if (ierror == 0) then
            select type (A => for_var)
                class is (diffpat_e_type)
                    if (ierror == 0) ierror = py_var%setitem('instr',A%instr)
                    if (ierror == 0) ierror = py_var%setitem('filename',A%filename)
                    if (ierror == 0) ierror = py_var%setitem('filepath',A%filepath)
                    if (ierror == 0) ierror = py_var%setitem('scal',A%scal)
                    if (ierror == 0) ierror = py_var%setitem('monitor',A%monitor)
                    if (ierror == 0) ierror = py_var%setitem('norm_mon',A%norm_mon)
                    if (ierror == 0) ierror = py_var%setitem('col_time',A%col_time)
                    if (ierror == 0) ierror = py_var%setitem('tsample',A%tsample)
                    if (ierror == 0) ierror = py_var%setitem('tset',A%tset)
                    if (ierror == 0) ierror = py_var%setitem('ct_step',A%ct_step)
                    if (ierror == 0) ierror = py_var%setitem('al_x',A%al_x)
                    if (ierror == 0) ierror = py_var%setitem('al_y',A%al_y)
                    if (ierror == 0) ierror = py_var%setitem('al_sigma',A%al_sigma)
                    if (ierror == 0) ierror = py_var%setitem('al_ycalc',A%al_ycalc)
                    if (ierror == 0) ierror = py_var%setitem('al_bgr',A%al_bgr)
                    if (ierror == 0) ierror = py_var%setitem('al_istat',A%al_istat)
                    if (ierror == 0) ierror = ndarray_create(nd_ycalc,for_var%ycalc)
                    if (ierror == 0) ierror = py_var%setitem('ycalc',nd_ycalc)
                    if (ierror == 0) ierror = ndarray_create(nd_bgr,for_var%bgr)
                    if (ierror == 0) ierror = py_var%setitem('bgr',nd_bgr)
                    if (ierror == 0) ierror = ndarray_create(nd_istat,for_var%istat)
                    if (ierror == 0) ierror = py_var%setitem('istat',nd_istat)
                    if (ierror == 0) ierror = ndarray_create(nd_nd,for_var%nd)
                    if (ierror == 0) ierror = py_var%setitem('nd',nd_nd)
            end select
            select type (A => for_var)
                class is (diffpat_g_type)
                    if (ierror == 0) ierror = py_var%setitem('legend_x',A%legend_x)
                    if (ierror == 0) ierror = py_var%setitem('legend_y',A%legend_y)
                    if (ierror == 0) ierror = py_var%setitem('gy',A%gy)
                    if (ierror == 0) ierror = py_var%setitem('gycalc',A%gycalc)
                    if (ierror == 0) ierror = py_var%setitem('gsigma',A%gsigma)
                    if (ierror == 0) ierror = py_var%setitem('gbgr',A%gbgr)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_diffpat_type: Wrapping failed'
        end if

    End Subroutine Wrap_diffpat_type

end submodule