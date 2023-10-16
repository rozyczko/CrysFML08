!!----
!!----
!!----
SubModule (CFML_Python)  Metrics_Python_Wraps
    implicit none
    Contains

    Module Subroutine Wrap_Cell_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        class(cell_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: py_var
        integer,          intent(out)   :: ierror

        !---- Local Variables ----!
        type(ndarray) :: nd_cell,nd_rcell,nd_scell,nd_ang,nd_rang,nd_sang,nd_GD,nd_GR,&
                         nd_cr_orth_cel,nd_orth_cr_cel,nd_bl_m,nd_inv_bl_m,nd_lcell,nd_lang

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_type')
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = py_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_scell,for_var%scell)
        if (ierror == 0) ierror = py_var%setitem('scell',nd_scell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = py_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = ndarray_create(nd_sang,for_var%sang)
        if (ierror == 0) ierror = py_var%setitem('sang',nd_sang)
        if (ierror == 0) ierror = py_var%setitem('vol',for_var%vol)
        if (ierror == 0) ierror = py_var%setitem('svol',for_var%svol)
        select type (A => for_var)
            class is (cell_g_type)
                if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_g_type')
                if (ierror == 0) ierror = ndarray_create(nd_rcell,A%rcell)
                if (ierror == 0) ierror = py_var%setitem('rcell',nd_rcell)
                if (ierror == 0) ierror = ndarray_create(nd_rang,A%rang)
                if (ierror == 0) ierror = py_var%setitem('rang',nd_rang)
                if (ierror == 0) ierror = py_var%setitem('rvol',A%rvol)
                if (ierror == 0) ierror = ndarray_create(nd_GD,A%GD)
                if (ierror == 0) ierror = py_var%setitem('gd',nd_GD)
                if (ierror == 0) ierror = ndarray_create(nd_GR,A%GR)
                if (ierror == 0) ierror = py_var%setitem('gr',nd_GR)
                if (ierror == 0) ierror = ndarray_create(nd_cr_orth_cel,A%cr_orth_cel)
                if (ierror == 0) ierror = py_var%setitem('cr_orth_cel',nd_cr_orth_cel)
                if (ierror == 0) ierror = ndarray_create(nd_orth_cr_cel,A%orth_cr_cel)
                if (ierror == 0) ierror = py_var%setitem('orth_cr_cel',nd_orth_cr_cel)
                if (ierror == 0) ierror = ndarray_create(nd_bl_m,A%bl_m)
                if (ierror == 0) ierror = py_var%setitem('bl_m',nd_bl_m)
                if (ierror == 0) ierror = ndarray_create(nd_inv_bl_m,A%inv_bl_m)
                if (ierror == 0) ierror = py_var%setitem('inv_bl_m',nd_inv_bl_m)
                if (ierror == 0) ierror = py_var%setitem('cartype',A%carttype)
        end select
        if (ierror == 0) then
            select type (A => for_var)
                type is (cell_ls_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_ls_type')
                    if (ierror == 0) ierror = ndarray_create(nd_lcell,A%lcell)
                    if (ierror == 0) ierror = py_var%setitem('lcell',nd_lcell)
                    if (ierror == 0) ierror = ndarray_create(nd_lang,A%lang)
                    if (ierror == 0) ierror = py_var%setitem('lang',nd_lang)
                type is (cell_gls_type)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','cell_gls_type')
                    if (ierror == 0) ierror = ndarray_create(nd_lcell,A%lcell)
                    if (ierror == 0) ierror = py_var%setitem('lcell',nd_lcell)
                    if (ierror == 0) ierror = ndarray_create(nd_lang,A%lang)
                    if (ierror == 0) ierror = py_var%setitem('lang',nd_lang)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Cell_Type: Wrapping failed'
        end if

    End Subroutine Wrap_Cell_Type

    Module Subroutine Unwrap_Cell_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                    intent(inout) :: py_var
        class(cell_type), allocatable, intent(out)   :: for_var
        integer,                       intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Cell_Type: Cannot determine fortran type'
        else
            if (fortran_type == 'cell_type') then
                allocate(cell_type :: for_var)
            else if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: for_var)
            else if (fortran_type == 'cell_ls_type') then
                allocate(cell_ls_type :: for_var)
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Cell_Type: Unknown fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (cell_g_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','rcell',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','rcell',p_real_1d,A%rcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','rang',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','rang',p_real_1d,A%rang,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','rvol',py_var,A%rvol,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','gd',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','gd',p_real_2d,A%gd,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','gr',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','gr',p_real_2d,A%gr,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','cr_orth_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','cr_orth_cel',p_real_2d,A%cr_orth_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','orth_cr_cel',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','orth_cr_cel',p_real_2d,A%orth_cr_cel,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','bl_m',p_real_2d,A%bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','inv_bl_m',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','inv_bl_m',p_real_2d,A%inv_bl_m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','cartype',py_var,A%carttype,ierror)
            end select
        end if
        if (ierror == 0) then
            select type (A => for_var)
                type is (cell_ls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','lang',p_int_1d,A%lang,ierror)
                type is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_Type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_Type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'UnWrap_Cell_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Cell_Type

    Module Subroutine Unwrap_Cell_G_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                      intent(inout) :: py_var
        class(cell_g_type), allocatable, intent(out)   :: for_var
        integer,                         intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Cell_G_Type: Cannot determine fortran type'
        else
            if (fortran_type == 'cell_g_type') then
                allocate(cell_g_type :: for_var)
            else if (fortran_type == 'cell_gls_type') then
                allocate(cell_gls_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Cell_G_Type: Wrong fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','scell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','scell',p_real_1d,for_var%scell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','sang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','sang',p_real_1d,for_var%sang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','vol',py_var,for_var%vol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','svol',py_var,for_var%svol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','rcell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','rcell',p_real_1d,for_var%rcell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','rang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','rang',p_real_1d,for_var%rang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','rvol',py_var,for_var%rvol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','gd',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','gd',p_real_2d,for_var%gd,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','gr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','gr',p_real_2d,for_var%gr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','cr_orth_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','cr_orth_cel',p_real_2d,for_var%cr_orth_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','orth_cr_cel',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','orth_cr_cel',p_real_2d,for_var%orth_cr_cel,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','bl_m',p_real_2d,for_var%bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','inv_bl_m',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','inv_bl_m',p_real_2d,for_var%inv_bl_m,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','cartype',py_var,for_var%carttype,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                type is (cell_gls_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','lcell',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','lcell',p_int_1d,A%lcell,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Cell_G_Type','lang',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array('Unwrap_Cell_G_Type','lang',p_int_1d,A%lang,ierror)
            end select
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Cell_G_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Cell_G_Type

    Module Subroutine Wrap_Twofold_Axes_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        type(twofold_axes_type), intent(in)    :: for_var
        type(dict),              intent(inout) :: py_var
        integer,                 intent(out)   :: ierror

        !---- Local Variables ----!
        type(ndarray) :: nd_caxes,nd_dtwofold,nd_rtwofold,nd_dot,nd_cross,nd_maxes,nd_a,nd_b,nd_c

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','twofold_axes_type')
        if (ierror == 0) ierror = py_var%setitem('ntwo',for_var%ntwo)
        if (ierror == 0) ierror = py_var%setitem('tol',for_var%tol)
        if (ierror == 0) ierror = ndarray_create(nd_caxes,for_var%caxes)
        if (ierror == 0) ierror = py_var%setitem('caxes',nd_caxes)
        if (ierror == 0) ierror = ndarray_create(nd_dtwofold,for_var%dtwofold)
        if (ierror == 0) ierror = py_var%setitem('dtwofold',nd_dtwofold)
        if (ierror == 0) ierror = ndarray_create(nd_rtwofold,for_var%rtwofold)
        if (ierror == 0) ierror = py_var%setitem('rtwofold',nd_rtwofold)
        if (ierror == 0) ierror = ndarray_create(nd_dot,for_var%dot)
        if (ierror == 0) ierror = py_var%setitem('dot',nd_dot)
        if (ierror == 0) ierror = ndarray_create(nd_cross,for_var%cross)
        if (ierror == 0) ierror = py_var%setitem('cross',nd_cross)
        if (ierror == 0) ierror = ndarray_create(nd_maxes,for_var%maxes)
        if (ierror == 0) ierror = py_var%setitem('maxes',nd_maxes)
        if (ierror == 0) ierror = ndarray_create(nd_a,for_var%a)
        if (ierror == 0) ierror = py_var%setitem('a',nd_a)
        if (ierror == 0) ierror = ndarray_create(nd_b,for_var%b)
        if (ierror == 0) ierror = py_var%setitem('b',nd_b)
        if (ierror == 0) ierror = ndarray_create(nd_c,for_var%c)
        if (ierror == 0) ierror = py_var%setitem('c',nd_c)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Twofold_Axes_Type wrapping failed'
        end if

    End Subroutine Wrap_Twofold_Axes_Type

    Module Subroutine Unwrap_Twofold_Axes_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),              intent(inout) :: py_var
        type(twofold_axes_type), intent(out)   :: for_var
        integer,                 intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Twofold_Axes_Type: Cannot determine fortran type'
        else
            if (fortran_type /= 'twofold_axes_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Twofold_Axes_Type: Wrong fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','ntwo',py_var,for_var%ntwo,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','tol',py_var,for_var%tol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','caxes',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','caxes',p_real_2d,for_var%caxes,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','dtwofold',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','dtwofold',p_int_2d,for_var%dtwofold,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','rtwofold',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','rtwofold',p_int_2d,for_var%rtwofold,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','dot',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','dot',p_int_1d,for_var%dot,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','cross',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','cross',p_real_1d,for_var%cross,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','maxes',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','maxes',p_real_1d,for_var%maxes,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Twofold_Axes_Type','c',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_Twofold_Axes_Type','c',p_real_1d,for_var%c,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Twofold_Axes_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Twofold_Axes_Type

End SubModule Metrics_Python_Wraps