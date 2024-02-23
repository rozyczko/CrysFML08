submodule (CFML_Wraps) Wraps_kvec_Symmetry

    implicit none
    contains

    Module Subroutine Unwrap_sym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(sym_oper_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_sym_oper_type: Cannot determine fortran type'
        else
            if (fortran_type == 'sym_oper_type') then
                allocate(sym_oper_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_sym_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_sym_oper_type','rot',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_sym_oper_type','rot',p_int_2d,for_var%rot,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_sym_oper_type','tr',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_sym_oper_type','tr',p_real_1d,for_var%tr,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_sym_oper_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_sym_oper_type

    Module Subroutine Wrap_sym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(sym_oper_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rot,nd_tr

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        if (ierror == 0) ierror = ndarray_create(nd_tr,for_var%tr)
        if (ierror == 0) ierror = py_var%setitem('tr',nd_tr)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_sym_oper_type: Wrapping failed'
        end if

    End Subroutine Wrap_sym_oper_type

    Module Subroutine Unwrap_msym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(msym_oper_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_msym_oper_type: Cannot determine fortran type'
        else
            if (fortran_type == 'msym_oper_type') then
                allocate(msym_oper_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_msym_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_msym_oper_type','rot',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_msym_oper_type','rot',p_int_2d,for_var%rot,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_msym_oper_type','phas',py_var,for_var%phas,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_msym_oper_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_msym_oper_type

    Module Subroutine Wrap_msym_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(msym_oper_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_rot

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
        if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
        if (ierror == 0) ierror = py_var%setitem('phas',for_var%phas)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_msym_oper_type: Wrapping failed'
        end if

    End Subroutine Wrap_msym_oper_type

    Module Subroutine Unwrap_magnetic_domain_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magnetic_domain_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magnetic_domain_type: Cannot determine fortran type'
        else
            if (fortran_type == 'magnetic_domain_type') then
                allocate(magnetic_domain_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magnetic_domain_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','nd',py_var,for_var%nd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','chir',py_var,for_var%chir,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','trans',py_var,for_var%trans,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','twin',py_var,for_var%twin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','dmat',py_var,p_int_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','dmat',p_int_3d,for_var%dmat,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','dt',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','dt',p_real_2d,for_var%dt,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','pop',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','pop',p_real_2d,for_var%pop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','lpop',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','lpop',p_int_2d,for_var%lpop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','mpop',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','mpop',p_real_2d,for_var%mpop,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','pop_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_domain_type','pop_std',p_real_2d,for_var%pop_std,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_domain_type','lab',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magnetic_domain_type','lab',my_list,for_var%lab,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_magnetic_domain_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_magnetic_domain_type

    Module Subroutine Wrap_magnetic_domain_type(py_var,for_var,ierror)

        ! Arguments
        type(magnetic_domain_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_lab
        type(ndarray) :: nd_dmat,nd_dt,nd_pop,nd_lpop,nd_mpop,nd_pop_std

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nd',for_var%nd)
        if (ierror == 0) ierror = py_var%setitem('chir',for_var%chir)
        if (ierror == 0) ierror = py_var%setitem('trans',for_var%trans)
        if (ierror == 0) ierror = py_var%setitem('twin',for_var%twin)
        if (ierror == 0) ierror = ndarray_create(nd_dmat,for_var%dmat)
        if (ierror == 0) ierror = py_var%setitem('dmat',nd_dmat)
        if (ierror == 0) ierror = ndarray_create(nd_dt,for_var%dt)
        if (ierror == 0) ierror = py_var%setitem('dt',nd_dt)
        if (ierror == 0) ierror = ndarray_create(nd_pop,for_var%pop)
        if (ierror == 0) ierror = py_var%setitem('pop',nd_pop)
        if (ierror == 0) ierror = ndarray_create(nd_lpop,for_var%lpop)
        if (ierror == 0) ierror = py_var%setitem('lpop',nd_lpop)
        if (ierror == 0) ierror = ndarray_create(nd_mpop,for_var%mpop)
        if (ierror == 0) ierror = py_var%setitem('mpop',nd_mpop)
        if (ierror == 0) ierror = ndarray_create(nd_pop_std,for_var%pop_std)
        if (ierror == 0) ierror = py_var%setitem('pop_std',nd_pop_std)
        if (ierror == 0) ierror = list_create(li_lab)
        if (ierror == 0) then
            do i = 1 , size(for_var%lab)
                if (ierror == 0) ierror = li_lab%append(for_var%lab(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_magnetic_domain_type: Wrapping failed'
        end if

    End Subroutine Wrap_magnetic_domain_type

    Module Subroutine Unwrap_magsymm_k_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magsymm_k_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magsymm_k_type: Cannot determine fortran type'
        else
            if (fortran_type == 'magsymm_k_type') then
                allocate(magsymm_k_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magsymm_k_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','magmodel',py_var,for_var%magmodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','sk_type',py_var,for_var%sk_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','bns_number',py_var,for_var%bns_number,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','og_number',py_var,for_var%og_number,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','bns_symbol',py_var,for_var%bns_symbol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','og_symbol',py_var,for_var%og_symbol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','magtype',py_var,for_var%magtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','latt',py_var,for_var%latt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nirreps',py_var,for_var%nirreps,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_dim',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','irrep_dim',p_int_1d,for_var%irrep_dim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','small_irrep_dim',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','small_irrep_dim',p_int_1d,for_var%small_irrep_dim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_modes_number',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','irrep_modes_number',p_int_1d,for_var%irrep_modes_number,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_id',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','irrep_id',my_list,for_var%irrep_id,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_direction',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','irrep_direction',my_list,for_var%irrep_direction,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','irrep_action',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','irrep_action',my_list,for_var%irrep_action,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nmsym',py_var,for_var%nmsym,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','mcentred',py_var,for_var%mcentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nkv',py_var,for_var%nkv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','kvec',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','kvec',p_real_2d,for_var%kvec,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','ltr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','ltr',p_real_2d,for_var%ltr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','nbas',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','nbas',p_int_1d,for_var%nbas,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','icomp',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_magsymm_k_type','icomp',p_int_2d,for_var%icomp,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','basf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','basf',my_list,for_var%basf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','symopsymb',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','symopsymb',my_list,for_var%symopsymb,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','symop',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','symop',my_list,for_var%symop,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','msymopsymb',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','msymopsymb',my_list,for_var%msymopsymb,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magsymm_k_type','msymop',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magsymm_k_type','msymop',my_list,for_var%msymop,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_magsymm_k_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_magsymm_k_type

    Module Subroutine Wrap_magsymm_k_type(py_var,for_var,ierror)

        ! Arguments
        type(magsymm_k_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_irrep_id,li_irrep_direction,li_irrep_action,li_basf,li_symopsymb,li_symop,li_msymopsymb,li_msymop
        type(ndarray) :: nd_irrep_dim,nd_small_irrep_dim,nd_irrep_modes_number,nd_kvec,nd_ltr,nd_nbas,nd_icomp

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('magmodel',for_var%magmodel)
        if (ierror == 0) ierror = py_var%setitem('sk_type',for_var%sk_type)
        if (ierror == 0) ierror = py_var%setitem('bns_number',for_var%bns_number)
        if (ierror == 0) ierror = py_var%setitem('og_number',for_var%og_number)
        if (ierror == 0) ierror = py_var%setitem('bns_symbol',for_var%bns_symbol)
        if (ierror == 0) ierror = py_var%setitem('og_symbol',for_var%og_symbol)
        if (ierror == 0) ierror = py_var%setitem('magtype',for_var%magtype)
        if (ierror == 0) ierror = py_var%setitem('parent_num',for_var%parent_num)
        if (ierror == 0) ierror = py_var%setitem('parent_spg',for_var%parent_spg)
        if (ierror == 0) ierror = py_var%setitem('latt',for_var%latt)
        if (ierror == 0) ierror = py_var%setitem('nirreps',for_var%nirreps)
        if (ierror == 0) ierror = ndarray_create(nd_irrep_dim,for_var%irrep_dim)
        if (ierror == 0) ierror = py_var%setitem('irrep_dim',nd_irrep_dim)
        if (ierror == 0) ierror = ndarray_create(nd_small_irrep_dim,for_var%small_irrep_dim)
        if (ierror == 0) ierror = py_var%setitem('small_irrep_dim',nd_small_irrep_dim)
        if (ierror == 0) ierror = ndarray_create(nd_irrep_modes_number,for_var%irrep_modes_number)
        if (ierror == 0) ierror = py_var%setitem('irrep_modes_number',nd_irrep_modes_number)
        if (ierror == 0) ierror = list_create(li_irrep_id)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_id)
                if (ierror == 0) ierror = li_irrep_id%append(for_var%irrep_id(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_id',li_irrep_id)
        if (ierror == 0) ierror = list_create(li_irrep_direction)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_direction)
                if (ierror == 0) ierror = li_irrep_direction%append(for_var%irrep_direction(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_direction',li_irrep_direction)
        if (ierror == 0) ierror = list_create(li_irrep_action)
        if (ierror == 0) then
            do i = 1 , size(for_var%irrep_action)
                if (ierror == 0) ierror = li_irrep_action%append(for_var%irrep_action(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('irrep_action',li_irrep_action)
        if (ierror == 0) ierror = py_var%setitem('nmsym',for_var%nmsym)
        if (ierror == 0) ierror = py_var%setitem('centred',for_var%centred)
        if (ierror == 0) ierror = py_var%setitem('mcentred',for_var%mcentred)
        if (ierror == 0) ierror = py_var%setitem('nkv',for_var%nkv)
        if (ierror == 0) ierror = ndarray_create(nd_kvec,for_var%kvec)
        if (ierror == 0) ierror = py_var%setitem('kvec',nd_kvec)
        if (ierror == 0) ierror = py_var%setitem('num_lat',for_var%num_lat)
        if (ierror == 0) ierror = ndarray_create(nd_ltr,for_var%ltr)
        if (ierror == 0) ierror = py_var%setitem('ltr',nd_ltr)
        if (ierror == 0) ierror = py_var%setitem('numops',for_var%numops)
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (ierror == 0) ierror = ndarray_create(nd_nbas,for_var%nbas)
        if (ierror == 0) ierror = py_var%setitem('nbas',nd_nbas)
        if (ierror == 0) ierror = ndarray_create(nd_icomp,for_var%icomp)
        if (ierror == 0) ierror = py_var%setitem('icomp',nd_icomp)
        if (ierror == 0) ierror = list_create(li_basf)
        if (ierror == 0) then
            do i = 1 , size(for_var%basf)
                if (ierror == 0) ierror = li_basf%append(for_var%basf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('basf',li_basf)
        if (ierror == 0) ierror = list_create(li_symopsymb)
        if (ierror == 0) then
            do i = 1 , size(for_var%symopsymb)
                if (ierror == 0) ierror = li_symopsymb%append(for_var%symopsymb(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('symopsymb',li_symopsymb)
        if (ierror == 0) ierror = list_create(li_symop)
        if (ierror == 0) allocate(di_symop(size(for_var%symop)))
        if (ierror == 0) then
            do i = 1 , size(for_var%symop)
                ierror = dict_create(di_symop(i))
                if (ierror == 0) call wrap_sym_oper_type(for_var%symop,(di_symop(i),ierror))
                if (ierror == 0) ierror = li_symop%append(di_symop(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('symop',li_symop)
        if (ierror == 0) ierror = list_create(li_msymopsymb)
        if (ierror == 0) then
            do i = 1 , size(for_var%msymopsymb)
                if (ierror == 0) ierror = li_msymopsymb%append(for_var%msymopsymb(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('msymopsymb',li_msymopsymb)
        if (ierror == 0) ierror = list_create(li_msymop)
        if (ierror == 0) allocate(di_msymop(size(for_var%msymop)))
        if (ierror == 0) then
            do i = 1 , size(for_var%msymop)
                ierror = dict_create(di_msymop(i))
                if (ierror == 0) call wrap_msym_oper_type(for_var%msymop,(di_msymop(i),ierror))
                if (ierror == 0) ierror = li_msymop%append(di_msymop(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('msymop',li_msymop)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_magsymm_k_type: Wrapping failed'
        end if

    End Subroutine Wrap_magsymm_k_type

end submodule