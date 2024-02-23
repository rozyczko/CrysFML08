submodule (CFML_Wraps) Wraps_Molecules

    implicit none
    contains

    Module Subroutine Unwrap_molecule_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(molecule_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_molecule_type: Cannot determine fortran type'
        else
            if (fortran_type == 'molecule_type') then
                allocate(molecule_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_molecule_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','name_mol',py_var,for_var%name_mol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','in_xtal',py_var,for_var%in_xtal,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','is_eulermat',py_var,for_var%is_eulermat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','is_connect',py_var,for_var%is_connect,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','rot_type',py_var,for_var%rot_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','coor_type',py_var,for_var%coor_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','therm_type',py_var,for_var%therm_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','xcentre',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','xcentre',p_real_1d,for_var%xcentre,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','mxcentre',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','mxcentre',p_real_1d,for_var%mxcentre,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','lxcentre',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','lxcentre',p_int_1d,for_var%lxcentre,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','orient',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','orient',p_real_1d,for_var%orient,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','morient',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','morient',p_real_1d,for_var%morient,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','lorient',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','lorient',p_int_1d,for_var%lorient,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','t_tls',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','t_tls',p_real_1d,for_var%t_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','mt_tls',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','mt_tls',p_real_1d,for_var%mt_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','lt_tls',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','lt_tls',p_int_1d,for_var%lt_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','l_tls',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','l_tls',p_real_1d,for_var%l_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','ml_tls',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','ml_tls',p_real_1d,for_var%ml_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','ll_tls',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','ll_tls',p_int_1d,for_var%ll_tls,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','s_tls',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','s_tls',p_real_2d,for_var%s_tls,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','ms_tls',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','ms_tls',p_real_2d,for_var%ms_tls,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','ls_tls',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','ls_tls',p_int_2d,for_var%ls_tls,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','euler',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_molecule_type','euler',p_real_2d,for_var%euler,ierror,order)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','atname',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_molecule_type','atname',my_list,for_var%atname,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','atsymb',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_molecule_type','atsymb',my_list,for_var%atsymb,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','atz',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','atz',p_int_1d,for_var%atz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','ptr',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','ptr',p_int_2d,for_var%ptr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','i_coor',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','i_coor',p_real_2d,for_var%i_coor,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','mi_coor',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','mi_coor',p_real_2d,for_var%mi_coor,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','li_coor',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','li_coor',p_int_2d,for_var%li_coor,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','u_iso',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','u_iso',p_real_1d,for_var%u_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','mu_iso',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','mu_iso',p_real_1d,for_var%mu_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','lu_iso',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','lu_iso',p_int_1d,for_var%lu_iso,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','occ',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','occ',p_real_1d,for_var%occ,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','mocc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','mocc',p_real_1d,for_var%mocc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','locc',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','locc',p_int_1d,for_var%locc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','nb',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','nb',p_int_1d,for_var%nb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','inb',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','inb',p_int_2d,for_var%inb,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','tb',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','tb',p_int_2d,for_var%tb,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molecule_type','conn',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_molecule_type','conn',p_int_2d,for_var%conn,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_molecule_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_molecule_type

    Module Subroutine Wrap_molecule_type(py_var,for_var,ierror)

        ! Arguments
        type(molecule_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_atname,li_atsymb
        type(ndarray) :: nd_xcentre,nd_mxcentre,nd_lxcentre,nd_orient,nd_morient,nd_lorient,nd_t_tls,nd_mt_tls,nd_lt_tls,nd_l_tls,nd_ml_tls,nd_ll_tls,nd_s_tls,nd_ms_tls,nd_ls_tls,nd_euler,nd_atz,nd_ptr,nd_i_coor,nd_mi_coor,nd_li_coor,nd_u_iso,nd_mu_iso,nd_lu_iso,nd_occ,nd_mocc,nd_locc,nd_nb,nd_inb,nd_tb,nd_conn

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('name_mol',for_var%name_mol)
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('in_xtal',for_var%in_xtal)
        if (ierror == 0) ierror = py_var%setitem('is_eulermat',for_var%is_eulermat)
        if (ierror == 0) ierror = py_var%setitem('is_connect',for_var%is_connect)
        if (ierror == 0) ierror = py_var%setitem('rot_type',for_var%rot_type)
        if (ierror == 0) ierror = py_var%setitem('coor_type',for_var%coor_type)
        if (ierror == 0) ierror = py_var%setitem('therm_type',for_var%therm_type)
        if (ierror == 0) ierror = ndarray_create(nd_xcentre,for_var%xcentre)
        if (ierror == 0) ierror = py_var%setitem('xcentre',nd_xcentre)
        if (ierror == 0) ierror = ndarray_create(nd_mxcentre,for_var%mxcentre)
        if (ierror == 0) ierror = py_var%setitem('mxcentre',nd_mxcentre)
        if (ierror == 0) ierror = ndarray_create(nd_lxcentre,for_var%lxcentre)
        if (ierror == 0) ierror = py_var%setitem('lxcentre',nd_lxcentre)
        if (ierror == 0) ierror = ndarray_create(nd_orient,for_var%orient)
        if (ierror == 0) ierror = py_var%setitem('orient',nd_orient)
        if (ierror == 0) ierror = ndarray_create(nd_morient,for_var%morient)
        if (ierror == 0) ierror = py_var%setitem('morient',nd_morient)
        if (ierror == 0) ierror = ndarray_create(nd_lorient,for_var%lorient)
        if (ierror == 0) ierror = py_var%setitem('lorient',nd_lorient)
        if (ierror == 0) ierror = ndarray_create(nd_t_tls,for_var%t_tls)
        if (ierror == 0) ierror = py_var%setitem('t_tls',nd_t_tls)
        if (ierror == 0) ierror = ndarray_create(nd_mt_tls,for_var%mt_tls)
        if (ierror == 0) ierror = py_var%setitem('mt_tls',nd_mt_tls)
        if (ierror == 0) ierror = ndarray_create(nd_lt_tls,for_var%lt_tls)
        if (ierror == 0) ierror = py_var%setitem('lt_tls',nd_lt_tls)
        if (ierror == 0) ierror = ndarray_create(nd_l_tls,for_var%l_tls)
        if (ierror == 0) ierror = py_var%setitem('l_tls',nd_l_tls)
        if (ierror == 0) ierror = ndarray_create(nd_ml_tls,for_var%ml_tls)
        if (ierror == 0) ierror = py_var%setitem('ml_tls',nd_ml_tls)
        if (ierror == 0) ierror = ndarray_create(nd_ll_tls,for_var%ll_tls)
        if (ierror == 0) ierror = py_var%setitem('ll_tls',nd_ll_tls)
        if (ierror == 0) ierror = ndarray_create(nd_s_tls,for_var%s_tls)
        if (ierror == 0) ierror = py_var%setitem('s_tls',nd_s_tls)
        if (ierror == 0) ierror = ndarray_create(nd_ms_tls,for_var%ms_tls)
        if (ierror == 0) ierror = py_var%setitem('ms_tls',nd_ms_tls)
        if (ierror == 0) ierror = ndarray_create(nd_ls_tls,for_var%ls_tls)
        if (ierror == 0) ierror = py_var%setitem('ls_tls',nd_ls_tls)
        if (ierror == 0) ierror = ndarray_create(nd_euler,for_var%euler)
        if (ierror == 0) ierror = py_var%setitem('euler',nd_euler)
        if (ierror == 0) ierror = list_create(li_atname)
        if (ierror == 0) then
            do i = 1 , size(for_var%atname)
                if (ierror == 0) ierror = li_atname%append(for_var%atname(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('atname',li_atname)
        if (ierror == 0) ierror = list_create(li_atsymb)
        if (ierror == 0) then
            do i = 1 , size(for_var%atsymb)
                if (ierror == 0) ierror = li_atsymb%append(for_var%atsymb(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('atsymb',li_atsymb)
        if (ierror == 0) ierror = ndarray_create(nd_atz,for_var%atz)
        if (ierror == 0) ierror = py_var%setitem('atz',nd_atz)
        if (ierror == 0) ierror = ndarray_create(nd_ptr,for_var%ptr)
        if (ierror == 0) ierror = py_var%setitem('ptr',nd_ptr)
        if (ierror == 0) ierror = ndarray_create(nd_i_coor,for_var%i_coor)
        if (ierror == 0) ierror = py_var%setitem('i_coor',nd_i_coor)
        if (ierror == 0) ierror = ndarray_create(nd_mi_coor,for_var%mi_coor)
        if (ierror == 0) ierror = py_var%setitem('mi_coor',nd_mi_coor)
        if (ierror == 0) ierror = ndarray_create(nd_li_coor,for_var%li_coor)
        if (ierror == 0) ierror = py_var%setitem('li_coor',nd_li_coor)
        if (ierror == 0) ierror = ndarray_create(nd_u_iso,for_var%u_iso)
        if (ierror == 0) ierror = py_var%setitem('u_iso',nd_u_iso)
        if (ierror == 0) ierror = ndarray_create(nd_mu_iso,for_var%mu_iso)
        if (ierror == 0) ierror = py_var%setitem('mu_iso',nd_mu_iso)
        if (ierror == 0) ierror = ndarray_create(nd_lu_iso,for_var%lu_iso)
        if (ierror == 0) ierror = py_var%setitem('lu_iso',nd_lu_iso)
        if (ierror == 0) ierror = ndarray_create(nd_occ,for_var%occ)
        if (ierror == 0) ierror = py_var%setitem('occ',nd_occ)
        if (ierror == 0) ierror = ndarray_create(nd_mocc,for_var%mocc)
        if (ierror == 0) ierror = py_var%setitem('mocc',nd_mocc)
        if (ierror == 0) ierror = ndarray_create(nd_locc,for_var%locc)
        if (ierror == 0) ierror = py_var%setitem('locc',nd_locc)
        if (ierror == 0) ierror = ndarray_create(nd_nb,for_var%nb)
        if (ierror == 0) ierror = py_var%setitem('nb',nd_nb)
        if (ierror == 0) ierror = ndarray_create(nd_inb,for_var%inb)
        if (ierror == 0) ierror = py_var%setitem('inb',nd_inb)
        if (ierror == 0) ierror = ndarray_create(nd_tb,for_var%tb)
        if (ierror == 0) ierror = py_var%setitem('tb',nd_tb)
        if (ierror == 0) ierror = ndarray_create(nd_conn,for_var%conn)
        if (ierror == 0) ierror = py_var%setitem('conn',nd_conn)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_molecule_type: Wrapping failed'
        end if

    End Subroutine Wrap_molecule_type

    Module Subroutine Unwrap_molcrystal_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(molcrystal_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list
        type(dict) :: dict_cell,dict_spg

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_molcrystal_type: Cannot determine fortran type'
        else
            if (fortran_type == 'molcrystal_type') then
                allocate(molcrystal_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_molcrystal_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','n_free',py_var,for_var%n_free,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','n_mol',py_var,for_var%n_mol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','n_species',py_var,for_var%n_species,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','npat',py_var,for_var%npat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','cell',py_var,dict_cell,ierror)
        if (ierror == 0) call unwrap_cell_type('Unwrap_molcrystal_type','cell',dict_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','spg',py_var,dict_spg,ierror)
        if (ierror == 0) call unwrap_group_type('Unwrap_molcrystal_type','spg',dict_spg,for_var%spg,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','atm',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_molcrystal_type','atm',my_list,for_var%atm,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_molcrystal_type','mol',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_molcrystal_type','mol',my_list,for_var%mol,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_molcrystal_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_molcrystal_type

    Module Subroutine Wrap_molcrystal_type(py_var,for_var,ierror)

        ! Arguments
        type(molcrystal_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_cell,di_spg
        type(list) :: li_atm,li_mol

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('n_free',for_var%n_free)
        if (ierror == 0) ierror = py_var%setitem('n_mol',for_var%n_mol)
        if (ierror == 0) ierror = py_var%setitem('n_species',for_var%n_species)
        if (ierror == 0) ierror = py_var%setitem('npat',for_var%npat)
        if (ierror == 0) call wrap_cell_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) call wrap_group_type(for_var%spg,di_spg,ierror)
        if (ierror == 0) ierror = py_var%setitem('spg',di_spg)
        if (ierror == 0) ierror = list_create(li_atm)
        if (ierror == 0) allocate(di_atm(size(for_var%atm)))
        if (ierror == 0) then
            do i = 1 , size(for_var%atm)
                ierror = dict_create(di_atm(i))
                if (ierror == 0) call wrap_atm_type(for_var%atm,(di_atm(i),ierror))
                if (ierror == 0) ierror = li_atm%append(di_atm(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('atm',li_atm)
        if (ierror == 0) ierror = list_create(li_mol)
        if (ierror == 0) allocate(di_mol(size(for_var%mol)))
        if (ierror == 0) then
            do i = 1 , size(for_var%mol)
                ierror = dict_create(di_mol(i))
                if (ierror == 0) call wrap_molecule_type(for_var%mol,(di_mol(i),ierror))
                if (ierror == 0) ierror = li_mol%append(di_mol(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mol',li_mol)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_molcrystal_type: Wrapping failed'
        end if

    End Subroutine Wrap_molcrystal_type

end submodule