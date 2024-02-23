submodule (CFML_Wraps) Wraps_gSpaceGroups

    implicit none
    contains

    Module Subroutine Unwrap_symm_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(symm_oper_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_symm_oper_type: Cannot determine fortran type'
        else
            if (fortran_type == 'symm_oper_type') then
                allocate(symm_oper_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_symm_oper_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','time_inv',py_var,for_var%time_inv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_symm_oper_type','mat',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_symm_oper_type','mat',my_list,for_var%mat,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_symm_oper_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_symm_oper_type

    Module Subroutine Wrap_symm_oper_type(py_var,for_var,ierror)

        ! Arguments
        type(symm_oper_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_mat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('time_inv',for_var%time_inv)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (ierror == 0) ierror = list_create(li_mat)
        if (ierror == 0) allocate(di_mat(size(for_var%mat)))
        if (ierror == 0) then
            do i = 1 , size(for_var%mat)
                ierror = dict_create(di_mat(i))
                if (ierror == 0) call wrap_rational(for_var%mat,(di_mat(i),ierror))
                if (ierror == 0) ierror = li_mat%append(di_mat(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mat',li_mat)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_symm_oper_type: Wrapping failed'
        end if

    End Subroutine Wrap_symm_oper_type

    Module Subroutine Unwrap_group_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(group_type), allocatable, intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_group_type: Cannot determine fortran type'
        else
            if (fortran_type == 'group_type') then
                allocate(group_type :: for_var)
            else if (fortran_type == 'spg_type') then
                allocate(spg_type :: for_var)
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_group_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_group_type','symb_op',my_list,for_var%symb_op,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,A%magnetic,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,A%standard_setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,A%numspg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,A%numshu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,A%numops,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,A%centred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,A%anticentred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,A%mag_type,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,A%num_lat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,A%num_alat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,A%parent_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,A%bravais_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,A%spg_lat,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','shu_lat',my_list,A%shu_lat,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','init_label',py_var,A%init_label,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_spg',py_var,A%parent_spg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','tfrom_parent',py_var,A%tfrom_parent,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre',py_var,A%centre,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_symb',py_var,A%spg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bns_num',py_var,A%bns_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','og_num',py_var,A%og_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bns_symb',py_var,A%bns_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','og_symb',py_var,A%og_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','hall',py_var,A%hall,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','uni',py_var,A%uni,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','uni_num',py_var,A%uni_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','crystalsys',py_var,A%crystalsys,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','pg',py_var,A%pg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_pg',py_var,A%mag_pg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','laue',py_var,A%laue,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','setting',py_var,A%setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mat2std',py_var,A%mat2std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mat2std_shu',py_var,A%mat2std_shu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','matfrom',py_var,A%matfrom,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','generators_list',py_var,A%generators_list,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','centre_coord',my_list,A%centre_coord,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','anticentre_coord',my_list,A%anticentre_coord,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','lat_tr',my_list,A%lat_tr,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','alat_tr',my_list,A%alat_tr,ierror)
                    if (ierror == 0) call my_list%destroy
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_group_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_group_type

    Module Subroutine Unwrap_group_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(group_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        integer, dimension(:,:,:), pointer :: p_int_3d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_group_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'group_type' &
                .and. fortran_type /= 'spg_type' &
                .and. fortran_type /= 'superspacegroup_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_group_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','inv',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_group_type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_group_type','op',my_list,for_var%op,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_group_type','symb_op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_group_type','symb_op',my_list,for_var%symb_op,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','magnetic',py_var,A%magnetic,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','standard_setting',py_var,A%standard_setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numspg',py_var,A%numspg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numshu',py_var,A%numshu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','numops',py_var,A%numops,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centred',py_var,A%centred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentred',py_var,A%anticentred,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_type',py_var,A%mag_type,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_lat',py_var,A%num_lat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','num_alat',py_var,A%num_alat,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_num',py_var,A%parent_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bravais_num',py_var,A%bravais_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_lat',py_var,A%spg_lat,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','shu_lat',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','shu_lat',my_list,A%shu_lat,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','init_label',py_var,A%init_label,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','parent_spg',py_var,A%parent_spg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','tfrom_parent',py_var,A%tfrom_parent,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre',py_var,A%centre,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','spg_symb',py_var,A%spg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bns_num',py_var,A%bns_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','og_num',py_var,A%og_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','bns_symb',py_var,A%bns_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','og_symb',py_var,A%og_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','hall',py_var,A%hall,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','uni',py_var,A%uni,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','uni_num',py_var,A%uni_num,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','crystalsys',py_var,A%crystalsys,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','pg',py_var,A%pg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mag_pg',py_var,A%mag_pg,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','laue',py_var,A%laue,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','setting',py_var,A%setting,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mat2std',py_var,A%mat2std,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','mat2std_shu',py_var,A%mat2std_shu,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','matfrom',py_var,A%matfrom,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','generators_list',py_var,A%generators_list,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','centre_coord',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','centre_coord',my_list,A%centre_coord,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','anticentre_coord',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','anticentre_coord',my_list,A%anticentre_coord,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','lat_tr',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','lat_tr',my_list,A%lat_tr,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_spg_type','alat_tr',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array('Unwrap_spg_type','alat_tr',my_list,A%alat_tr,ierror)
                    if (ierror == 0) call my_list%destroy
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nk',py_var,A%nk,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nq',py_var,A%nq,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_symb',py_var,A%ssg_symb,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_bravais',py_var,A%ssg_bravais,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ssg_nlabel',py_var,A%ssg_nlabel,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','kv',p_real_2d,A%kv,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','kv_std',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','kv_std',p_real_2d,A%kv_std,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','sintlim',py_var,p_real_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','sintlim',p_real_1d,A%sintlim,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','nharm',py_var,p_int_1d,ierror)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','nharm',p_int_1d,A%nharm,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','q_coeff',py_var,p_int_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','q_coeff',p_int_2d,A%q_coeff,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','rot',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','rot',p_int_3d,A%rot,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','m',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','m',p_int_3d,A%m,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ep',py_var,p_int_3d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','ep',p_int_3d,A%ep,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','t',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','t',p_real_2d,A%t,ierror,order)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_superspacegroup_type','ti',py_var,p_real_2d,ierror,order)
                    if (ierror == 0) call pointer_to_array_alloc('Unwrap_superspacegroup_type','ti',p_real_2d,A%ti,ierror,order)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_group_type_no_alloc: Unwrapping failed'
        end if

    End Subroutine Unwrap_group_type_no_alloc

    Module Subroutine Wrap_group_type(py_var,for_var,ierror)

        ! Arguments
        class(group_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_op,li_symb_op,li_shu_lat,li_centre_coord,li_anticentre_coord,li_lat_tr,li_alat_tr
        type(ndarray) :: nd_inv,nd_kv,nd_kv_std,nd_sintlim,nd_nharm,nd_q_coeff,nd_rot,nd_m,nd_ep,nd_t,nd_ti

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (ierror == 0) ierror = py_var%setitem('d',for_var%d)
        if (ierror == 0) ierror = ndarray_create(nd_inv,for_var%inv)
        if (ierror == 0) ierror = py_var%setitem('inv',nd_inv)
        if (ierror == 0) ierror = list_create(li_op)
        if (ierror == 0) allocate(di_op(size(for_var%op)))
        if (ierror == 0) then
            do i = 1 , size(for_var%op)
                ierror = dict_create(di_op(i))
                if (ierror == 0) call wrap_symm_oper_type(for_var%op,(di_op(i),ierror))
                if (ierror == 0) ierror = li_op%append(di_op(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('op',li_op)
        if (ierror == 0) ierror = list_create(li_symb_op)
        if (ierror == 0) then
            do i = 1 , size(for_var%symb_op)
                if (ierror == 0) ierror = li_symb_op%append(for_var%symb_op(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('symb_op',li_symb_op)
        if (ierror == 0) then
            select type (A => for_var)
                class is (spg_type)
                    if (ierror == 0) ierror = py_var%setitem('magnetic',A%magnetic)
                    if (ierror == 0) ierror = py_var%setitem('standard_setting',A%standard_setting)
                    if (ierror == 0) ierror = py_var%setitem('numspg',A%numspg)
                    if (ierror == 0) ierror = py_var%setitem('numshu',A%numshu)
                    if (ierror == 0) ierror = py_var%setitem('numops',A%numops)
                    if (ierror == 0) ierror = py_var%setitem('centred',A%centred)
                    if (ierror == 0) ierror = py_var%setitem('anticentred',A%anticentred)
                    if (ierror == 0) ierror = py_var%setitem('mag_type',A%mag_type)
                    if (ierror == 0) ierror = py_var%setitem('num_lat',A%num_lat)
                    if (ierror == 0) ierror = py_var%setitem('num_alat',A%num_alat)
                    if (ierror == 0) ierror = py_var%setitem('parent_num',A%parent_num)
                    if (ierror == 0) ierror = py_var%setitem('bravais_num',A%bravais_num)
                    if (ierror == 0) ierror = py_var%setitem('spg_lat',A%spg_lat)
                    if (ierror == 0) ierror = list_create(li_shu_lat)
                    if (ierror == 0) then
                        do i = 1 , size(for_var%shu_lat)
                            if (ierror == 0) ierror = li_shu_lat%append(for_var%shu_lat(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('shu_lat',li_shu_lat)
                    if (ierror == 0) ierror = py_var%setitem('init_label',A%init_label)
                    if (ierror == 0) ierror = py_var%setitem('parent_spg',A%parent_spg)
                    if (ierror == 0) ierror = py_var%setitem('tfrom_parent',A%tfrom_parent)
                    if (ierror == 0) ierror = py_var%setitem('centre',A%centre)
                    if (ierror == 0) ierror = py_var%setitem('spg_symb',A%spg_symb)
                    if (ierror == 0) ierror = py_var%setitem('bns_num',A%bns_num)
                    if (ierror == 0) ierror = py_var%setitem('og_num',A%og_num)
                    if (ierror == 0) ierror = py_var%setitem('bns_symb',A%bns_symb)
                    if (ierror == 0) ierror = py_var%setitem('og_symb',A%og_symb)
                    if (ierror == 0) ierror = py_var%setitem('hall',A%hall)
                    if (ierror == 0) ierror = py_var%setitem('uni',A%uni)
                    if (ierror == 0) ierror = py_var%setitem('uni_num',A%uni_num)
                    if (ierror == 0) ierror = py_var%setitem('crystalsys',A%crystalsys)
                    if (ierror == 0) ierror = py_var%setitem('pg',A%pg)
                    if (ierror == 0) ierror = py_var%setitem('mag_pg',A%mag_pg)
                    if (ierror == 0) ierror = py_var%setitem('laue',A%laue)
                    if (ierror == 0) ierror = py_var%setitem('setting',A%setting)
                    if (ierror == 0) ierror = py_var%setitem('mat2std',A%mat2std)
                    if (ierror == 0) ierror = py_var%setitem('mat2std_shu',A%mat2std_shu)
                    if (ierror == 0) ierror = py_var%setitem('matfrom',A%matfrom)
                    if (ierror == 0) ierror = py_var%setitem('generators_list',A%generators_list)
                    if (ierror == 0) ierror = list_create(li_centre_coord)
                    if (ierror == 0) allocate(di_centre_coord(size(for_var%centre_coord)))
                    if (ierror == 0) then
                        do i = 1 , size(for_var%centre_coord)
                            ierror = dict_create(di_centre_coord(i))
                            if (ierror == 0) call wrap_rational(for_var%centre_coord,(di_centre_coord(i),ierror))
                            if (ierror == 0) ierror = li_centre_coord%append(di_centre_coord(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('centre_coord',li_centre_coord)
                    if (ierror == 0) ierror = list_create(li_anticentre_coord)
                    if (ierror == 0) allocate(di_anticentre_coord(size(for_var%anticentre_coord)))
                    if (ierror == 0) then
                        do i = 1 , size(for_var%anticentre_coord)
                            ierror = dict_create(di_anticentre_coord(i))
                            if (ierror == 0) call wrap_rational(for_var%anticentre_coord,(di_anticentre_coord(i),ierror))
                            if (ierror == 0) ierror = li_anticentre_coord%append(di_anticentre_coord(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('anticentre_coord',li_anticentre_coord)
                    if (ierror == 0) ierror = list_create(li_lat_tr)
                    if (ierror == 0) allocate(di_lat_tr(size(for_var%lat_tr)))
                    if (ierror == 0) then
                        do i = 1 , size(for_var%lat_tr)
                            ierror = dict_create(di_lat_tr(i))
                            if (ierror == 0) call wrap_rational(for_var%lat_tr,(di_lat_tr(i),ierror))
                            if (ierror == 0) ierror = li_lat_tr%append(di_lat_tr(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('lat_tr',li_lat_tr)
                    if (ierror == 0) ierror = list_create(li_alat_tr)
                    if (ierror == 0) allocate(di_alat_tr(size(for_var%alat_tr)))
                    if (ierror == 0) then
                        do i = 1 , size(for_var%alat_tr)
                            ierror = dict_create(di_alat_tr(i))
                            if (ierror == 0) call wrap_rational(for_var%alat_tr,(di_alat_tr(i),ierror))
                            if (ierror == 0) ierror = li_alat_tr%append(di_alat_tr(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('alat_tr',li_alat_tr)
            end select
            select type (A => for_var)
                class is (superspacegroup_type)
                    if (ierror == 0) ierror = py_var%setitem('nk',A%nk)
                    if (ierror == 0) ierror = py_var%setitem('nq',A%nq)
                    if (ierror == 0) ierror = py_var%setitem('ssg_symb',A%ssg_symb)
                    if (ierror == 0) ierror = py_var%setitem('ssg_bravais',A%ssg_bravais)
                    if (ierror == 0) ierror = py_var%setitem('ssg_nlabel',A%ssg_nlabel)
                    if (ierror == 0) ierror = ndarray_create(nd_kv,for_var%kv)
                    if (ierror == 0) ierror = py_var%setitem('kv',nd_kv)
                    if (ierror == 0) ierror = ndarray_create(nd_kv_std,for_var%kv_std)
                    if (ierror == 0) ierror = py_var%setitem('kv_std',nd_kv_std)
                    if (ierror == 0) ierror = ndarray_create(nd_sintlim,for_var%sintlim)
                    if (ierror == 0) ierror = py_var%setitem('sintlim',nd_sintlim)
                    if (ierror == 0) ierror = ndarray_create(nd_nharm,for_var%nharm)
                    if (ierror == 0) ierror = py_var%setitem('nharm',nd_nharm)
                    if (ierror == 0) ierror = ndarray_create(nd_q_coeff,for_var%q_coeff)
                    if (ierror == 0) ierror = py_var%setitem('q_coeff',nd_q_coeff)
                    if (ierror == 0) ierror = ndarray_create(nd_rot,for_var%rot)
                    if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
                    if (ierror == 0) ierror = ndarray_create(nd_m,for_var%m)
                    if (ierror == 0) ierror = py_var%setitem('m',nd_m)
                    if (ierror == 0) ierror = ndarray_create(nd_ep,for_var%ep)
                    if (ierror == 0) ierror = py_var%setitem('ep',nd_ep)
                    if (ierror == 0) ierror = ndarray_create(nd_t,for_var%t)
                    if (ierror == 0) ierror = py_var%setitem('t',nd_t)
                    if (ierror == 0) ierror = ndarray_create(nd_ti,for_var%ti)
                    if (ierror == 0) ierror = py_var%setitem('ti',nd_ti)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_group_type: Wrapping failed'
        end if

    End Subroutine Wrap_group_type

    Module Subroutine Unwrap_kvect_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(kvect_info_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_kvect_info_type: Cannot determine fortran type'
        else
            if (fortran_type == 'kvect_info_type') then
                allocate(kvect_info_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_kvect_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nk',py_var,for_var%nk,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','kv',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_kvect_info_type','kv',p_real_2d,for_var%kv,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','kv_std',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_kvect_info_type','kv_std',p_real_2d,for_var%kv_std,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','sintlim',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_kvect_info_type','sintlim',p_real_1d,for_var%sintlim,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nharm',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_kvect_info_type','nharm',p_int_1d,for_var%nharm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','nq',py_var,for_var%nq,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_kvect_info_type','q_coeff',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_kvect_info_type','q_coeff',p_int_2d,for_var%q_coeff,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_kvect_info_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_kvect_info_type

    Module Subroutine Wrap_kvect_info_type(py_var,for_var,ierror)

        ! Arguments
        type(kvect_info_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_kv,nd_kv_std,nd_sintlim,nd_nharm,nd_q_coeff

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nk',for_var%nk)
        if (ierror == 0) ierror = ndarray_create(nd_kv,for_var%kv)
        if (ierror == 0) ierror = py_var%setitem('kv',nd_kv)
        if (ierror == 0) ierror = ndarray_create(nd_kv_std,for_var%kv_std)
        if (ierror == 0) ierror = py_var%setitem('kv_std',nd_kv_std)
        if (ierror == 0) ierror = ndarray_create(nd_sintlim,for_var%sintlim)
        if (ierror == 0) ierror = py_var%setitem('sintlim',nd_sintlim)
        if (ierror == 0) ierror = ndarray_create(nd_nharm,for_var%nharm)
        if (ierror == 0) ierror = py_var%setitem('nharm',nd_nharm)
        if (ierror == 0) ierror = py_var%setitem('nq',for_var%nq)
        if (ierror == 0) ierror = ndarray_create(nd_q_coeff,for_var%q_coeff)
        if (ierror == 0) ierror = py_var%setitem('q_coeff',nd_q_coeff)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_kvect_info_type: Wrapping failed'
        end if

    End Subroutine Wrap_kvect_info_type

    Module Subroutine Unwrap_point_orbit(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(point_orbit), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_orbit: Cannot determine fortran type'
        else
            if (fortran_type == 'point_orbit') then
                allocate(point_orbit :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_orbit: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pos',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_orbit','pos',p_real_2d,for_var%pos,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','mom',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_orbit','mom',p_real_2d,for_var%mom,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','pts',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_orbit','pts',p_int_1d,for_var%pts,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_orbit','lat',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_orbit','lat',p_int_2d,for_var%lat,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_point_orbit: Unwrapping failed'
        end if

    End Subroutine Unwrap_point_orbit

    Module Subroutine Wrap_point_orbit(py_var,for_var,ierror)

        ! Arguments
        type(point_orbit), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_pos,nd_mom,nd_pts,nd_lat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = ndarray_create(nd_pos,for_var%pos)
        if (ierror == 0) ierror = py_var%setitem('pos',nd_pos)
        if (ierror == 0) ierror = ndarray_create(nd_mom,for_var%mom)
        if (ierror == 0) ierror = py_var%setitem('mom',nd_mom)
        if (ierror == 0) ierror = ndarray_create(nd_pts,for_var%pts)
        if (ierror == 0) ierror = py_var%setitem('pts',nd_pts)
        if (ierror == 0) ierror = ndarray_create(nd_lat,for_var%lat)
        if (ierror == 0) ierror = py_var%setitem('lat',nd_lat)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_point_orbit: Wrapping failed'
        end if

    End Subroutine Wrap_point_orbit

end submodule