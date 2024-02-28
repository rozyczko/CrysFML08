!!----
!!----
!!----
SubModule (CFML_Python)  gSpaceGroups_Python_Wraps
    implicit none
    contains

    Module Subroutine Wrap_Symm_Oper_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        type(symm_oper_type), intent(in)    :: for_var
        type(dict),           intent(inout) :: py_var
        integer,              intent(out)   :: ierror

        !---- Local Variables ----!
        real, dimension(:,:), allocatable :: mat
        type(ndarray) :: nd_mat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','symm_oper_type')
        if (ierror == 0) ierror = py_var%setitem('time_inv',for_var%time_inv)
        if (ierror == 0) ierror = py_var%setitem('dt',for_var%dt)
        if (ierror == 0) then
            allocate(mat(size(for_var%mat,1),size(for_var%mat,2)))
            mat = real(for_var%mat)
            if (ierror == 0) ierror = ndarray_create(nd_mat,mat)
            if (ierror == 0) ierror = py_var%setitem('mat',nd_mat)
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Symm_Oper_Type: Wrapping failed'
         end if

    End Subroutine Wrap_Symm_Oper_Type

    Module Subroutine Unwrap_Symm_Oper_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),           intent(inout) :: py_var
        type(symm_oper_type), intent(out)   :: for_var
        integer,              intent(out)   :: ierror

        !---- Local Variables ----!
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type
        real, dimension(:,:), pointer :: p_real_2d

        ierror = 0
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Symm_Oper_Type: Cannot determine fortran type'
        else
            if (fortran_type /= 'symm_oper_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Symm_Oper_type: Wrong fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Symm_Oper_Type','time_inv',py_var,for_var%time_inv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Symm_Oper_Type','dt',py_var,for_var%dt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Symm_Oper_Type','mat',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Symm_Oper_Type','mat',p_real_2d,for_var%mat,ierror,order)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Symm_Oper_Type: Unwrapping failed'
         end if

    End Subroutine Unwrap_Symm_Oper_Type

    Module Subroutine Wrap_Group_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        class(group_type),    intent(in)    :: for_var
        type(dict),           intent(inout) :: py_var
        integer,              intent(out)   :: ierror

        !---- Local Variables ----!
        integer :: i
        real, dimension(:), allocatable :: centre_coord,anticentre_coord
        real, dimension(:,:), allocatable :: lat_tr,alat_tr,kv,kv_std
        type(ndarray) :: nd_inv,nd_centre_coord,nd_anticentre_coord,nd_lat_tr,nd_alat_tr,nd_kv,nd_kv_std,nd_sintlim,nd_nharm,nd_q_coeff,nd_rot,nd_m,nd_ep,nd_t,nd_ti
        type(list) :: li_op,li_symb_op
        type(dict), dimension(:), allocatable :: di_op

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','group_type')
        if (ierror == 0) ierror = py_var%setitem('multip',for_var%multip)
        if (ierror == 0) ierror = py_var%setitem('d',for_var%d)
        if (ierror == 0) ierror = ndarray_create(nd_inv,for_var%inv)
        if (ierror == 0) ierror = py_var%setitem('inv',nd_inv)
        if (ierror == 0) ierror = list_create(li_op)
        if (ierror == 0) allocate(di_op(size(for_var%op)))
        if (ierror == 0) then
            do i = 1 , size(for_var%op)
                ierror = dict_create(di_op(i))
                if (ierror == 0) call wrap_symm_oper_type(for_var%op(i),di_op(i),ierror)
                if (ierror == 0) ierror = li_op%append(di_op(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('op',li_op)
        if (ierror == 0) ierror = list_create(li_symb_op)
        if (ierror == 0) then
            do i = 1 , size(for_var%symb_op)
                if (ierror == 0) ierror = li_symb_op%append(trim(for_var%symb_op(i)))
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
                if (ierror == 0) ierror = py_var%setitem('shu_lat',A%shu_lat)
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
                if (ierror == 0) ierror = py_var%setitem('generators_list',A%generators_list)
                if (ierror == 0) nd_centre_coord = rational_to_ndarray(a%centre_coord,ierror)
                if (ierror == 0) ierror = py_var%setitem('centre_coord',nd_centre_coord)
                if (ierror == 0) then
                    if (allocated(a%anticentre_coord)) nd_anticentre_coord = rational_to_ndarray(a%anticentre_coord,ierror)
                    if (ierror == 0) ierror = py_var%setitem('anticentre_coord',nd_anticentre_coord)
                end if
                if (ierror == 0) nd_lat_tr = rational_to_ndarray(a%lat_tr,ierror)
                if (ierror == 0) ierror = py_var%setitem('lat_tr',nd_lat_tr)
                if (ierror == 0) nd_alat_tr = rational_to_ndarray(a%alat_tr,ierror)
                if (ierror == 0) ierror = py_var%setitem('alat_tr',nd_alat_tr)
                if (ierror == 0) ierror = py_var%setitem('fortran_type','spg_type')
            end select
        end if
        if (ierror == 0) then
            select type (A => for_var)
                type is (superspacegroup_type)
                    if (ierror == 0) ierror = py_var%setitem('ssg_symb',A%ssg_symb)
                    if (ierror == 0) ierror = py_var%setitem('ssg_bravais',A%ssg_bravais)
                    if (ierror == 0) ierror = py_var%setitem('ssg_nlabel',A%ssg_nlabel)
                    if (ierror == 0) ierror = py_var%setitem('nk',A%nk)
                    if (ierror == 0) ierror = py_var%setitem('nq',A%nq)
                    if (ierror == 0) ierror = ndarray_create(nd_kv,a%kv)
                    if (ierror == 0) ierror = py_var%setitem('kv',nd_kv)
                    if (ierror == 0) ierror = ndarray_create(nd_kv_std,a%kv_std)
                    if (ierror == 0) ierror = py_var%setitem('kv_std',nd_kv_std)
                    if (ierror == 0) ierror = ndarray_create(nd_sintlim,a%sintlim)
                    if (ierror == 0) ierror = py_var%setitem('sintlim',nd_sintlim)
                    if (ierror == 0) ierror = ndarray_create(nd_nharm,a%nharm)
                    if (ierror == 0) ierror = py_var%setitem('nharm',nd_nharm)
                    if (ierror == 0) ierror = ndarray_create(nd_q_coeff,a%q_coeff)
                    if (ierror == 0) ierror = py_var%setitem('q_coeff',nd_q_coeff)
                    if (ierror == 0) ierror = ndarray_create(nd_rot,a%rot)
                    if (ierror == 0) ierror = py_var%setitem('rot',nd_rot)
                    if (ierror == 0) ierror = ndarray_create(nd_m,a%m)
                    if (ierror == 0) ierror = py_var%setitem('m',nd_m)
                    if (ierror == 0) ierror = ndarray_create(nd_ep,a%ep)
                    if (ierror == 0) ierror = py_var%setitem('ep',nd_ep)
                    if (ierror == 0) ierror = ndarray_create(nd_t,a%t)
                    if (ierror == 0) ierror = py_var%setitem('t',nd_t)
                    if (ierror == 0) ierror = ndarray_create(nd_ti,a%ti)
                    if (ierror == 0) ierror = py_var%setitem('ti',nd_ti)
                    if (ierror == 0) ierror = py_var%setitem('fortran_type','superspacegroup_type')
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Group_Type: Wrapping failed'
        end if

    End Subroutine Wrap_Group_Type

    Module Subroutine Unwrap_Spg_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                   intent(inout) :: py_var
        class(spg_type), allocatable, intent(out)   :: for_var
        integer,                      intent(out)   :: ierror

        !---- Local Variables ----!
        integer :: i,n,m
        integer, dimension(:), pointer :: p_int_1d
        character(len=1) :: order
        character(len=2) :: shu_lat
        character(len=:), allocatable :: fortran_type,my_str
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        type(object) :: item
        type(list) :: my_list
        type(dict) :: my_dict

        ierror = 0
        if (ierror == 0) ierror = list_create(my_list)

        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Spg_Type: Cannot determine fortran type'
        else
            if (fortran_type == 'spg_type') then
                allocate(spg_type :: for_var)
            else if (fortran_type == 'superspacegroup_type') then
                allocate(superspacegroup_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Spg_Type: Wrong fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','multip',py_var,for_var%multip,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','d',py_var,for_var%d,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','inv',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Spg_Type','inv',p_int_1d,for_var%inv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_Spg_Type','op',my_list,for_var%op,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','symb_op',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_Spg_Type','op',my_list,for_var%symb_op,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','magnetic',py_var,for_var%magnetic,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','standard_setting',py_var,for_var%standard_setting,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','numspg',py_var,for_var%numspg,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','numshu',py_var,for_var%numshu,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','numops',py_var,for_var%numops,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','centred',py_var,for_var%centred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','anticentred',py_var,for_var%anticentred,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','mag_type',py_var,for_var%mag_type,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','num_lat',py_var,for_var%num_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','num_alat',py_var,for_var%num_alat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','parent_num',py_var,for_var%parent_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','bravais_num',py_var,for_var%bravais_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','spg_lat',py_var,for_var%spg_lat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','shu_lat',py_var,shu_lat,ierror)
        if (ierror == 0) then
            for_var%shu_lat(1) = shu_lat(1:1)
            for_var%shu_lat(2) = shu_lat(2:2)
        end if
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','init_label',py_var,for_var%init_label,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','parent_spg',py_var,for_var%parent_spg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','tfrom_parent',py_var,for_var%tfrom_parent,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','centre',py_var,for_var%centre,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','spg_symb',py_var,for_var%spg_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','bns_num',py_var,for_var%bns_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','og_num',py_var,for_var%og_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','bns_symb',py_var,for_var%bns_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','og_symb',py_var,for_var%og_symb,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','hall',py_var,for_var%hall,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','uni',py_var,for_var%uni,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','uni_num',py_var,for_var%uni_num,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','crystalsys',py_var,for_var%crystalsys,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','pg',py_var,for_var%pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','mag_pg',py_var,for_var%mag_pg,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','laue',py_var,for_var%laue,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','setting',py_var,for_var%setting,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','mat2std',py_var,for_var%mat2std,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','mat2std_shu',py_var,for_var%mat2std_shu,ierror)
        if (ierror == 0) call unwrap_dict_item_string_alloc('Unwrap_Spg_Type','generators_list',py_var,for_var%generators_list,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','centre_coord',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Spg_Type','centre_coord',p_real_1d,for_var%centre_coord,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','anticentre_coord',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Spg_Type','anticentre_coord',p_real_1d,for_var%anticentre_coord,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','lat_tr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Spg_Type','lat_tr',p_real_2d,for_var%lat_tr,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Spg_Type','alat_tr',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Spg_Type','alat_tr',p_real_2d,for_var%alat_tr,ierror,order)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Spg_Type: Unwrapping failed'
         end if

    End Subroutine Unwrap_Spg_Type

End SubModule gSpaceGroups_Python_Wraps