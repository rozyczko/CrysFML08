!!----
!!----
!!----
SubModule (CFML_Python)  gSpaceGroups_Python_Wraps
    implicit none
    contains

    !!----
    !!---- WRAP_Symm_Oper_TYPE
    !!----
    !!---- 03/07/2023
    !!
    Module Subroutine Wrap_Symm_Oper_Type(for_var, py_var)
        !---- Arguments ----!
        type(symm_oper_type), intent(in)    :: for_var
        type(dict),           intent(inout) :: py_var

        !---- Local Variables ----!
        integer :: ierror
        real, dimension(:,:), allocatable :: mat
        type(ndarray) :: nd_mat

        ierror = 0
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

    Module Subroutine Wrap_Group_Type(for_var, py_var)
        !---- Arguments ----!
        class(group_type),    intent(in)    :: for_var
        type(dict),           intent(inout) :: py_var

        !---- Local Variables ----!
        integer :: ierror,i
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
        if (ierror == 0) ierror = list_create(li_op)
        if (ierror == 0) allocate(di_op(size(for_var%op)))
        if (ierror == 0) then
            do i = 1 , size(for_var%op)
                ierror = dict_create(di_op(i))
                if (ierror == 0) call wrap_symm_oper_type(for_var%op(i),di_op(i))
                ierror = err_cfml%ierr
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
                    if (ierror == 0) ierror = py_var%setitem('ssg_symb',A%ssg_symb)
                    if (ierror == 0) ierror = py_var%setitem('ssg_bravais',A%ssg_bravais)
                    if (ierror == 0) ierror = py_var%setitem('ssg_nlabel',A%ssg_nlabel)
                    if (ierror == 0) nd_centre_coord = rational_to_ndarray(a%centre_coord,ierror)
                    if (ierror == 0) ierror = py_var%setitem('centre_coord',nd_centre_coord)
                    if (ierror == 0) nd_anticentre_coord = rational_to_ndarray(a%anticentre_coord,ierror)
                    if (ierror == 0) ierror = py_var%setitem('anticentre_coord',nd_anticentre_coord)
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

End SubModule gSpaceGroups_Python_Wraps