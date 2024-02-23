submodule (CFML_Wraps) Wraps_EoS

    implicit none
    contains

    Module Subroutine Unwrap_pvt_table(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(pvt_table), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_pvt_table: Cannot determine fortran type'
        else
            if (fortran_type == 'pvt_table') then
                allocate(pvt_table :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_pvt_table: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','np',py_var,for_var%np,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','nt',py_var,for_var%nt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','pmin',py_var,for_var%pmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','pmax',py_var,for_var%pmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','tmin',py_var,for_var%tmin,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','tmax',py_var,for_var%tmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_pvt_table','ptv',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_pvt_table','ptv',p_real_3d,for_var%ptv,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_pvt_table: Unwrapping failed'
        end if

    End Subroutine Unwrap_pvt_table

    Module Subroutine Wrap_pvt_table(py_var,for_var,ierror)

        ! Arguments
        type(pvt_table), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_ptv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('np',for_var%np)
        if (ierror == 0) ierror = py_var%setitem('nt',for_var%nt)
        if (ierror == 0) ierror = py_var%setitem('pmin',for_var%pmin)
        if (ierror == 0) ierror = py_var%setitem('pmax',for_var%pmax)
        if (ierror == 0) ierror = py_var%setitem('tmin',for_var%tmin)
        if (ierror == 0) ierror = py_var%setitem('tmax',for_var%tmax)
        if (ierror == 0) ierror = ndarray_create(nd_ptv,for_var%ptv)
        if (ierror == 0) ierror = py_var%setitem('ptv',nd_ptv)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_pvt_table: Wrapping failed'
        end if

    End Subroutine Wrap_pvt_table

    Module Subroutine Unwrap_eos_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(eos_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list
        type(dict) :: dict_table

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_eos_type: Cannot determine fortran type'
        else
            if (fortran_type == 'eos_type') then
                allocate(eos_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_eos_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','system',py_var,for_var%system,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','model',py_var,for_var%model,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','tmodel',py_var,for_var%tmodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','tranmodel',py_var,for_var%tranmodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','smodel',py_var,for_var%smodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','cmodel',py_var,for_var%cmodel,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','oscmodel',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_type','oscmodel',my_list,for_var%oscmodel,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','parname',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_type','parname',my_list,for_var%parname,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','comment',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_type','comment',my_list,for_var%comment,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','pscale_name',py_var,for_var%pscale_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','vscale_name',py_var,for_var%vscale_name,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','doc',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_type','doc',my_list,for_var%doc,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','savedate',py_var,for_var%savedate,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','lineardir',py_var,for_var%lineardir,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','imodel',py_var,for_var%imodel,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iorder',py_var,for_var%iorder,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','linear',py_var,for_var%linear,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','itherm',py_var,for_var%itherm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','itran',py_var,for_var%itran,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','ishear',py_var,for_var%ishear,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','icross',py_var,for_var%icross,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iangle',py_var,for_var%iangle,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iosc',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','iosc',p_int_1d,for_var%iosc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iuse',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','iuse',p_int_1d,for_var%iuse,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','pref',py_var,for_var%pref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','tref',py_var,for_var%tref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','stoich',py_var,for_var%stoich,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','density0',py_var,for_var%density0,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','tref_fixed',py_var,for_var%tref_fixed,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','pthermaleos',py_var,for_var%pthermaleos,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','osc_allowed',py_var,for_var%osc_allowed,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','allowed_orders',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_type','allowed_orders',my_list,for_var%allowed_orders,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','params',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','params',p_real_1d,for_var%params,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','esd',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','esd',p_real_1d,for_var%esd,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','x',py_var,for_var%x,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','wchi2',py_var,for_var%wchi2,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','delpmax',py_var,for_var%delpmax,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iwt',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','iwt',p_int_1d,for_var%iwt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','iref',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','iref',p_int_1d,for_var%iref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','factor',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','factor',p_real_1d,for_var%factor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','alphafactor',py_var,for_var%alphafactor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','lastshift',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','lastshift',p_real_1d,for_var%lastshift,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','vcv',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','vcv',p_real_2d,for_var%vcv,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','angpoly',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_type','angpoly',p_real_3d,for_var%angpoly,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','table',py_var,dict_table,ierror)
        if (ierror == 0) call unwrap_pvt_table('Unwrap_eos_type','table',dict_table,for_var%table,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','cv_table',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_eos_type','cv_table',p_real_2d,for_var%cv_table,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_type','cv_external',py_var,for_var%cv_external,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_eos_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_eos_type

    Module Subroutine Wrap_eos_type(py_var,for_var,ierror)

        ! Arguments
        type(eos_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_table
        type(list) :: li_oscmodel,li_parname,li_comment,li_doc,li_allowed_orders
        type(ndarray) :: nd_iosc,nd_iuse,nd_params,nd_esd,nd_iwt,nd_iref,nd_factor,nd_lastshift,nd_vcv,nd_angpoly,nd_cv_table

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('title',for_var%title)
        if (ierror == 0) ierror = py_var%setitem('system',for_var%system)
        if (ierror == 0) ierror = py_var%setitem('model',for_var%model)
        if (ierror == 0) ierror = py_var%setitem('tmodel',for_var%tmodel)
        if (ierror == 0) ierror = py_var%setitem('tranmodel',for_var%tranmodel)
        if (ierror == 0) ierror = py_var%setitem('smodel',for_var%smodel)
        if (ierror == 0) ierror = py_var%setitem('cmodel',for_var%cmodel)
        if (ierror == 0) ierror = list_create(li_oscmodel)
        if (ierror == 0) then
            do i = 1 , size(for_var%oscmodel)
                if (ierror == 0) ierror = li_oscmodel%append(for_var%oscmodel(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('oscmodel',li_oscmodel)
        if (ierror == 0) ierror = list_create(li_parname)
        if (ierror == 0) then
            do i = 1 , size(for_var%parname)
                if (ierror == 0) ierror = li_parname%append(for_var%parname(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('parname',li_parname)
        if (ierror == 0) ierror = list_create(li_comment)
        if (ierror == 0) then
            do i = 1 , size(for_var%comment)
                if (ierror == 0) ierror = li_comment%append(for_var%comment(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('comment',li_comment)
        if (ierror == 0) ierror = py_var%setitem('pscale_name',for_var%pscale_name)
        if (ierror == 0) ierror = py_var%setitem('vscale_name',for_var%vscale_name)
        if (ierror == 0) ierror = list_create(li_doc)
        if (ierror == 0) then
            do i = 1 , size(for_var%doc)
                if (ierror == 0) ierror = li_doc%append(for_var%doc(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('doc',li_doc)
        if (ierror == 0) ierror = py_var%setitem('savedate',for_var%savedate)
        if (ierror == 0) ierror = py_var%setitem('lineardir',for_var%lineardir)
        if (ierror == 0) ierror = py_var%setitem('imodel',for_var%imodel)
        if (ierror == 0) ierror = py_var%setitem('iorder',for_var%iorder)
        if (ierror == 0) ierror = py_var%setitem('linear',for_var%linear)
        if (ierror == 0) ierror = py_var%setitem('itherm',for_var%itherm)
        if (ierror == 0) ierror = py_var%setitem('itran',for_var%itran)
        if (ierror == 0) ierror = py_var%setitem('ishear',for_var%ishear)
        if (ierror == 0) ierror = py_var%setitem('icross',for_var%icross)
        if (ierror == 0) ierror = py_var%setitem('iangle',for_var%iangle)
        if (ierror == 0) ierror = ndarray_create(nd_iosc,for_var%iosc)
        if (ierror == 0) ierror = py_var%setitem('iosc',nd_iosc)
        if (ierror == 0) ierror = ndarray_create(nd_iuse,for_var%iuse)
        if (ierror == 0) ierror = py_var%setitem('iuse',nd_iuse)
        if (ierror == 0) ierror = py_var%setitem('pref',for_var%pref)
        if (ierror == 0) ierror = py_var%setitem('tref',for_var%tref)
        if (ierror == 0) ierror = py_var%setitem('stoich',for_var%stoich)
        if (ierror == 0) ierror = py_var%setitem('density0',for_var%density0)
        if (ierror == 0) ierror = py_var%setitem('tref_fixed',for_var%tref_fixed)
        if (ierror == 0) ierror = py_var%setitem('pthermaleos',for_var%pthermaleos)
        if (ierror == 0) ierror = py_var%setitem('osc_allowed',for_var%osc_allowed)
        if (ierror == 0) ierror = list_create(li_allowed_orders)
        if (ierror == 0) then
            do i = 1 , size(for_var%allowed_orders)
                if (ierror == 0) ierror = li_allowed_orders%append(for_var%allowed_orders(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('allowed_orders',li_allowed_orders)
        if (ierror == 0) ierror = ndarray_create(nd_params,for_var%params)
        if (ierror == 0) ierror = py_var%setitem('params',nd_params)
        if (ierror == 0) ierror = ndarray_create(nd_esd,for_var%esd)
        if (ierror == 0) ierror = py_var%setitem('esd',nd_esd)
        if (ierror == 0) ierror = py_var%setitem('x',for_var%x)
        if (ierror == 0) ierror = py_var%setitem('wchi2',for_var%wchi2)
        if (ierror == 0) ierror = py_var%setitem('delpmax',for_var%delpmax)
        if (ierror == 0) ierror = ndarray_create(nd_iwt,for_var%iwt)
        if (ierror == 0) ierror = py_var%setitem('iwt',nd_iwt)
        if (ierror == 0) ierror = ndarray_create(nd_iref,for_var%iref)
        if (ierror == 0) ierror = py_var%setitem('iref',nd_iref)
        if (ierror == 0) ierror = ndarray_create(nd_factor,for_var%factor)
        if (ierror == 0) ierror = py_var%setitem('factor',nd_factor)
        if (ierror == 0) ierror = py_var%setitem('alphafactor',for_var%alphafactor)
        if (ierror == 0) ierror = ndarray_create(nd_lastshift,for_var%lastshift)
        if (ierror == 0) ierror = py_var%setitem('lastshift',nd_lastshift)
        if (ierror == 0) ierror = ndarray_create(nd_vcv,for_var%vcv)
        if (ierror == 0) ierror = py_var%setitem('vcv',nd_vcv)
        if (ierror == 0) ierror = ndarray_create(nd_angpoly,for_var%angpoly)
        if (ierror == 0) ierror = py_var%setitem('angpoly',nd_angpoly)
        if (ierror == 0) call wrap_pvt_table(for_var%table,di_table,ierror)
        if (ierror == 0) ierror = py_var%setitem('table',di_table)
        if (ierror == 0) ierror = ndarray_create(nd_cv_table,for_var%cv_table)
        if (ierror == 0) ierror = py_var%setitem('cv_table',nd_cv_table)
        if (ierror == 0) ierror = py_var%setitem('cv_external',for_var%cv_external)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_eos_type: Wrapping failed'
        end if

    End Subroutine Wrap_eos_type

    Module Subroutine Unwrap_eos_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(eos_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_eos_list_type: Cannot determine fortran type'
        else
            if (fortran_type == 'eos_list_type') then
                allocate(eos_list_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_eos_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_list_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_list_type','system',py_var,for_var%system,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_list_type','eos',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_list_type','eos',my_list,for_var%eos,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_eos_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_eos_list_type

    Module Subroutine Wrap_eos_list_type(py_var,for_var,ierror)

        ! Arguments
        type(eos_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_eos

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (ierror == 0) ierror = py_var%setitem('system',for_var%system)
        if (ierror == 0) ierror = list_create(li_eos)
        if (ierror == 0) allocate(di_eos(size(for_var%eos)))
        if (ierror == 0) then
            do i = 1 , size(for_var%eos)
                ierror = dict_create(di_eos(i))
                if (ierror == 0) call wrap_eos_type(for_var%eos,(di_eos(i),ierror))
                if (ierror == 0) ierror = li_eos%append(di_eos(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('eos',li_eos)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_eos_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_eos_list_type

    Module Subroutine Unwrap_eos_cell_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(eos_cell_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        type(list) :: my_list
        type(dict) :: dict_eosc,dict_eosang

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_eos_cell_type: Cannot determine fortran type'
        else
            if (fortran_type == 'eos_cell_type') then
                allocate(eos_cell_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_eos_cell_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','system',py_var,for_var%system,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','eos',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_cell_type','eos',my_list,for_var%eos,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','unique_label',py_var,for_var%unique_label,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','unique',py_var,for_var%unique,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','obtuse',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_cell_type','obtuse',my_list,for_var%obtuse,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','eosc',py_var,dict_eosc,ierror)
        if (ierror == 0) call unwrap_eos_type('Unwrap_eos_cell_type','eosc',dict_eosc,for_var%eosc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','eosang',py_var,dict_eosang,ierror)
        if (ierror == 0) call unwrap_eos_type('Unwrap_eos_cell_type','eosang',dict_eosang,for_var%eosang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','loaded',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_cell_type','loaded',p_int_1d,for_var%loaded,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','cout',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_cell_type','cout',my_list,for_var%cout,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_cell_type','inputlist',py_var,for_var%inputlist,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_eos_cell_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_eos_cell_type

    Module Subroutine Wrap_eos_cell_type(py_var,for_var,ierror)

        ! Arguments
        type(eos_cell_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_eosc,di_eosang
        type(list) :: li_eos,li_obtuse,li_cout
        type(ndarray) :: nd_loaded

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (ierror == 0) ierror = py_var%setitem('system',for_var%system)
        if (ierror == 0) ierror = list_create(li_eos)
        if (ierror == 0) allocate(di_eos(size(for_var%eos)))
        if (ierror == 0) then
            do i = 1 , size(for_var%eos)
                ierror = dict_create(di_eos(i))
                if (ierror == 0) call wrap_eos_type(for_var%eos,(di_eos(i),ierror))
                if (ierror == 0) ierror = li_eos%append(di_eos(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('eos',li_eos)
        if (ierror == 0) ierror = py_var%setitem('unique_label',for_var%unique_label)
        if (ierror == 0) ierror = py_var%setitem('unique',for_var%unique)
        if (ierror == 0) ierror = list_create(li_obtuse)
        if (ierror == 0) then
            do i = 1 , size(for_var%obtuse)
                if (ierror == 0) ierror = li_obtuse%append(for_var%obtuse(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('obtuse',li_obtuse)
        if (ierror == 0) call wrap_eos_type(for_var%eosc,di_eosc,ierror)
        if (ierror == 0) ierror = py_var%setitem('eosc',di_eosc)
        if (ierror == 0) call wrap_eos_type(for_var%eosang,di_eosang,ierror)
        if (ierror == 0) ierror = py_var%setitem('eosang',di_eosang)
        if (ierror == 0) ierror = ndarray_create(nd_loaded,for_var%loaded)
        if (ierror == 0) ierror = py_var%setitem('loaded',nd_loaded)
        if (ierror == 0) ierror = list_create(li_cout)
        if (ierror == 0) then
            do i = 1 , size(for_var%cout)
                if (ierror == 0) ierror = li_cout%append(for_var%cout(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('cout',li_cout)
        if (ierror == 0) ierror = py_var%setitem('inputlist',for_var%inputlist)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_eos_cell_type: Wrapping failed'
        end if

    End Subroutine Wrap_eos_cell_type

    Module Subroutine Unwrap_axis_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(axis_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_axis_type: Cannot determine fortran type'
        else
            if (fortran_type == 'axis_type') then
                allocate(axis_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_axis_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_axis_type','v',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_axis_type','v',p_real_1d,for_var%v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_axis_type','atype',py_var,for_var%atype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_axis_type','ieos',py_var,for_var%ieos,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_axis_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_axis_type

    Module Subroutine Wrap_axis_type(py_var,for_var,ierror)

        ! Arguments
        type(axis_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_v

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_v,for_var%v)
        if (ierror == 0) ierror = py_var%setitem('v',nd_v)
        if (ierror == 0) ierror = py_var%setitem('atype',for_var%atype)
        if (ierror == 0) ierror = py_var%setitem('ieos',for_var%ieos)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_axis_type: Wrapping failed'
        end if

    End Subroutine Wrap_axis_type

    Module Subroutine Unwrap_eos_data_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(eos_data_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_eos_data_type: Cannot determine fortran type'
        else
            if (fortran_type == 'eos_data_type') then
                allocate(eos_data_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_eos_data_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','iuse',py_var,for_var%iuse,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','igrp',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_type','igrp',p_int_1d,for_var%igrp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','xtype',py_var,for_var%xtype,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','t',py_var,for_var%t,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','p',py_var,for_var%p,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','v',py_var,for_var%v,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','cell',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_type','cell',p_real_1d,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','ang',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_type','ang',p_real_1d,for_var%ang,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','sigt',py_var,for_var%sigt,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','sigp',py_var,for_var%sigp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','sigv',py_var,for_var%sigv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','sigc',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_type','sigc',p_real_1d,for_var%sigc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_type','siga',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_type','siga',p_real_1d,for_var%siga,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_eos_data_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_eos_data_type

    Module Subroutine Wrap_eos_data_type(py_var,for_var,ierror)

        ! Arguments
        type(eos_data_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_igrp,nd_cell,nd_ang,nd_sigc,nd_siga

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('iuse',for_var%iuse)
        if (ierror == 0) ierror = ndarray_create(nd_igrp,for_var%igrp)
        if (ierror == 0) ierror = py_var%setitem('igrp',nd_igrp)
        if (ierror == 0) ierror = py_var%setitem('xtype',for_var%xtype)
        if (ierror == 0) ierror = py_var%setitem('t',for_var%t)
        if (ierror == 0) ierror = py_var%setitem('p',for_var%p)
        if (ierror == 0) ierror = py_var%setitem('v',for_var%v)
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = py_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = py_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = py_var%setitem('sigt',for_var%sigt)
        if (ierror == 0) ierror = py_var%setitem('sigp',for_var%sigp)
        if (ierror == 0) ierror = py_var%setitem('sigv',for_var%sigv)
        if (ierror == 0) ierror = ndarray_create(nd_sigc,for_var%sigc)
        if (ierror == 0) ierror = py_var%setitem('sigc',nd_sigc)
        if (ierror == 0) ierror = ndarray_create(nd_siga,for_var%siga)
        if (ierror == 0) ierror = py_var%setitem('siga',nd_siga)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_eos_data_type: Wrapping failed'
        end if

    End Subroutine Wrap_eos_data_type

    Module Subroutine Unwrap_eos_data_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(eos_data_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_eos_data_list_type: Cannot determine fortran type'
        else
            if (fortran_type == 'eos_data_list_type') then
                allocate(eos_data_list_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_eos_data_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','system',py_var,for_var%system,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','n',py_var,for_var%n,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','ic_dat',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_eos_data_list_type','ic_dat',p_int_1d,for_var%ic_dat,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','pscale_name',py_var,for_var%pscale_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','vscale_name',py_var,for_var%vscale_name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','lscale_name',py_var,for_var%lscale_name,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_eos_data_list_type','eosd',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_eos_data_list_type','eosd',my_list,for_var%eosd,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_eos_data_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_eos_data_list_type

    Module Subroutine Wrap_eos_data_list_type(py_var,for_var,ierror)

        ! Arguments
        type(eos_data_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_eosd
        type(ndarray) :: nd_ic_dat

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('title',for_var%title)
        if (ierror == 0) ierror = py_var%setitem('system',for_var%system)
        if (ierror == 0) ierror = py_var%setitem('n',for_var%n)
        if (ierror == 0) ierror = ndarray_create(nd_ic_dat,for_var%ic_dat)
        if (ierror == 0) ierror = py_var%setitem('ic_dat',nd_ic_dat)
        if (ierror == 0) ierror = py_var%setitem('pscale_name',for_var%pscale_name)
        if (ierror == 0) ierror = py_var%setitem('vscale_name',for_var%vscale_name)
        if (ierror == 0) ierror = py_var%setitem('lscale_name',for_var%lscale_name)
        if (ierror == 0) ierror = list_create(li_eosd)
        if (ierror == 0) allocate(di_eosd(size(for_var%eosd)))
        if (ierror == 0) then
            do i = 1 , size(for_var%eosd)
                ierror = dict_create(di_eosd(i))
                if (ierror == 0) call wrap_eos_data_type(for_var%eosd,(di_eosd(i),ierror))
                if (ierror == 0) ierror = li_eosd%append(di_eosd(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('eosd',li_eosd)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_eos_data_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_eos_data_list_type

end submodule