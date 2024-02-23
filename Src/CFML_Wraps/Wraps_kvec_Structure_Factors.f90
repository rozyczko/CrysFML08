submodule (CFML_Wraps) Wraps_kvec_Structure_Factors

    implicit none
    contains

    Module Subroutine Unwrap_magh_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magh_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magh_type: Cannot determine fortran type'
        else
            if (fortran_type == 'magh_type') then
                allocate(magh_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magh_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','keqv_minus',py_var,for_var%keqv_minus,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','num_k',py_var,for_var%num_k,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','signp',py_var,for_var%signp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magh_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','msf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magh_type','msf',my_list,for_var%msf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','tmsf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magh_type','tmsf',my_list,for_var%tmsf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','miv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magh_type','miv',my_list,for_var%miv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_type','mivc',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magh_type','mivc',my_list,for_var%mivc,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_magh_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_magh_type

    Module Subroutine Wrap_magh_type(py_var,for_var,ierror)

        ! Arguments
        type(magh_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_msf,li_tmsf,li_miv,li_mivc
        type(ndarray) :: nd_h

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('keqv_minus',for_var%keqv_minus)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('num_k',for_var%num_k)
        if (ierror == 0) ierror = py_var%setitem('signp',for_var%signp)
        if (ierror == 0) ierror = py_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = py_var%setitem('sqmiv',for_var%sqmiv)
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = list_create(li_msf)
        if (ierror == 0) then
            do i = 1 , size(for_var%msf)
                if (ierror == 0) ierror = li_msf%append(for_var%msf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('msf',li_msf)
        if (ierror == 0) ierror = list_create(li_tmsf)
        if (ierror == 0) then
            do i = 1 , size(for_var%tmsf)
                if (ierror == 0) ierror = li_tmsf%append(for_var%tmsf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('tmsf',li_tmsf)
        if (ierror == 0) ierror = list_create(li_miv)
        if (ierror == 0) then
            do i = 1 , size(for_var%miv)
                if (ierror == 0) ierror = li_miv%append(for_var%miv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('miv',li_miv)
        if (ierror == 0) ierror = list_create(li_mivc)
        if (ierror == 0) then
            do i = 1 , size(for_var%mivc)
                if (ierror == 0) ierror = li_mivc%append(for_var%mivc(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mivc',li_mivc)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_magh_type: Wrapping failed'
        end if

    End Subroutine Wrap_magh_type

    Module Subroutine Unwrap_magh_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magh_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magh_list_type: Cannot determine fortran type'
        else
            if (fortran_type == 'magh_list_type') then
                allocate(magh_list_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magh_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magh_list_type','mh',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_magh_list_type','mh',my_list,for_var%mh,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_magh_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_magh_list_type

    Module Subroutine Wrap_magh_list_type(py_var,for_var,ierror)

        ! Arguments
        type(magh_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_mh

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_mh)
        if (ierror == 0) allocate(di_mh(size(for_var%mh)))
        if (ierror == 0) then
            do i = 1 , size(for_var%mh)
                ierror = dict_create(di_mh(i))
                if (ierror == 0) call wrap_magh_type(for_var%mh,(di_mh(i),ierror))
                if (ierror == 0) ierror = li_mh%append(di_mh(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mh',li_mh)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_magh_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_magh_list_type

    Module Subroutine Unwrap_maghd_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(maghd_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_maghd_type: Cannot determine fortran type'
        else
            if (fortran_type == 'maghd_type') then
                allocate(maghd_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_maghd_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','keqv_minus',py_var,for_var%keqv_minus,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','num_k',py_var,for_var%num_k,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','signp',py_var,for_var%signp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','sqamiv',py_var,for_var%sqamiv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_maghd_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','msf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_maghd_type','msf',my_list,for_var%msf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','miv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_maghd_type','miv',my_list,for_var%miv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','mivc',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_maghd_type','mivc',my_list,for_var%mivc,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_type','amiv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_maghd_type','amiv',my_list,for_var%amiv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_maghd_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_maghd_type

    Module Subroutine Wrap_maghd_type(py_var,for_var,ierror)

        ! Arguments
        type(maghd_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_msf,li_miv,li_mivc,li_amiv
        type(ndarray) :: nd_h

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('keqv_minus',for_var%keqv_minus)
        if (ierror == 0) ierror = py_var%setitem('num_k',for_var%num_k)
        if (ierror == 0) ierror = py_var%setitem('signp',for_var%signp)
        if (ierror == 0) ierror = py_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = py_var%setitem('sqamiv',for_var%sqamiv)
        if (ierror == 0) ierror = py_var%setitem('sqmiv',for_var%sqmiv)
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = list_create(li_msf)
        if (ierror == 0) then
            do i = 1 , size(for_var%msf)
                if (ierror == 0) ierror = li_msf%append(for_var%msf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('msf',li_msf)
        if (ierror == 0) ierror = list_create(li_miv)
        if (ierror == 0) then
            do i = 1 , size(for_var%miv)
                if (ierror == 0) ierror = li_miv%append(for_var%miv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('miv',li_miv)
        if (ierror == 0) ierror = list_create(li_mivc)
        if (ierror == 0) then
            do i = 1 , size(for_var%mivc)
                if (ierror == 0) ierror = li_mivc%append(for_var%mivc(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mivc',li_mivc)
        if (ierror == 0) ierror = list_create(li_amiv)
        if (ierror == 0) then
            do i = 1 , size(for_var%amiv)
                if (ierror == 0) ierror = li_amiv%append(for_var%amiv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('amiv',li_amiv)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_maghd_type: Wrapping failed'
        end if

    End Subroutine Wrap_maghd_type

    Module Subroutine Unwrap_maghd_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(maghd_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_maghd_list_type: Cannot determine fortran type'
        else
            if (fortran_type == 'maghd_list_type') then
                allocate(maghd_list_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_maghd_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_maghd_list_type','mh',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_maghd_list_type','mh',my_list,for_var%mh,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_maghd_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_maghd_list_type

    Module Subroutine Wrap_maghd_list_type(py_var,for_var,ierror)

        ! Arguments
        type(maghd_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_mh

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_mh)
        if (ierror == 0) allocate(di_mh(size(for_var%mh)))
        if (ierror == 0) then
            do i = 1 , size(for_var%mh)
                ierror = dict_create(di_mh(i))
                if (ierror == 0) call wrap_maghd_type(for_var%mh,(di_mh(i),ierror))
                if (ierror == 0) ierror = li_mh%append(di_mh(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mh',li_mh)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_maghd_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_maghd_list_type

end submodule