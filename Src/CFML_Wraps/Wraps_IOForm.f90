submodule (CFML_Wraps) Wraps_IOForm

    implicit none
    contains

    Module Subroutine list_to_array_interval_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_interval_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_interval_type

    Module Subroutine list_to_array_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_interval_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_interval_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_interval_type_no_alloc

    Module Subroutine list_to_array2d_interval_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_interval_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_interval_type

    Module Subroutine list_to_array2d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(interval_type), dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array2d_interval_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (n /= size(arr,2)) then
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array2d_interval_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_interval_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_interval_type_no_alloc

    Module Subroutine Unwrap_interval_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(interval_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_interval_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'interval_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_interval_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_interval_type','mina',py_var,for_var%mina,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_interval_type','maxb',py_var,for_var%maxb,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_interval_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_interval_type

    Module Subroutine Wrap_interval_type(for_var,py_var,ierror)

        ! Arguments
        type(interval_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('mina',for_var%mina)
        if (ierror == 0) ierror = py_var%setitem('maxb',for_var%maxb)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_interval_type: Wrapping failed'
        end if

    End Subroutine Wrap_interval_type

    Module Subroutine list_to_array_job_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_job_info_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_job_info_type

    Module Subroutine list_to_array_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_job_info_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_job_info_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_job_info_type_no_alloc

    Module Subroutine list_to_array2d_job_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_job_info_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_job_info_type

    Module Subroutine list_to_array2d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(job_info_type), dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array2d_job_info_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (n /= size(arr,2)) then
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array2d_job_info_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_job_info_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_job_info_type_no_alloc

    Module Subroutine Unwrap_job_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(job_info_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_job_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'job_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_job_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','title',py_var,for_var%title,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_phases',py_var,for_var%num_phases,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_patterns',py_var,for_var%num_patterns,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','num_cmd',py_var,for_var%num_cmd,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','patt_typ',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','patt_typ',my_list,for_var%patt_typ,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','phas_nam',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','phas_nam',my_list,for_var%phas_nam,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','cmd',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','cmd',my_list,for_var%cmd,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_stl',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_stl',my_list,for_var%range_stl,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_q',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_q',my_list,for_var%range_q,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_d',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_d',my_list,for_var%range_d,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_2theta',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_2theta',my_list,for_var%range_2theta,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_energy',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_energy',my_list,for_var%range_energy,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','range_tof',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','range_tof',my_list,for_var%range_tof,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','lambda',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_job_info_type','lambda',my_list,for_var%lambda,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','ratio',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_job_info_type','ratio',p_real_1d,for_var%ratio,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','dtt1',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_job_info_type','dtt1',p_real_1d,for_var%dtt1,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_job_info_type','dtt2',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_job_info_type','dtt2',p_real_1d,for_var%dtt2,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_job_info_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_job_info_type

    Module Subroutine Wrap_job_info_type(for_var,py_var,ierror)

        ! Arguments
        type(job_info_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_patt_typ,li_phas_nam,li_cmd,li_range_stl,li_range_q,li_range_d,li_range_2theta,li_range_energy,li_range_tof,li_lambda
        type(dict), dimension(:), allocatable :: di_range_stl,di_range_q,di_range_d,di_range_2theta,di_range_energy,di_range_tof,di_lambda
        type(ndarray) :: nd_ratio,nd_dtt1,nd_dtt2

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('title',for_var%title)
        if (ierror == 0) ierror = py_var%setitem('num_phases',for_var%num_phases)
        if (ierror == 0) ierror = py_var%setitem('num_patterns',for_var%num_patterns)
        if (ierror == 0) ierror = py_var%setitem('num_cmd',for_var%num_cmd)
        if (ierror == 0) ierror = list_create(li_patt_typ)
        if (ierror == 0) then
            do i = 1 , size(for_var%patt_typ)
                if (ierror == 0) ierror = li_patt_typ%append(for_var%patt_typ(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('patt_typ',li_patt_typ)
        if (ierror == 0) ierror = list_create(li_phas_nam)
        if (ierror == 0) then
            do i = 1 , size(for_var%phas_nam)
                if (ierror == 0) ierror = li_phas_nam%append(for_var%phas_nam(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('phas_nam',li_phas_nam)
        if (ierror == 0) ierror = list_create(li_cmd)
        if (ierror == 0) then
            do i = 1 , size(for_var%cmd)
                if (ierror == 0) ierror = li_cmd%append(for_var%cmd(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('cmd',li_cmd)
        if (ierror == 0) ierror = list_create(li_range_stl)
        if (ierror == 0) allocate(di_range_stl(size(for_var%range_stl)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_stl)
                ierror = dict_create(di_range_stl(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_stl(i),di_range_stl(i),ierror)
                if (ierror == 0) ierror = li_range_stl%append(di_range_stl(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_stl',li_range_stl)
        if (ierror == 0) ierror = list_create(li_range_q)
        if (ierror == 0) allocate(di_range_q(size(for_var%range_q)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_q)
                ierror = dict_create(di_range_q(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_q(i),di_range_q(i),ierror)
                if (ierror == 0) ierror = li_range_q%append(di_range_q(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_q',li_range_q)
        if (ierror == 0) ierror = list_create(li_range_d)
        if (ierror == 0) allocate(di_range_d(size(for_var%range_d)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_d)
                ierror = dict_create(di_range_d(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_d(i),di_range_d(i),ierror)
                if (ierror == 0) ierror = li_range_d%append(di_range_d(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_d',li_range_d)
        if (ierror == 0) ierror = list_create(li_range_2theta)
        if (ierror == 0) allocate(di_range_2theta(size(for_var%range_2theta)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_2theta)
                ierror = dict_create(di_range_2theta(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_2theta(i),di_range_2theta(i),ierror)
                if (ierror == 0) ierror = li_range_2theta%append(di_range_2theta(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_2theta',li_range_2theta)
        if (ierror == 0) ierror = list_create(li_range_energy)
        if (ierror == 0) allocate(di_range_energy(size(for_var%range_energy)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_energy)
                ierror = dict_create(di_range_energy(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_energy(i),di_range_energy(i),ierror)
                if (ierror == 0) ierror = li_range_energy%append(di_range_energy(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_energy',li_range_energy)
        if (ierror == 0) ierror = list_create(li_range_tof)
        if (ierror == 0) allocate(di_range_tof(size(for_var%range_tof)))
        if (ierror == 0) then
            do i = 1 , size(for_var%range_tof)
                ierror = dict_create(di_range_tof(i))
                if (ierror == 0) call wrap_interval_type(for_var%range_tof(i),di_range_tof(i),ierror)
                if (ierror == 0) ierror = li_range_tof%append(di_range_tof(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('range_tof',li_range_tof)
        if (ierror == 0) ierror = list_create(li_lambda)
        if (ierror == 0) allocate(di_lambda(size(for_var%lambda)))
        if (ierror == 0) then
            do i = 1 , size(for_var%lambda)
                ierror = dict_create(di_lambda(i))
                if (ierror == 0) call wrap_interval_type(for_var%lambda(i),di_lambda(i),ierror)
                if (ierror == 0) ierror = li_lambda%append(di_lambda(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('lambda',li_lambda)
        if (ierror == 0) ierror = ndarray_create(nd_ratio,for_var%ratio)
        if (ierror == 0) ierror = py_var%setitem('ratio',nd_ratio)
        if (ierror == 0) ierror = ndarray_create(nd_dtt1,for_var%dtt1)
        if (ierror == 0) ierror = py_var%setitem('dtt1',nd_dtt1)
        if (ierror == 0) ierror = ndarray_create(nd_dtt2,for_var%dtt2)
        if (ierror == 0) ierror = py_var%setitem('dtt2',nd_dtt2)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_job_info_type: Wrapping failed'
        end if

    End Subroutine Wrap_job_info_type

    Module Subroutine list_to_array_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_blockinfo_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_blockinfo_type

    Module Subroutine list_to_array_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_blockinfo_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_blockinfo_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_blockinfo_type_no_alloc

    Module Subroutine list_to_array2d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_blockinfo_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_blockinfo_type

    Module Subroutine list_to_array2d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(blockinfo_type), dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array2d_blockinfo_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (n /= size(arr,2)) then
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array2d_blockinfo_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_blockinfo_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_blockinfo_type_no_alloc

    Module Subroutine Unwrap_blockinfo_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(blockinfo_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_blockinfo_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'blockinfo_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_blockinfo_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','strname',py_var,for_var%strname,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','blname',py_var,for_var%blname,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','ibl',py_var,for_var%ibl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','nl',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_blockinfo_type','nl',p_int_1d,for_var%nl,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_blockinfo_type','iex',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_blockinfo_type','iex',p_int_1d,for_var%iex,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_blockinfo_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_blockinfo_type

    Module Subroutine Wrap_blockinfo_type(for_var,py_var,ierror)

        ! Arguments
        type(blockinfo_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_nl,nd_iex

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('strname',for_var%strname)
        if (ierror == 0) ierror = py_var%setitem('blname',for_var%blname)
        if (ierror == 0) ierror = py_var%setitem('ibl',for_var%ibl)
        if (ierror == 0) ierror = ndarray_create(nd_nl,for_var%nl)
        if (ierror == 0) ierror = py_var%setitem('nl',nd_nl)
        if (ierror == 0) ierror = ndarray_create(nd_iex,for_var%iex)
        if (ierror == 0) ierror = py_var%setitem('iex',nd_iex)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_blockinfo_type: Wrapping failed'
        end if

    End Subroutine Wrap_blockinfo_type

    Module Subroutine list_to_array_genvec_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_genvec_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_genvec_type

    Module Subroutine list_to_array_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_genvec_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_genvec_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_genvec_type_no_alloc

    Module Subroutine list_to_array2d_genvec_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_genvec_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_genvec_type

    Module Subroutine list_to_array2d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(genvec_type), dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(dict) :: my_dict
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array2d_genvec_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. m > 0) then
            if (ierror == 0) ierror = my_list%getitem(item,0)
            if (ierror == 0) ierror = cast(li,item)
            if (ierror == 0) ierror = li%len(n)
            if (ierror == 0) then
                do i = 0 , m-1
                    if (ierror == 0) ierror = my_list%getitem(item,i)
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0) ierror = li%len(n)
                    if (n /= size(arr,2)) then
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array2d_genvec_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_genvec_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_genvec_type_no_alloc

    Module Subroutine Unwrap_genvec_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(genvec_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_genvec_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'genvec_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_genvec_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','mystr',py_var,for_var%mystr,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','npar',py_var,for_var%npar,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','iv',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_genvec_type','iv',p_int_1d,for_var%iv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','rv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_genvec_type','rv',p_real_1d,for_var%rv,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_genvec_type','cv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array_no_alloc('Unwrap_genvec_type','cv',my_list,for_var%cv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_genvec_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_genvec_type

    Module Subroutine Wrap_genvec_type(for_var,py_var,ierror)

        ! Arguments
        type(genvec_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_cv
        type(ndarray) :: nd_iv,nd_rv

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('mystr',for_var%mystr)
        if (ierror == 0) ierror = py_var%setitem('npar',for_var%npar)
        if (ierror == 0) ierror = ndarray_create(nd_iv,for_var%iv)
        if (ierror == 0) ierror = py_var%setitem('iv',nd_iv)
        if (ierror == 0) ierror = ndarray_create(nd_rv,for_var%rv)
        if (ierror == 0) ierror = py_var%setitem('rv',nd_rv)
        if (ierror == 0) ierror = list_create(li_cv)
        if (ierror == 0) then
            do i = 1 , size(for_var%cv)
                if (ierror == 0) ierror = li_cv%append(for_var%cv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('cv',li_cv)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_genvec_type: Wrapping failed'
        end if

    End Subroutine Wrap_genvec_type

end submodule