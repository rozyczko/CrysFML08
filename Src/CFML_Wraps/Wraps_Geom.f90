submodule (CFML_Wraps) Wraps_Geom

    implicit none
    contains

    Module Subroutine list_to_array_coordination_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_coordination_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_coordination_type

    Module Subroutine list_to_array_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_coordination_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_coordination_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_coordination_type_no_alloc

    Module Subroutine list_to_array2d_coordination_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_coordination_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_coordination_type

    Module Subroutine list_to_array2d_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(coordination_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_coordination_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_coordination_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_coordination_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_coordination_type_no_alloc

    Module Subroutine Unwrap_coordination_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(coordination_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        integer, dimension(:,:), pointer :: p_int_2d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_coordination_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'coordination_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_coordination_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','max_coor',py_var,for_var%max_coor,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','coord_num',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','coord_num',p_int_1d,for_var%coord_num,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','n_cooatm',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','n_cooatm',p_int_2d,for_var%n_cooatm,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','n_sym',py_var,p_int_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','n_sym',p_int_2d,for_var%n_sym,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','dist',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','dist',p_real_2d,for_var%dist,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','s_dist',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','s_dist',p_real_2d,for_var%s_dist,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_coordination_type','tr_coo',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_coordination_type','tr_coo',p_real_3d,for_var%tr_coo,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_coordination_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_coordination_type

    Module Subroutine Wrap_coordination_type(for_var,py_var,ierror)

        ! Arguments
        type(coordination_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_coord_num,nd_n_cooatm,nd_n_sym,nd_dist,nd_s_dist,nd_tr_coo

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('max_coor',for_var%max_coor)
        if (ierror == 0) ierror = ndarray_create(nd_coord_num,for_var%coord_num)
        if (ierror == 0) ierror = py_var%setitem('coord_num',nd_coord_num)
        if (ierror == 0) ierror = ndarray_create(nd_n_cooatm,for_var%n_cooatm)
        if (ierror == 0) ierror = py_var%setitem('n_cooatm',nd_n_cooatm)
        if (ierror == 0) ierror = ndarray_create(nd_n_sym,for_var%n_sym)
        if (ierror == 0) ierror = py_var%setitem('n_sym',nd_n_sym)
        if (ierror == 0) ierror = ndarray_create(nd_dist,for_var%dist)
        if (ierror == 0) ierror = py_var%setitem('dist',nd_dist)
        if (ierror == 0) ierror = ndarray_create(nd_s_dist,for_var%s_dist)
        if (ierror == 0) ierror = py_var%setitem('s_dist',nd_s_dist)
        if (ierror == 0) ierror = ndarray_create(nd_tr_coo,for_var%tr_coo)
        if (ierror == 0) ierror = py_var%setitem('tr_coo',nd_tr_coo)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_coordination_type: Wrapping failed'
        end if

    End Subroutine Wrap_coordination_type

    Module Subroutine list_to_array_point_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_point_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_point_list_type

    Module Subroutine list_to_array_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_point_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_point_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_point_list_type_no_alloc

    Module Subroutine list_to_array2d_point_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_point_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_point_list_type

    Module Subroutine list_to_array2d_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(point_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_point_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_point_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_point_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_point_list_type_no_alloc

    Module Subroutine Unwrap_point_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(point_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_point_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'point_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_point_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_list_type','np',py_var,for_var%np,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_list_type','nam',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_point_list_type','nam',my_list,for_var%nam,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_list_type','p',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_list_type','p',p_int_1d,for_var%p,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_point_list_type','x',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_point_list_type','x',p_real_2d,for_var%x,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_point_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_point_list_type

    Module Subroutine Wrap_point_list_type(for_var,py_var,ierror)

        ! Arguments
        type(point_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_nam
        type(ndarray) :: nd_p,nd_x

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('np',for_var%np)
        if (ierror == 0) ierror = list_create(li_nam)
        if (ierror == 0) then
            do i = 1 , size(for_var%nam)
                if (ierror == 0) ierror = li_nam%append(for_var%nam(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('nam',li_nam)
        if (ierror == 0) ierror = ndarray_create(nd_p,for_var%p)
        if (ierror == 0) ierror = py_var%setitem('p',nd_p)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = py_var%setitem('x',nd_x)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_point_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_point_list_type

end submodule