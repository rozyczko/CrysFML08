submodule (CFML_Wraps) Wraps_kvec_Polarimetry

    implicit none
    contains

    Module Subroutine list_to_array_polar_calc_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calc_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_type

    Module Subroutine list_to_array_polar_calc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calc_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calc_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_type_no_alloc

    Module Subroutine list_to_array2d_polar_calc_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calc_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_type

    Module Subroutine list_to_array2d_polar_calc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calc_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calc_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calc_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_type_no_alloc

    Module Subroutine Unwrap_polar_calc_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calc_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        real, dimension(:,:,:), pointer :: p_real_3d
        character(len=1) :: order
        type(list) :: my_list
        type(dict) :: di_cell

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calc_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calc_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calc_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','spv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','spv',p_real_1d,for_var%spv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','cell',py_var,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_type(di_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','p',py_var,for_var%p,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','miv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array_no_alloc('Unwrap_polar_calc_type','miv',my_list,for_var%miv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','nsf',py_var,for_var%nsf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','nc',py_var,for_var%nc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','my',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','my',p_real_2d,for_var%my,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','mz',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','mz',p_real_2d,for_var%mz,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','ry',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','ry',p_real_2d,for_var%ry,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','rz',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','rz',p_real_2d,for_var%rz,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','iy',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','iy',p_real_2d,for_var%iy,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','iz',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','iz',p_real_2d,for_var%iz,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','tc',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','tc',p_real_2d,for_var%tc,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','mm',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','mm',p_real_2d,for_var%mm,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','cs',py_var,p_real_3d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','cs',p_real_3d,for_var%cs,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_type','pij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_type','pij',p_real_2d,for_var%pij,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calc_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calc_type

    Module Subroutine Wrap_polar_calc_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calc_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i,j
        type(list) :: li_miv,li
        type(dict) :: di_cell
        type(ndarray) :: nd_h,nd_spv,nd_my,nd_mz,nd_ry,nd_rz,nd_iy,nd_iz,nd_tc,nd_mm,nd_cs,nd_pij

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = ndarray_create(nd_spv,for_var%spv)
        if (ierror == 0) ierror = py_var%setitem('spv',nd_spv)
        if (ierror == 0) call wrap_cell_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) ierror = py_var%setitem('p',for_var%p)
        if (ierror == 0) ierror = py_var%setitem('nsf',for_var%nsf)
        if (ierror == 0) ierror = py_var%setitem('nc',for_var%nc)
        if (ierror == 0) ierror = ndarray_create(nd_my,for_var%my)
        if (ierror == 0) ierror = py_var%setitem('my',nd_my)
        if (ierror == 0) ierror = ndarray_create(nd_mz,for_var%mz)
        if (ierror == 0) ierror = py_var%setitem('mz',nd_mz)
        if (ierror == 0) ierror = ndarray_create(nd_ry,for_var%ry)
        if (ierror == 0) ierror = py_var%setitem('ry',nd_ry)
        if (ierror == 0) ierror = ndarray_create(nd_rz,for_var%rz)
        if (ierror == 0) ierror = py_var%setitem('rz',nd_rz)
        if (ierror == 0) ierror = ndarray_create(nd_iy,for_var%iy)
        if (ierror == 0) ierror = py_var%setitem('iy',nd_iy)
        if (ierror == 0) ierror = ndarray_create(nd_iz,for_var%iz)
        if (ierror == 0) ierror = py_var%setitem('iz',nd_iz)
        if (ierror == 0) ierror = ndarray_create(nd_tc,for_var%tc)
        if (ierror == 0) ierror = py_var%setitem('tc',nd_tc)
        if (ierror == 0) ierror = ndarray_create(nd_mm,for_var%mm)
        if (ierror == 0) ierror = py_var%setitem('mm',nd_mm)
        if (ierror == 0) ierror = ndarray_create(nd_cs,for_var%cs)
        if (ierror == 0) ierror = py_var%setitem('cs',nd_cs)
        if (ierror == 0) ierror = ndarray_create(nd_pij,for_var%pij)
        if (ierror == 0) ierror = py_var%setitem('pij',nd_pij)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calc_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calc_type

    Module Subroutine list_to_array_polar_calc_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calc_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_list_type

    Module Subroutine list_to_array_polar_calc_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calc_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calc_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_calc_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calc_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_list_type

    Module Subroutine list_to_array2d_polar_calc_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calc_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calc_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calc_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_list_type_no_alloc

    Module Subroutine Unwrap_polar_calc_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calc_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calc_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calc_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calc_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_list_type','polari',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_calc_list_type','polari',my_list,for_var%polari,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calc_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calc_list_type

    Module Subroutine Wrap_polar_calc_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calc_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polari
        type(dict), dimension(:), allocatable :: di_polari

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_polari)
        if (ierror == 0) allocate(di_polari(size(for_var%polari)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polari)
                ierror = dict_create(di_polari(i))
                if (ierror == 0) call wrap_polar_calc_type(for_var%polari(i),di_polari(i),ierror)
                if (ierror == 0) ierror = li_polari%append(di_polari(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polari',li_polari)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calc_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calc_list_type

    Module Subroutine list_to_array_polar_calcmulti_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calcmulti_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calcmulti_list_type

    Module Subroutine list_to_array_polar_calcmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calcmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calcmulti_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calcmulti_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_calcmulti_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calcmulti_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calcmulti_list_type

    Module Subroutine list_to_array2d_polar_calcmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calcmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calcmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calcmulti_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calcmulti_list_type_no_alloc

    Module Subroutine Unwrap_polar_calcmulti_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calcmulti_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calcmulti_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calcmulti_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calcmulti_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calcmulti_list_type','nset',py_var,for_var%nset,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calcmulti_list_type','polarilist',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_calcmulti_list_type','polarilist',my_list,for_var%polarilist,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calcmulti_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calcmulti_list_type

    Module Subroutine Wrap_polar_calcmulti_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calcmulti_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polarilist
        type(dict), dimension(:), allocatable :: di_polarilist

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nset',for_var%nset)
        if (ierror == 0) ierror = list_create(li_polarilist)
        if (ierror == 0) allocate(di_polarilist(size(for_var%polarilist)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polarilist)
                ierror = dict_create(di_polarilist(i))
                if (ierror == 0) call wrap_polar_calc_list_type(for_var%polarilist(i),di_polarilist(i),ierror)
                if (ierror == 0) ierror = li_polarilist%append(di_polarilist(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polarilist',li_polarilist)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calcmulti_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calcmulti_list_type

    Module Subroutine list_to_array_polar_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_info_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_info_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_info_type

    Module Subroutine list_to_array_polar_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_info_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_info_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_info_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_info_type_no_alloc

    Module Subroutine list_to_array2d_polar_info_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_info_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_info_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_info_type

    Module Subroutine list_to_array2d_polar_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_info_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_info_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_info_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_info_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_info_type_no_alloc

    Module Subroutine Unwrap_polar_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_info_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(list) :: my_list
        type(dict) :: di_cell

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_info_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_info_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_info_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','spv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_info_type','spv',p_real_1d,for_var%spv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','cell',py_var,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_type(di_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','p',py_var,for_var%p,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','miv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array_no_alloc('Unwrap_polar_info_type','miv',my_list,for_var%miv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','nsf',py_var,for_var%nsf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','nc',py_var,for_var%nc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','my',py_var,for_var%my,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','mz',py_var,for_var%mz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','ry',py_var,for_var%ry,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','rz',py_var,for_var%rz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','iy',py_var,for_var%iy,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','iz',py_var,for_var%iz,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','tc',py_var,for_var%tc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','mm',py_var,for_var%mm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','cs',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_info_type','cs',p_real_1d,for_var%cs,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_info_type','pij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_info_type','pij',p_real_2d,for_var%pij,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_info_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_info_type

    Module Subroutine Wrap_polar_info_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_info_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_miv
        type(dict) :: di_cell
        type(ndarray) :: nd_h,nd_spv,nd_cs,nd_pij

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = ndarray_create(nd_spv,for_var%spv)
        if (ierror == 0) ierror = py_var%setitem('spv',nd_spv)
        if (ierror == 0) call wrap_cell_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) ierror = py_var%setitem('p',for_var%p)
        if (ierror == 0) ierror = list_create(li_miv)
        if (ierror == 0) then
            do i = 1 , size(for_var%miv)
                if (ierror == 0) ierror = li_miv%append(for_var%miv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('miv',li_miv)
        if (ierror == 0) ierror = py_var%setitem('nsf',for_var%nsf)
        if (ierror == 0) ierror = py_var%setitem('nc',for_var%nc)
        if (ierror == 0) ierror = py_var%setitem('my',for_var%my)
        if (ierror == 0) ierror = py_var%setitem('mz',for_var%mz)
        if (ierror == 0) ierror = py_var%setitem('ry',for_var%ry)
        if (ierror == 0) ierror = py_var%setitem('rz',for_var%rz)
        if (ierror == 0) ierror = py_var%setitem('iy',for_var%iy)
        if (ierror == 0) ierror = py_var%setitem('iz',for_var%iz)
        if (ierror == 0) ierror = py_var%setitem('tc',for_var%tc)
        if (ierror == 0) ierror = py_var%setitem('mm',for_var%mm)
        if (ierror == 0) ierror = ndarray_create(nd_cs,for_var%cs)
        if (ierror == 0) ierror = py_var%setitem('cs',nd_cs)
        if (ierror == 0) ierror = ndarray_create(nd_pij,for_var%pij)
        if (ierror == 0) ierror = py_var%setitem('pij',nd_pij)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_info_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_info_type

    Module Subroutine list_to_array_polar_obs_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_obs_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obs_type

    Module Subroutine list_to_array_polar_obs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_obs_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_obs_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obs_type_no_alloc

    Module Subroutine list_to_array2d_polar_obs_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_obs_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obs_type

    Module Subroutine list_to_array2d_polar_obs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_obs_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_obs_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_obs_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obs_type_no_alloc

    Module Subroutine Unwrap_polar_obs_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_obs_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_obs_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_obs_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_obs_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_obs_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','spv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_obs_type','spv',p_real_1d,for_var%spv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','p',py_var,for_var%p,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','opij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_obs_type','opij',p_real_2d,for_var%opij,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','sopij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_obs_type','sopij',p_real_2d,for_var%sopij,ierror,order)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_type','wopij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_obs_type','wopij',p_real_2d,for_var%wopij,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_obs_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_obs_type

    Module Subroutine Wrap_polar_obs_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_obs_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_h,nd_spv,nd_opij,nd_sopij,nd_wopij

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = ndarray_create(nd_spv,for_var%spv)
        if (ierror == 0) ierror = py_var%setitem('spv',nd_spv)
        if (ierror == 0) ierror = py_var%setitem('p',for_var%p)
        if (ierror == 0) ierror = ndarray_create(nd_opij,for_var%opij)
        if (ierror == 0) ierror = py_var%setitem('opij',nd_opij)
        if (ierror == 0) ierror = ndarray_create(nd_sopij,for_var%sopij)
        if (ierror == 0) ierror = py_var%setitem('sopij',nd_sopij)
        if (ierror == 0) ierror = ndarray_create(nd_wopij,for_var%wopij)
        if (ierror == 0) ierror = py_var%setitem('wopij',nd_wopij)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_obs_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_obs_type

    Module Subroutine list_to_array_polar_obs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_obs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obs_list_type

    Module Subroutine list_to_array_polar_obs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_obs_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_obs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obs_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_obs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_obs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obs_list_type

    Module Subroutine list_to_array2d_polar_obs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obs_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_obs_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_obs_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_obs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obs_list_type_no_alloc

    Module Subroutine Unwrap_polar_obs_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_obs_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_obs_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_obs_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_obs_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obs_list_type','polaro',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_obs_list_type','polaro',my_list,for_var%polaro,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_obs_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_obs_list_type

    Module Subroutine Wrap_polar_obs_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_obs_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polaro
        type(dict), dimension(:), allocatable :: di_polaro

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_polaro)
        if (ierror == 0) allocate(di_polaro(size(for_var%polaro)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polaro)
                ierror = dict_create(di_polaro(i))
                if (ierror == 0) call wrap_polar_obs_type(for_var%polaro(i),di_polaro(i),ierror)
                if (ierror == 0) ierror = li_polaro%append(di_polaro(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polaro',li_polaro)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_obs_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_obs_list_type

    Module Subroutine list_to_array_polar_obsmulti_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obsmulti_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_obsmulti_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obsmulti_list_type

    Module Subroutine list_to_array_polar_obsmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obsmulti_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_obsmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_obsmulti_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_obsmulti_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_obsmulti_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obsmulti_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_obsmulti_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obsmulti_list_type

    Module Subroutine list_to_array2d_polar_obsmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_obsmulti_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_obsmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_obsmulti_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_obsmulti_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_obsmulti_list_type_no_alloc

    Module Subroutine Unwrap_polar_obsmulti_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_obsmulti_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_obsmulti_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_obsmulti_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_obsmulti_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obsmulti_list_type','nset',py_var,for_var%nset,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_obsmulti_list_type','polarolist',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_obsmulti_list_type','polarolist',my_list,for_var%polarolist,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_obsmulti_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_obsmulti_list_type

    Module Subroutine Wrap_polar_obsmulti_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_obsmulti_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polarolist
        type(dict), dimension(:), allocatable :: di_polarolist

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nset',for_var%nset)
        if (ierror == 0) ierror = list_create(li_polarolist)
        if (ierror == 0) allocate(di_polarolist(size(for_var%polarolist)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polarolist)
                ierror = dict_create(di_polarolist(i))
                if (ierror == 0) call wrap_polar_obs_list_type(for_var%polarolist(i),di_polarolist(i),ierror)
                if (ierror == 0) ierror = li_polarolist%append(di_polarolist(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polarolist',li_polarolist)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_obsmulti_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_obsmulti_list_type

    Module Subroutine list_to_array_polar_calc_svs_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calc_svs_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_svs_type

    Module Subroutine list_to_array_polar_calc_svs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calc_svs_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calc_svs_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_svs_type_no_alloc

    Module Subroutine list_to_array2d_polar_calc_svs_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calc_svs_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_svs_type

    Module Subroutine list_to_array2d_polar_calc_svs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calc_svs_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calc_svs_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calc_svs_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_svs_type_no_alloc

    Module Subroutine Unwrap_polar_calc_svs_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calc_svs_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        real, dimension(:,:), pointer :: p_real_2d
        character(len=1) :: order
        type(dict) :: di_cell

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calc_svs_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calc_svs_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calc_svs_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_type','h',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_svs_type','h',p_real_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_type','spv',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_svs_type','spv',p_real_1d,for_var%spv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_type','cell',py_var,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_type(di_cell,for_var%cell,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_type','p',py_var,for_var%p,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_type','pij',py_var,p_real_2d,ierror,order)
        if (ierror == 0) call pointer_to_array('Unwrap_polar_calc_svs_type','pij',p_real_2d,for_var%pij,ierror,order)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calc_svs_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calc_svs_type

    Module Subroutine Wrap_polar_calc_svs_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calc_svs_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(dict) :: di_cell
        type(ndarray) :: nd_h,nd_spv,nd_pij

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = ndarray_create(nd_spv,for_var%spv)
        if (ierror == 0) ierror = py_var%setitem('spv',nd_spv)
        if (ierror == 0) call wrap_cell_type(for_var%cell,di_cell,ierror)
        if (ierror == 0) ierror = py_var%setitem('cell',di_cell)
        if (ierror == 0) ierror = py_var%setitem('p',for_var%p)
        if (ierror == 0) ierror = ndarray_create(nd_pij,for_var%pij)
        if (ierror == 0) ierror = py_var%setitem('pij',nd_pij)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calc_svs_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calc_svs_type

    Module Subroutine list_to_array_polar_calc_svs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calc_svs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_svs_list_type

    Module Subroutine list_to_array_polar_calc_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calc_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calc_svs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calc_svs_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_calc_svs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calc_svs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_svs_list_type

    Module Subroutine list_to_array2d_polar_calc_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calc_svs_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calc_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calc_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calc_svs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calc_svs_list_type_no_alloc

    Module Subroutine Unwrap_polar_calc_svs_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calc_svs_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calc_svs_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calc_svs_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calc_svs_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_list_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calc_svs_list_type','polarisvs',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_calc_svs_list_type','polarisvs',my_list,for_var%polarisvs,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calc_svs_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calc_svs_list_type

    Module Subroutine Wrap_polar_calc_svs_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calc_svs_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polarisvs
        type(dict), dimension(:), allocatable :: di_polarisvs

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_polarisvs)
        if (ierror == 0) allocate(di_polarisvs(size(for_var%polarisvs)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polarisvs)
                ierror = dict_create(di_polarisvs(i))
                if (ierror == 0) call wrap_polar_calc_svs_type(for_var%polarisvs(i),di_polarisvs(i),ierror)
                if (ierror == 0) ierror = li_polarisvs%append(di_polarisvs(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polarisvs',li_polarisvs)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calc_svs_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calc_svs_list_type

    Module Subroutine list_to_array_polar_calcmulti_svs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_svs_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_polar_calcmulti_svs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calcmulti_svs_list_type

    Module Subroutine list_to_array_polar_calcmulti_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_svs_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_polar_calcmulti_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_polar_calcmulti_svs_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_polar_calcmulti_svs_list_type_no_alloc

    Module Subroutine list_to_array2d_polar_calcmulti_svs_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_svs_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_polar_calcmulti_svs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calcmulti_svs_list_type

    Module Subroutine list_to_array2d_polar_calcmulti_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(polar_calcmulti_svs_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_polar_calcmulti_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_polar_calcmulti_svs_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_polar_calcmulti_svs_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_polar_calcmulti_svs_list_type_no_alloc

    Module Subroutine Unwrap_polar_calcmulti_svs_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(polar_calcmulti_svs_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_polar_calcmulti_svs_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'polar_calcmulti_svs_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_polar_calcmulti_svs_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calcmulti_svs_list_type','nset',py_var,for_var%nset,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_polar_calcmulti_svs_list_type','polarisvslist',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_polar_calcmulti_svs_list_type','polarisvslist',my_list,for_var%polarisvslist,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_polar_calcmulti_svs_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_polar_calcmulti_svs_list_type

    Module Subroutine Wrap_polar_calcmulti_svs_list_type(for_var,py_var,ierror)

        ! Arguments
        type(polar_calcmulti_svs_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_polarisvslist
        type(dict), dimension(:), allocatable :: di_polarisvslist

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nset',for_var%nset)
        if (ierror == 0) ierror = list_create(li_polarisvslist)
        if (ierror == 0) allocate(di_polarisvslist(size(for_var%polarisvslist)))
        if (ierror == 0) then
            do i = 1 , size(for_var%polarisvslist)
                ierror = dict_create(di_polarisvslist(i))
                if (ierror == 0) call wrap_polar_calc_svs_list_type(for_var%polarisvslist(i),di_polarisvslist(i),ierror)
                if (ierror == 0) ierror = li_polarisvslist%append(di_polarisvslist(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('polarisvslist',li_polarisvslist)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_polar_calcmulti_svs_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_polar_calcmulti_svs_list_type

end submodule