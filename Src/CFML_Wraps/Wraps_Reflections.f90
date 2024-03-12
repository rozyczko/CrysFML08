submodule (CFML_Wraps) Wraps_Reflections

    implicit none
    contains

    Module Subroutine list_to_array_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_refl_type

    Module Subroutine list_to_array_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_refl_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_refl_type_no_alloc

    Module Subroutine list_to_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_refl_type

    Module Subroutine list_to_array2d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(refl_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_refl_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_refl_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_refl_type_no_alloc

    Module Subroutine Unwrap_refl_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refl_type), allocatable, intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_refl_type: Cannot determine fortran type'
        else
            if (fortran_type == 'refl_type') then
                allocate(refl_type :: for_var)
            else if (fortran_type == 'srefl_type') then
                allocate(srefl_type :: for_var)
            else if (fortran_type == 'mrefl_type') then
                allocate(mrefl_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refl_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','iph',py_var,A%iph,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,A%w,ierror)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array_no_alloc('Unwrap_mrefl_type','msf',my_list,A%msf,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array_no_alloc('Unwrap_mrefl_type','miv',my_list,A%miv,ierror)
                    if (ierror == 0) call my_list%destroy
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_refl_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_refl_type

    Module Subroutine Unwrap_refl_type_no_alloc(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        class(refl_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Cannot determine fortran type'
        else
            if (fortran_type /= 'refl_type' &
                .and. fortran_type /= 'srefl_type' &
                .and. fortran_type /= 'mrefl_type' &
                ) then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','h',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_refl_type','h',p_int_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','s',py_var,for_var%s,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_refl_type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','iph',py_var,A%iph,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fo',py_var,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','fc',py_var,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','sfo',py_var,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','phase',py_var,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','a',py_var,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','b',py_var,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_srefl_type','w',py_var,A%w,ierror)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','mivo',py_var,A%mivo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','smivo',py_var,A%smivo,ierror)
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','msf',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array_no_alloc('Unwrap_mrefl_type','msf',my_list,A%msf,ierror)
                    if (ierror == 0) call my_list%destroy
                    if (ierror == 0) ierror = list_create(my_list)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_mrefl_type','miv',py_var,my_list,ierror)
                    if (ierror == 0) call list_to_array_no_alloc('Unwrap_mrefl_type','miv',my_list,A%miv,ierror)
                    if (ierror == 0) call my_list%destroy
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_refl_type_no_alloc: Unwrapping failed'
        end if

    End Subroutine Unwrap_refl_type_no_alloc

    Module Subroutine list_to_array_refl_type_class(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        class(refl_type), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: fortran_type
        type(refl_type) :: src1
        type(srefl_type) :: src2
        type(mrefl_type) :: src3
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) then
                    ierror = my_dict%getitem(fortran_type,'fortran_type')
                    if (ierror /= 0) then
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array_refl_type_class: Cannot determine fortran type'
                    else if (fortran_type == 'refl_type') then
                        allocate(arr(n),source=src1)
                    else if (fortran_type == 'srefl_type') then
                        allocate(arr(n),source=src2)
                    else if (fortran_type == 'mrefl_type') then
                        allocate(arr(n),source=src3)
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'list_to_array_refl_type_class: Wrong fortran type'
                    end if
                end if
                if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_refl_type_class

    Module Subroutine Wrap_refl_type(for_var,py_var,ierror)

        ! Arguments
        class(refl_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_msf,li_miv
        type(ndarray) :: nd_h

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = py_var%setitem('imag',for_var%imag)
        if (ierror == 0) ierror = py_var%setitem('pcoeff',for_var%pcoeff)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) ierror = py_var%setitem('iph',A%iph)
                    if (ierror == 0) ierror = py_var%setitem('fo',A%fo)
                    if (ierror == 0) ierror = py_var%setitem('fc',A%fc)
                    if (ierror == 0) ierror = py_var%setitem('sfo',A%sfo)
                    if (ierror == 0) ierror = py_var%setitem('phase',A%phase)
                    if (ierror == 0) ierror = py_var%setitem('a',A%a)
                    if (ierror == 0) ierror = py_var%setitem('b',A%b)
                    if (ierror == 0) ierror = py_var%setitem('w',A%w)
            end select
            select type (A => for_var)
                class is (mrefl_type)
                    if (ierror == 0) ierror = py_var%setitem('mivo',A%mivo)
                    if (ierror == 0) ierror = py_var%setitem('smivo',A%smivo)
                    if (ierror == 0) ierror = list_create(li_msf)
                    if (ierror == 0) then
                        do i = 1 , size(A%msf)
                            if (ierror == 0) ierror = li_msf%append(A%msf(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('msf',li_msf)
                    if (ierror == 0) ierror = list_create(li_miv)
                    if (ierror == 0) then
                        do i = 1 , size(A%miv)
                            if (ierror == 0) ierror = li_miv%append(A%miv(i))
                        end do
                    end if
                    if (ierror == 0) ierror = py_var%setitem('miv',li_miv)
            end select
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_refl_type: Wrapping failed'
        end if

    End Subroutine Wrap_refl_type

    Module Subroutine list_to_array_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_srefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_srefl_type

    Module Subroutine list_to_array_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_srefl_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_srefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_srefl_type_no_alloc

    Module Subroutine list_to_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_srefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_srefl_type

    Module Subroutine list_to_array2d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(srefl_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_srefl_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_srefl_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_srefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_srefl_type_no_alloc

    Module Subroutine list_to_array_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_mrefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_mrefl_type

    Module Subroutine list_to_array_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_mrefl_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_mrefl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_mrefl_type_no_alloc

    Module Subroutine list_to_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_mrefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_mrefl_type

    Module Subroutine list_to_array2d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(mrefl_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_mrefl_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_mrefl_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_mrefl_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_mrefl_type_no_alloc

    Module Subroutine list_to_array_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_reflist_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_reflist_type

    Module Subroutine list_to_array_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_reflist_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_reflist_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_reflist_type_no_alloc

    Module Subroutine list_to_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_reflist_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_reflist_type

    Module Subroutine list_to_array2d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(reflist_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_reflist_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_reflist_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_reflist_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_reflist_type_no_alloc

    Module Subroutine Unwrap_reflist_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(reflist_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_reflist_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'reflist_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_reflist_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_reflist_type','ref',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array_class('Unwrap_reflist_type','ref',my_list,for_var%ref,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_reflist_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_reflist_type

    Module Subroutine Wrap_reflist_type(for_var,py_var,ierror)

        ! Arguments
        type(reflist_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_ref
        type(dict), dimension(:), allocatable :: di_ref

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_ref)
        if (ierror == 0) allocate(di_ref(size(for_var%ref)))
        if (ierror == 0) then
            do i = 1 , size(for_var%ref)
                ierror = dict_create(di_ref(i))
                if (ierror == 0) call wrap_refl_type(for_var%ref(i),di_ref(i),ierror)
                if (ierror == 0) ierror = li_ref%append(di_ref(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('ref',li_ref)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_reflist_type: Wrapping failed'
        end if

    End Subroutine Wrap_reflist_type

end submodule