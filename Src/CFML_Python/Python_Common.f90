!!----
!!----
!!----
SubModule (CFML_Python)  Unwrap

implicit none
    Contains

    Module Subroutine Array_To_List_Str(procedure_name,var_name,arr,my_list,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        character(len=*), dimension(:),                  intent(in)    :: arr
        type(list),                                      intent(inout) :: my_list
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i

        ierror = 0
        do i = 1 , size(arr)
            if (ierror == 0) then
                ierror = my_list%append(adjustl(trim(arr(i))))
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error building list '//var_name
                return
            end if
        end do

    End Subroutine Array_To_List_Str

    Module Subroutine Get_Dict_from_Item(procedure_name,var_name,item,di,ierror)
        !---- Arguments ----!
        character(len=*), intent(in)    :: procedure_name
        character(len=*), intent(in)    :: var_name
        type(object),     intent(inout) :: item
        type(dict),       intent(inout) :: di
        integer,          intent(inout) :: ierror

        ierror = 0
        if (.not. is_dict(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be a dictionary'
        else
            ierror = cast(di,item)
        end if
    End Subroutine Get_Dict_from_Item

    Module Subroutine Get_Int32_from_Item(procedure_name,var_name,item,var,ierror)
        !---- Arguments ----!
        character(len=*), intent(in)    :: procedure_name
        character(len=*), intent(in)    :: var_name
        type(object),     intent(inout) :: item
        integer,          intent(out)   :: var
        integer,          intent(inout) :: ierror

        ierror = 0
        if (.not. is_int(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be a number'
        else
            ierror = cast(var,item)
        end if
    End Subroutine Get_Int32_from_Item

    Module Subroutine Get_Logical_from_Item(procedure_name,var_name,item,var,ierror)
        !---- Arguments ----!
        character(len=*), intent(in)    :: procedure_name
        character(len=*), intent(in)    :: var_name
        type(object),     intent(inout) :: item
        logical,          intent(out)   :: var
        integer,          intent(inout) :: ierror

        ierror = 0
        if (.not. is_bool(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be boolean'
        else
            ierror = cast(var,item)
        end if
    End Subroutine Get_Logical_from_Item

    Module Subroutine Get_Ndarray_from_Item(procedure_name,var_name,item,nd,ierror)
        !---- Arguments ----!
        character(len=*), intent(in)    :: procedure_name
        character(len=*), intent(in)    :: var_name
        type(object),     intent(inout) :: item
        type(ndarray),    intent(inout) :: nd
        integer,          intent(inout) :: ierror

        ierror = 0
        if (.not. is_ndarray(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be a ndarray'
        else
            ierror = cast(nd,item)
        end if

     End Subroutine Get_Ndarray_from_Item

    Module Subroutine Get_Real32_from_Item(procedure_name,var_name,item,var,ierror)
        !---- Arguments ----!
        character(len=*), intent(in)    :: procedure_name
        character(len=*), intent(in)    :: var_name
        type(object),     intent(inout) :: item
        real,             intent(out)   :: var
        integer,          intent(inout) :: ierror

        ierror = 0
        if (.not. is_float(item) .and. .not. is_int(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be a number'
        else
            ierror = cast(var,item)
        end if
    End Subroutine Get_Real32_from_Item

    Module Subroutine Get_String_from_Item(procedure_name,var_name,item,var,ierror)
        !---- Arguments ----!
        character(len=*),              intent(in)    :: procedure_name
        character(len=*),              intent(in)    :: var_name
        type(object),                  intent(inout) :: item
        character(len=:), allocatable, intent(out)   :: var
        integer,                       intent(inout) :: ierror

        ierror = 0
        if (.not. is_str(item)) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': '//var_name//' must be a string'
        else
            ierror = cast(var,item)
        end if
    End Subroutine Get_String_from_Item

    Module Subroutine List_To_Array_Atm_Type(procedure_name,var_name,my_list,arr,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        type(list),                                      intent(inout) :: my_list
        class(atm_type), dimension(:), allocatable,      intent(out)   :: arr
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        logical :: is_alloc
        character(len=:), allocatable :: fortran_type
        type(atm_type) :: src1
        type(atm_std_type) :: src2
        type(modatm_std_type) :: src3
        type(atm_ref_type) :: src4
        type(modatm_ref_type) :: src5
        type(object) :: item
        type(dict) :: my_dict

        is_alloc = .false.
        ierror = my_list%len(n)
        if (n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0 .and. .not. is_alloc) then
                    ierror = my_dict%getitem(fortran_type,"fortran_type")
                    if (ierror /= 0) then
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_To_Array_Atm_Type: Cannot determine fortran type'
                    else if (fortran_type == 'atm_type') then
                        allocate(arr(n),source=src1)
                        is_alloc = .true.
                    else if (fortran_type == 'atm_std_type') then
                        allocate(arr(n),source=src2)
                        is_alloc = .true.
                    else if (fortran_type == 'modatm_std_type') then
                        allocate(arr(n),source=src3)
                        is_alloc = .true.
                    else if (fortran_type == 'atm_ref_type') then
                        allocate(arr(n),source=src4)
                        is_alloc = .true.
                    else if (fortran_type == 'modatm_ref_type') then
                        allocate(arr(n),source=src5)
                        is_alloc = .true.
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_To_Array_Atm_Type: Wrong fortran type'
                    end if
                end if
                if (ierror == 0) call unwrap_atm_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if
    End Subroutine List_To_Array_Atm_Type

    Module Subroutine List_To_Array_Logical(procedure_name,var_name,my_list,arr,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        type(list),                                      intent(inout) :: my_list
        logical, dimension(:), allocatable,              intent(out)   :: arr
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item

        ierror = my_list%len(n)
        if (n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(arr(i+1),item)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if
    End Subroutine List_To_Array_Logical

    Module Subroutine List_To_Array_Refl_Type(procedure_name,var_name,my_list,arr,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        type(list),                                      intent(inout) :: my_list
        class(refl_type),     dimension(:), allocatable, intent(out)   :: arr
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        logical :: is_alloc
        character(len=:), allocatable :: fortran_type
        type(refl_type) :: src1
        type(srefl_type) :: src2
        type(object) :: item
        type(dict) :: my_dict

        is_alloc = .false.
        ierror = my_list%len(n)
        if (n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0 .and. .not. is_alloc) then
                    ierror = my_dict%getitem(fortran_type,"fortran_type")
                    if (ierror /= 0) then
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_To_Array_Refl_Type: Cannot determine fortran type'
                    else if (fortran_type == 'refl_type') then
                        allocate(arr(n),source=src1)
                        is_alloc = .true.
                    else if (fortran_type == 'srefl_type') then
                        allocate(arr(n),source=src2)
                        is_alloc = .true.
                    else
                        ierror = -1
                        err_cfml%flag = .true.
                        err_cfml%ierr = -1
                        err_cfml%msg  = 'List_To_Array_Refl_Type: Wrong fortran type'
                    end if
                end if
                if (ierror == 0) call unwrap_refl_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if
    End Subroutine List_To_Array_Refl_Type

    Module Subroutine List_To_Array_Str(procedure_name,var_name,my_list,arr,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        type(list),                                      intent(inout) :: my_list
        character(len=*), dimension(:), allocatable,     intent(out)   :: arr
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: my_str
        type(object) :: item

        ierror = my_list%len(n)
        if (n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_str,item)
                if (ierror == 0) arr(i+1) = my_str
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if
    End Subroutine List_To_Array_Str

    Module Subroutine List_To_Array_Symm_Oper_Type(procedure_name,var_name,my_list,arr,ierror)
        !---- Arguments ----!
        character(len=*),                                intent(in)    :: procedure_name
        character(len=*),                                intent(in)    :: var_name
        type(list),                                      intent(inout) :: my_list
        type(symm_oper_type), dimension(:), allocatable, intent(out)   :: arr
        integer,                                         intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item
        type(dict) :: my_dict

        ierror = my_list%len(n)
        if (n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_symm_oper_type(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if
    End Subroutine List_To_Array_Symm_Oper_Type

    Module Subroutine Ndarray_Int32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        integer, dimension(:), pointer,     intent(out)   :: p
        integer,                            intent(inout) :: ierror

        ierror = nd%get_data(p)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Int32_1d_To_Pointer

    Module Subroutine Ndarray_Int32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        integer, dimension(:,:), pointer,   intent(out)   :: p
        integer,                            intent(inout) :: ierror
        character(len=1),                   intent(out)   :: order

        order = 'F'
        ierror = nd%get_data(p)
        if (ierror /= 0) then
            call err_clear()
            ierror = nd%get_data(p,order='C')
            if (ierror == 0) order = 'C'
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Int32_2d_To_Pointer

    Module Subroutine Ndarray_Int32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        integer, dimension(:,:,:), pointer, intent(out)   :: p
        integer,                            intent(inout) :: ierror
        character(len=1),                   intent(out)   :: order

        order = 'F'
        ierror = nd%get_data(p)
        if (ierror /= 0) then
            call err_clear()
            ierror = nd%get_data(p,order='C')
            if (ierror == 0) order = 'C'
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Int32_3d_To_Pointer

    Module Subroutine Ndarray_Real32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        real, dimension(:), pointer,        intent(out)   :: p
        integer,                            intent(inout) :: ierror

        ierror = nd%get_data(p)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Real32_1d_To_Pointer

    Module Subroutine Ndarray_Real32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        real, dimension(:,:), pointer,      intent(out)   :: p
        integer,                            intent(inout) :: ierror
        character(len=1),                   intent(out)   :: order

        order = 'F'
        ierror = nd%get_data(p)
        if (ierror /= 0) then
            call err_clear()
            ierror = nd%get_data(p,order='C')
            if (ierror == 0) order = 'C'
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Real32_2d_To_Pointer

    Module Subroutine Ndarray_Real32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(ndarray),                      intent(in)    :: nd
        real, dimension(:,:,:), pointer,    intent(out)   :: p
        integer,                            intent(inout) :: ierror
        character(len=1),                   intent(out)   :: order

        order = 'F'
        ierror = nd%get_data(p)
        if (ierror /= 0) then
            call err_clear()
            ierror = nd%get_data(p,order='C')
            if (ierror == 0) order = 'C'
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting data from '//var_name
        end if
    End Subroutine Ndarray_Real32_3d_To_Pointer

    Module Subroutine Pointer_Int32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        integer, dimension(:), pointer,     intent(in)    :: p
        integer, dimension(:),              intent(out)   :: arr
        integer,                            intent(inout) :: ierror

        ! Local variables
        integer :: n

        if (size(p) == size(arr)) then
            arr(:) = p(:)
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Int32_1d_To_Array

    Module Subroutine Pointer_Int32_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        integer, dimension(:), pointer,     intent(in)    :: p
        integer, dimension(:), allocatable, intent(out)   :: arr
        integer,                            intent(inout) :: ierror

        ! Local variables
        integer :: n

        n = size(p)
        if (n > 0) then
            allocate(arr(n),stat=ierror)
            if (ierror == 0) then
                arr(:) = p(:)
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Int32_1d_To_Array_Alloc

    Module Subroutine Pointer_Int32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                     intent(in)    :: procedure_name
        character(len=*),                     intent(in)    :: var_name
        integer, dimension(:,:), pointer,     intent(in)    :: p
        integer, dimension(:,:),              intent(out)   :: arr
        integer,                              intent(inout) :: ierror
        character(len=1),                     intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2)) then
            if (order == 'C') then
                arr(:,:) = transpose(p(:,:))
            else
                arr(:,:) = p(:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Int32_2d_To_Array

    Module Subroutine Pointer_Int32_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                     intent(in)    :: procedure_name
        character(len=*),                     intent(in)    :: var_name
        integer, dimension(:,:), pointer,     intent(in)    :: p
        integer, dimension(:,:), allocatable, intent(out)   :: arr
        integer,                              intent(inout) :: ierror
        character(len=1),                     intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 > 0 .and. n2 > 0) then
            allocate(arr(n1,n2),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    arr(:,:) = transpose(p(:,:))
                else
                    arr(:,:) = p(:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Int32_2d_To_Array_Alloc

    Module Subroutine Pointer_Int32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                       intent(in)    :: procedure_name
        character(len=*),                       intent(in)    :: var_name
        integer, dimension(:,:,:), pointer,     intent(in)    :: p
        integer, dimension(:,:,:),              intent(out)   :: arr
        integer,                                intent(inout) :: ierror
        character(len=1),                       intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2) .and. n3 == size(arr,3)) then
            if (order == 'C') then
                do i = 1 , n1
                    do j = 1 , n2
                        do k = 1 , n3
                            arr(i,j,k) = p(k,j,i)
                        end do
                    end do
                end do
            else
                arr(:,:,:) = p(:,:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Int32_3d_To_Array

    Module Subroutine Pointer_Int32_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                       intent(in)    :: procedure_name
        character(len=*),                       intent(in)    :: var_name
        integer, dimension(:,:,:), pointer,     intent(in)    :: p
        integer, dimension(:,:,:), allocatable, intent(out)   :: arr
        integer,                                intent(inout) :: ierror
        character(len=1),                       intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 > 0 .and. n2 > 0 .and. n3 > 0) then
            allocate(arr(n1,n2,n3),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    do i = 1 , n1
                        do j = 1 , n2
                            do k = 1 , n3
                                arr(i,j,k) = p(k,j,i)
                            end do
                        end do
                    end do
                else
                    arr(:,:,:) = p(:,:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Int32_3d_To_Array_Alloc

    Module Subroutine Pointer_Rational_1d_To_Array(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),             intent(in)    :: procedure_name
        character(len=*),             intent(in)    :: var_name
        real, dimension(:), pointer,  intent(in)    :: p
        type(rational), dimension(:), intent(out)   :: arr
        integer,                      intent(inout) :: ierror

        ! Local variables
        integer :: n

        if (size(p) == size(arr)) then
            arr(:) = p(:)
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Rational_1d_To_Array

    Module Subroutine Pointer_Rational_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),                          intent(in)    :: procedure_name
        character(len=*),                          intent(in)    :: var_name
        real, dimension(:), pointer,               intent(in)    :: p
        type(rational), dimension(:), allocatable, intent(out)   :: arr
        integer,                                   intent(inout) :: ierror

        ! Local variables
        integer :: n

        n = size(p)
        if (n > 0) then
            allocate(arr(n),stat=ierror)
            if (ierror == 0) then
                arr(:) = p(:)
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Rational_1d_To_Array_Alloc

    Module Subroutine Pointer_Rational_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),               intent(in)    :: procedure_name
        character(len=*),               intent(in)    :: var_name
        real, dimension(:,:), pointer,  intent(in)    :: p
        type(rational), dimension(:,:), intent(out)   :: arr
        integer,                        intent(inout) :: ierror
        character(len=1),               intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2)) then
            if (order == 'C') then
                arr(:,:) = transpose(p(:,:))
            else
                arr(:,:) = p(:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Rational_2d_To_Array

    Module Subroutine Pointer_Rational_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                            intent(in)    :: procedure_name
        character(len=*),                            intent(in)    :: var_name
        real, dimension(:,:), pointer,               intent(in)    :: p
        type(rational), dimension(:,:), allocatable, intent(out)   :: arr
        integer,                                     intent(inout) :: ierror
        character(len=1),                            intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 > 0 .and. n2 > 0) then
            allocate(arr(n1,n2),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    arr(:,:) = transpose(p(:,:))
                else
                    arr(:,:) = p(:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Rational_2d_To_Array_Alloc

    Module Subroutine Pointer_Rational_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                 intent(in)    :: procedure_name
        character(len=*),                 intent(in)    :: var_name
        real, dimension(:,:,:), pointer,  intent(in)    :: p
        type(rational), dimension(:,:,:), intent(out)   :: arr
        integer,                          intent(inout) :: ierror
        character(len=1),                 intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2) .and. n3 == size(arr,3)) then
            if (order == 'C') then
                do i = 1 , n1
                    do j = 1 , n2
                        do k = 1 , n3
                            arr(i,j,k) = p(k,j,i)
                        end do
                    end do
                end do
            else
                arr(:,:,:) = p(:,:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Rational_3d_To_Array

    Module Subroutine Pointer_Rational_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                              intent(in)    :: procedure_name
        character(len=*),                              intent(in)    :: var_name
        real, dimension(:,:,:), pointer,               intent(in)    :: p
        type(rational), dimension(:,:,:), allocatable, intent(out)   :: arr
        integer,                                       intent(inout) :: ierror
        character(len=1),                              intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 > 0 .and. n2 > 0 .and. n3 > 0) then
            allocate(arr(n1,n2,n3),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    do i = 1 , n1
                        do j = 1 , n2
                            do k = 1 , n3
                                arr(i,j,k) = p(k,j,i)
                            end do
                        end do
                    end do
                else
                    arr(:,:,:) = p(:,:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Rational_3d_To_Array_Alloc

    Module Subroutine Pointer_Real32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        real, dimension(:), pointer, intent(in)    :: p
        real, dimension(:),          intent(out)   :: arr
        integer,                     intent(inout) :: ierror

        ! Local variables
        integer :: n

        if (size(p) == size(arr)) then
            arr(:) = p(:)
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Real32_1d_To_Array

    Module Subroutine Pointer_Real32_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
        !---- Arguments ----!
        character(len=*),                intent(in)    :: procedure_name
        character(len=*),                intent(in)    :: var_name
        real, dimension(:), pointer,     intent(in)    :: p
        real, dimension(:), allocatable, intent(out)   :: arr
        integer,                         intent(inout) :: ierror

        ! Local variables
        integer :: n

        n = size(p)
        if (n > 0) then
            allocate(arr(n),stat=ierror)
            if (ierror == 0) then
                arr(:) = p(:)
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Real32_1d_To_Array_Alloc

    Module Subroutine Pointer_Real32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),              intent(in)    :: procedure_name
        character(len=*),              intent(in)    :: var_name
        real, dimension(:,:), pointer, intent(in)    :: p
        real, dimension(:,:),          intent(out)   :: arr
        integer,                       intent(inout) :: ierror
        character(len=1),              intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2)) then
            if (order == 'C') then
                arr(:,:) = transpose(p(:,:))
            else
                arr(:,:) = p(:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Real32_2d_To_Array

    Module Subroutine Pointer_Real32_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                  intent(in)    :: procedure_name
        character(len=*),                  intent(in)    :: var_name
        real, dimension(:,:), pointer,     intent(in)    :: p
        real, dimension(:,:), allocatable, intent(out)   :: arr
        integer,                           intent(inout) :: ierror
        character(len=1),                  intent(in)    :: order

        ! Local variables
        integer :: n1,n2

        if (order == 'C') then
            n1 = size(p,2)
            n2 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
        end if
        if (n1 > 0 .and. n2 > 0) then
            allocate(arr(n1,n2),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    arr(:,:) = transpose(p(:,:))
                else
                    arr(:,:) = p(:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Real32_2d_To_Array_Alloc

    Module Subroutine Pointer_Real32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                intent(in)    :: procedure_name
        character(len=*),                intent(in)    :: var_name
        real, dimension(:,:,:), pointer, intent(in)    :: p
        real, dimension(:,:,:),          intent(out)   :: arr
        integer,                         intent(inout) :: ierror
        character(len=1),                intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 == size(arr,1) .and. n2 == size(arr,2) .and. n3 == size(arr,3)) then
            if (order == 'C') then
                do i = 1 , n1
                    do j = 1 , n2
                        do k = 1 , n3
                            arr(i,j,k) = p(k,j,i)
                        end do
                    end do
                end do
            else
                arr(:,:,:) = p(:,:,:)
            end if
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error in dimension of '//var_name
        end if

    End Subroutine Pointer_Real32_3d_To_Array

    Module Subroutine Pointer_Real32_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
        !---- Arguments ----!
        character(len=*),                    intent(in)    :: procedure_name
        character(len=*),                    intent(in)    :: var_name
        real, dimension(:,:,:), pointer,     intent(in)    :: p
        real, dimension(:,:,:), allocatable, intent(out)   :: arr
        integer,                             intent(inout) :: ierror
        character(len=1),                    intent(in)    :: order

        ! Local variables
        integer :: n1,n2,n3,i,j,k

        if (order == 'C') then
            n1 = size(p,3)
            n2 = size(p,2)
            n3 = size(p,1)
        else
            n1 = size(p,1)
            n2 = size(p,2)
            n3 = size(p,3)
        end if
        if (n1 > 0 .and. n2 > 0 .and. n3 > 0) then
            allocate(arr(n1,n2,n3),stat=ierror)
            if (ierror == 0) then
                if (order == 'C') then
                    do i = 1 , n1
                        do j = 1 , n2
                            do k = 1 , n3
                                arr(i,j,k) = p(k,j,i)
                            end do
                        end do
                    end do
                else
                    arr(:,:,:) = p(:,:,:)
                end if
            else
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = procedure_name//': Error allocating '//var_name
            end if
        end if

    End Subroutine Pointer_Real32_3d_To_Array_Alloc

    Module Subroutine Unwrap_Dict_Item_Dict(procedure_name,var_name,di,my_dict,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        type(dict),                  intent(inout) :: my_dict
        integer,                     intent(inout) :: ierror

        ! Local variables
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror == 0) then
            ierror = cast(my_dict,item)
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error casting '//var_name
        end if

     End Subroutine Unwrap_Dict_Item_Dict

    Module Subroutine Unwrap_Dict_Item_Int32(procedure_name,var_name,di,f,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        integer,                     intent(out)   :: f
        integer,                     intent(inout) :: ierror

        ierror = 0
        if (ierror == 0) ierror = di%getitem(f,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if

    End Subroutine Unwrap_Dict_Item_Int32

    Module Subroutine Unwrap_Dict_Item_List(procedure_name,var_name,di,my_list,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        type(list),                  intent(inout) :: my_list
        integer,                     intent(inout) :: ierror

        ! Local variables
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror == 0) then
            ierror = cast(my_list,item)
        else
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error casting '//var_name
        end if

    End Subroutine Unwrap_Dict_Item_List

    Module Subroutine Unwrap_Dict_Item_Logical(procedure_name,var_name,di,l,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        logical,                     intent(out)   :: l
        integer,                     intent(inout) :: ierror

        ierror = 0
        if (ierror == 0) ierror = di%getitem(l,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if

    End Subroutine Unwrap_Dict_Item_Logical

    Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d(procedure_name,var_name,di,p,ierror)
        !---- Arguments ----!
        character(len=*),               intent(in)    :: procedure_name
        character(len=*),               intent(in)    :: var_name
        type(dict),                     intent(inout) :: di
        integer, dimension(:), pointer, intent(out)   :: p
        integer,                        intent(inout) :: ierror

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d

    Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d(procedure_name,var_name,di,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                 intent(in)    :: procedure_name
        character(len=*),                 intent(in)    :: var_name
        type(dict),                       intent(inout) :: di
        integer, dimension(:,:), pointer, intent(out)   :: p
        integer,                          intent(inout) :: ierror
        character(len=1),                 intent(out)   :: order

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror,order)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d

    Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d(procedure_name,var_name,di,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                   intent(in)    :: procedure_name
        character(len=*),                   intent(in)    :: var_name
        type(dict),                         intent(inout) :: di
        integer, dimension(:,:,:), pointer, intent(out)   :: p
        integer,                            intent(inout) :: ierror
        character(len=1),                   intent(out)   :: order

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror,order)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d

    Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d(procedure_name,var_name,di,p,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        real, dimension(:), pointer, intent(out)   :: p
        integer,                     intent(inout) :: ierror

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d

    Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d(procedure_name,var_name,di,p,ierror,order)
        !---- Arguments ----!
        character(len=*),              intent(in)    :: procedure_name
        character(len=*),              intent(in)    :: var_name
        type(dict),                    intent(inout) :: di
        real, dimension(:,:), pointer, intent(out)   :: p
        integer,                       intent(inout) :: ierror
        character(len=1),              intent(out)   :: order

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror,order)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d

    Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d(procedure_name,var_name,di,p,ierror,order)
        !---- Arguments ----!
        character(len=*),                intent(in)    :: procedure_name
        character(len=*),                intent(in)    :: var_name
        type(dict),                      intent(inout) :: di
        real, dimension(:,:,:), pointer, intent(out)   :: p
        integer,                         intent(inout) :: ierror
        character(len=1),                intent(out)   :: order

        ! Local variable
        type(ndarray) :: nd
        type(object) :: item

        ierror = 0
        if (ierror == 0) ierror = di%getitem(item,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item from '//var_name
        else
            if (ierror == 0) call get_var_from_item(procedure_name,var_name,item,nd,ierror)
            if (ierror == 0) call ndarray_to_pointer(procedure_name,var_name,nd,p,ierror,order)
        end if

    End Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d

    Module Subroutine Unwrap_Dict_Item_Real32(procedure_name,var_name,di,f,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        real,                        intent(out)   :: f
        integer,                     intent(inout) :: ierror

        ierror = 0
        if (ierror == 0) ierror = di%getitem(f,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if

    End Subroutine Unwrap_Dict_Item_Real32

    Module Subroutine Unwrap_Dict_Item_String(procedure_name,var_name,di,s,ierror)
        !---- Arguments ----!
        character(len=*),            intent(in)    :: procedure_name
        character(len=*),            intent(in)    :: var_name
        type(dict),                  intent(inout) :: di
        character(len=*),            intent(inout) :: s
        integer,                     intent(inout) :: ierror

        !---- Local variables ----!
        character(len=:), allocatable :: my_str

        ierror = 0
        if (ierror == 0) ierror = di%getitem(my_str,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        else
            s = my_str
        end if

    End Subroutine Unwrap_Dict_Item_String

    Module Subroutine Unwrap_Dict_Item_String_Alloc(procedure_name,var_name,di,my_str,ierror)
        !---- Arguments ----!
        character(len=*),              intent(in)    :: procedure_name
        character(len=*),              intent(in)    :: var_name
        type(dict),                    intent(inout) :: di
        character(len=:), allocatable, intent(inout) :: my_str
        integer,                       intent(inout) :: ierror

        ierror = 0
        if (ierror == 0) ierror = di%getitem(my_str,var_name)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = procedure_name//': Error getting item '//var_name
        end if

    End Subroutine Unwrap_Dict_Item_String_Alloc

End Submodule Unwrap