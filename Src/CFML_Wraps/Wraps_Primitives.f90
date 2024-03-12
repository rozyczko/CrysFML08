submodule (CFML_Wraps) Wraps_Primitives

    implicit none

    contains

    subroutine list_to_array_character(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        character(len=*), dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: my_str
        type(object) :: item

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_str,item)
                if (ierror == 0) arr(i+1) = my_str
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    end subroutine list_to_array_character

    subroutine list_to_array2d_character(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        character(len=*), dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        character(len=:), allocatable :: my_str
        type(object) :: item
        type(object) :: li

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
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_str,item)
                        if (ierror == 0) arr(i+1,j+1) = my_str
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    end subroutine list_to_array2d_character

    subroutine list_to_array_character_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        character(len=*), dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: my_str
        type(object) :: item

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_character_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_str,item)
                if (ierror == 0) arr(i+1) = my_str
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    end subroutine list_to_array_character_no_alloc

    subroutine list_to_array2d_character_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        character(len=*), dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        character(len=:), allocatable :: my_str
        type(object) :: item
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_character2d_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (n > 0) then
            do i = 0 , m-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(li,item)
                ierror = li%len(n)
                if (n /= size(arr,2)) then
                    ierror = -1
                    err_cfml%flag = .true.
                    err_cfml%ierr = -1
                    err_cfml%msg  = 'list_to_array_character2d_no_alloc: Dimension of list and arr inconsistent'
                end if
                do j = 0 , n-1
                    if (ierror == 0) ierror = li%getitem(item,j)
                    if (ierror == 0) ierror = cast(my_str,item)
                    if (ierror == 0) arr(i+1,j+1) = my_str
                    if (ierror == 0) ierror = err_cfml%ierr
                end do
            end do
        end if

    end subroutine list_to_array2d_character_no_alloc

    subroutine list_to_array_logical(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        logical, dimension(:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item

        ierror = my_list%len(n)
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(arr(i+1),item)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    end subroutine list_to_array_logical

    subroutine list_to_array2d_logical(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        logical, dimension(:,:), allocatable, intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(object) :: li

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
                    if (ierror == 0) ierror = cast(li,item)
                    if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(arr(i+1,j+1),item)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    end subroutine list_to_array2d_logical

    subroutine list_to_array_logical_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        logical, dimension(:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,n
        type(object) :: item

        ierror = my_list%len(n)
        if (n /= size(arr)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array_logical_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            allocate(arr(n))
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(arr(i+1),item)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    end subroutine list_to_array_logical_no_alloc

    subroutine list_to_array2d_logical_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        logical, dimension(:,:), intent(out) :: arr
        integer, intent(inout) :: ierror

        ! Local variables
        integer :: i,j,m,n
        type(object) :: item
        type(list) :: li

        ierror = my_list%len(m)
        if (m /= size(arr,1)) then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'list_to_array2d_logical_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (n > 0) then
            do i = 0 , m-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(li,item)
                ierror = li%len(n)
                if (n /= size(arr,2)) then
                    ierror = -1
                    err_cfml%flag = .true.
                    err_cfml%ierr = -1
                    err_cfml%msg  = 'list_to_array2d_logical_no_alloc: Dimension of list and arr inconsistent'
                end if
                do j = 0 , n-1
                    if (ierror == 0) ierror = li%getitem(item,j)
                    if (ierror == 0) ierror = cast(arr(i+1,j+1),item)
                    if (ierror == 0) ierror = err_cfml%ierr
                end do
            end do
        end if

    end subroutine list_to_array2d_logical_no_alloc

end submodule