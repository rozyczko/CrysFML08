! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------


module wraps

    use forpy_mod

    implicit none

    contains

    subroutine charray_to_list(charr,li_charr,ierror)

        ! Arguments
        character(len=*), dimension(:), intent(in)    :: charr
        type(list),                     intent(inout) :: li_charr
        integer,                        intent(out)   :: ierror

        ! Local variables
        integer :: i,n

        ierror = 0

        ierror = li_charr%len(n)
        do i = 1 , size(charr)
            if (ierror == 0) then
                if (i <= n) then
                    ierror = li_charr%setitem(i-1,charr(i))
                else
                    ierror = li_charr%append(charr(i))
                end if
            end if
        end do

    end subroutine charray_to_list

    subroutine list_to_charray(li_charr,charr,ierror)

        ! Arguments
        type(list),                     intent(inout) :: li_charr
        character(len=*), dimension(:), intent(inout) :: charr
        integer,                        intent(out)   :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: str
        type(object) :: item

        ierror = li_charr%len(n)
        do i = 1 , n
            if (ierror == 0) ierror = li_charr%getitem(item,i-1)
            if (ierror == 0) ierror = cast(str,item)
            if (ierror == 0) charr(i) = str
        end do

    end subroutine list_to_charray

    subroutine maxlen_from_li_charr(li_charr,l,ierror)

        ! Arguments
        type(list),                     intent(inout) :: li_charr
        integer,                        intent(out)   :: l
        integer,                        intent(out)   :: ierror

        ! Local variables
        integer :: i,n
        character(len=:), allocatable :: mystr
        type(object) :: item

        l = 0
        ierror = li_charr%len(n)
        do i = 1 , n
            if (ierror == 0) ierror = li_charr%getitem(item,i-1)
            if (ierror == 0) ierror = cast(mystr,item)
            if (ierror == 0) then 
                if (len(trim(mystr)) > l) l = len(trim(mystr)) 
            end if
        end do

    end subroutine maxlen_from_li_charr

end module wraps
