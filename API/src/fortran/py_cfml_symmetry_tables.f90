! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------

module py_cfml_symmetry_tables

    use forpy_mod 
    use iso_c_binding 

    use cfml_globaldeps 
    use cfml_symmetry_tables

    implicit none

    contains

    function py_get_compact_hm(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        character(len=:),allocatable :: hm ! CrysFML type: character
        character(len=:),allocatable :: c_hm ! CrysFML type: character

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(hm,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            c_hm = get_compact_hm(hm)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"get_compact_hm: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,c_hm)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_get_compact_hm

    function py_get_hm_compact_hm(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        character(len=:),allocatable :: c_hm ! CrysFML type: character
        character(len=:),allocatable :: hm ! CrysFML type: character

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(c_hm,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            hm = get_hm_compact_hm(c_hm)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"get_hm_compact_hm: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,hm)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_get_hm_compact_hm

    function py_get_it_generators(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        character(len=:),allocatable :: spg ! CrysFML type: character
        character(len=:),allocatable :: strgen ! CrysFML type: character

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(spg,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            strgen = get_it_generators(spg)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"get_it_generators: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ret%setitem(0,strgen)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_get_it_generators

    function py_get_spacegroup_symbols(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        character(len=:),allocatable :: strsg ! CrysFML type: character
        character(len=:),allocatable :: hm ! CrysFML type: character
        character(len=:),allocatable :: hall ! CrysFML type: character
        integer :: it ! CrysFML type: integer
        character(len=:),allocatable :: c_hm ! CrysFML type: character

        ! Local variables
        integer :: ierror,ii
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        ! Reset error variable
        ierror = 0

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(strsg,item)

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            call get_spacegroup_symbols(strsg,hm,hall,it,c_hm)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"get_spacegroup_symbols: "//trim(err_cfml%msg))
            end if
        end if

        ! Return tuple
        if (ierror == 0) ierror = tuple_create(ret,4)
        if (ierror == 0) ierror = ret%setitem(0,hm)
        if (ierror == 0) ierror = ret%setitem(1,hall)
        if (ierror == 0) ierror = ret%setitem(2,it)
        if (ierror == 0) ierror = ret%setitem(3,c_hm)
        if (ierror == 0) then 
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_get_spacegroup_symbols

end module py_cfml_symmetry_tables
