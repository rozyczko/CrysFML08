!!----
!!----
!!----
SubModule (CFML_Python)  Refl_Python_Wraps
    implicit none
    Contains

    Module Subroutine Wrap_Refl_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        class(refl_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: py_var
        integer,          intent(out)   :: ierror

        !---- Local Variables ----!
        type(ndarray) :: nd_h

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','refl_type')
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = py_var%setitem('h',nd_h)
        if (ierror == 0) ierror = py_var%setitem('imag',for_var%imag)
        if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = py_var%setitem('pcoeff',for_var%pcoeff)
        if (ierror == 0) ierror = py_var%setitem('s',for_var%s)
        select type (A => for_var)
            class is (srefl_type)
                if (ierror == 0) ierror = py_var%setitem('fortran_type','srefl_type')
                if (ierror == 0) ierror = py_var%setitem('a',A%a)
                if (ierror == 0) ierror = py_var%setitem('b',A%b)
                if (ierror == 0) ierror = py_var%setitem('fc',A%fc)
                if (ierror == 0) ierror = py_var%setitem('fo',A%fo)
                if (ierror == 0) ierror = py_var%setitem('iph',A%iph)
                if (ierror == 0) ierror = py_var%setitem('phase',A%phase)
                if (ierror == 0) ierror = py_var%setitem('sfo',A%sfo)
                if (ierror == 0) ierror = py_var%setitem('w',A%w)
        end select
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Refl_Type: Wrapping failed'
        end if

    End Subroutine Wrap_Refl_Type

    Module Subroutine Unwrap_Refl_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                    intent(inout) :: py_var
        class(refl_type), allocatable, intent(out)   :: for_var
        integer,                       intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        character(len=:), allocatable :: fortran_type

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Refl_Type: Cannot determine fortran type'
        else
            if (fortran_type == 'refl_type') then
                allocate(refl_type :: for_var)
            else if (fortran_type == 'srefl_g_type') then
                allocate(srefl_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Refl_Type: Unknown fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','h',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Refl_Type','h',p_int_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','s',py_var,for_var%s,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','a',py_var,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','b',py_var,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','fc',py_var,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','fo',py_var,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','iph',py_var,A%iph,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','phase',py_var,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','sfo',py_var,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','w',py_var,A%w,ierror)
            end select
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'UnWrap_Refl_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Refl_Type

    Module Subroutine Unwrap_Refl_Type_No_Alloc(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                    intent(inout) :: py_var
        class(refl_type),              intent(out)   :: for_var
        integer,                       intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        character(len=:), allocatable :: fortran_type

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Refl_Type: Cannot determine fortran type'
        else
            if (fortran_type == 'refl_type' .and. .not. fortran_type == 'srefl_g_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Refl_Type: Unknown fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','h',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_Refl_Type','h',p_int_1d,for_var%h,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','imag',py_var,for_var%imag,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','mult',py_var,for_var%mult,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','pcoeff',py_var,for_var%pcoeff,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','s',py_var,for_var%s,ierror)
        if (ierror == 0) then
            select type (A => for_var)
                class is (srefl_type)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','a',py_var,A%a,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','b',py_var,A%b,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','fc',py_var,A%fc,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','fo',py_var,A%fo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','iph',py_var,A%iph,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','phase',py_var,A%phase,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','sfo',py_var,A%sfo,ierror)
                    if (ierror == 0) call unwrap_dict_item('Unwrap_Refl_Type','w',py_var,A%w,ierror)
            end select
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'UnWrap_Refl_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Refl_Type_No_Alloc

    Module Subroutine Wrap_Reflist_Type(for_var,py_var,ierror)
        !---- Arguments ----!
        type(reflist_type), intent(in)   :: for_var
        type(dict),         intent(inout) :: py_var
        integer,            intent(out)   :: ierror

        !---- Local Variables ----!
        integer :: i
        type(list) :: li_ref
        type(dict), dimension(:), allocatable :: di_ref

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('fortran_type','reflist_type')
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) then
            if (ierror == 0) ierror = list_create(li_ref)
            allocate(di_ref(for_var%nref))
            do i = 1 , for_var%nref
                if (ierror == 0) ierror = dict_create(di_ref(i))
                if (ierror == 0) call wrap_refl_type(for_var%ref(i),di_ref(i),ierror)
                if (ierror == 0) ierror = li_ref%append(di_ref(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('ref',li_ref)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Reflist_Type: Wrapping failed'
        end if

    end subroutine Wrap_Reflist_Type

    Module Subroutine Unwrap_Reflist_Type(py_var,for_var,ierror)
        !---- Arguments ----!
        type(dict),                    intent(inout) :: py_var
        type(reflist_type),            intent(out)   :: for_var
        integer,                       intent(out)   :: ierror

        !---- Local Variables ----!
        integer, dimension(:), pointer :: p_int_1d
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0

        ! fortran_type
        ierror = py_var%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_Reflist_Type: Cannot determine fortran type'
        else
            if (fortran_type /= 'reflist_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = -1
                err_cfml%msg  = 'Unwrap_Reflist_Type: Unknown fortran type'
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_Reflist_Type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_Reflist_Type','ref',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_Reflist_Type','ref',my_list,for_var%ref,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'UnWrap_Reflist_Type: Unwrapping failed'
        end if

    End Subroutine Unwrap_Reflist_Type

End SubModule Refl_Python_Wraps