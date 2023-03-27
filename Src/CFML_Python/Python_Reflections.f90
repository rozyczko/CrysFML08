!!----
!!----
!!----
SubModule (CFML_Python)  Refl_Python_Wraps
    implicit none
    Contains

    !!----
    !!---- WRAP_REFL_TYPE
    !!----
    !!---- 24/03/2023
    !!
    Module Subroutine Wrap_Refl_Type(for_var, dic_var)
        !---- Arguments ----!
        class(refl_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: dic_var

        !---- Local Variables ----!
        integer :: ierror
        type(ndarray) :: nd_h

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = dic_var%setitem('h',nd_h)
        if (ierror == 0) ierror = dic_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = dic_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = dic_var%setitem('imag',for_var%imag)
        if (ierror == 0) ierror = dic_var%setitem('pcoeff',for_var%pcoeff)
        select type (A => for_var)
            class is (srefl_type)
                if (ierror == 0) ierror = dic_var%setitem('fo',A%fo)
                if (ierror == 0) ierror = dic_var%setitem('fc',A%fc)
                if (ierror == 0) ierror = dic_var%setitem('sfo',A%sfo)
                if (ierror == 0) ierror = dic_var%setitem('phase',A%phase)
                if (ierror == 0) ierror = dic_var%setitem('a',A%a)
                if (ierror == 0) ierror = dic_var%setitem('b',A%b)
                if (ierror == 0) ierror = dic_var%setitem('w',A%w)
        end select
        select type (A => for_var)
            type is (refl_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','refl_type')
            type is (srefl_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','srefl_type')
        end select
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Refl_Type: Wrapping failed'
        end if

    End Subroutine Wrap_Refl_Type

    !!----
    !!---- WRAP_REFLIST_TYPE
    !!----
    !!---- 24/03/2023
    !!
    Module Subroutine Wrap_Reflist_Type(for_var, dic_var)
        !---- Arguments ----!
        type(reflist_type),  intent(in)   :: for_var
        type(dict),         intent(inout) :: dic_var

        !---- Local Variables ----!
        integer :: ierror,i
        type(list) :: li_ref
        type(dict), dimension(:), allocatable :: di_ref

        ierror = 0
        ierror = 0
        if (ierror == 0) ierror = dic_var%setitem('nref',for_var%nref)
        if (ierror == 0) then
            if (ierror == 0) ierror = list_create(li_ref)
            allocate(di_ref(for_var%nref))
            do i = 1 , for_var%nref
                if (ierror == 0) ierror = dict_create(di_ref(i))
                if (ierror == 0) call wrap_refl_type(for_var%ref(i),di_ref(i))
                ierror = err_cfml%ierr
                if (ierror == 0) ierror = li_ref%append(di_ref(i))
            end do
        end if
        if (ierror == 0) ierror = dic_var%setitem('ref',li_ref)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Reflist_Type: Wrapping failed'
        end if

    end subroutine Wrap_Reflist_Type

End SubModule Refl_Python_Wraps