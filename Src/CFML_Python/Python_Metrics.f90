!!----
!!----
!!----
SubModule (CFML_Python)  Metrics_Python_Wraps
    implicit none
    Contains

    !!----
    !!---- WRAP_CELL_TYPE
    !!----
    !!---- 24/03/2023
    !!
    Module Subroutine Wrap_Cell_Type(for_var, dic_var)
        !---- Arguments ----!
        class(cell_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: dic_var

        !---- Local Variables ----!
        integer :: ierror
        type(ndarray) :: nd_cell,nd_rcell,nd_scell,nd_ang,nd_rang,nd_sang,nd_GD,nd_GR,&
                         nd_cr_orth_cel,nd_orth_cr_cel,nd_bl_m,nd_inv_bl_m

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = dic_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_scell,for_var%scell)
        if (ierror == 0) ierror = dic_var%setitem('scell',nd_scell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = dic_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = ndarray_create(nd_sang,for_var%sang)
        if (ierror == 0) ierror = dic_var%setitem('sang',nd_sang)
        if (ierror == 0) ierror = dic_var%setitem('vol',for_var%vol)
        if (ierror == 0) ierror = dic_var%setitem('svol',for_var%svol)
        select type (A => for_var)
            type is (cell_g_type)
                if (ierror == 0) ierror = ndarray_create(nd_rcell,A%rcell)
                if (ierror == 0) ierror = dic_var%setitem('rcell',nd_rcell)
                if (ierror == 0) ierror = ndarray_create(nd_rang,A%rang)
                if (ierror == 0) ierror = dic_var%setitem('rang',nd_rang)
                if (ierror == 0) ierror = dic_var%setitem('rvol',A%rvol)
                if (ierror == 0) ierror = ndarray_create(nd_GD,A%GD)
                if (ierror == 0) ierror = dic_var%setitem('GD',nd_GD)
                if (ierror == 0) ierror = ndarray_create(nd_GR,A%GR)
                if (ierror == 0) ierror = dic_var%setitem('GR',nd_GR)
                if (ierror == 0) ierror = ndarray_create(nd_cr_orth_cel,A%cr_orth_cel)
                if (ierror == 0) ierror = dic_var%setitem('cr_orth_cel',nd_cr_orth_cel)
                if (ierror == 0) ierror = ndarray_create(nd_orth_cr_cel,A%orth_cr_cel)
                if (ierror == 0) ierror = dic_var%setitem('orth_cr_cel',nd_orth_cr_cel)
                if (ierror == 0) ierror = ndarray_create(nd_bl_m,A%bl_m)
                if (ierror == 0) ierror = dic_var%setitem('bl_m',nd_bl_m)
                if (ierror == 0) ierror = ndarray_create(nd_inv_bl_m,A%inv_bl_m)
                if (ierror == 0) ierror = dic_var%setitem('inv_bl_m',nd_inv_bl_m)
                if (ierror == 0) ierror = dic_var%setitem('carttype',A%carttype)
        end select
        select type (A => for_var)
            type is (cell_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','cell_type')
            type is (cell_g_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','cell_g_type')
        end select
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_Cell_Type: Wrapping failed'
        end if

    End Subroutine Wrap_Cell_Type

End SubModule Metrics_Python_Wraps