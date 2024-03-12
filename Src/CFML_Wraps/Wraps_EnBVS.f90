submodule (CFML_Wraps) Wraps_EnBVS

    implicit none
    contains

    Module Subroutine list_to_array_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atoms_conf_list_type), dimension(:), allocatable, intent(out) :: arr
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
                if (ierror == 0) call unwrap_atoms_conf_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_atoms_conf_list_type

    Module Subroutine list_to_array_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atoms_conf_list_type), dimension(:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array_atoms_conf_list_type_no_alloc: Dimension of list and arr inconsistent'
        end if
        if (ierror == 0 .and. n > 0) then
            do i = 0 , n-1
                if (ierror == 0) ierror = my_list%getitem(item,i)
                if (ierror == 0) ierror = cast(my_dict,item)
                if (ierror == 0) call unwrap_atoms_conf_list_type_no_alloc(my_dict,arr(i+1),ierror)
                if (ierror == 0) ierror = err_cfml%ierr
            end do
        end if

    End Subroutine list_to_array_atoms_conf_list_type_no_alloc

    Module Subroutine list_to_array2d_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atoms_conf_list_type), dimension(:,:), allocatable, intent(out) :: arr
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
                        if (ierror == 0) call unwrap_atoms_conf_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_atoms_conf_list_type

    Module Subroutine list_to_array2d_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)

        ! Arguments
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: var_name
        type(list), intent(inout) :: my_list
        type(atoms_conf_list_type), dimension(:,:), intent(out) :: arr
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
            err_cfml%msg  = 'list_to_array2d_atoms_conf_list_type_no_alloc: Dimension of list and arr inconsistent'
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
                        err_cfml%msg  = 'list_to_array2d_atoms_conf_list_type_no_alloc: Dimension of list and arr inconsistent'
                    end if
                    do j = 0 , n-1
                        if (ierror == 0) ierror = li%getitem(item,j)
                        if (ierror == 0) ierror = cast(my_dict,item)
                        if (ierror == 0) call unwrap_atoms_conf_list_type_no_alloc(my_dict,arr(i+1,j+1),ierror)
                        if (ierror == 0) ierror = err_cfml%ierr
                    end do
                end do
            end if
        end if

    End Subroutine list_to_array2d_atoms_conf_list_type_no_alloc

    Module Subroutine Unwrap_atoms_conf_list_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(atoms_conf_list_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_atoms_conf_list_type: Cannot determine fortran type'
        else
            if (fortran_type /= 'atoms_conf_list_type') then
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_atoms_conf_list_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','natoms',py_var,for_var%natoms,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','n_spec',py_var,for_var%n_spec,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','n_anions',py_var,for_var%n_anions,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','n_cations',py_var,for_var%n_cations,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','tol',py_var,for_var%tol,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','totatoms',py_var,for_var%totatoms,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','species',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_atoms_conf_list_type','species',my_list,for_var%species,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','radius',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_atoms_conf_list_type','radius',p_real_1d,for_var%radius,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_atoms_conf_list_type','atom',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_atoms_conf_list_type','atom',my_list,for_var%atom,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_atoms_conf_list_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_atoms_conf_list_type

    Module Subroutine Wrap_atoms_conf_list_type(for_var,py_var,ierror)

        ! Arguments
        type(atoms_conf_list_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_species,li_atom
        type(dict), dimension(:), allocatable :: di_atom
        type(ndarray) :: nd_radius

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = py_var%setitem('n_spec',for_var%n_spec)
        if (ierror == 0) ierror = py_var%setitem('n_anions',for_var%n_anions)
        if (ierror == 0) ierror = py_var%setitem('n_cations',for_var%n_cations)
        if (ierror == 0) ierror = py_var%setitem('tol',for_var%tol)
        if (ierror == 0) ierror = py_var%setitem('totatoms',for_var%totatoms)
        if (ierror == 0) ierror = list_create(li_species)
        if (ierror == 0) then
            do i = 1 , size(for_var%species)
                if (ierror == 0) ierror = li_species%append(for_var%species(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('species',li_species)
        if (ierror == 0) ierror = ndarray_create(nd_radius,for_var%radius)
        if (ierror == 0) ierror = py_var%setitem('radius',nd_radius)
        if (ierror == 0) ierror = list_create(li_atom)
        if (ierror == 0) allocate(di_atom(size(for_var%atom)))
        if (ierror == 0) then
            do i = 1 , size(for_var%atom)
                ierror = dict_create(di_atom(i))
                if (ierror == 0) call wrap_atm_type(for_var%atom(i),di_atom(i),ierror)
                if (ierror == 0) ierror = li_atom%append(di_atom(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('atom',li_atom)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_atoms_conf_list_type: Wrapping failed'
        end if

    End Subroutine Wrap_atoms_conf_list_type

end submodule