submodule (CFML_Wraps) Wraps_Structure_Factors

    implicit none
    contains

    Module Subroutine Unwrap_scattering_species_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(scattering_species_type), intent(out) :: for_var
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
            err_cfml%msg  = 'Unwrap_scattering_species_type: Cannot determine fortran type'
        else
            if (fortran_type == 'scattering_species_type') then
                allocate(scattering_species_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_scattering_species_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','num_species',py_var,for_var%num_species,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','num_magspc',py_var,for_var%num_magspc,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','symb',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_scattering_species_type','symb',my_list,for_var%symb,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','symb_mag',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_scattering_species_type','symb_mag',my_list,for_var%symb_mag,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','br',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_scattering_species_type','br',p_real_1d,for_var%br,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','bi',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_scattering_species_type','bi',p_real_1d,for_var%bi,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','delta_fp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_scattering_species_type','delta_fp',p_real_1d,for_var%delta_fp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','delta_fpp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array_alloc('Unwrap_scattering_species_type','delta_fpp',p_real_1d,for_var%delta_fpp,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','xcoef',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_scattering_species_type','xcoef',my_list,for_var%xcoef,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_scattering_species_type','mcoef',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_scattering_species_type','mcoef',my_list,for_var%mcoef,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_scattering_species_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_scattering_species_type

    Module Subroutine Wrap_scattering_species_type(py_var,for_var,ierror)

        ! Arguments
        type(scattering_species_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_symb,li_symb_mag,li_xcoef,li_mcoef
        type(ndarray) :: nd_br,nd_bi,nd_delta_fp,nd_delta_fpp

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('num_species',for_var%num_species)
        if (ierror == 0) ierror = py_var%setitem('num_magspc',for_var%num_magspc)
        if (ierror == 0) ierror = list_create(li_symb)
        if (ierror == 0) then
            do i = 1 , size(for_var%symb)
                if (ierror == 0) ierror = li_symb%append(for_var%symb(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('symb',li_symb)
        if (ierror == 0) ierror = list_create(li_symb_mag)
        if (ierror == 0) then
            do i = 1 , size(for_var%symb_mag)
                if (ierror == 0) ierror = li_symb_mag%append(for_var%symb_mag(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('symb_mag',li_symb_mag)
        if (ierror == 0) ierror = ndarray_create(nd_br,for_var%br)
        if (ierror == 0) ierror = py_var%setitem('br',nd_br)
        if (ierror == 0) ierror = ndarray_create(nd_bi,for_var%bi)
        if (ierror == 0) ierror = py_var%setitem('bi',nd_bi)
        if (ierror == 0) ierror = ndarray_create(nd_delta_fp,for_var%delta_fp)
        if (ierror == 0) ierror = py_var%setitem('delta_fp',nd_delta_fp)
        if (ierror == 0) ierror = ndarray_create(nd_delta_fpp,for_var%delta_fpp)
        if (ierror == 0) ierror = py_var%setitem('delta_fpp',nd_delta_fpp)
        if (ierror == 0) ierror = list_create(li_xcoef)
        if (ierror == 0) allocate(di_xcoef(size(for_var%xcoef)))
        if (ierror == 0) then
            do i = 1 , size(for_var%xcoef)
                ierror = dict_create(di_xcoef(i))
                if (ierror == 0) call wrap_xray_form_type(for_var%xcoef,(di_xcoef(i),ierror))
                if (ierror == 0) ierror = li_xcoef%append(di_xcoef(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('xcoef',li_xcoef)
        if (ierror == 0) ierror = list_create(li_mcoef)
        if (ierror == 0) allocate(di_mcoef(size(for_var%mcoef)))
        if (ierror == 0) then
            do i = 1 , size(for_var%mcoef)
                ierror = dict_create(di_mcoef(i))
                if (ierror == 0) call wrap_magnetic_form_type(for_var%mcoef,(di_mcoef(i),ierror))
                if (ierror == 0) ierror = li_mcoef%append(di_mcoef(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mcoef',li_mcoef)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_scattering_species_type: Wrapping failed'
        end if

    End Subroutine Wrap_scattering_species_type

    Module Subroutine Unwrap_strf_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(strf_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_strf_type: Cannot determine fortran type'
        else
            if (fortran_type == 'strf_type') then
                allocate(strf_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_strf_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','sqnuc',py_var,for_var%sqnuc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','sqmiv',py_var,for_var%sqmiv,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','nsf',py_var,for_var%nsf,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','msf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_strf_type','msf',my_list,for_var%msf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','miv',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_strf_type','miv',my_list,for_var%miv,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strf_type','mivc',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_strf_type','mivc',my_list,for_var%mivc,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_strf_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_strf_type

    Module Subroutine Wrap_strf_type(py_var,for_var,ierror)

        ! Arguments
        type(strf_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_msf,li_miv,li_mivc

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('sqnuc',for_var%sqnuc)
        if (ierror == 0) ierror = py_var%setitem('sqmiv',for_var%sqmiv)
        if (ierror == 0) ierror = py_var%setitem('nsf',for_var%nsf)
        if (ierror == 0) ierror = list_create(li_msf)
        if (ierror == 0) then
            do i = 1 , size(for_var%msf)
                if (ierror == 0) ierror = li_msf%append(for_var%msf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('msf',li_msf)
        if (ierror == 0) ierror = list_create(li_miv)
        if (ierror == 0) then
            do i = 1 , size(for_var%miv)
                if (ierror == 0) ierror = li_miv%append(for_var%miv(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('miv',li_miv)
        if (ierror == 0) ierror = list_create(li_mivc)
        if (ierror == 0) then
            do i = 1 , size(for_var%mivc)
                if (ierror == 0) ierror = li_mivc%append(for_var%mivc(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('mivc',li_mivc)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_strf_type: Wrapping failed'
        end if

    End Subroutine Wrap_strf_type

    Module Subroutine Unwrap_strflist_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(strflist_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        type(list) :: my_list

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_strflist_type: Cannot determine fortran type'
        else
            if (fortran_type == 'strflist_type') then
                allocate(strflist_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_strflist_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_strflist_type','nref',py_var,for_var%nref,ierror)
        if (ierror == 0) ierror = list_create(my_list)
        if (ierror == 0) call unwrap_dict_item('Unwrap_strflist_type','strf',py_var,my_list,ierror)
        if (ierror == 0) call list_to_array('Unwrap_strflist_type','strf',my_list,for_var%strf,ierror)
        if (ierror == 0) call my_list%destroy
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_strflist_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_strflist_type

    Module Subroutine Wrap_strflist_type(py_var,for_var,ierror)

        ! Arguments
        type(strflist_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(list) :: li_strf

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('nref',for_var%nref)
        if (ierror == 0) ierror = list_create(li_strf)
        if (ierror == 0) allocate(di_strf(size(for_var%strf)))
        if (ierror == 0) then
            do i = 1 , size(for_var%strf)
                ierror = dict_create(di_strf(i))
                if (ierror == 0) call wrap_strf_type(for_var%strf,(di_strf(i),ierror))
                if (ierror == 0) ierror = li_strf%append(di_strf(i))
            end do
        end if
        if (ierror == 0) ierror = py_var%setitem('strf',li_strf)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_strflist_type: Wrapping failed'
        end if

    End Subroutine Wrap_strflist_type

end submodule