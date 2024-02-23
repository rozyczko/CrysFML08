submodule (CFML_Wraps) Wraps_Scattering_Tables

    implicit none
    contains

    Module Subroutine Unwrap_anomalous_sc_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(anomalous_sc_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_anomalous_sc_type: Cannot determine fortran type'
        else
            if (fortran_type == 'anomalous_sc_type') then
                allocate(anomalous_sc_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_anomalous_sc_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_anomalous_sc_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_anomalous_sc_type','fp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_anomalous_sc_type','fp',p_real_1d,for_var%fp,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_anomalous_sc_type','fpp',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_anomalous_sc_type','fpp',p_real_1d,for_var%fpp,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_anomalous_sc_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_anomalous_sc_type

    Module Subroutine Wrap_anomalous_sc_type(py_var,for_var,ierror)

        ! Arguments
        type(anomalous_sc_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_fp,nd_fpp

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('symb',for_var%symb)
        if (ierror == 0) ierror = ndarray_create(nd_fp,for_var%fp)
        if (ierror == 0) ierror = py_var%setitem('fp',nd_fp)
        if (ierror == 0) ierror = ndarray_create(nd_fpp,for_var%fpp)
        if (ierror == 0) ierror = py_var%setitem('fpp',nd_fpp)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_anomalous_sc_type: Wrapping failed'
        end if

    End Subroutine Wrap_anomalous_sc_type

    Module Subroutine Unwrap_chem_info_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(chem_info_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        integer, dimension(:), pointer :: p_int_1d
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_chem_info_type: Cannot determine fortran type'
        else
            if (fortran_type == 'chem_info_type') then
                allocate(chem_info_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_chem_info_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','name',py_var,for_var%name,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','atwe',py_var,for_var%atwe,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','rcov',py_var,for_var%rcov,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','rwaals',py_var,for_var%rwaals,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','vatm',py_var,for_var%vatm,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','oxid',py_var,p_int_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_chem_info_type','oxid',p_int_1d,for_var%oxid,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','rion',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_chem_info_type','rion',p_real_1d,for_var%rion,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','sctf',py_var,for_var%sctf,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','sedinc',py_var,for_var%sedinc,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_chem_info_type','sea',py_var,for_var%sea,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_chem_info_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_chem_info_type

    Module Subroutine Wrap_chem_info_type(py_var,for_var,ierror)

        ! Arguments
        type(chem_info_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_oxid,nd_rion

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('symb',for_var%symb)
        if (ierror == 0) ierror = py_var%setitem('name',for_var%name)
        if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
        if (ierror == 0) ierror = py_var%setitem('atwe',for_var%atwe)
        if (ierror == 0) ierror = py_var%setitem('rcov',for_var%rcov)
        if (ierror == 0) ierror = py_var%setitem('rwaals',for_var%rwaals)
        if (ierror == 0) ierror = py_var%setitem('vatm',for_var%vatm)
        if (ierror == 0) ierror = ndarray_create(nd_oxid,for_var%oxid)
        if (ierror == 0) ierror = py_var%setitem('oxid',nd_oxid)
        if (ierror == 0) ierror = ndarray_create(nd_rion,for_var%rion)
        if (ierror == 0) ierror = py_var%setitem('rion',nd_rion)
        if (ierror == 0) ierror = py_var%setitem('sctf',for_var%sctf)
        if (ierror == 0) ierror = py_var%setitem('sedinc',for_var%sedinc)
        if (ierror == 0) ierror = py_var%setitem('sea',for_var%sea)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_chem_info_type: Wrapping failed'
        end if

    End Subroutine Wrap_chem_info_type

    Module Subroutine Unwrap_magnetic_form_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(magnetic_form_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_magnetic_form_type: Cannot determine fortran type'
        else
            if (fortran_type == 'magnetic_form_type') then
                allocate(magnetic_form_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_magnetic_form_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_form_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_magnetic_form_type','sctm',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_magnetic_form_type','sctm',p_real_1d,for_var%sctm,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_magnetic_form_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_magnetic_form_type

    Module Subroutine Wrap_magnetic_form_type(py_var,for_var,ierror)

        ! Arguments
        type(magnetic_form_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_sctm

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('symb',for_var%symb)
        if (ierror == 0) ierror = ndarray_create(nd_sctm,for_var%sctm)
        if (ierror == 0) ierror = py_var%setitem('sctm',nd_sctm)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_magnetic_form_type: Wrapping failed'
        end if

    End Subroutine Wrap_magnetic_form_type

    Module Subroutine Unwrap_xray_form_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(xray_form_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_xray_form_type: Cannot determine fortran type'
        else
            if (fortran_type == 'xray_form_type') then
                allocate(xray_form_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_xray_form_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_form_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_form_type','z',py_var,for_var%z,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_form_type','a',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_xray_form_type','a',p_real_1d,for_var%a,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_form_type','b',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_xray_form_type','b',p_real_1d,for_var%b,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_form_type','c',py_var,for_var%c,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_xray_form_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_xray_form_type

    Module Subroutine Wrap_xray_form_type(py_var,for_var,ierror)

        ! Arguments
        type(xray_form_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_a,nd_b

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('symb',for_var%symb)
        if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
        if (ierror == 0) ierror = ndarray_create(nd_a,for_var%a)
        if (ierror == 0) ierror = py_var%setitem('a',nd_a)
        if (ierror == 0) ierror = ndarray_create(nd_b,for_var%b)
        if (ierror == 0) ierror = py_var%setitem('b',nd_b)
        if (ierror == 0) ierror = py_var%setitem('c',for_var%c)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_xray_form_type: Wrapping failed'
        end if

    End Subroutine Wrap_xray_form_type

    Module Subroutine Unwrap_xray_wavelength_type(py_var,for_var,ierror)

        ! Arguments
        type(dict), intent(inout) :: py_var
        type(xray_wavelength_type), intent(out) :: for_var
        integer, intent(out) :: ierror

        ! Local variables
        character(len=:), allocatable :: fortran_type
        real, dimension(:), pointer :: p_real_1d

        ierror = 0
        ierror = py_var%getitem(fortran_type,'fortran_type')
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_xray_wavelength_type: Cannot determine fortran type'
        else
            if (fortran_type == 'xray_wavelength_type') then
                allocate(xray_wavelength_type :: for_var)
            else
                ierror = -1
                err_cfml%flag = .true.
                err_cfml%ierr = ierror
                err_cfml%msg  = 'Unwrap_xray_wavelength_type: Wrong fortran type:'//adjustl(trim(fortran_type))
            end if
        end if
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_wavelength_type','symb',py_var,for_var%symb,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_wavelength_type','kalfa',py_var,p_real_1d,ierror)
        if (ierror == 0) call pointer_to_array('Unwrap_xray_wavelength_type','kalfa',p_real_1d,for_var%kalfa,ierror)
        if (ierror == 0) call unwrap_dict_item('Unwrap_xray_wavelength_type','kbeta',py_var,for_var%kbeta,ierror)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Unwrap_xray_wavelength_type: Unwrapping failed'
        end if

    End Subroutine Unwrap_xray_wavelength_type

    Module Subroutine Wrap_xray_wavelength_type(py_var,for_var,ierror)

        ! Arguments
        type(xray_wavelength_type), intent(in) :: for_var
        type(dict), intent(inout) :: py_var
        integer, intent(out) :: ierror

        ! Local variables
        type(ndarray) :: nd_kalfa

        ierror = 0
        if (ierror == 0) ierror = py_var%setitem('symb',for_var%symb)
        if (ierror == 0) ierror = ndarray_create(nd_kalfa,for_var%kalfa)
        if (ierror == 0) ierror = py_var%setitem('kalfa',nd_kalfa)
        if (ierror == 0) ierror = py_var%setitem('kbeta',for_var%kbeta)
        if (ierror /= 0) then
            err_cfml%flag = .true.
            err_cfml%ierr = -1
            err_cfml%msg  = 'Wrap_xray_wavelength_type: Wrapping failed'
        end if

    End Subroutine Wrap_xray_wavelength_type

end submodule