! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------


module api_init

    use forpy_mod
    use iso_fortran_env

    use py_cfml_atoms
    use py_cfml_scattering_tables
    use py_cfml_geom
    use py_cfml_extincorr
    use py_cfml_symmetry_tables
    use py_cfml_eos
    use py_cfml_random
    use py_cfml_reflections
    use py_cfml_magnetic_database
    use py_cfml_superspace_database
    use py_cfml_sxtal_geom
    use py_cfml_profiles
    use py_cfml_maths
    use py_cfml_gspacegroups

    implicit none

    type(PythonModule), save :: mod_Def
    type(PythonMethodTable), save :: method_Table

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_crysfml_api() bind(c,name="PyInit_crysfml_api") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml_api

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_crysfml_api

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call method_Table%init(65)
        call method_Table%add_method("equiv_atm",&
            "equiv_atm",METH_VARARGS,&
            c_funloc(py_equiv_atm))
        call method_Table%add_method("wrt_lab",&
            "wrt_lab",METH_VARARGS,&
            c_funloc(py_wrt_lab))
        call method_Table%add_method("get_abs_xs",&
            "get_abs_xs",METH_VARARGS,&
            c_funloc(py_get_abs_xs))
        call method_Table%add_method("get_atomic_mass",&
            "get_atomic_mass",METH_VARARGS,&
            c_funloc(py_get_atomic_mass))
        call method_Table%add_method("get_atomic_vol",&
            "get_atomic_vol",METH_VARARGS,&
            c_funloc(py_get_atomic_vol))
        call method_Table%add_method("get_chem_symb",&
            "get_chem_symb",METH_VARARGS,&
            c_funloc(py_get_chem_symb))
        call method_Table%add_method("get_covalent_radius",&
            "get_covalent_radius",METH_VARARGS,&
            c_funloc(py_get_covalent_radius))
        call method_Table%add_method("get_fermi_length",&
            "get_fermi_length",METH_VARARGS,&
            c_funloc(py_get_fermi_length))
        call method_Table%add_method("get_inc_xs",&
            "get_inc_xs",METH_VARARGS,&
            c_funloc(py_get_inc_xs))
        call method_Table%add_method("get_ionic_radius",&
            "get_ionic_radius",METH_VARARGS,&
            c_funloc(py_get_ionic_radius))
        call method_Table%add_method("get_z_symb",&
            "get_z_symb",METH_VARARGS,&
            c_funloc(py_get_z_symb))
        call method_Table%add_method("allocate_coordination_type",&
            "allocate_coordination_type",METH_VARARGS,&
            c_funloc(py_allocate_coordination_type))
        call method_Table%add_method("ag_theta",&
            "ag_theta",METH_VARARGS,&
            c_funloc(py_ag_theta))
        call method_Table%add_method("al_theta",&
            "al_theta",METH_VARARGS,&
            c_funloc(py_al_theta))
        call method_Table%add_method("bg_theta",&
            "bg_theta",METH_VARARGS,&
            c_funloc(py_bg_theta))
        call method_Table%add_method("bl_theta",&
            "bl_theta",METH_VARARGS,&
            c_funloc(py_bl_theta))
        call method_Table%add_method("get_compact_hm",&
            "get_compact_hm",METH_VARARGS,&
            c_funloc(py_get_compact_hm))
        call method_Table%add_method("get_hm_compact_hm",&
            "get_hm_compact_hm",METH_VARARGS,&
            c_funloc(py_get_hm_compact_hm))
        call method_Table%add_method("get_it_generators",&
            "get_it_generators",METH_VARARGS,&
            c_funloc(py_get_it_generators))
        call method_Table%add_method("get_spacegroup_symbols",&
            "get_spacegroup_symbols",METH_VARARGS,&
            c_funloc(py_get_spacegroup_symbols))
        call method_Table%add_method("thermal_pressure_eos",&
            "thermal_pressure_eos",METH_VARARGS,&
            c_funloc(py_thermal_pressure_eos))
        call method_Table%add_method("random_binomial1",&
            "random_binomial1",METH_VARARGS,&
            c_funloc(py_random_binomial1))
        call method_Table%add_method("random_binomial2",&
            "random_binomial2",METH_VARARGS,&
            c_funloc(py_random_binomial2))
        call method_Table%add_method("random_chisq",&
            "random_chisq",METH_VARARGS,&
            c_funloc(py_random_chisq))
        call method_Table%add_method("random_gamma",&
            "random_gamma",METH_VARARGS,&
            c_funloc(py_random_gamma))
        call method_Table%add_method("random_gamma1",&
            "random_gamma1",METH_VARARGS,&
            c_funloc(py_random_gamma1))
        call method_Table%add_method("random_gamma2",&
            "random_gamma2",METH_VARARGS,&
            c_funloc(py_random_gamma2))
        call method_Table%add_method("random_neg_binomial",&
            "random_neg_binomial",METH_VARARGS,&
            c_funloc(py_random_neg_binomial))
        call method_Table%add_method("random_poisson",&
            "random_poisson",METH_VARARGS,&
            c_funloc(py_random_poisson))
        call method_Table%add_method("random_t",&
            "random_t",METH_VARARGS,&
            c_funloc(py_random_t))
        call method_Table%add_method("random_von_mises",&
            "random_von_mises",METH_VARARGS,&
            c_funloc(py_random_von_mises))
        call method_Table%add_method("random_weibull",&
            "random_weibull",METH_VARARGS,&
            c_funloc(py_random_weibull))
        call method_Table%add_method("get_maxnumref",&
            "get_maxnumref",METH_VARARGS,&
            c_funloc(py_get_maxnumref))
        call method_Table%add_method("read_magnetic_binary",&
            "read_magnetic_binary",METH_VARARGS,&
            c_funloc(py_read_magnetic_binary))
        call method_Table%add_method("read_magnetic_data",&
            "read_magnetic_data",METH_VARARGS,&
            c_funloc(py_read_magnetic_data))
        call method_Table%add_method("read_ssg_dbase",&
            "read_ssg_dbase",METH_VARARGS,&
            c_funloc(py_read_ssg_dbase))
        call method_Table%add_method("read_single_ssg",&
            "read_single_ssg",METH_VARARGS,&
            c_funloc(py_read_single_ssg))
        call method_Table%add_method("chkin180",&
            "chkin180",METH_VARARGS,&
            c_funloc(py_chkin180))
        call method_Table%add_method("psd_convert",&
            "psd_convert",METH_VARARGS,&
            c_funloc(py_psd_convert))
        call method_Table%add_method("d19psd",&
            "d19psd",METH_VARARGS,&
            c_funloc(py_d19psd))
        call method_Table%add_method("set_psd",&
            "set_psd",METH_VARARGS,&
            c_funloc(py_set_psd))
        call method_Table%add_method("psvoigtian",&
            "psvoigtian",METH_VARARGS,&
            c_funloc(py_psvoigtian))
        call method_Table%add_method("prof_val",&
            "prof_val",METH_VARARGS,&
            c_funloc(py_prof_val))
        call method_Table%add_method("prof_gaussian",&
            "prof_gaussian",METH_VARARGS,&
            c_funloc(py_prof_gaussian))
        call method_Table%add_method("prof_lorentzian",&
            "prof_lorentzian",METH_VARARGS,&
            c_funloc(py_prof_lorentzian))
        call method_Table%add_method("erfc_deriv",&
            "erfc_deriv",METH_VARARGS,&
            c_funloc(py_erfc_deriv))
        call method_Table%add_method("debye",&
            "debye",METH_VARARGS,&
            c_funloc(py_debye))
        call method_Table%add_method("factorial_i",&
            "factorial_i",METH_VARARGS,&
            c_funloc(py_factorial_i))
        call method_Table%add_method("factorial_r",&
            "factorial_r",METH_VARARGS,&
            c_funloc(py_factorial_r))
        call method_Table%add_method("gcd",&
            "gcd",METH_VARARGS,&
            c_funloc(py_gcd))
        call method_Table%add_method("lcm",&
            "lcm",METH_VARARGS,&
            c_funloc(py_lcm))
        call method_Table%add_method("poly_legendre",&
            "poly_legendre",METH_VARARGS,&
            c_funloc(py_poly_legendre))
        call method_Table%add_method("cubic_harm_ang",&
            "cubic_harm_ang",METH_VARARGS,&
            c_funloc(py_cubic_harm_ang))
        call method_Table%add_method("integral_slater_bessel",&
            "integral_slater_bessel",METH_VARARGS,&
            c_funloc(py_integral_slater_bessel))
        call method_Table%add_method("real_spher_harm_ang",&
            "real_spher_harm_ang",METH_VARARGS,&
            c_funloc(py_real_spher_harm_ang))
        call method_Table%add_method("set_eps_math",&
            "set_eps_math",METH_VARARGS,&
            c_funloc(py_set_eps_math))
        call method_Table%add_method("get_dimension_symmop",&
            "get_dimension_symmop",METH_VARARGS,&
            c_funloc(py_get_dimension_symmop))
        call method_Table%add_method("get_hm_standard",&
            "get_hm_standard",METH_VARARGS,&
            c_funloc(py_get_hm_standard))
        call method_Table%add_method("iso_to_jones_notation",&
            "iso_to_jones_notation",METH_VARARGS,&
            c_funloc(py_iso_to_jones_notation))
        call method_Table%add_method("get_laue_num",&
            "get_laue_num",METH_VARARGS,&
            c_funloc(py_get_laue_num))
        call method_Table%add_method("get_laue_str",&
            "get_laue_str",METH_VARARGS,&
            c_funloc(py_get_laue_str))
        call method_Table%add_method("get_pointgroup_num",&
            "get_pointgroup_num",METH_VARARGS,&
            c_funloc(py_get_pointgroup_num))
        call method_Table%add_method("get_pointgroup_str",&
            "get_pointgroup_str",METH_VARARGS,&
            c_funloc(py_get_pointgroup_str))
        call method_Table%add_method("get_magpg_from_bns",&
            "get_magpg_from_bns",METH_VARARGS,&
            c_funloc(py_get_magpg_from_bns))
        call method_Table%add_method("set_conditions_numop_eps",&
            "set_conditions_numop_eps",METH_VARARGS,&
            c_funloc(py_set_conditions_numop_eps))

        ! Build mod_Def
        m = mod_Def%init("pycrysfml",&
            "A Python API for CrysFML08",method_Table)
    end function Init

end module api_init
