module py_cfml_structure_factors

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,                  only: CP, DP, TPI, err_cfml, clear_error
    Use CFML_Strings,                     only: L_Case, U_Case, File_Type, File_list_Type
    Use CFML_Atoms,                       only: AtList_type, Atm_Ref_Type, ModAtm_Ref_Type
    Use CFML_gSpaceGroups,                only: Spg_Type
    Use CFML_Metrics,                     only: Cell_G_Type
    Use CFML_Reflections,                 only: RefList_Type, Refl_Type, SRefl_Type, MRefl_Type,                                                   Gener_Reflections_Shub, Gener_Reflections     Use CFML_Scattering_Tables
    Use CFML_Rational

    implicit none

    type(PythonModule), save :: mod_structure_factors
    type(PythonMethodTable), save :: table_structure_factors

    contains

    function PyInit_py_cfml_structure_factors() bind(c,name='PyInit_py_py_cfml_structure_factors') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_structure_factors

    type(c_ptr) :: m

    end function PyInit_py_cfml_structure_factors

end module py_cfml_structure_factors