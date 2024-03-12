module py_cfml_reflections

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,                only: CP, PI, TPI, Err_CFML, Clear_Error
    Use CFML_gSpaceGroups,              only: Spg_Type,kvect_info_type, SuperSpaceGroup_type, Allocate_KVector
    Use CFML_Maths,                     only: Trace, Sort, Equal_vector
    Use CFML_Metrics,                   only: Cell_G_Type
    Use CFML_Strings,                   only: l_case
    Use CFML_Rational

    implicit none

    type(PythonModule), save :: mod_reflections
    type(PythonMethodTable), save :: table_reflections

    contains

    function PyInit_py_cfml_reflections() bind(c,name='PyInit_py_py_cfml_reflections') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_reflections

    type(c_ptr) :: m

    end function PyInit_py_cfml_reflections

end module py_cfml_reflections