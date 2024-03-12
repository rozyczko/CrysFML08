module py_cfml_bckpeaks

    use forpy
    use iso_c_binding
    use CFML_GlobalDeps, only: cp, Err_CFML, Set_Error
    use CFML_Maths,      only: Smoothing_Vec,spline_interpol, first_derivative, second_derivative
    use CFML_DiffPatt,   only: DiffPat_E_Type

    implicit none

    type(PythonModule), save :: mod_bckpeaks
    type(PythonMethodTable), save :: table_bckpeaks

    contains

    function PyInit_py_cfml_bckpeaks() bind(c,name='PyInit_py_py_cfml_bckpeaks') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_bckpeaks

    type(c_ptr) :: m

    end function PyInit_py_cfml_bckpeaks

end module py_cfml_bckpeaks