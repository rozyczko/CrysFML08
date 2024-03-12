module py_cfml_profiles

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only: CP, DP, PI, to_rad, to_deg
    Use CFML_Maths, only: erfc_deriv

    implicit none

    type(PythonModule), save :: mod_profiles
    type(PythonMethodTable), save :: table_profiles

    contains

    function PyInit_py_cfml_profiles() bind(c,name='PyInit_py_py_cfml_profiles') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_profiles

    type(c_ptr) :: m

    end function PyInit_py_cfml_profiles

end module py_cfml_profiles