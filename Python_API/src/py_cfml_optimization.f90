module py_cfml_optimization

    use forpy
    use iso_c_binding
    use CFML_GlobalDeps, only: cp, dp, Err_CFML, Clear_Error
    use CFML_Strings,    only: l_case

    implicit none

    type(PythonModule), save :: mod_optimization
    type(PythonMethodTable), save :: table_optimization

    contains

    function PyInit_py_cfml_optimization() bind(c,name='PyInit_py_py_cfml_optimization') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_optimization

    type(c_ptr) :: m

    end function PyInit_py_cfml_optimization

end module py_cfml_optimization