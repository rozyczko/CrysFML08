module py_cfml_extincorr

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only: CP, Err_CFML, Clear_Error

    implicit none

    type(PythonModule), save :: mod_extincorr
    type(PythonMethodTable), save :: table_extincorr

    contains

    function PyInit_py_cfml_extincorr() bind(c,name='PyInit_py_py_cfml_extincorr') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_extincorr

    type(c_ptr) :: m

    end function PyInit_py_cfml_extincorr

end module py_cfml_extincorr