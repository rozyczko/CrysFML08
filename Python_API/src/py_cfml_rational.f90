module py_cfml_rational

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only : CP, LI, Err_CFML,Clear_Error
    Use CFML_Maths,      only : Inverse_Matrix
    Use CFML_Strings,    only : Pack_String

    implicit none

    type(PythonModule), save :: mod_rational
    type(PythonMethodTable), save :: table_rational

    contains

    function PyInit_py_cfml_rational() bind(c,name='PyInit_py_py_cfml_rational') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_rational

    type(c_ptr) :: m

    end function PyInit_py_cfml_rational

end module py_cfml_rational