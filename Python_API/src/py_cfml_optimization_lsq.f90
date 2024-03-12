module py_cfml_optimization_lsq

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,   only: Cp, Dp, Err_CFML, Clear_Error, line_term
    Use CFML_Maths,        only: Inverse_Matrix, Upper_Triangular,SVDcmp

    implicit none

    type(PythonModule), save :: mod_optimization_lsq
    type(PythonMethodTable), save :: table_optimization_lsq

    contains

    function PyInit_py_cfml_optimization_lsq() bind(c,name='PyInit_py_py_cfml_optimization_lsq') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_optimization_lsq

    type(c_ptr) :: m

    end function PyInit_py_cfml_optimization_lsq

end module py_cfml_optimization_lsq