module py_cfml_metrics

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,  only: CP, EPS, PI, TO_RAD, Err_CFML
    Use CFML_Maths,       only: Inverse_Matrix, Determ_V, Determ, Cross_Product,                                   Co_Linear, Sort, Co_Prime, Swap     Use CFML_Strings,     only: U_Case, Get_Transf

    implicit none

    type(PythonModule), save :: mod_metrics
    type(PythonMethodTable), save :: table_metrics

    contains

    function PyInit_py_cfml_metrics() bind(c,name='PyInit_py_py_cfml_metrics') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_metrics

    type(c_ptr) :: m

    end function PyInit_py_cfml_metrics

end module py_cfml_metrics