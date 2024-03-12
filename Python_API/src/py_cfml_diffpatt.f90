module py_cfml_diffpatt

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only : cp, ops_sep, err_cfml, clear_error
    Use CFML_Maths,      only : spline_d2y, spline_interpol, locate, second_derivative, Linear_Interpol
    use CFML_Strings,    only : u_case, get_words, get_num, Get_NumStd

    implicit none

    type(PythonModule), save :: mod_diffpatt
    type(PythonMethodTable), save :: table_diffpatt

    contains

    function PyInit_py_cfml_diffpatt() bind(c,name='PyInit_py_py_cfml_diffpatt') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_diffpatt

    type(c_ptr) :: m

    end function PyInit_py_cfml_diffpatt

end module py_cfml_diffpatt