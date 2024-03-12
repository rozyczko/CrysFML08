module py_cfml_ill_instrm_data

    use forpy
    use iso_c_binding
    use, intrinsic :: iso_c_binding
    Use CFML_GlobalDeps
    Use CFML_Maths,    only: equal_vector, locate, second_derivative, Spline_Interpol, sort, invert => Inverse_Matrix,                               Cross_Product     use CFML_Strings,  only: u_case, l_case, Number_Lines, Reading_Lines, Get_Num
    use CFML_DiffPatt, only: DiffPat_E_Type, Allocate_Pattern

    implicit none

    type(PythonModule), save :: mod_ill_instrm_data
    type(PythonMethodTable), save :: table_ill_instrm_data

    contains

    function PyInit_py_cfml_ill_instrm_data() bind(c,name='PyInit_py_py_cfml_ill_instrm_data') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_ill_instrm_data

    type(c_ptr) :: m

    end function PyInit_py_cfml_ill_instrm_data

end module py_cfml_ill_instrm_data