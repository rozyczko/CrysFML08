module py_cfml_eos

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,only: CP, PI, TO_RAD, err_cfml, clear_error, set_error
    Use CFML_Maths,     only: Debye, First_Derivative, Second_Derivative, Spline_interpol, Diagonalize_SH,                                Orient_Eigenvectors, Locate     Use CFML_Metrics,   only: Cell_G_Type, Strain_Tensor_Type, Get_Cryst_Family, Set_Crystal_Cell, Cell_Type,                                Volume_from_Cell,SigmaV_From_Cell, Fix_Tensor, Calc_Paxes_Angles     Use CFML_Strings,   only: u_case, string_real, string_numstd, number_lines, get_words, get_numstd,                                get_separator_pos, get_num, reading_lines, read_key_str 
    implicit none

    type(PythonModule), save :: mod_eos
    type(PythonMethodTable), save :: table_eos

    contains

    function PyInit_py_cfml_eos() bind(c,name='PyInit_py_py_cfml_eos') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_eos

    type(c_ptr) :: m

    end function PyInit_py_cfml_eos

end module py_cfml_eos