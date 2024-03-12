module py_cfml_gspacegroups

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,        only: CP,DP, LI, EPS, err_cfml, clear_error, CFML_Debug,TPI
    Use CFML_Rational
    Use CFML_Symmetry_Tables
    Use CFML_Magnetic_Database
    Use CFML_SuperSpace_Database
    Use CFML_Maths,             only: Set_eps_math, modulo_lat, determ3D, Get_eps_math, Zbelong,EPSS,Diagonalize_RGEN,                                         equal_vector,resolv_sist_3x3,trace,Equal_Matrix, Inverse_Matrix,Lat_modulo     Use CFML_Strings,           only: u_case, l_case, pack_string, get_separator_pos, get_num,                                         get_words, String_Fraction_2Dig,Set_Symb_From_Mat, Get_Vec_from_FracStr 
    implicit none

    type(PythonModule), save :: mod_gspacegroups
    type(PythonMethodTable), save :: table_gspacegroups

    contains

    function PyInit_py_cfml_gspacegroups() bind(c,name='PyInit_py_py_cfml_gspacegroups') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_gspacegroups

    type(c_ptr) :: m

    end function PyInit_py_cfml_gspacegroups

end module py_cfml_gspacegroups