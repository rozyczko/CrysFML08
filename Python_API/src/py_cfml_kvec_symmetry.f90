module py_cfml_kvec_symmetry

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,         only: cp, dp, tpi, Err_CFML, Clear_Error, Set_Error
    Use CFML_Rational
    Use CFML_Maths,              only: Trace, Zbelong, Modulo_Lat, equal_matrix,                                                    Equal_Vector, Get_Cart_From_Spher,Determ3D     Use CFML_Symmetry_Tables,    only: ltr_a,ltr_b,ltr_c,ltr_i,ltr_r,ltr_f,Sys_cry,LATT
    Use CFML_gSpaceGroups,       only: SPG_Type, Set_SpaceGroup,                                        symmetry_symbol, get_stabilizer     Use CFML_Strings,            only: u_case, l_case, Frac_Trans_1Dig, Get_Separator_Pos,Pack_String,                                        Frac_Trans_2Dig, Get_Mat_From_Symb,                                              Get_Transf, file_list_type, string_fraction_2Dig     Use CFML_Atoms,              only: Allocate_mAtom_list, mAtom_List_Type
    Use CFML_Scattering_Tables,  only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form,                                        Magnetic_Form     Use CFML_Propagation_Vectors,only: K_Equiv_Minus_K
    Use CFML_Metrics,            only: Cell_G_Type, Set_Crystal_Cell

    implicit none

    type(PythonModule), save :: mod_kvec_symmetry
    type(PythonMethodTable), save :: table_kvec_symmetry

    contains

    function PyInit_py_cfml_kvec_symmetry() bind(c,name='PyInit_py_py_cfml_kvec_symmetry') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_kvec_symmetry

    type(c_ptr) :: m

    end function PyInit_py_cfml_kvec_symmetry

end module py_cfml_kvec_symmetry