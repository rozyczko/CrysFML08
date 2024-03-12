module py_cfml_sxtal_geom

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,        Only: Cp,Dp,To_Deg,To_Rad, clear_error, Err_CFML,ops_sep
    use CFML_gSpacegroups,      Only: Spg_Type,Set_Spacegroup
    Use CFML_Maths,             Only: Cross_Product, invert => Inverse_Matrix,co_linear,in_limits,sort
    Use CFML_Metrics,           Only: Cell_G_Type, Zone_Axis_type, Get_basis_from_uvw,                                         Rot_Gibbs_Matrix,Set_Crystal_Cell     Use CFML_Geom,              Only: Get_OmegaChiPhi, Get_Matrix_moving_v_to_u,                                        Get_Anglen_Axis_From_Rotmat, Set_Rotation_Matrix     Use CFML_ILL_Instrm_data,   Only: Current_Orient, Current_Instrm, SXTAL_Numor_type,                                         Diffractometer_Type     use CFML_Reflections,       Only: Reflist_Type,Hkl_Gen_Sxtal
    Use CFML_Strings,           Only: file_type,l_case

    implicit none

    type(PythonModule), save :: mod_sxtal_geom
    type(PythonMethodTable), save :: table_sxtal_geom

    contains

    function PyInit_py_cfml_sxtal_geom() bind(c,name='PyInit_py_py_cfml_sxtal_geom') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_sxtal_geom

    type(c_ptr) :: m

    end function PyInit_py_cfml_sxtal_geom

end module py_cfml_sxtal_geom