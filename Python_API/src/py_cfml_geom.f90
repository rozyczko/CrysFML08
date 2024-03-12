module py_cfml_geom

    use forpy
    use iso_c_binding
    use CFML_GlobalDeps
    use CFML_Rational
    use CFML_Maths,         only: Modulo_Lat, Cross_Product, Inverse_Matrix, Determ3D
    use CFML_Strings,       only: Frac_Trans_1Dig, L_Case,U_Case,pack_string,String_NumStd
    use CFML_Metrics,       only: Cell_G_Type, Get_Deriv_Orth_Cell,Rot_Gibbs_Matrix
    use CFML_Atoms,         only: AtList_Type,Atm_Cell_Type,Equiv_Atm, Wrt_Lab, Atom_Equiv_List_Type,                                     Allocate_Atom_List, Atm_Std_Type     use CFML_gSpaceGroups,  only: Spg_Type, Apply_OP, Get_Multip_Pos, Is_Lattice_Vec,                                     searchop, Write_SymTrans_Code, Get_Orbit, Point_Orbit 
    implicit none

    type(PythonModule), save :: mod_geom
    type(PythonMethodTable), save :: table_geom

    contains

    function PyInit_py_cfml_geom() bind(c,name='PyInit_py_py_cfml_geom') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_geom

    type(c_ptr) :: m

    end function PyInit_py_cfml_geom

end module py_cfml_geom