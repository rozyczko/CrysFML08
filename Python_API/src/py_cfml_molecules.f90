module py_cfml_molecules

    use forpy
    use iso_c_binding
    use CFML_GlobalDeps,    only: CP, EPS, TO_RAD, err_cfml, clear_error, set_error
    use CFML_gSpacegroups,  only: SpG_type, Write_SpaceGroup_Info
    Use CFML_Atoms,         only: AtList_Type, Init_Atom_Type, Allocate_Atom_List,                                     Atm_Type, Atm_Std_Type, Atm_Ref_Type, ModAtm_Std_Type, ModAtm_Ref_Type     Use CFML_Metrics,       only: Cell_G_Type,  Write_Crystal_Cell
    Use CFML_Strings,       only: L_Case, U_Case, File_Type, Get_Num, Cut_String, Get_words
    Use CFML_Maths,         only: Cross_Product, Get_Spher_from_Cart
    Use CFML_Geom,          only: Angle_Dihedral, Distance, Get_PhiTheChi
    Use CFML_Scattering_Tables, only: NUM_CHEM_INFO, Set_Chem_Info, Remove_Chem_Info, Get_Chem_Symb, Chem_Info

    implicit none

    type(PythonModule), save :: mod_molecules
    type(PythonMethodTable), save :: table_molecules

    contains

    function PyInit_py_cfml_molecules() bind(c,name='PyInit_py_py_cfml_molecules') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_molecules

    type(c_ptr) :: m

    end function PyInit_py_cfml_molecules

end module py_cfml_molecules