module py_cfml_ioform

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,        only: SP, CP, PI, EPS, TAB, Err_CFML, Clear_Error
    Use CFML_Maths,             only: Get_Eps_Math
    Use CFML_Rational
    Use CFML_Strings,           only: l_case, u_case, get_num, cut_string, get_words,                                        get_numstd, Read_Key_Value, Read_Key_ValueSTD,                                         string_numstd, Number_Lines, Reading_Lines,                                            FindFMT, Init_FindFMT, String_Array_Type,                                              File_type, Reading_File, Get_Transf,                                                   Get_Extension, Get_Datetime,Read_Fract,                                                Frac_Trans_2Dig, Pack_String, File_List_type,                                          String_Real, String_Count     Use CFML_Atoms,             only: Atm_Type, Atm_Std_Type, ModAtm_std_type, Atm_Ref_Type,                                        AtList_Type, Allocate_Atom_List, Init_Atom_Type, mAtom_Type,                                        mAtom_List_Type, Allocate_mAtom_list, deAllocate_mAtom_list     Use CFML_Metrics,           only: Cell_Type, Cell_G_Type, Cell_GLS_Type, Set_Crystal_Cell, U_equiv,                                        get_U_from_Betas, get_Betas_from_U, get_Betas_from_B     Use CFML_gSpaceGroups,      only: SpG_Type, SuperSpaceGroup_Type, Kvect_Info_Type,                                          Change_Setting_SpaceG, Set_SpaceGroup, Get_Multip_Pos,                                       Get_Orbit, Allocate_Kvector, Write_SpaceGroup_Info,                                        Get_Mat_from_Symb, Get_Symb_From_Mat, Get_Dimension_SymmOp,                                        Get_Symb_from_Rational_Mat     Use CFML_kvec_Symmetry,     only: MagSymm_k_Type, Readn_Set_Magnetic_Kv_Structure, Magnetic_Domain_type
    Use CFML_DiffPatt,          only: DiffPat_Type, DiffPat_E_Type
    Use CFML_Messages
    Use CFML_Scattering_Tables, only: Get_Z_Symb
    Use CFML_Molecules,         only: Molecule_type, Init_Molecule, Set_Euler_Matrix

    implicit none

    type(PythonModule), save :: mod_ioform
    type(PythonMethodTable), save :: table_ioform

    contains

    function PyInit_py_cfml_ioform() bind(c,name='PyInit_py_py_cfml_ioform') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_ioform

    type(c_ptr) :: m

    end function PyInit_py_cfml_ioform

end module py_cfml_ioform