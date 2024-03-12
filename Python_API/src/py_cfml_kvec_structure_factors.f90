module py_cfml_kvec_structure_factors

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,                  only: cp, sp, dp, tpi, Err_CFML, Clear_Error
    Use CFML_Maths,                       only: sort
    Use CFML_Strings,                     only: L_Case,U_Case
    Use CFML_Scattering_Tables,           only: Set_Magnetic_Form, Remove_Magnetic_Form, num_mag_form,                                                   Magnetic_Form     Use CFML_Metrics,                     only: Cell_G_Type
    Use CFML_gSpaceGroups,                only: SPG_Type, Set_SpaceGroup
    Use CFML_kvec_Symmetry,               only: ApplyMSO, MagSymm_k_type, Write_Magnetic_Structure,                                                   Magnetic_Domain_type     Use CFML_Reflections,                 only: Hkl_Gen_Sxtal, Get_MaxNumRef,                                                  Refl_Type, RefList_Type, h_s, Initialize_RefList     Use CFML_Atoms,                       only: Matom_list_type, Allocate_mAtom_list
    Use CFML_Propagation_Vectors,         only: K_Equiv_Minus_K
    Use CFML_Rational

    implicit none

    type(PythonModule), save :: mod_kvec_structure_factors
    type(PythonMethodTable), save :: table_kvec_structure_factors

    contains

    function PyInit_py_cfml_kvec_structure_factors() bind(c,name='PyInit_py_py_cfml_kvec_structure_factors') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_kvec_structure_factors

    type(c_ptr) :: m

    end function PyInit_py_cfml_kvec_structure_factors

end module py_cfml_kvec_structure_factors