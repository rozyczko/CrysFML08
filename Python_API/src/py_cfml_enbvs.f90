module py_cfml_enbvs

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,        only: Sp,Cp,dp,pi,tpi,clear_error,Err_CFML
    Use CFML_Rational
    Use CFML_Maths,             only: cross_product, Sort, Set_Eps_Math, epss
    use CFML_Strings,           only: Sort_Strings,Get_words, U_Case,pack_string
    Use CFML_Scattering_Tables, only: Get_Ionic_Radius, Get_Chem_Symb, Get_Z_Symb, Get_Covalent_Radius, Set_Chem_Info
    use CFML_Metrics,           only: Cell_G_Type
    use CFML_gSpaceGroups,      only: SPG_Type, Get_Orbit, Point_Orbit
    use CFML_Atoms,             only: Atm_Type, Init_Atom_type, Write_Atom_List, AtList_Type, Allocate_Atom_List,                                         Extend_Atom_List, Atm_Cell_Type, Allocate_Atoms_Cell     use CFML_Geom,              only: Coord_Info, Distance, calc_dist_angle_sigma, calc_dist_angle
    use CFML_Export_VTK,        only: write_grid_VESTA
    use CFML_BVS_Tables

    implicit none

    type(PythonModule), save :: mod_enbvs
    type(PythonMethodTable), save :: table_enbvs

    contains

    function PyInit_py_cfml_enbvs() bind(c,name='PyInit_py_py_cfml_enbvs') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_enbvs

    type(c_ptr) :: m

    end function PyInit_py_cfml_enbvs

end module py_cfml_enbvs