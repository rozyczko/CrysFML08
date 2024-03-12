module py_cfml_maps

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,   only: CP, DP, EPS, Err_CFML, Clear_Error
    Use CFML_Strings,      only: l_case
    Use CFML_gSpaceGroups, only: SpG_Type, Apply_OP
    Use CFML_Metrics,      only: Cell_Type, Cell_G_Type
    Use CFML_Geom,         only: Distance

    implicit none

    type(PythonModule), save :: mod_maps
    type(PythonMethodTable), save :: table_maps

    contains

    function PyInit_py_cfml_maps() bind(c,name='PyInit_py_py_cfml_maps') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_maps

    type(c_ptr) :: m

    end function PyInit_py_cfml_maps

end module py_cfml_maps