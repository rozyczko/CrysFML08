module py_cfml_atoms

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps
    Use CFML_Rational
    Use CFML_Maths,        only: modulo_lat, equal_vector
    Use CFML_Metrics,      only: Cell_G_Type
    Use CFML_Strings,      only: u_case,l_case
    Use CFML_gSpaceGroups, only: spg_type, apply_op, SuperSpaceGroup_Type, Get_moment_ctr, Get_TFourier_ctr

    implicit none

    type(PythonModule), save :: mod_atoms
    type(PythonMethodTable), save :: table_atoms

    contains

    function PyInit_py_cfml_atoms() bind(c,name='PyInit_py_py_cfml_atoms') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_atoms

    type(c_ptr) :: m

    end function PyInit_py_cfml_atoms

end module py_cfml_atoms