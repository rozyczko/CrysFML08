module py_cfml_propagation_vectors

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,                only: Cp
    Use CFML_Maths,                     only: Zbelong, Equal_Vector, Eps => epss
    Use CFML_Rational
    Use CFML_gSpaceGroups,              only: SpG_Type, Set_SpaceGroup

    implicit none

    type(PythonModule), save :: mod_propagation_vectors
    type(PythonMethodTable), save :: table_propagation_vectors

    contains

    function PyInit_py_cfml_propagation_vectors() bind(c,name='PyInit_py_py_cfml_propagation_vectors') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_propagation_vectors

    type(c_ptr) :: m

    end function PyInit_py_cfml_propagation_vectors

end module py_cfml_propagation_vectors