module py_cfml_bonds_tables

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,        only: CP
    Use CFML_Scattering_Tables, only: Get_Chem_Symb, Get_Z_Symb

    implicit none

    type(PythonModule), save :: mod_bonds_tables
    type(PythonMethodTable), save :: table_bonds_tables

    contains

    function PyInit_py_cfml_bonds_tables() bind(c,name='PyInit_py_py_cfml_bonds_tables') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_bonds_tables

    type(c_ptr) :: m

    end function PyInit_py_cfml_bonds_tables

end module py_cfml_bonds_tables