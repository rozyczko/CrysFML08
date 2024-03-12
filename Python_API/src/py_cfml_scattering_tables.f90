module py_cfml_scattering_tables

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only: CP
    Use CFML_Strings,    only: U_Case, L_Case

    implicit none

    type(PythonModule), save :: mod_scattering_tables
    type(PythonMethodTable), save :: table_scattering_tables

    contains

    function PyInit_py_cfml_scattering_tables() bind(c,name='PyInit_py_py_cfml_scattering_tables') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_scattering_tables

    type(c_ptr) :: m

    end function PyInit_py_cfml_scattering_tables

end module py_cfml_scattering_tables