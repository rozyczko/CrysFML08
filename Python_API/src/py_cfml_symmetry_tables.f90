module py_cfml_symmetry_tables

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only: CP
    Use CFML_Strings,    only: u_case, pack_string, string_count

    implicit none

    type(PythonModule), save :: mod_symmetry_tables
    type(PythonMethodTable), save :: table_symmetry_tables

    contains

    function PyInit_py_cfml_symmetry_tables() bind(c,name='PyInit_py_py_cfml_symmetry_tables') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_symmetry_tables

    type(c_ptr) :: m

    end function PyInit_py_cfml_symmetry_tables

end module py_cfml_symmetry_tables