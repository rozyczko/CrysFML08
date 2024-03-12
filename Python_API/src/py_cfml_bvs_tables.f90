module py_cfml_bvs_tables

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps, only: CP, DP

    implicit none

    type(PythonModule), save :: mod_bvs_tables
    type(PythonMethodTable), save :: table_bvs_tables

    contains

    function PyInit_py_cfml_bvs_tables() bind(c,name='PyInit_py_py_cfml_bvs_tables') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_bvs_tables

    type(c_ptr) :: m

    end function PyInit_py_cfml_bvs_tables

end module py_cfml_bvs_tables