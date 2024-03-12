module py_cfml_simulated_annealing

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,       only: Cp, Err_CFML, clear_error
    use CFML_Messages,         mess => write_scroll_text
    use CFML_Strings,          only: u_case, File_Type, file_list_type

    implicit none

    type(PythonModule), save :: mod_simulated_annealing
    type(PythonMethodTable), save :: table_simulated_annealing

    contains

    function PyInit_py_cfml_simulated_annealing() bind(c,name='PyInit_py_py_cfml_simulated_annealing') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_simulated_annealing

    type(c_ptr) :: m

    end function PyInit_py_cfml_simulated_annealing

end module py_cfml_simulated_annealing