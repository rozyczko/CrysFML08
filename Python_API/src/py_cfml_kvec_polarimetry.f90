module py_cfml_kvec_polarimetry

    use forpy
    use iso_c_binding
    Use CFML_GlobalDeps,                 only: cp, tpi, err_CFML
    Use CFML_Metrics,                    only: Set_Crystal_Cell, Cell_G_Type, Cart_Vector
    Use CFML_Maths,                      only: Cross_Product,Tensor_Product,Mat_Cross,Inverse_Matrix
    use CFML_kvec_Structure_Factors,     only: MagHD_Type
    use CFML_kvec_Symmetry,              only: Magnetic_domain_type
    use CFML_SXTAL_Geom,                 only: Phi_mat,Chi_mat, Psi_mat,Get_Angs_NB

    implicit none

    type(PythonModule), save :: mod_kvec_polarimetry
    type(PythonMethodTable), save :: table_kvec_polarimetry

    contains

    function PyInit_py_cfml_kvec_polarimetry() bind(c,name='PyInit_py_py_cfml_kvec_polarimetry') result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_kvec_polarimetry

    type(c_ptr) :: m

    end function PyInit_py_cfml_kvec_polarimetry

end module py_cfml_kvec_polarimetry