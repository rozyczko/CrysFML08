
Module CFML_Wraps

    use cfml_globaldeps
    use CFML_Maps, only: cube_info_type
    use CFML_BVS_Tables, only: atomic_properties_type,bvel_par_type,bvs_par_type,sbvs_par_type
    use CFML_ILL_Instrm_Data, only: basic_numc_type,basic_numi_type,basic_numr_type,calibration_detector_type,diffractometer_type,generic_numor_type,ill_data_record_type,powder_numor_type,sxtal_numor_type,sxtal_orient_type
    use CFML_Optimization_LSQ, only: lsq_conditions_type,lsq_data_type,lsq_state_vector_type
    use CFML_Scattering_Tables, only: anomalous_sc_type,chem_info_type,magnetic_form_type,xray_form_type,xray_wavelength_type
    use CFML_BckPeaks, only: pkb_type,peak_search_cond_type
    use CFML_SXTAL_Geom, only: psd_val_type,sxd_val_type,twin_type
    use CFML_Optimization, only: opt_conditions_type
    use CFML_Propagation_Vectors, only: group_k_type
    use CFML_DiffPatt, only: diffpat_type,diffpat_e_type,diffpat_g_type
    use CFML_kvec_Polarimetry, only: polar_calc_type,polar_calc_list_type,polar_calcmulti_list_type,polar_info_type,polar_obs_type,polar_obs_list_type,polar_obsmulti_list_type,polar_calc_svs_type,polar_calc_svs_list_type,polar_calcmulti_svs_list_type
    use CFML_Rational, only: rational
    use CFML_Metrics, only: cell_type,cell_g_type,cell_ls_type,cell_gls_type,twofold_axes_type,zone_axis_type,strain_tensor_type
    use CFML_gSpaceGroups, only: symm_oper_type,group_type,spg_type,superspacegroup_type,kvect_info_type,point_orbit
    use CFML_Geom, only: coordination_type,point_list_type
    use CFML_kvec_Symmetry, only: sym_oper_type,msym_oper_type,magnetic_domain_type,magsymm_k_type
    use CFML_Reflections, only: refl_type,srefl_type,mrefl_type,reflist_type
    use CFML_Symmetry_Tables, only: shub_spgr_info_type,spgr_info_type,table_equiv_type,wyck_info_type
    use CFML_Molecules, only: molecule_type,molcrystal_type
    use CFML_IOForm, only: interval_type,job_info_type,blockinfo_type,genvec_type
    use CFML_EnBVS, only: atoms_conf_list_type
    use CFML_Profiles, only: deriv_tof_type
    use CFML_Simulated_Annealing, only: multistate_vector_type,simann_conditions_type,state_vector_type
    use CFML_Atoms, only: atm_type,atm_std_type,modatm_std_type,atm_ref_type,modatm_ref_type,atm_cell_type,atom_equiv_type,atom_equiv_list_type,atlist_type,matom_type,matom_list_type
    use CFML_Structure_Factors, only: scattering_species_type,strf_type,strflist_type
    use CFML_kvec_Structure_Factors, only: magh_type,magh_list_type,maghd_type,maghd_list_type
    use CFML_EoS, only: pvt_table,eos_type,eos_list_type,eos_cell_type,axis_type,eos_data_type,eos_data_list_type

    implicit none

    private

    public :: unwrap_cube_info_type,unwrap_atomic_properties_type,unwrap_bvel_par_type,unwrap_bvs_par_type,unwrap_sbvs_par_type,&
              unwrap_basic_numc_type,unwrap_basic_numi_type,unwrap_basic_numr_type,unwrap_calibration_detector_type,unwrap_diffractometer_type,&
              unwrap_generic_numor_type,unwrap_ill_data_record_type,unwrap_powder_numor_type,unwrap_sxtal_numor_type,unwrap_sxtal_orient_type,&
              unwrap_lsq_conditions_type,unwrap_lsq_data_type,unwrap_lsq_state_vector_type,unwrap_anomalous_sc_type,unwrap_chem_info_type,&
              unwrap_magnetic_form_type,unwrap_xray_form_type,unwrap_xray_wavelength_type,unwrap_pkb_type,unwrap_peak_search_cond_type,&
              unwrap_psd_val_type,unwrap_sxd_val_type,unwrap_twin_type,unwrap_opt_conditions_type,unwrap_group_k_type,&
              unwrap_diffpat_type,unwrap_polar_calc_type,unwrap_polar_calc_list_type,unwrap_polar_calcmulti_list_type,unwrap_polar_info_type,&
              unwrap_polar_obs_type,unwrap_polar_obs_list_type,unwrap_polar_obsmulti_list_type,unwrap_polar_calc_svs_type,unwrap_polar_calc_svs_list_type,&
              unwrap_polar_calcmulti_svs_list_type,unwrap_rational,unwrap_cell_type,unwrap_twofold_axes_type,unwrap_zone_axis_type,&
              unwrap_strain_tensor_type,unwrap_symm_oper_type,unwrap_group_type,unwrap_kvect_info_type,unwrap_point_orbit,&
              unwrap_coordination_type,unwrap_point_list_type,unwrap_sym_oper_type,unwrap_msym_oper_type,unwrap_magnetic_domain_type,&
              unwrap_magsymm_k_type,unwrap_refl_type,unwrap_reflist_type,unwrap_shub_spgr_info_type,unwrap_spgr_info_type,&
              unwrap_table_equiv_type,unwrap_wyck_info_type,unwrap_molecule_type,unwrap_molcrystal_type,unwrap_interval_type,&
              unwrap_job_info_type,unwrap_blockinfo_type,unwrap_genvec_type,unwrap_atoms_conf_list_type,unwrap_deriv_tof_type,&
              unwrap_multistate_vector_type,unwrap_simann_conditions_type,unwrap_state_vector_type,unwrap_atm_type,unwrap_atm_cell_type,&
              unwrap_atom_equiv_type,unwrap_atom_equiv_list_type,unwrap_atlist_type,unwrap_matom_type,unwrap_matom_list_type,&
              unwrap_scattering_species_type,unwrap_strf_type,unwrap_strflist_type,unwrap_magh_type,unwrap_magh_list_type,&
              unwrap_maghd_type,unwrap_maghd_list_type,unwrap_pvt_table,unwrap_eos_type,unwrap_eos_list_type,&
              unwrap_eos_cell_type,unwrap_axis_type,unwrap_eos_data_type,unwrap_eos_data_list_type
    public :: unwrap_diffpat_type_no_alloc,unwrap_cell_type_no_alloc,unwrap_group_type_no_alloc,unwrap_refl_type_no_alloc,unwrap_atm_type_no_alloc
    public :: wrap_cube_info_type,wrap_atomic_properties_type,wrap_bvel_par_type,wrap_bvs_par_type,wrap_sbvs_par_type,&
              wrap_basic_numc_type,wrap_basic_numi_type,wrap_basic_numr_type,wrap_calibration_detector_type,wrap_diffractometer_type,&
              wrap_generic_numor_type,wrap_ill_data_record_type,wrap_powder_numor_type,wrap_sxtal_numor_type,wrap_sxtal_orient_type,&
              wrap_lsq_conditions_type,wrap_lsq_data_type,wrap_lsq_state_vector_type,wrap_anomalous_sc_type,wrap_chem_info_type,&
              wrap_magnetic_form_type,wrap_xray_form_type,wrap_xray_wavelength_type,wrap_pkb_type,wrap_peak_search_cond_type,&
              wrap_psd_val_type,wrap_sxd_val_type,wrap_twin_type,wrap_opt_conditions_type,wrap_group_k_type,&
              wrap_diffpat_type,wrap_polar_calc_type,wrap_polar_calc_list_type,wrap_polar_calcmulti_list_type,wrap_polar_info_type,&
              wrap_polar_obs_type,wrap_polar_obs_list_type,wrap_polar_obsmulti_list_type,wrap_polar_calc_svs_type,wrap_polar_calc_svs_list_type,&
              wrap_polar_calcmulti_svs_list_type,wrap_rational,wrap_cell_type,wrap_twofold_axes_type,wrap_zone_axis_type,&
              wrap_strain_tensor_type,wrap_symm_oper_type,wrap_group_type,wrap_kvect_info_type,wrap_point_orbit,&
              wrap_coordination_type,wrap_point_list_type,wrap_sym_oper_type,wrap_msym_oper_type,wrap_magnetic_domain_type,&
              wrap_magsymm_k_type,wrap_refl_type,wrap_reflist_type,wrap_shub_spgr_info_type,wrap_spgr_info_type,&
              wrap_table_equiv_type,wrap_wyck_info_type,wrap_molecule_type,wrap_molcrystal_type,wrap_interval_type,&
              wrap_job_info_type,wrap_blockinfo_type,wrap_genvec_type,wrap_atoms_conf_list_type,wrap_deriv_tof_type,&
              wrap_multistate_vector_type,wrap_simann_conditions_type,wrap_state_vector_type,wrap_atm_type,wrap_atm_cell_type,&
              wrap_atom_equiv_type,wrap_atom_equiv_list_type,wrap_atlist_type,wrap_matom_type,wrap_matom_list_type,&
              wrap_scattering_species_type,wrap_strf_type,wrap_strflist_type,wrap_magh_type,wrap_magh_list_type,&
              wrap_maghd_type,wrap_maghd_list_type,wrap_pvt_table,wrap_eos_type,wrap_eos_list_type,&
              wrap_eos_cell_type,wrap_axis_type,wrap_eos_data_type,wrap_eos_data_list_type

    interface

        module subroutine unwrap_cube_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(cube_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_cube_info_type

        module subroutine unwrap_atomic_properties_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atomic_properties_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atomic_properties_type

        module subroutine unwrap_bvel_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvel_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_bvel_par_type

        module subroutine unwrap_bvs_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_bvs_par_type

        module subroutine unwrap_sbvs_par_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sbvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_sbvs_par_type

        module subroutine unwrap_basic_numc_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_basic_numc_type

        module subroutine unwrap_basic_numi_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numi_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_basic_numi_type

        module subroutine unwrap_basic_numr_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numr_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_basic_numr_type

        module subroutine unwrap_calibration_detector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(calibration_detector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_calibration_detector_type

        module subroutine unwrap_diffractometer_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(diffractometer_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_diffractometer_type

        module subroutine unwrap_generic_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(generic_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_generic_numor_type

        module subroutine unwrap_ill_data_record_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(ill_data_record_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_ill_data_record_type

        module subroutine unwrap_powder_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(powder_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_powder_numor_type

        module subroutine unwrap_sxtal_numor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_sxtal_numor_type

        module subroutine unwrap_sxtal_orient_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_orient_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_sxtal_orient_type

        module subroutine unwrap_lsq_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_lsq_conditions_type

        module subroutine unwrap_lsq_data_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_data_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_lsq_data_type

        module subroutine unwrap_lsq_state_vector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_state_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_lsq_state_vector_type

        module subroutine unwrap_anomalous_sc_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(anomalous_sc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_anomalous_sc_type

        module subroutine unwrap_chem_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(chem_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_chem_info_type

        module subroutine unwrap_magnetic_form_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_magnetic_form_type

        module subroutine unwrap_xray_form_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_xray_form_type

        module subroutine unwrap_xray_wavelength_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_wavelength_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_xray_wavelength_type

        module subroutine unwrap_pkb_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pkb_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_pkb_type

        module subroutine unwrap_peak_search_cond_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peak_search_cond_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_peak_search_cond_type

        module subroutine unwrap_psd_val_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(psd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_psd_val_type

        module subroutine unwrap_sxd_val_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_sxd_val_type

        module subroutine unwrap_twin_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twin_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_twin_type

        module subroutine unwrap_opt_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(opt_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_opt_conditions_type

        module subroutine unwrap_group_k_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(group_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_group_k_type

        module subroutine unwrap_diffpat_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_diffpat_type

        module subroutine unwrap_polar_calc_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calc_type

        module subroutine unwrap_polar_calc_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calc_list_type

        module subroutine unwrap_polar_calcmulti_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calcmulti_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calcmulti_list_type

        module subroutine unwrap_polar_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_info_type

        module subroutine unwrap_polar_obs_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obs_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_obs_type

        module subroutine unwrap_polar_obs_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_obs_list_type

        module subroutine unwrap_polar_obsmulti_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obsmulti_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_obsmulti_list_type

        module subroutine unwrap_polar_calc_svs_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_svs_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calc_svs_type

        module subroutine unwrap_polar_calc_svs_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_svs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calc_svs_list_type

        module subroutine unwrap_polar_calcmulti_svs_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calcmulti_svs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_polar_calcmulti_svs_list_type

        module subroutine unwrap_rational(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(rational), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_rational

        module subroutine unwrap_cell_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_cell_type

        module subroutine unwrap_twofold_axes_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twofold_axes_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_twofold_axes_type

        module subroutine unwrap_zone_axis_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(zone_axis_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_zone_axis_type

        module subroutine unwrap_strain_tensor_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strain_tensor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_strain_tensor_type

        module subroutine unwrap_symm_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(symm_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_symm_oper_type

        module subroutine unwrap_group_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(group_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_group_type

        module subroutine unwrap_kvect_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(kvect_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_kvect_info_type

        module subroutine unwrap_point_orbit(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(point_orbit), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_point_orbit

        module subroutine unwrap_coordination_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_coordination_type

        module subroutine unwrap_point_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(point_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_point_list_type

        module subroutine unwrap_sym_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_sym_oper_type

        module subroutine unwrap_msym_oper_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(msym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_msym_oper_type

        module subroutine unwrap_magnetic_domain_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_domain_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_magnetic_domain_type

        module subroutine unwrap_magsymm_k_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magsymm_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_magsymm_k_type

        module subroutine unwrap_refl_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refl_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_refl_type

        module subroutine unwrap_reflist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(reflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_reflist_type

        module subroutine unwrap_shub_spgr_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(shub_spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_shub_spgr_info_type

        module subroutine unwrap_spgr_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_spgr_info_type

        module subroutine unwrap_table_equiv_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(table_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_table_equiv_type

        module subroutine unwrap_wyck_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(wyck_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_wyck_info_type

        module subroutine unwrap_molecule_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molecule_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_molecule_type

        module subroutine unwrap_molcrystal_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molcrystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_molcrystal_type

        module subroutine unwrap_interval_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(interval_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_interval_type

        module subroutine unwrap_job_info_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(job_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_job_info_type

        module subroutine unwrap_blockinfo_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(blockinfo_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_blockinfo_type

        module subroutine unwrap_genvec_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(genvec_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_genvec_type

        module subroutine unwrap_atoms_conf_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atoms_conf_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atoms_conf_list_type

        module subroutine unwrap_deriv_tof_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(deriv_tof_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_deriv_tof_type

        module subroutine unwrap_multistate_vector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(multistate_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_multistate_vector_type

        module subroutine unwrap_simann_conditions_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(simann_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_simann_conditions_type

        module subroutine unwrap_state_vector_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(state_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_state_vector_type

        module subroutine unwrap_atm_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_type), allocatable, intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atm_type

        module subroutine unwrap_atm_cell_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atm_cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atm_cell_type

        module subroutine unwrap_atom_equiv_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atom_equiv_type

        module subroutine unwrap_atom_equiv_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atom_equiv_list_type

        module subroutine unwrap_atlist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atlist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atlist_type

        module subroutine unwrap_matom_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_matom_type

        module subroutine unwrap_matom_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_matom_list_type

        module subroutine unwrap_scattering_species_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(scattering_species_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_scattering_species_type

        module subroutine unwrap_strf_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strf_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_strf_type

        module subroutine unwrap_strflist_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_strflist_type

        module subroutine unwrap_magh_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magh_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_magh_type

        module subroutine unwrap_magh_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magh_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_magh_list_type

        module subroutine unwrap_maghd_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(maghd_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_maghd_type

        module subroutine unwrap_maghd_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(maghd_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_maghd_list_type

        module subroutine unwrap_pvt_table(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pvt_table), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_pvt_table

        module subroutine unwrap_eos_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_eos_type

        module subroutine unwrap_eos_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_eos_list_type

        module subroutine unwrap_eos_cell_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_eos_cell_type

        module subroutine unwrap_axis_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(axis_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_axis_type

        module subroutine unwrap_eos_data_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_data_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_eos_data_type

        module subroutine unwrap_eos_data_list_type(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_data_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_eos_data_list_type

        module subroutine unwrap_diffpat_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_diffpat_type_no_alloc

        module subroutine unwrap_cell_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_cell_type_no_alloc

        module subroutine unwrap_group_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(group_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_group_type_no_alloc

        module subroutine unwrap_refl_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refl_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_refl_type_no_alloc

        module subroutine unwrap_atm_type_no_alloc(py_var,for_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine unwrap_atm_type_no_alloc

        module subroutine wrap_cube_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(cube_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_cube_info_type

        module subroutine wrap_atomic_properties_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atomic_properties_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atomic_properties_type

        module subroutine wrap_bvel_par_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvel_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvel_par_type

        module subroutine wrap_bvs_par_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(bvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvs_par_type

        module subroutine wrap_sbvs_par_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sbvs_par_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_sbvs_par_type

        module subroutine wrap_basic_numc_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numc_type

        module subroutine wrap_basic_numi_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numi_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numi_type

        module subroutine wrap_basic_numr_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(basic_numr_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numr_type

        module subroutine wrap_calibration_detector_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(calibration_detector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_calibration_detector_type

        module subroutine wrap_diffractometer_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(diffractometer_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffractometer_type

        module subroutine wrap_generic_numor_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(generic_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_generic_numor_type

        module subroutine wrap_ill_data_record_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(ill_data_record_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_ill_data_record_type

        module subroutine wrap_powder_numor_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(powder_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_powder_numor_type

        module subroutine wrap_sxtal_numor_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_numor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_numor_type

        module subroutine wrap_sxtal_orient_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxtal_orient_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_orient_type

        module subroutine wrap_lsq_conditions_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_conditions_type

        module subroutine wrap_lsq_data_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_data_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_data_type

        module subroutine wrap_lsq_state_vector_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(lsq_state_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_state_vector_type

        module subroutine wrap_anomalous_sc_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(anomalous_sc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_anomalous_sc_type

        module subroutine wrap_chem_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(chem_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_chem_info_type

        module subroutine wrap_magnetic_form_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_form_type

        module subroutine wrap_xray_form_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_form_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_form_type

        module subroutine wrap_xray_wavelength_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(xray_wavelength_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_wavelength_type

        module subroutine wrap_pkb_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pkb_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_pkb_type

        module subroutine wrap_peak_search_cond_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(peak_search_cond_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_peak_search_cond_type

        module subroutine wrap_psd_val_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(psd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_psd_val_type

        module subroutine wrap_sxd_val_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sxd_val_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxd_val_type

        module subroutine wrap_twin_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twin_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_twin_type

        module subroutine wrap_opt_conditions_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(opt_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_opt_conditions_type

        module subroutine wrap_group_k_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(group_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_k_type

        module subroutine wrap_diffpat_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            class(diffpat_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffpat_type

        module subroutine wrap_polar_calc_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_type

        module subroutine wrap_polar_calc_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_list_type

        module subroutine wrap_polar_calcmulti_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calcmulti_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calcmulti_list_type

        module subroutine wrap_polar_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_info_type

        module subroutine wrap_polar_obs_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obs_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obs_type

        module subroutine wrap_polar_obs_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obs_list_type

        module subroutine wrap_polar_obsmulti_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_obsmulti_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obsmulti_list_type

        module subroutine wrap_polar_calc_svs_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_svs_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_svs_type

        module subroutine wrap_polar_calc_svs_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calc_svs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_svs_list_type

        module subroutine wrap_polar_calcmulti_svs_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(polar_calcmulti_svs_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calcmulti_svs_list_type

        module subroutine wrap_rational(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(rational), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_rational

        module subroutine wrap_cell_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            class(cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_cell_type

        module subroutine wrap_twofold_axes_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(twofold_axes_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_twofold_axes_type

        module subroutine wrap_zone_axis_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(zone_axis_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_zone_axis_type

        module subroutine wrap_strain_tensor_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strain_tensor_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_strain_tensor_type

        module subroutine wrap_symm_oper_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(symm_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_symm_oper_type

        module subroutine wrap_group_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            class(group_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_type

        module subroutine wrap_kvect_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(kvect_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_kvect_info_type

        module subroutine wrap_point_orbit(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(point_orbit), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_orbit

        module subroutine wrap_coordination_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(coordination_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_type

        module subroutine wrap_point_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(point_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_list_type

        module subroutine wrap_sym_oper_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(sym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_sym_oper_type

        module subroutine wrap_msym_oper_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(msym_oper_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_msym_oper_type

        module subroutine wrap_magnetic_domain_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magnetic_domain_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_domain_type

        module subroutine wrap_magsymm_k_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magsymm_k_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_magsymm_k_type

        module subroutine wrap_refl_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            class(refl_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_refl_type

        module subroutine wrap_reflist_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(reflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_reflist_type

        module subroutine wrap_shub_spgr_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(shub_spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_shub_spgr_info_type

        module subroutine wrap_spgr_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(spgr_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_spgr_info_type

        module subroutine wrap_table_equiv_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(table_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_table_equiv_type

        module subroutine wrap_wyck_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(wyck_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_wyck_info_type

        module subroutine wrap_molecule_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molecule_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_molecule_type

        module subroutine wrap_molcrystal_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(molcrystal_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_molcrystal_type

        module subroutine wrap_interval_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(interval_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_interval_type

        module subroutine wrap_job_info_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(job_info_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_job_info_type

        module subroutine wrap_blockinfo_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(blockinfo_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_blockinfo_type

        module subroutine wrap_genvec_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(genvec_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_genvec_type

        module subroutine wrap_atoms_conf_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atoms_conf_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atoms_conf_list_type

        module subroutine wrap_deriv_tof_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(deriv_tof_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_deriv_tof_type

        module subroutine wrap_multistate_vector_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(multistate_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_multistate_vector_type

        module subroutine wrap_simann_conditions_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(simann_conditions_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_simann_conditions_type

        module subroutine wrap_state_vector_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(state_vector_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_state_vector_type

        module subroutine wrap_atm_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            class(atm_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_type

        module subroutine wrap_atm_cell_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atm_cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_cell_type

        module subroutine wrap_atom_equiv_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_type

        module subroutine wrap_atom_equiv_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atom_equiv_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_list_type

        module subroutine wrap_atlist_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(atlist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_atlist_type

        module subroutine wrap_matom_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_type

        module subroutine wrap_matom_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(matom_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_list_type

        module subroutine wrap_scattering_species_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(scattering_species_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_scattering_species_type

        module subroutine wrap_strf_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strf_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_strf_type

        module subroutine wrap_strflist_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(strflist_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_strflist_type

        module subroutine wrap_magh_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magh_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_magh_type

        module subroutine wrap_magh_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(magh_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_magh_list_type

        module subroutine wrap_maghd_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(maghd_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_maghd_type

        module subroutine wrap_maghd_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(maghd_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_maghd_list_type

        module subroutine wrap_pvt_table(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(pvt_table), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_pvt_table

        module subroutine wrap_eos_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_type

        module subroutine wrap_eos_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_list_type

        module subroutine wrap_eos_cell_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_cell_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_cell_type

        module subroutine wrap_axis_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(axis_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_axis_type

        module subroutine wrap_eos_data_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_data_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_data_type

        module subroutine wrap_eos_data_list_type(for_var,py_var,ierror)
            type(dict), intent(inout) :: py_var
            type(eos_data_list_type), intent(out) :: for_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_data_list_type

    end interface

End Module CFML_Wraps
