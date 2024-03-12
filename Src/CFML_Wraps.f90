
Module CFML_Wraps

    use cfml_globaldeps

    use forpy_mod, str_forpy => str

    use cfml_wraps_utils
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

    public :: unwrap_cube_info_type,unwrap_atomic_properties_type,unwrap_bvel_par_type,&
              unwrap_bvs_par_type,unwrap_sbvs_par_type,unwrap_basic_numc_type,&
              unwrap_basic_numi_type,unwrap_basic_numr_type,unwrap_calibration_detector_type,&
              unwrap_diffractometer_type,unwrap_generic_numor_type,unwrap_ill_data_record_type,&
              unwrap_powder_numor_type,unwrap_sxtal_numor_type,unwrap_sxtal_orient_type,&
              unwrap_lsq_conditions_type,unwrap_lsq_data_type,unwrap_lsq_state_vector_type,&
              unwrap_anomalous_sc_type,unwrap_chem_info_type,unwrap_magnetic_form_type,&
              unwrap_xray_form_type,unwrap_xray_wavelength_type,unwrap_pkb_type,&
              unwrap_peak_search_cond_type,unwrap_psd_val_type,unwrap_sxd_val_type,&
              unwrap_twin_type,unwrap_opt_conditions_type,unwrap_group_k_type,&
              unwrap_diffpat_type,unwrap_polar_calc_type,unwrap_polar_calc_list_type,&
              unwrap_polar_calcmulti_list_type,unwrap_polar_info_type,unwrap_polar_obs_type,&
              unwrap_polar_obs_list_type,unwrap_polar_obsmulti_list_type,unwrap_polar_calc_svs_type,&
              unwrap_polar_calc_svs_list_type,unwrap_polar_calcmulti_svs_list_type,unwrap_rational,&
              unwrap_cell_type,unwrap_twofold_axes_type,unwrap_zone_axis_type,&
              unwrap_strain_tensor_type,unwrap_symm_oper_type,unwrap_group_type,&
              unwrap_kvect_info_type,unwrap_point_orbit,unwrap_coordination_type,&
              unwrap_point_list_type,unwrap_sym_oper_type,unwrap_msym_oper_type,&
              unwrap_magnetic_domain_type,unwrap_magsymm_k_type,unwrap_refl_type,&
              unwrap_reflist_type,unwrap_shub_spgr_info_type,unwrap_spgr_info_type,&
              unwrap_table_equiv_type,unwrap_wyck_info_type,unwrap_molecule_type,&
              unwrap_molcrystal_type,unwrap_interval_type,unwrap_job_info_type,&
              unwrap_blockinfo_type,unwrap_genvec_type,unwrap_atoms_conf_list_type,&
              unwrap_deriv_tof_type,unwrap_multistate_vector_type,unwrap_simann_conditions_type,&
              unwrap_state_vector_type,unwrap_atm_type,unwrap_atm_cell_type,&
              unwrap_atom_equiv_type,unwrap_atom_equiv_list_type,unwrap_atlist_type,&
              unwrap_matom_type,unwrap_matom_list_type,unwrap_scattering_species_type,&
              unwrap_strf_type,unwrap_strflist_type,unwrap_magh_type,&
              unwrap_magh_list_type,unwrap_maghd_type,unwrap_maghd_list_type,&
              unwrap_pvt_table,unwrap_eos_type,unwrap_eos_list_type,&
              unwrap_eos_cell_type,unwrap_axis_type,unwrap_eos_data_type,&
              unwrap_eos_data_list_type
    public :: unwrap_diffpat_type_no_alloc,unwrap_cell_type_no_alloc,unwrap_group_type_no_alloc,&
              unwrap_refl_type_no_alloc,unwrap_atm_type_no_alloc
    public :: wrap_cube_info_type,wrap_atomic_properties_type,wrap_bvel_par_type,&
              wrap_bvs_par_type,wrap_sbvs_par_type,wrap_basic_numc_type,&
              wrap_basic_numi_type,wrap_basic_numr_type,wrap_calibration_detector_type,&
              wrap_diffractometer_type,wrap_generic_numor_type,wrap_ill_data_record_type,&
              wrap_powder_numor_type,wrap_sxtal_numor_type,wrap_sxtal_orient_type,&
              wrap_lsq_conditions_type,wrap_lsq_data_type,wrap_lsq_state_vector_type,&
              wrap_anomalous_sc_type,wrap_chem_info_type,wrap_magnetic_form_type,&
              wrap_xray_form_type,wrap_xray_wavelength_type,wrap_pkb_type,&
              wrap_peak_search_cond_type,wrap_psd_val_type,wrap_sxd_val_type,&
              wrap_twin_type,wrap_opt_conditions_type,wrap_group_k_type,&
              wrap_diffpat_type,wrap_polar_calc_type,wrap_polar_calc_list_type,&
              wrap_polar_calcmulti_list_type,wrap_polar_info_type,wrap_polar_obs_type,&
              wrap_polar_obs_list_type,wrap_polar_obsmulti_list_type,wrap_polar_calc_svs_type,&
              wrap_polar_calc_svs_list_type,wrap_polar_calcmulti_svs_list_type,wrap_rational,&
              wrap_cell_type,wrap_twofold_axes_type,wrap_zone_axis_type,&
              wrap_strain_tensor_type,wrap_symm_oper_type,wrap_group_type,&
              wrap_kvect_info_type,wrap_point_orbit,wrap_coordination_type,&
              wrap_point_list_type,wrap_sym_oper_type,wrap_msym_oper_type,&
              wrap_magnetic_domain_type,wrap_magsymm_k_type,wrap_refl_type,&
              wrap_reflist_type,wrap_shub_spgr_info_type,wrap_spgr_info_type,&
              wrap_table_equiv_type,wrap_wyck_info_type,wrap_molecule_type,&
              wrap_molcrystal_type,wrap_interval_type,wrap_job_info_type,&
              wrap_blockinfo_type,wrap_genvec_type,wrap_atoms_conf_list_type,&
              wrap_deriv_tof_type,wrap_multistate_vector_type,wrap_simann_conditions_type,&
              wrap_state_vector_type,wrap_atm_type,wrap_atm_cell_type,&
              wrap_atom_equiv_type,wrap_atom_equiv_list_type,wrap_atlist_type,&
              wrap_matom_type,wrap_matom_list_type,wrap_scattering_species_type,&
              wrap_strf_type,wrap_strflist_type,wrap_magh_type,&
              wrap_magh_list_type,wrap_maghd_type,wrap_maghd_list_type,&
              wrap_pvt_table,wrap_eos_type,wrap_eos_list_type,&
              wrap_eos_cell_type,wrap_axis_type,wrap_eos_data_type,&
              wrap_eos_data_list_type
    public :: list_to_array,list_to_array_no_alloc,list_to_array_class

    interface list_to_array
        module procedure list_to_array_character
        module procedure list_to_array2d_character
        module procedure list_to_array_logical
        module procedure list_to_array2d_logical
        module procedure list_to_array_cube_info_type
        module procedure list_to_array2d_cube_info_type
        module procedure list_to_array_atomic_properties_type
        module procedure list_to_array2d_atomic_properties_type
        module procedure list_to_array_bvel_par_type
        module procedure list_to_array2d_bvel_par_type
        module procedure list_to_array_bvs_par_type
        module procedure list_to_array2d_bvs_par_type
        module procedure list_to_array_sbvs_par_type
        module procedure list_to_array2d_sbvs_par_type
        module procedure list_to_array_basic_numc_type
        module procedure list_to_array2d_basic_numc_type
        module procedure list_to_array_basic_numi_type
        module procedure list_to_array2d_basic_numi_type
        module procedure list_to_array_basic_numr_type
        module procedure list_to_array2d_basic_numr_type
        module procedure list_to_array_calibration_detector_type
        module procedure list_to_array2d_calibration_detector_type
        module procedure list_to_array_diffractometer_type
        module procedure list_to_array2d_diffractometer_type
        module procedure list_to_array_generic_numor_type
        module procedure list_to_array2d_generic_numor_type
        module procedure list_to_array_ill_data_record_type
        module procedure list_to_array2d_ill_data_record_type
        module procedure list_to_array_powder_numor_type
        module procedure list_to_array2d_powder_numor_type
        module procedure list_to_array_sxtal_numor_type
        module procedure list_to_array2d_sxtal_numor_type
        module procedure list_to_array_sxtal_orient_type
        module procedure list_to_array2d_sxtal_orient_type
        module procedure list_to_array_lsq_conditions_type
        module procedure list_to_array2d_lsq_conditions_type
        module procedure list_to_array_lsq_data_type
        module procedure list_to_array2d_lsq_data_type
        module procedure list_to_array_lsq_state_vector_type
        module procedure list_to_array2d_lsq_state_vector_type
        module procedure list_to_array_anomalous_sc_type
        module procedure list_to_array2d_anomalous_sc_type
        module procedure list_to_array_chem_info_type
        module procedure list_to_array2d_chem_info_type
        module procedure list_to_array_magnetic_form_type
        module procedure list_to_array2d_magnetic_form_type
        module procedure list_to_array_xray_form_type
        module procedure list_to_array2d_xray_form_type
        module procedure list_to_array_xray_wavelength_type
        module procedure list_to_array2d_xray_wavelength_type
        module procedure list_to_array_pkb_type
        module procedure list_to_array2d_pkb_type
        module procedure list_to_array_peak_search_cond_type
        module procedure list_to_array2d_peak_search_cond_type
        module procedure list_to_array_psd_val_type
        module procedure list_to_array2d_psd_val_type
        module procedure list_to_array_sxd_val_type
        module procedure list_to_array2d_sxd_val_type
        module procedure list_to_array_twin_type
        module procedure list_to_array2d_twin_type
        module procedure list_to_array_opt_conditions_type
        module procedure list_to_array2d_opt_conditions_type
        module procedure list_to_array_group_k_type
        module procedure list_to_array2d_group_k_type
        module procedure list_to_array_diffpat_type
        module procedure list_to_array2d_diffpat_type
        module procedure list_to_array_diffpat_e_type
        module procedure list_to_array2d_diffpat_e_type
        module procedure list_to_array_diffpat_g_type
        module procedure list_to_array2d_diffpat_g_type
        module procedure list_to_array_polar_calc_type
        module procedure list_to_array2d_polar_calc_type
        module procedure list_to_array_polar_calc_list_type
        module procedure list_to_array2d_polar_calc_list_type
        module procedure list_to_array_polar_calcmulti_list_type
        module procedure list_to_array2d_polar_calcmulti_list_type
        module procedure list_to_array_polar_info_type
        module procedure list_to_array2d_polar_info_type
        module procedure list_to_array_polar_obs_type
        module procedure list_to_array2d_polar_obs_type
        module procedure list_to_array_polar_obs_list_type
        module procedure list_to_array2d_polar_obs_list_type
        module procedure list_to_array_polar_obsmulti_list_type
        module procedure list_to_array2d_polar_obsmulti_list_type
        module procedure list_to_array_polar_calc_svs_type
        module procedure list_to_array2d_polar_calc_svs_type
        module procedure list_to_array_polar_calc_svs_list_type
        module procedure list_to_array2d_polar_calc_svs_list_type
        module procedure list_to_array_polar_calcmulti_svs_list_type
        module procedure list_to_array2d_polar_calcmulti_svs_list_type
        module procedure list_to_array_rational
        module procedure list_to_array2d_rational
        module procedure list_to_array_cell_type
        module procedure list_to_array2d_cell_type
        module procedure list_to_array_cell_g_type
        module procedure list_to_array2d_cell_g_type
        module procedure list_to_array_cell_ls_type
        module procedure list_to_array2d_cell_ls_type
        module procedure list_to_array_cell_gls_type
        module procedure list_to_array2d_cell_gls_type
        module procedure list_to_array_twofold_axes_type
        module procedure list_to_array2d_twofold_axes_type
        module procedure list_to_array_zone_axis_type
        module procedure list_to_array2d_zone_axis_type
        module procedure list_to_array_strain_tensor_type
        module procedure list_to_array2d_strain_tensor_type
        module procedure list_to_array_symm_oper_type
        module procedure list_to_array2d_symm_oper_type
        module procedure list_to_array_group_type
        module procedure list_to_array2d_group_type
        module procedure list_to_array_spg_type
        module procedure list_to_array2d_spg_type
        module procedure list_to_array_superspacegroup_type
        module procedure list_to_array2d_superspacegroup_type
        module procedure list_to_array_kvect_info_type
        module procedure list_to_array2d_kvect_info_type
        module procedure list_to_array_point_orbit
        module procedure list_to_array2d_point_orbit
        module procedure list_to_array_coordination_type
        module procedure list_to_array2d_coordination_type
        module procedure list_to_array_point_list_type
        module procedure list_to_array2d_point_list_type
        module procedure list_to_array_sym_oper_type
        module procedure list_to_array2d_sym_oper_type
        module procedure list_to_array_msym_oper_type
        module procedure list_to_array2d_msym_oper_type
        module procedure list_to_array_magnetic_domain_type
        module procedure list_to_array2d_magnetic_domain_type
        module procedure list_to_array_magsymm_k_type
        module procedure list_to_array2d_magsymm_k_type
        module procedure list_to_array_refl_type
        module procedure list_to_array2d_refl_type
        module procedure list_to_array_srefl_type
        module procedure list_to_array2d_srefl_type
        module procedure list_to_array_mrefl_type
        module procedure list_to_array2d_mrefl_type
        module procedure list_to_array_reflist_type
        module procedure list_to_array2d_reflist_type
        module procedure list_to_array_shub_spgr_info_type
        module procedure list_to_array2d_shub_spgr_info_type
        module procedure list_to_array_spgr_info_type
        module procedure list_to_array2d_spgr_info_type
        module procedure list_to_array_table_equiv_type
        module procedure list_to_array2d_table_equiv_type
        module procedure list_to_array_wyck_info_type
        module procedure list_to_array2d_wyck_info_type
        module procedure list_to_array_molecule_type
        module procedure list_to_array2d_molecule_type
        module procedure list_to_array_molcrystal_type
        module procedure list_to_array2d_molcrystal_type
        module procedure list_to_array_interval_type
        module procedure list_to_array2d_interval_type
        module procedure list_to_array_job_info_type
        module procedure list_to_array2d_job_info_type
        module procedure list_to_array_blockinfo_type
        module procedure list_to_array2d_blockinfo_type
        module procedure list_to_array_genvec_type
        module procedure list_to_array2d_genvec_type
        module procedure list_to_array_atoms_conf_list_type
        module procedure list_to_array2d_atoms_conf_list_type
        module procedure list_to_array_deriv_tof_type
        module procedure list_to_array2d_deriv_tof_type
        module procedure list_to_array_multistate_vector_type
        module procedure list_to_array2d_multistate_vector_type
        module procedure list_to_array_simann_conditions_type
        module procedure list_to_array2d_simann_conditions_type
        module procedure list_to_array_state_vector_type
        module procedure list_to_array2d_state_vector_type
        module procedure list_to_array_atm_type
        module procedure list_to_array2d_atm_type
        module procedure list_to_array_atm_std_type
        module procedure list_to_array2d_atm_std_type
        module procedure list_to_array_modatm_std_type
        module procedure list_to_array2d_modatm_std_type
        module procedure list_to_array_atm_ref_type
        module procedure list_to_array2d_atm_ref_type
        module procedure list_to_array_modatm_ref_type
        module procedure list_to_array2d_modatm_ref_type
        module procedure list_to_array_atm_cell_type
        module procedure list_to_array2d_atm_cell_type
        module procedure list_to_array_atom_equiv_type
        module procedure list_to_array2d_atom_equiv_type
        module procedure list_to_array_atom_equiv_list_type
        module procedure list_to_array2d_atom_equiv_list_type
        module procedure list_to_array_atlist_type
        module procedure list_to_array2d_atlist_type
        module procedure list_to_array_matom_type
        module procedure list_to_array2d_matom_type
        module procedure list_to_array_matom_list_type
        module procedure list_to_array2d_matom_list_type
        module procedure list_to_array_scattering_species_type
        module procedure list_to_array2d_scattering_species_type
        module procedure list_to_array_strf_type
        module procedure list_to_array2d_strf_type
        module procedure list_to_array_strflist_type
        module procedure list_to_array2d_strflist_type
        module procedure list_to_array_magh_type
        module procedure list_to_array2d_magh_type
        module procedure list_to_array_magh_list_type
        module procedure list_to_array2d_magh_list_type
        module procedure list_to_array_maghd_type
        module procedure list_to_array2d_maghd_type
        module procedure list_to_array_maghd_list_type
        module procedure list_to_array2d_maghd_list_type
        module procedure list_to_array_pvt_table
        module procedure list_to_array2d_pvt_table
        module procedure list_to_array_eos_type
        module procedure list_to_array2d_eos_type
        module procedure list_to_array_eos_list_type
        module procedure list_to_array2d_eos_list_type
        module procedure list_to_array_eos_cell_type
        module procedure list_to_array2d_eos_cell_type
        module procedure list_to_array_axis_type
        module procedure list_to_array2d_axis_type
        module procedure list_to_array_eos_data_type
        module procedure list_to_array2d_eos_data_type
        module procedure list_to_array_eos_data_list_type
        module procedure list_to_array2d_eos_data_list_type
    end interface

    interface list_to_array_no_alloc
        module procedure list_to_array_character_no_alloc
        module procedure list_to_array2d_character_no_alloc
        module procedure list_to_array_logical_no_alloc
        module procedure list_to_array2d_logical_no_alloc
        module procedure list_to_array_cube_info_type_no_alloc
        module procedure list_to_array2d_cube_info_type_no_alloc
        module procedure list_to_array_atomic_properties_type_no_alloc
        module procedure list_to_array2d_atomic_properties_type_no_alloc
        module procedure list_to_array_bvel_par_type_no_alloc
        module procedure list_to_array2d_bvel_par_type_no_alloc
        module procedure list_to_array_bvs_par_type_no_alloc
        module procedure list_to_array2d_bvs_par_type_no_alloc
        module procedure list_to_array_sbvs_par_type_no_alloc
        module procedure list_to_array2d_sbvs_par_type_no_alloc
        module procedure list_to_array_basic_numc_type_no_alloc
        module procedure list_to_array2d_basic_numc_type_no_alloc
        module procedure list_to_array_basic_numi_type_no_alloc
        module procedure list_to_array2d_basic_numi_type_no_alloc
        module procedure list_to_array_basic_numr_type_no_alloc
        module procedure list_to_array2d_basic_numr_type_no_alloc
        module procedure list_to_array_calibration_detector_type_no_alloc
        module procedure list_to_array2d_calibration_detector_type_no_alloc
        module procedure list_to_array_diffractometer_type_no_alloc
        module procedure list_to_array2d_diffractometer_type_no_alloc
        module procedure list_to_array_generic_numor_type_no_alloc
        module procedure list_to_array2d_generic_numor_type_no_alloc
        module procedure list_to_array_ill_data_record_type_no_alloc
        module procedure list_to_array2d_ill_data_record_type_no_alloc
        module procedure list_to_array_powder_numor_type_no_alloc
        module procedure list_to_array2d_powder_numor_type_no_alloc
        module procedure list_to_array_sxtal_numor_type_no_alloc
        module procedure list_to_array2d_sxtal_numor_type_no_alloc
        module procedure list_to_array_sxtal_orient_type_no_alloc
        module procedure list_to_array2d_sxtal_orient_type_no_alloc
        module procedure list_to_array_lsq_conditions_type_no_alloc
        module procedure list_to_array2d_lsq_conditions_type_no_alloc
        module procedure list_to_array_lsq_data_type_no_alloc
        module procedure list_to_array2d_lsq_data_type_no_alloc
        module procedure list_to_array_lsq_state_vector_type_no_alloc
        module procedure list_to_array2d_lsq_state_vector_type_no_alloc
        module procedure list_to_array_anomalous_sc_type_no_alloc
        module procedure list_to_array2d_anomalous_sc_type_no_alloc
        module procedure list_to_array_chem_info_type_no_alloc
        module procedure list_to_array2d_chem_info_type_no_alloc
        module procedure list_to_array_magnetic_form_type_no_alloc
        module procedure list_to_array2d_magnetic_form_type_no_alloc
        module procedure list_to_array_xray_form_type_no_alloc
        module procedure list_to_array2d_xray_form_type_no_alloc
        module procedure list_to_array_xray_wavelength_type_no_alloc
        module procedure list_to_array2d_xray_wavelength_type_no_alloc
        module procedure list_to_array_pkb_type_no_alloc
        module procedure list_to_array2d_pkb_type_no_alloc
        module procedure list_to_array_peak_search_cond_type_no_alloc
        module procedure list_to_array2d_peak_search_cond_type_no_alloc
        module procedure list_to_array_psd_val_type_no_alloc
        module procedure list_to_array2d_psd_val_type_no_alloc
        module procedure list_to_array_sxd_val_type_no_alloc
        module procedure list_to_array2d_sxd_val_type_no_alloc
        module procedure list_to_array_twin_type_no_alloc
        module procedure list_to_array2d_twin_type_no_alloc
        module procedure list_to_array_opt_conditions_type_no_alloc
        module procedure list_to_array2d_opt_conditions_type_no_alloc
        module procedure list_to_array_group_k_type_no_alloc
        module procedure list_to_array2d_group_k_type_no_alloc
        module procedure list_to_array_diffpat_type_no_alloc
        module procedure list_to_array2d_diffpat_type_no_alloc
        module procedure list_to_array_diffpat_e_type_no_alloc
        module procedure list_to_array2d_diffpat_e_type_no_alloc
        module procedure list_to_array_diffpat_g_type_no_alloc
        module procedure list_to_array2d_diffpat_g_type_no_alloc
        module procedure list_to_array_polar_calc_type_no_alloc
        module procedure list_to_array2d_polar_calc_type_no_alloc
        module procedure list_to_array_polar_calc_list_type_no_alloc
        module procedure list_to_array2d_polar_calc_list_type_no_alloc
        module procedure list_to_array_polar_calcmulti_list_type_no_alloc
        module procedure list_to_array2d_polar_calcmulti_list_type_no_alloc
        module procedure list_to_array_polar_info_type_no_alloc
        module procedure list_to_array2d_polar_info_type_no_alloc
        module procedure list_to_array_polar_obs_type_no_alloc
        module procedure list_to_array2d_polar_obs_type_no_alloc
        module procedure list_to_array_polar_obs_list_type_no_alloc
        module procedure list_to_array2d_polar_obs_list_type_no_alloc
        module procedure list_to_array_polar_obsmulti_list_type_no_alloc
        module procedure list_to_array2d_polar_obsmulti_list_type_no_alloc
        module procedure list_to_array_polar_calc_svs_type_no_alloc
        module procedure list_to_array2d_polar_calc_svs_type_no_alloc
        module procedure list_to_array_polar_calc_svs_list_type_no_alloc
        module procedure list_to_array2d_polar_calc_svs_list_type_no_alloc
        module procedure list_to_array_polar_calcmulti_svs_list_type_no_alloc
        module procedure list_to_array2d_polar_calcmulti_svs_list_type_no_alloc
        module procedure list_to_array_rational_no_alloc
        module procedure list_to_array2d_rational_no_alloc
        module procedure list_to_array_cell_type_no_alloc
        module procedure list_to_array2d_cell_type_no_alloc
        module procedure list_to_array_cell_g_type_no_alloc
        module procedure list_to_array2d_cell_g_type_no_alloc
        module procedure list_to_array_cell_ls_type_no_alloc
        module procedure list_to_array2d_cell_ls_type_no_alloc
        module procedure list_to_array_cell_gls_type_no_alloc
        module procedure list_to_array2d_cell_gls_type_no_alloc
        module procedure list_to_array_twofold_axes_type_no_alloc
        module procedure list_to_array2d_twofold_axes_type_no_alloc
        module procedure list_to_array_zone_axis_type_no_alloc
        module procedure list_to_array2d_zone_axis_type_no_alloc
        module procedure list_to_array_strain_tensor_type_no_alloc
        module procedure list_to_array2d_strain_tensor_type_no_alloc
        module procedure list_to_array_symm_oper_type_no_alloc
        module procedure list_to_array2d_symm_oper_type_no_alloc
        module procedure list_to_array_group_type_no_alloc
        module procedure list_to_array2d_group_type_no_alloc
        module procedure list_to_array_spg_type_no_alloc
        module procedure list_to_array2d_spg_type_no_alloc
        module procedure list_to_array_superspacegroup_type_no_alloc
        module procedure list_to_array2d_superspacegroup_type_no_alloc
        module procedure list_to_array_kvect_info_type_no_alloc
        module procedure list_to_array2d_kvect_info_type_no_alloc
        module procedure list_to_array_point_orbit_no_alloc
        module procedure list_to_array2d_point_orbit_no_alloc
        module procedure list_to_array_coordination_type_no_alloc
        module procedure list_to_array2d_coordination_type_no_alloc
        module procedure list_to_array_point_list_type_no_alloc
        module procedure list_to_array2d_point_list_type_no_alloc
        module procedure list_to_array_sym_oper_type_no_alloc
        module procedure list_to_array2d_sym_oper_type_no_alloc
        module procedure list_to_array_msym_oper_type_no_alloc
        module procedure list_to_array2d_msym_oper_type_no_alloc
        module procedure list_to_array_magnetic_domain_type_no_alloc
        module procedure list_to_array2d_magnetic_domain_type_no_alloc
        module procedure list_to_array_magsymm_k_type_no_alloc
        module procedure list_to_array2d_magsymm_k_type_no_alloc
        module procedure list_to_array_refl_type_no_alloc
        module procedure list_to_array2d_refl_type_no_alloc
        module procedure list_to_array_srefl_type_no_alloc
        module procedure list_to_array2d_srefl_type_no_alloc
        module procedure list_to_array_mrefl_type_no_alloc
        module procedure list_to_array2d_mrefl_type_no_alloc
        module procedure list_to_array_reflist_type_no_alloc
        module procedure list_to_array2d_reflist_type_no_alloc
        module procedure list_to_array_shub_spgr_info_type_no_alloc
        module procedure list_to_array2d_shub_spgr_info_type_no_alloc
        module procedure list_to_array_spgr_info_type_no_alloc
        module procedure list_to_array2d_spgr_info_type_no_alloc
        module procedure list_to_array_table_equiv_type_no_alloc
        module procedure list_to_array2d_table_equiv_type_no_alloc
        module procedure list_to_array_wyck_info_type_no_alloc
        module procedure list_to_array2d_wyck_info_type_no_alloc
        module procedure list_to_array_molecule_type_no_alloc
        module procedure list_to_array2d_molecule_type_no_alloc
        module procedure list_to_array_molcrystal_type_no_alloc
        module procedure list_to_array2d_molcrystal_type_no_alloc
        module procedure list_to_array_interval_type_no_alloc
        module procedure list_to_array2d_interval_type_no_alloc
        module procedure list_to_array_job_info_type_no_alloc
        module procedure list_to_array2d_job_info_type_no_alloc
        module procedure list_to_array_blockinfo_type_no_alloc
        module procedure list_to_array2d_blockinfo_type_no_alloc
        module procedure list_to_array_genvec_type_no_alloc
        module procedure list_to_array2d_genvec_type_no_alloc
        module procedure list_to_array_atoms_conf_list_type_no_alloc
        module procedure list_to_array2d_atoms_conf_list_type_no_alloc
        module procedure list_to_array_deriv_tof_type_no_alloc
        module procedure list_to_array2d_deriv_tof_type_no_alloc
        module procedure list_to_array_multistate_vector_type_no_alloc
        module procedure list_to_array2d_multistate_vector_type_no_alloc
        module procedure list_to_array_simann_conditions_type_no_alloc
        module procedure list_to_array2d_simann_conditions_type_no_alloc
        module procedure list_to_array_state_vector_type_no_alloc
        module procedure list_to_array2d_state_vector_type_no_alloc
        module procedure list_to_array_atm_type_no_alloc
        module procedure list_to_array2d_atm_type_no_alloc
        module procedure list_to_array_atm_std_type_no_alloc
        module procedure list_to_array2d_atm_std_type_no_alloc
        module procedure list_to_array_modatm_std_type_no_alloc
        module procedure list_to_array2d_modatm_std_type_no_alloc
        module procedure list_to_array_atm_ref_type_no_alloc
        module procedure list_to_array2d_atm_ref_type_no_alloc
        module procedure list_to_array_modatm_ref_type_no_alloc
        module procedure list_to_array2d_modatm_ref_type_no_alloc
        module procedure list_to_array_atm_cell_type_no_alloc
        module procedure list_to_array2d_atm_cell_type_no_alloc
        module procedure list_to_array_atom_equiv_type_no_alloc
        module procedure list_to_array2d_atom_equiv_type_no_alloc
        module procedure list_to_array_atom_equiv_list_type_no_alloc
        module procedure list_to_array2d_atom_equiv_list_type_no_alloc
        module procedure list_to_array_atlist_type_no_alloc
        module procedure list_to_array2d_atlist_type_no_alloc
        module procedure list_to_array_matom_type_no_alloc
        module procedure list_to_array2d_matom_type_no_alloc
        module procedure list_to_array_matom_list_type_no_alloc
        module procedure list_to_array2d_matom_list_type_no_alloc
        module procedure list_to_array_scattering_species_type_no_alloc
        module procedure list_to_array2d_scattering_species_type_no_alloc
        module procedure list_to_array_strf_type_no_alloc
        module procedure list_to_array2d_strf_type_no_alloc
        module procedure list_to_array_strflist_type_no_alloc
        module procedure list_to_array2d_strflist_type_no_alloc
        module procedure list_to_array_magh_type_no_alloc
        module procedure list_to_array2d_magh_type_no_alloc
        module procedure list_to_array_magh_list_type_no_alloc
        module procedure list_to_array2d_magh_list_type_no_alloc
        module procedure list_to_array_maghd_type_no_alloc
        module procedure list_to_array2d_maghd_type_no_alloc
        module procedure list_to_array_maghd_list_type_no_alloc
        module procedure list_to_array2d_maghd_list_type_no_alloc
        module procedure list_to_array_pvt_table_no_alloc
        module procedure list_to_array2d_pvt_table_no_alloc
        module procedure list_to_array_eos_type_no_alloc
        module procedure list_to_array2d_eos_type_no_alloc
        module procedure list_to_array_eos_list_type_no_alloc
        module procedure list_to_array2d_eos_list_type_no_alloc
        module procedure list_to_array_eos_cell_type_no_alloc
        module procedure list_to_array2d_eos_cell_type_no_alloc
        module procedure list_to_array_axis_type_no_alloc
        module procedure list_to_array2d_axis_type_no_alloc
        module procedure list_to_array_eos_data_type_no_alloc
        module procedure list_to_array2d_eos_data_type_no_alloc
        module procedure list_to_array_eos_data_list_type_no_alloc
        module procedure list_to_array2d_eos_data_list_type_no_alloc
    end interface

    interface list_to_array_class
        module procedure list_to_array_diffpat_type_class
        module procedure list_to_array_cell_type_class
        module procedure list_to_array_group_type_class
        module procedure list_to_array_refl_type_class
        module procedure list_to_array_atm_type_class
    end interface

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
            type(cube_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_cube_info_type

        module subroutine wrap_atomic_properties_type(for_var,py_var,ierror)
            type(atomic_properties_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atomic_properties_type

        module subroutine wrap_bvel_par_type(for_var,py_var,ierror)
            type(bvel_par_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvel_par_type

        module subroutine wrap_bvs_par_type(for_var,py_var,ierror)
            type(bvs_par_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_bvs_par_type

        module subroutine wrap_sbvs_par_type(for_var,py_var,ierror)
            type(sbvs_par_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sbvs_par_type

        module subroutine wrap_basic_numc_type(for_var,py_var,ierror)
            type(basic_numc_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numc_type

        module subroutine wrap_basic_numi_type(for_var,py_var,ierror)
            type(basic_numi_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numi_type

        module subroutine wrap_basic_numr_type(for_var,py_var,ierror)
            type(basic_numr_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_basic_numr_type

        module subroutine wrap_calibration_detector_type(for_var,py_var,ierror)
            type(calibration_detector_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_calibration_detector_type

        module subroutine wrap_diffractometer_type(for_var,py_var,ierror)
            type(diffractometer_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffractometer_type

        module subroutine wrap_generic_numor_type(for_var,py_var,ierror)
            type(generic_numor_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_generic_numor_type

        module subroutine wrap_ill_data_record_type(for_var,py_var,ierror)
            type(ill_data_record_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_ill_data_record_type

        module subroutine wrap_powder_numor_type(for_var,py_var,ierror)
            type(powder_numor_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_powder_numor_type

        module subroutine wrap_sxtal_numor_type(for_var,py_var,ierror)
            type(sxtal_numor_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_numor_type

        module subroutine wrap_sxtal_orient_type(for_var,py_var,ierror)
            type(sxtal_orient_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxtal_orient_type

        module subroutine wrap_lsq_conditions_type(for_var,py_var,ierror)
            type(lsq_conditions_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_conditions_type

        module subroutine wrap_lsq_data_type(for_var,py_var,ierror)
            type(lsq_data_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_data_type

        module subroutine wrap_lsq_state_vector_type(for_var,py_var,ierror)
            type(lsq_state_vector_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_lsq_state_vector_type

        module subroutine wrap_anomalous_sc_type(for_var,py_var,ierror)
            type(anomalous_sc_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_anomalous_sc_type

        module subroutine wrap_chem_info_type(for_var,py_var,ierror)
            type(chem_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_chem_info_type

        module subroutine wrap_magnetic_form_type(for_var,py_var,ierror)
            type(magnetic_form_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_form_type

        module subroutine wrap_xray_form_type(for_var,py_var,ierror)
            type(xray_form_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_form_type

        module subroutine wrap_xray_wavelength_type(for_var,py_var,ierror)
            type(xray_wavelength_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_xray_wavelength_type

        module subroutine wrap_pkb_type(for_var,py_var,ierror)
            type(pkb_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_pkb_type

        module subroutine wrap_peak_search_cond_type(for_var,py_var,ierror)
            type(peak_search_cond_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_peak_search_cond_type

        module subroutine wrap_psd_val_type(for_var,py_var,ierror)
            type(psd_val_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_psd_val_type

        module subroutine wrap_sxd_val_type(for_var,py_var,ierror)
            type(sxd_val_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sxd_val_type

        module subroutine wrap_twin_type(for_var,py_var,ierror)
            type(twin_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_twin_type

        module subroutine wrap_opt_conditions_type(for_var,py_var,ierror)
            type(opt_conditions_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_opt_conditions_type

        module subroutine wrap_group_k_type(for_var,py_var,ierror)
            type(group_k_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_k_type

        module subroutine wrap_diffpat_type(for_var,py_var,ierror)
            class(diffpat_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_diffpat_type

        module subroutine wrap_polar_calc_type(for_var,py_var,ierror)
            type(polar_calc_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_type

        module subroutine wrap_polar_calc_list_type(for_var,py_var,ierror)
            type(polar_calc_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_list_type

        module subroutine wrap_polar_calcmulti_list_type(for_var,py_var,ierror)
            type(polar_calcmulti_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calcmulti_list_type

        module subroutine wrap_polar_info_type(for_var,py_var,ierror)
            type(polar_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_info_type

        module subroutine wrap_polar_obs_type(for_var,py_var,ierror)
            type(polar_obs_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obs_type

        module subroutine wrap_polar_obs_list_type(for_var,py_var,ierror)
            type(polar_obs_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obs_list_type

        module subroutine wrap_polar_obsmulti_list_type(for_var,py_var,ierror)
            type(polar_obsmulti_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_obsmulti_list_type

        module subroutine wrap_polar_calc_svs_type(for_var,py_var,ierror)
            type(polar_calc_svs_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_svs_type

        module subroutine wrap_polar_calc_svs_list_type(for_var,py_var,ierror)
            type(polar_calc_svs_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calc_svs_list_type

        module subroutine wrap_polar_calcmulti_svs_list_type(for_var,py_var,ierror)
            type(polar_calcmulti_svs_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_polar_calcmulti_svs_list_type

        module subroutine wrap_rational(for_var,py_var,ierror)
            type(rational), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_rational

        module subroutine wrap_cell_type(for_var,py_var,ierror)
            class(cell_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_cell_type

        module subroutine wrap_twofold_axes_type(for_var,py_var,ierror)
            type(twofold_axes_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_twofold_axes_type

        module subroutine wrap_zone_axis_type(for_var,py_var,ierror)
            type(zone_axis_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_zone_axis_type

        module subroutine wrap_strain_tensor_type(for_var,py_var,ierror)
            type(strain_tensor_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strain_tensor_type

        module subroutine wrap_symm_oper_type(for_var,py_var,ierror)
            type(symm_oper_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_symm_oper_type

        module subroutine wrap_group_type(for_var,py_var,ierror)
            class(group_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_group_type

        module subroutine wrap_kvect_info_type(for_var,py_var,ierror)
            type(kvect_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_kvect_info_type

        module subroutine wrap_point_orbit(for_var,py_var,ierror)
            type(point_orbit), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_orbit

        module subroutine wrap_coordination_type(for_var,py_var,ierror)
            type(coordination_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_coordination_type

        module subroutine wrap_point_list_type(for_var,py_var,ierror)
            type(point_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_point_list_type

        module subroutine wrap_sym_oper_type(for_var,py_var,ierror)
            type(sym_oper_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_sym_oper_type

        module subroutine wrap_msym_oper_type(for_var,py_var,ierror)
            type(msym_oper_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_msym_oper_type

        module subroutine wrap_magnetic_domain_type(for_var,py_var,ierror)
            type(magnetic_domain_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magnetic_domain_type

        module subroutine wrap_magsymm_k_type(for_var,py_var,ierror)
            type(magsymm_k_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magsymm_k_type

        module subroutine wrap_refl_type(for_var,py_var,ierror)
            class(refl_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_refl_type

        module subroutine wrap_reflist_type(for_var,py_var,ierror)
            type(reflist_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_reflist_type

        module subroutine wrap_shub_spgr_info_type(for_var,py_var,ierror)
            type(shub_spgr_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_shub_spgr_info_type

        module subroutine wrap_spgr_info_type(for_var,py_var,ierror)
            type(spgr_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_spgr_info_type

        module subroutine wrap_table_equiv_type(for_var,py_var,ierror)
            type(table_equiv_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_table_equiv_type

        module subroutine wrap_wyck_info_type(for_var,py_var,ierror)
            type(wyck_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_wyck_info_type

        module subroutine wrap_molecule_type(for_var,py_var,ierror)
            type(molecule_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_molecule_type

        module subroutine wrap_molcrystal_type(for_var,py_var,ierror)
            type(molcrystal_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_molcrystal_type

        module subroutine wrap_interval_type(for_var,py_var,ierror)
            type(interval_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_interval_type

        module subroutine wrap_job_info_type(for_var,py_var,ierror)
            type(job_info_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_job_info_type

        module subroutine wrap_blockinfo_type(for_var,py_var,ierror)
            type(blockinfo_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_blockinfo_type

        module subroutine wrap_genvec_type(for_var,py_var,ierror)
            type(genvec_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_genvec_type

        module subroutine wrap_atoms_conf_list_type(for_var,py_var,ierror)
            type(atoms_conf_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atoms_conf_list_type

        module subroutine wrap_deriv_tof_type(for_var,py_var,ierror)
            type(deriv_tof_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_deriv_tof_type

        module subroutine wrap_multistate_vector_type(for_var,py_var,ierror)
            type(multistate_vector_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_multistate_vector_type

        module subroutine wrap_simann_conditions_type(for_var,py_var,ierror)
            type(simann_conditions_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_simann_conditions_type

        module subroutine wrap_state_vector_type(for_var,py_var,ierror)
            type(state_vector_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_state_vector_type

        module subroutine wrap_atm_type(for_var,py_var,ierror)
            class(atm_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_type

        module subroutine wrap_atm_cell_type(for_var,py_var,ierror)
            type(atm_cell_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atm_cell_type

        module subroutine wrap_atom_equiv_type(for_var,py_var,ierror)
            type(atom_equiv_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_type

        module subroutine wrap_atom_equiv_list_type(for_var,py_var,ierror)
            type(atom_equiv_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atom_equiv_list_type

        module subroutine wrap_atlist_type(for_var,py_var,ierror)
            type(atlist_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_atlist_type

        module subroutine wrap_matom_type(for_var,py_var,ierror)
            type(matom_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_type

        module subroutine wrap_matom_list_type(for_var,py_var,ierror)
            type(matom_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_matom_list_type

        module subroutine wrap_scattering_species_type(for_var,py_var,ierror)
            type(scattering_species_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_scattering_species_type

        module subroutine wrap_strf_type(for_var,py_var,ierror)
            type(strf_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strf_type

        module subroutine wrap_strflist_type(for_var,py_var,ierror)
            type(strflist_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_strflist_type

        module subroutine wrap_magh_type(for_var,py_var,ierror)
            type(magh_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magh_type

        module subroutine wrap_magh_list_type(for_var,py_var,ierror)
            type(magh_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_magh_list_type

        module subroutine wrap_maghd_type(for_var,py_var,ierror)
            type(maghd_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_maghd_type

        module subroutine wrap_maghd_list_type(for_var,py_var,ierror)
            type(maghd_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_maghd_list_type

        module subroutine wrap_pvt_table(for_var,py_var,ierror)
            type(pvt_table), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_pvt_table

        module subroutine wrap_eos_type(for_var,py_var,ierror)
            type(eos_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_type

        module subroutine wrap_eos_list_type(for_var,py_var,ierror)
            type(eos_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_list_type

        module subroutine wrap_eos_cell_type(for_var,py_var,ierror)
            type(eos_cell_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_cell_type

        module subroutine wrap_axis_type(for_var,py_var,ierror)
            type(axis_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_axis_type

        module subroutine wrap_eos_data_type(for_var,py_var,ierror)
            type(eos_data_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_data_type

        module subroutine wrap_eos_data_list_type(for_var,py_var,ierror)
            type(eos_data_list_type), intent(in) :: for_var
            type(dict), intent(inout) :: py_var
            integer, intent(out) :: ierror
        end subroutine wrap_eos_data_list_type

        module subroutine list_to_array_diffpat_type_class(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(diffpat_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_type_class

        module subroutine list_to_array_cell_type_class(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_type_class

        module subroutine list_to_array_group_type_class(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(group_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_group_type_class

        module subroutine list_to_array_refl_type_class(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(refl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_refl_type_class

        module subroutine list_to_array_atm_type_class(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            class(atm_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_type_class

        module subroutine list_to_array_character(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            character(len=*), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_character

        module subroutine list_to_array2d_character(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            character(len=*), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_character

        module subroutine list_to_array_logical(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            logical, dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_logical

        module subroutine list_to_array2d_logical(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            logical, dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_logical

        module subroutine list_to_array_cube_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cube_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cube_info_type

        module subroutine list_to_array2d_cube_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cube_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cube_info_type

        module subroutine list_to_array_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atomic_properties_type

        module subroutine list_to_array2d_atomic_properties_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atomic_properties_type

        module subroutine list_to_array_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_bvel_par_type

        module subroutine list_to_array2d_bvel_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_bvel_par_type

        module subroutine list_to_array_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_bvs_par_type

        module subroutine list_to_array2d_bvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_bvs_par_type

        module subroutine list_to_array_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sbvs_par_type

        module subroutine list_to_array2d_sbvs_par_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sbvs_par_type

        module subroutine list_to_array_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numc_type

        module subroutine list_to_array2d_basic_numc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numc_type

        module subroutine list_to_array_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numi_type

        module subroutine list_to_array2d_basic_numi_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numi_type

        module subroutine list_to_array_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numr_type

        module subroutine list_to_array2d_basic_numr_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numr_type

        module subroutine list_to_array_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_calibration_detector_type

        module subroutine list_to_array2d_calibration_detector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_calibration_detector_type

        module subroutine list_to_array_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffractometer_type

        module subroutine list_to_array2d_diffractometer_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffractometer_type

        module subroutine list_to_array_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_generic_numor_type

        module subroutine list_to_array2d_generic_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_generic_numor_type

        module subroutine list_to_array_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_ill_data_record_type

        module subroutine list_to_array2d_ill_data_record_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_ill_data_record_type

        module subroutine list_to_array_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_powder_numor_type

        module subroutine list_to_array2d_powder_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_powder_numor_type

        module subroutine list_to_array_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxtal_numor_type

        module subroutine list_to_array2d_sxtal_numor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxtal_numor_type

        module subroutine list_to_array_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxtal_orient_type

        module subroutine list_to_array2d_sxtal_orient_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxtal_orient_type

        module subroutine list_to_array_lsq_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_conditions_type

        module subroutine list_to_array2d_lsq_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_conditions_type

        module subroutine list_to_array_lsq_data_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_data_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_data_type

        module subroutine list_to_array2d_lsq_data_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_data_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_data_type

        module subroutine list_to_array_lsq_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_state_vector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_state_vector_type

        module subroutine list_to_array2d_lsq_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_state_vector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_state_vector_type

        module subroutine list_to_array_anomalous_sc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_anomalous_sc_type

        module subroutine list_to_array2d_anomalous_sc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_anomalous_sc_type

        module subroutine list_to_array_chem_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_chem_info_type

        module subroutine list_to_array2d_chem_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_chem_info_type

        module subroutine list_to_array_magnetic_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magnetic_form_type

        module subroutine list_to_array2d_magnetic_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magnetic_form_type

        module subroutine list_to_array_xray_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_xray_form_type

        module subroutine list_to_array2d_xray_form_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_xray_form_type

        module subroutine list_to_array_xray_wavelength_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_xray_wavelength_type

        module subroutine list_to_array2d_xray_wavelength_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_xray_wavelength_type

        module subroutine list_to_array_pkb_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_pkb_type

        module subroutine list_to_array2d_pkb_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_pkb_type

        module subroutine list_to_array_peak_search_cond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_peak_search_cond_type

        module subroutine list_to_array2d_peak_search_cond_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_peak_search_cond_type

        module subroutine list_to_array_psd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_psd_val_type

        module subroutine list_to_array2d_psd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_psd_val_type

        module subroutine list_to_array_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxd_val_type

        module subroutine list_to_array2d_sxd_val_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxd_val_type

        module subroutine list_to_array_twin_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_twin_type

        module subroutine list_to_array2d_twin_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_twin_type

        module subroutine list_to_array_opt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(opt_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_opt_conditions_type

        module subroutine list_to_array2d_opt_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(opt_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_opt_conditions_type

        module subroutine list_to_array_group_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_group_k_type

        module subroutine list_to_array2d_group_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_group_k_type

        module subroutine list_to_array_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_type

        module subroutine list_to_array2d_diffpat_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_type

        module subroutine list_to_array_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_e_type

        module subroutine list_to_array2d_diffpat_e_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_e_type

        module subroutine list_to_array_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_g_type

        module subroutine list_to_array2d_diffpat_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_g_type

        module subroutine list_to_array_polar_calc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_type

        module subroutine list_to_array2d_polar_calc_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_type

        module subroutine list_to_array_polar_calc_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_list_type

        module subroutine list_to_array2d_polar_calc_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_list_type

        module subroutine list_to_array_polar_calcmulti_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calcmulti_list_type

        module subroutine list_to_array2d_polar_calcmulti_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calcmulti_list_type

        module subroutine list_to_array_polar_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_info_type

        module subroutine list_to_array2d_polar_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_info_type

        module subroutine list_to_array_polar_obs_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obs_type

        module subroutine list_to_array2d_polar_obs_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obs_type

        module subroutine list_to_array_polar_obs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obs_list_type

        module subroutine list_to_array2d_polar_obs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obs_list_type

        module subroutine list_to_array_polar_obsmulti_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obsmulti_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obsmulti_list_type

        module subroutine list_to_array2d_polar_obsmulti_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obsmulti_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obsmulti_list_type

        module subroutine list_to_array_polar_calc_svs_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_svs_type

        module subroutine list_to_array2d_polar_calc_svs_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_svs_type

        module subroutine list_to_array_polar_calc_svs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_svs_list_type

        module subroutine list_to_array2d_polar_calc_svs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_svs_list_type

        module subroutine list_to_array_polar_calcmulti_svs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_svs_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calcmulti_svs_list_type

        module subroutine list_to_array2d_polar_calcmulti_svs_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_svs_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calcmulti_svs_list_type

        module subroutine list_to_array_rational(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_rational

        module subroutine list_to_array2d_rational(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_rational

        module subroutine list_to_array_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_type

        module subroutine list_to_array2d_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_type

        module subroutine list_to_array_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_g_type

        module subroutine list_to_array2d_cell_g_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_g_type

        module subroutine list_to_array_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_ls_type

        module subroutine list_to_array2d_cell_ls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_ls_type

        module subroutine list_to_array_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_gls_type

        module subroutine list_to_array2d_cell_gls_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_gls_type

        module subroutine list_to_array_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_twofold_axes_type

        module subroutine list_to_array2d_twofold_axes_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_twofold_axes_type

        module subroutine list_to_array_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_zone_axis_type

        module subroutine list_to_array2d_zone_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_zone_axis_type

        module subroutine list_to_array_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strain_tensor_type

        module subroutine list_to_array2d_strain_tensor_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strain_tensor_type

        module subroutine list_to_array_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_symm_oper_type

        module subroutine list_to_array2d_symm_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_symm_oper_type

        module subroutine list_to_array_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_group_type

        module subroutine list_to_array2d_group_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_group_type

        module subroutine list_to_array_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_spg_type

        module subroutine list_to_array2d_spg_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_spg_type

        module subroutine list_to_array_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_superspacegroup_type

        module subroutine list_to_array2d_superspacegroup_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_superspacegroup_type

        module subroutine list_to_array_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_kvect_info_type

        module subroutine list_to_array2d_kvect_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_kvect_info_type

        module subroutine list_to_array_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_point_orbit

        module subroutine list_to_array2d_point_orbit(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_point_orbit

        module subroutine list_to_array_coordination_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_coordination_type

        module subroutine list_to_array2d_coordination_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_coordination_type

        module subroutine list_to_array_point_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_point_list_type

        module subroutine list_to_array2d_point_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_point_list_type

        module subroutine list_to_array_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sym_oper_type

        module subroutine list_to_array2d_sym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sym_oper_type

        module subroutine list_to_array_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_msym_oper_type

        module subroutine list_to_array2d_msym_oper_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_msym_oper_type

        module subroutine list_to_array_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magnetic_domain_type

        module subroutine list_to_array2d_magnetic_domain_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magnetic_domain_type

        module subroutine list_to_array_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magsymm_k_type

        module subroutine list_to_array2d_magsymm_k_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magsymm_k_type

        module subroutine list_to_array_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_refl_type

        module subroutine list_to_array2d_refl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_refl_type

        module subroutine list_to_array_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_srefl_type

        module subroutine list_to_array2d_srefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_srefl_type

        module subroutine list_to_array_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_mrefl_type

        module subroutine list_to_array2d_mrefl_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_mrefl_type

        module subroutine list_to_array_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_reflist_type

        module subroutine list_to_array2d_reflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_reflist_type

        module subroutine list_to_array_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_shub_spgr_info_type

        module subroutine list_to_array2d_shub_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_shub_spgr_info_type

        module subroutine list_to_array_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_spgr_info_type

        module subroutine list_to_array2d_spgr_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_spgr_info_type

        module subroutine list_to_array_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_table_equiv_type

        module subroutine list_to_array2d_table_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_table_equiv_type

        module subroutine list_to_array_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_wyck_info_type

        module subroutine list_to_array2d_wyck_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_wyck_info_type

        module subroutine list_to_array_molecule_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_molecule_type

        module subroutine list_to_array2d_molecule_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_molecule_type

        module subroutine list_to_array_molcrystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_molcrystal_type

        module subroutine list_to_array2d_molcrystal_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_molcrystal_type

        module subroutine list_to_array_interval_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_interval_type

        module subroutine list_to_array2d_interval_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_interval_type

        module subroutine list_to_array_job_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_job_info_type

        module subroutine list_to_array2d_job_info_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_job_info_type

        module subroutine list_to_array_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_blockinfo_type

        module subroutine list_to_array2d_blockinfo_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_blockinfo_type

        module subroutine list_to_array_genvec_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_genvec_type

        module subroutine list_to_array2d_genvec_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_genvec_type

        module subroutine list_to_array_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atoms_conf_list_type

        module subroutine list_to_array2d_atoms_conf_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atoms_conf_list_type

        module subroutine list_to_array_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_deriv_tof_type

        module subroutine list_to_array2d_deriv_tof_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_deriv_tof_type

        module subroutine list_to_array_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_multistate_vector_type

        module subroutine list_to_array2d_multistate_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_multistate_vector_type

        module subroutine list_to_array_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_simann_conditions_type

        module subroutine list_to_array2d_simann_conditions_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_simann_conditions_type

        module subroutine list_to_array_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_state_vector_type

        module subroutine list_to_array2d_state_vector_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_state_vector_type

        module subroutine list_to_array_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_type

        module subroutine list_to_array2d_atm_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_type

        module subroutine list_to_array_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_std_type

        module subroutine list_to_array2d_atm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_std_type

        module subroutine list_to_array_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_modatm_std_type

        module subroutine list_to_array2d_modatm_std_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_modatm_std_type

        module subroutine list_to_array_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_ref_type

        module subroutine list_to_array2d_atm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_ref_type

        module subroutine list_to_array_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_modatm_ref_type

        module subroutine list_to_array2d_modatm_ref_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_modatm_ref_type

        module subroutine list_to_array_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_cell_type

        module subroutine list_to_array2d_atm_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_cell_type

        module subroutine list_to_array_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atom_equiv_type

        module subroutine list_to_array2d_atom_equiv_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atom_equiv_type

        module subroutine list_to_array_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atom_equiv_list_type

        module subroutine list_to_array2d_atom_equiv_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atom_equiv_list_type

        module subroutine list_to_array_atlist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atlist_type

        module subroutine list_to_array2d_atlist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atlist_type

        module subroutine list_to_array_matom_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_matom_type

        module subroutine list_to_array2d_matom_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_matom_type

        module subroutine list_to_array_matom_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_matom_list_type

        module subroutine list_to_array2d_matom_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_matom_list_type

        module subroutine list_to_array_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_scattering_species_type

        module subroutine list_to_array2d_scattering_species_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_scattering_species_type

        module subroutine list_to_array_strf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strf_type

        module subroutine list_to_array2d_strf_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strf_type

        module subroutine list_to_array_strflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strflist_type

        module subroutine list_to_array2d_strflist_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strflist_type

        module subroutine list_to_array_magh_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magh_type

        module subroutine list_to_array2d_magh_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magh_type

        module subroutine list_to_array_magh_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magh_list_type

        module subroutine list_to_array2d_magh_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magh_list_type

        module subroutine list_to_array_maghd_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_maghd_type

        module subroutine list_to_array2d_maghd_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_maghd_type

        module subroutine list_to_array_maghd_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_maghd_list_type

        module subroutine list_to_array2d_maghd_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_maghd_list_type

        module subroutine list_to_array_pvt_table(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pvt_table), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_pvt_table

        module subroutine list_to_array2d_pvt_table(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pvt_table), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_pvt_table

        module subroutine list_to_array_eos_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_type

        module subroutine list_to_array2d_eos_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_type

        module subroutine list_to_array_eos_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_list_type

        module subroutine list_to_array2d_eos_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_list_type

        module subroutine list_to_array_eos_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_cell_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_cell_type

        module subroutine list_to_array2d_eos_cell_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_cell_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_cell_type

        module subroutine list_to_array_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(axis_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_axis_type

        module subroutine list_to_array2d_axis_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(axis_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_axis_type

        module subroutine list_to_array_eos_data_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_data_type

        module subroutine list_to_array2d_eos_data_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_data_type

        module subroutine list_to_array_eos_data_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_list_type), dimension(:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_data_list_type

        module subroutine list_to_array2d_eos_data_list_type(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_list_type), dimension(:,:), allocatable, intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_data_list_type

        module subroutine list_to_array_character_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            character(len=*), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_character_no_alloc

        module subroutine list_to_array2d_character_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            character(len=*), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_character_no_alloc

        module subroutine list_to_array_logical_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            logical, dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_logical_no_alloc

        module subroutine list_to_array2d_logical_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            logical, dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_logical_no_alloc

        module subroutine list_to_array_cube_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cube_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cube_info_type_no_alloc

        module subroutine list_to_array2d_cube_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cube_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cube_info_type_no_alloc

        module subroutine list_to_array_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atomic_properties_type_no_alloc

        module subroutine list_to_array2d_atomic_properties_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atomic_properties_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atomic_properties_type_no_alloc

        module subroutine list_to_array_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_bvel_par_type_no_alloc

        module subroutine list_to_array2d_bvel_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvel_par_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_bvel_par_type_no_alloc

        module subroutine list_to_array_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_bvs_par_type_no_alloc

        module subroutine list_to_array2d_bvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(bvs_par_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_bvs_par_type_no_alloc

        module subroutine list_to_array_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sbvs_par_type_no_alloc

        module subroutine list_to_array2d_sbvs_par_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sbvs_par_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sbvs_par_type_no_alloc

        module subroutine list_to_array_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numc_type_no_alloc

        module subroutine list_to_array2d_basic_numc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numc_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numc_type_no_alloc

        module subroutine list_to_array_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numi_type_no_alloc

        module subroutine list_to_array2d_basic_numi_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numi_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numi_type_no_alloc

        module subroutine list_to_array_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_basic_numr_type_no_alloc

        module subroutine list_to_array2d_basic_numr_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(basic_numr_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_basic_numr_type_no_alloc

        module subroutine list_to_array_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_calibration_detector_type_no_alloc

        module subroutine list_to_array2d_calibration_detector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(calibration_detector_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_calibration_detector_type_no_alloc

        module subroutine list_to_array_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffractometer_type_no_alloc

        module subroutine list_to_array2d_diffractometer_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffractometer_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffractometer_type_no_alloc

        module subroutine list_to_array_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_generic_numor_type_no_alloc

        module subroutine list_to_array2d_generic_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(generic_numor_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_generic_numor_type_no_alloc

        module subroutine list_to_array_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_ill_data_record_type_no_alloc

        module subroutine list_to_array2d_ill_data_record_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(ill_data_record_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_ill_data_record_type_no_alloc

        module subroutine list_to_array_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_powder_numor_type_no_alloc

        module subroutine list_to_array2d_powder_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(powder_numor_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_powder_numor_type_no_alloc

        module subroutine list_to_array_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxtal_numor_type_no_alloc

        module subroutine list_to_array2d_sxtal_numor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_numor_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxtal_numor_type_no_alloc

        module subroutine list_to_array_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxtal_orient_type_no_alloc

        module subroutine list_to_array2d_sxtal_orient_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxtal_orient_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxtal_orient_type_no_alloc

        module subroutine list_to_array_lsq_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_conditions_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_conditions_type_no_alloc

        module subroutine list_to_array2d_lsq_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_conditions_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_conditions_type_no_alloc

        module subroutine list_to_array_lsq_data_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_data_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_data_type_no_alloc

        module subroutine list_to_array2d_lsq_data_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_data_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_data_type_no_alloc

        module subroutine list_to_array_lsq_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_state_vector_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_lsq_state_vector_type_no_alloc

        module subroutine list_to_array2d_lsq_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(lsq_state_vector_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_lsq_state_vector_type_no_alloc

        module subroutine list_to_array_anomalous_sc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_anomalous_sc_type_no_alloc

        module subroutine list_to_array2d_anomalous_sc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(anomalous_sc_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_anomalous_sc_type_no_alloc

        module subroutine list_to_array_chem_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_chem_info_type_no_alloc

        module subroutine list_to_array2d_chem_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(chem_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_chem_info_type_no_alloc

        module subroutine list_to_array_magnetic_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magnetic_form_type_no_alloc

        module subroutine list_to_array2d_magnetic_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_form_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magnetic_form_type_no_alloc

        module subroutine list_to_array_xray_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_xray_form_type_no_alloc

        module subroutine list_to_array2d_xray_form_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_form_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_xray_form_type_no_alloc

        module subroutine list_to_array_xray_wavelength_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_xray_wavelength_type_no_alloc

        module subroutine list_to_array2d_xray_wavelength_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(xray_wavelength_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_xray_wavelength_type_no_alloc

        module subroutine list_to_array_pkb_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_pkb_type_no_alloc

        module subroutine list_to_array2d_pkb_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pkb_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_pkb_type_no_alloc

        module subroutine list_to_array_peak_search_cond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_peak_search_cond_type_no_alloc

        module subroutine list_to_array2d_peak_search_cond_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(peak_search_cond_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_peak_search_cond_type_no_alloc

        module subroutine list_to_array_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_psd_val_type_no_alloc

        module subroutine list_to_array2d_psd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(psd_val_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_psd_val_type_no_alloc

        module subroutine list_to_array_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sxd_val_type_no_alloc

        module subroutine list_to_array2d_sxd_val_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sxd_val_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sxd_val_type_no_alloc

        module subroutine list_to_array_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_twin_type_no_alloc

        module subroutine list_to_array2d_twin_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twin_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_twin_type_no_alloc

        module subroutine list_to_array_opt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(opt_conditions_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_opt_conditions_type_no_alloc

        module subroutine list_to_array2d_opt_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(opt_conditions_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_opt_conditions_type_no_alloc

        module subroutine list_to_array_group_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_group_k_type_no_alloc

        module subroutine list_to_array2d_group_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_k_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_group_k_type_no_alloc

        module subroutine list_to_array_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_type_no_alloc

        module subroutine list_to_array2d_diffpat_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_type_no_alloc

        module subroutine list_to_array_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_e_type_no_alloc

        module subroutine list_to_array2d_diffpat_e_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_e_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_e_type_no_alloc

        module subroutine list_to_array_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_diffpat_g_type_no_alloc

        module subroutine list_to_array2d_diffpat_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(diffpat_g_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_diffpat_g_type_no_alloc

        module subroutine list_to_array_polar_calc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_type_no_alloc

        module subroutine list_to_array2d_polar_calc_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_type_no_alloc

        module subroutine list_to_array_polar_calc_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_list_type_no_alloc

        module subroutine list_to_array2d_polar_calc_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_list_type_no_alloc

        module subroutine list_to_array_polar_calcmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calcmulti_list_type_no_alloc

        module subroutine list_to_array2d_polar_calcmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calcmulti_list_type_no_alloc

        module subroutine list_to_array_polar_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_info_type_no_alloc

        module subroutine list_to_array2d_polar_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_info_type_no_alloc

        module subroutine list_to_array_polar_obs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obs_type_no_alloc

        module subroutine list_to_array2d_polar_obs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obs_type_no_alloc

        module subroutine list_to_array_polar_obs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obs_list_type_no_alloc

        module subroutine list_to_array2d_polar_obs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obs_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obs_list_type_no_alloc

        module subroutine list_to_array_polar_obsmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obsmulti_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_obsmulti_list_type_no_alloc

        module subroutine list_to_array2d_polar_obsmulti_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_obsmulti_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_obsmulti_list_type_no_alloc

        module subroutine list_to_array_polar_calc_svs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_svs_type_no_alloc

        module subroutine list_to_array2d_polar_calc_svs_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_svs_type_no_alloc

        module subroutine list_to_array_polar_calc_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calc_svs_list_type_no_alloc

        module subroutine list_to_array2d_polar_calc_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calc_svs_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calc_svs_list_type_no_alloc

        module subroutine list_to_array_polar_calcmulti_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_svs_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_polar_calcmulti_svs_list_type_no_alloc

        module subroutine list_to_array2d_polar_calcmulti_svs_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(polar_calcmulti_svs_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_polar_calcmulti_svs_list_type_no_alloc

        module subroutine list_to_array_rational_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_rational_no_alloc

        module subroutine list_to_array2d_rational_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(rational), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_rational_no_alloc

        module subroutine list_to_array_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_type_no_alloc

        module subroutine list_to_array2d_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_type_no_alloc

        module subroutine list_to_array_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_g_type_no_alloc

        module subroutine list_to_array2d_cell_g_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_g_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_g_type_no_alloc

        module subroutine list_to_array_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_ls_type_no_alloc

        module subroutine list_to_array2d_cell_ls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_ls_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_ls_type_no_alloc

        module subroutine list_to_array_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_cell_gls_type_no_alloc

        module subroutine list_to_array2d_cell_gls_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(cell_gls_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_cell_gls_type_no_alloc

        module subroutine list_to_array_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_twofold_axes_type_no_alloc

        module subroutine list_to_array2d_twofold_axes_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(twofold_axes_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_twofold_axes_type_no_alloc

        module subroutine list_to_array_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_zone_axis_type_no_alloc

        module subroutine list_to_array2d_zone_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(zone_axis_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_zone_axis_type_no_alloc

        module subroutine list_to_array_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strain_tensor_type_no_alloc

        module subroutine list_to_array2d_strain_tensor_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strain_tensor_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strain_tensor_type_no_alloc

        module subroutine list_to_array_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_symm_oper_type_no_alloc

        module subroutine list_to_array2d_symm_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(symm_oper_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_symm_oper_type_no_alloc

        module subroutine list_to_array_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_group_type_no_alloc

        module subroutine list_to_array2d_group_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(group_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_group_type_no_alloc

        module subroutine list_to_array_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_spg_type_no_alloc

        module subroutine list_to_array2d_spg_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spg_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_spg_type_no_alloc

        module subroutine list_to_array_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_superspacegroup_type_no_alloc

        module subroutine list_to_array2d_superspacegroup_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(superspacegroup_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_superspacegroup_type_no_alloc

        module subroutine list_to_array_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_kvect_info_type_no_alloc

        module subroutine list_to_array2d_kvect_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(kvect_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_kvect_info_type_no_alloc

        module subroutine list_to_array_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_point_orbit_no_alloc

        module subroutine list_to_array2d_point_orbit_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_orbit), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_point_orbit_no_alloc

        module subroutine list_to_array_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_coordination_type_no_alloc

        module subroutine list_to_array2d_coordination_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(coordination_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_coordination_type_no_alloc

        module subroutine list_to_array_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_point_list_type_no_alloc

        module subroutine list_to_array2d_point_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(point_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_point_list_type_no_alloc

        module subroutine list_to_array_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_sym_oper_type_no_alloc

        module subroutine list_to_array2d_sym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(sym_oper_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_sym_oper_type_no_alloc

        module subroutine list_to_array_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_msym_oper_type_no_alloc

        module subroutine list_to_array2d_msym_oper_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(msym_oper_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_msym_oper_type_no_alloc

        module subroutine list_to_array_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magnetic_domain_type_no_alloc

        module subroutine list_to_array2d_magnetic_domain_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magnetic_domain_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magnetic_domain_type_no_alloc

        module subroutine list_to_array_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magsymm_k_type_no_alloc

        module subroutine list_to_array2d_magsymm_k_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magsymm_k_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magsymm_k_type_no_alloc

        module subroutine list_to_array_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_refl_type_no_alloc

        module subroutine list_to_array2d_refl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(refl_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_refl_type_no_alloc

        module subroutine list_to_array_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_srefl_type_no_alloc

        module subroutine list_to_array2d_srefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(srefl_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_srefl_type_no_alloc

        module subroutine list_to_array_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_mrefl_type_no_alloc

        module subroutine list_to_array2d_mrefl_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(mrefl_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_mrefl_type_no_alloc

        module subroutine list_to_array_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_reflist_type_no_alloc

        module subroutine list_to_array2d_reflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(reflist_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_reflist_type_no_alloc

        module subroutine list_to_array_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_shub_spgr_info_type_no_alloc

        module subroutine list_to_array2d_shub_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(shub_spgr_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_shub_spgr_info_type_no_alloc

        module subroutine list_to_array_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_spgr_info_type_no_alloc

        module subroutine list_to_array2d_spgr_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(spgr_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_spgr_info_type_no_alloc

        module subroutine list_to_array_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_table_equiv_type_no_alloc

        module subroutine list_to_array2d_table_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(table_equiv_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_table_equiv_type_no_alloc

        module subroutine list_to_array_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_wyck_info_type_no_alloc

        module subroutine list_to_array2d_wyck_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(wyck_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_wyck_info_type_no_alloc

        module subroutine list_to_array_molecule_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_molecule_type_no_alloc

        module subroutine list_to_array2d_molecule_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molecule_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_molecule_type_no_alloc

        module subroutine list_to_array_molcrystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_molcrystal_type_no_alloc

        module subroutine list_to_array2d_molcrystal_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(molcrystal_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_molcrystal_type_no_alloc

        module subroutine list_to_array_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_interval_type_no_alloc

        module subroutine list_to_array2d_interval_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(interval_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_interval_type_no_alloc

        module subroutine list_to_array_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_job_info_type_no_alloc

        module subroutine list_to_array2d_job_info_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(job_info_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_job_info_type_no_alloc

        module subroutine list_to_array_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_blockinfo_type_no_alloc

        module subroutine list_to_array2d_blockinfo_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(blockinfo_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_blockinfo_type_no_alloc

        module subroutine list_to_array_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_genvec_type_no_alloc

        module subroutine list_to_array2d_genvec_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(genvec_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_genvec_type_no_alloc

        module subroutine list_to_array_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atoms_conf_list_type_no_alloc

        module subroutine list_to_array2d_atoms_conf_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atoms_conf_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atoms_conf_list_type_no_alloc

        module subroutine list_to_array_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_deriv_tof_type_no_alloc

        module subroutine list_to_array2d_deriv_tof_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(deriv_tof_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_deriv_tof_type_no_alloc

        module subroutine list_to_array_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_multistate_vector_type_no_alloc

        module subroutine list_to_array2d_multistate_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(multistate_vector_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_multistate_vector_type_no_alloc

        module subroutine list_to_array_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_simann_conditions_type_no_alloc

        module subroutine list_to_array2d_simann_conditions_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(simann_conditions_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_simann_conditions_type_no_alloc

        module subroutine list_to_array_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_state_vector_type_no_alloc

        module subroutine list_to_array2d_state_vector_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(state_vector_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_state_vector_type_no_alloc

        module subroutine list_to_array_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_type_no_alloc

        module subroutine list_to_array2d_atm_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_type_no_alloc

        module subroutine list_to_array_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_std_type_no_alloc

        module subroutine list_to_array2d_atm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_std_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_std_type_no_alloc

        module subroutine list_to_array_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_modatm_std_type_no_alloc

        module subroutine list_to_array2d_modatm_std_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_std_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_modatm_std_type_no_alloc

        module subroutine list_to_array_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_ref_type_no_alloc

        module subroutine list_to_array2d_atm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_ref_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_ref_type_no_alloc

        module subroutine list_to_array_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_modatm_ref_type_no_alloc

        module subroutine list_to_array2d_modatm_ref_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(modatm_ref_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_modatm_ref_type_no_alloc

        module subroutine list_to_array_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atm_cell_type_no_alloc

        module subroutine list_to_array2d_atm_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atm_cell_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atm_cell_type_no_alloc

        module subroutine list_to_array_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atom_equiv_type_no_alloc

        module subroutine list_to_array2d_atom_equiv_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atom_equiv_type_no_alloc

        module subroutine list_to_array_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atom_equiv_list_type_no_alloc

        module subroutine list_to_array2d_atom_equiv_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atom_equiv_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atom_equiv_list_type_no_alloc

        module subroutine list_to_array_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_atlist_type_no_alloc

        module subroutine list_to_array2d_atlist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(atlist_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_atlist_type_no_alloc

        module subroutine list_to_array_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_matom_type_no_alloc

        module subroutine list_to_array2d_matom_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_matom_type_no_alloc

        module subroutine list_to_array_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_matom_list_type_no_alloc

        module subroutine list_to_array2d_matom_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(matom_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_matom_list_type_no_alloc

        module subroutine list_to_array_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_scattering_species_type_no_alloc

        module subroutine list_to_array2d_scattering_species_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(scattering_species_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_scattering_species_type_no_alloc

        module subroutine list_to_array_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strf_type_no_alloc

        module subroutine list_to_array2d_strf_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strf_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strf_type_no_alloc

        module subroutine list_to_array_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_strflist_type_no_alloc

        module subroutine list_to_array2d_strflist_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(strflist_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_strflist_type_no_alloc

        module subroutine list_to_array_magh_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magh_type_no_alloc

        module subroutine list_to_array2d_magh_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magh_type_no_alloc

        module subroutine list_to_array_magh_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_magh_list_type_no_alloc

        module subroutine list_to_array2d_magh_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(magh_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_magh_list_type_no_alloc

        module subroutine list_to_array_maghd_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_maghd_type_no_alloc

        module subroutine list_to_array2d_maghd_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_maghd_type_no_alloc

        module subroutine list_to_array_maghd_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_maghd_list_type_no_alloc

        module subroutine list_to_array2d_maghd_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(maghd_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_maghd_list_type_no_alloc

        module subroutine list_to_array_pvt_table_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pvt_table), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_pvt_table_no_alloc

        module subroutine list_to_array2d_pvt_table_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(pvt_table), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_pvt_table_no_alloc

        module subroutine list_to_array_eos_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_type_no_alloc

        module subroutine list_to_array2d_eos_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_type_no_alloc

        module subroutine list_to_array_eos_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_list_type_no_alloc

        module subroutine list_to_array2d_eos_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_list_type_no_alloc

        module subroutine list_to_array_eos_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_cell_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_cell_type_no_alloc

        module subroutine list_to_array2d_eos_cell_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_cell_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_cell_type_no_alloc

        module subroutine list_to_array_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(axis_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_axis_type_no_alloc

        module subroutine list_to_array2d_axis_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(axis_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_axis_type_no_alloc

        module subroutine list_to_array_eos_data_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_data_type_no_alloc

        module subroutine list_to_array2d_eos_data_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_data_type_no_alloc

        module subroutine list_to_array_eos_data_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_list_type), dimension(:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array_eos_data_list_type_no_alloc

        module subroutine list_to_array2d_eos_data_list_type_no_alloc(procedure_name,var_name,my_list,arr,ierror)
            character(len=*), intent(in) :: procedure_name
            character(len=*), intent(in) :: var_name
            type(list), intent(inout) :: my_list
            type(eos_data_list_type), dimension(:,:), intent(out) :: arr
            integer, intent(inout) :: ierror
        end subroutine list_to_array2d_eos_data_list_type_no_alloc

    end interface

End Module CFML_Wraps
