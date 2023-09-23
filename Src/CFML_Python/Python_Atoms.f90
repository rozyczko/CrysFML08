!!----
!!----
!!----
SubModule (CFML_Python)  Atm_Python_Wraps
   implicit none
   Contains

   Module Subroutine Unwrap_Atlist_Type(py_var,for_var,ierror)
      !---- Arguments ----!
      type(dict),                    intent(inout) :: py_var
      type(atlist_type),             intent(out)   :: for_var
      integer,                       intent(out)   :: ierror

      !---- Local Variables ----!
      integer, dimension(:), pointer :: p_int_1d
      character(len=:), allocatable :: fortran_type
      type(list) :: my_list

      ierror = 0

      ! fortran_type
      ierror = py_var%getitem(fortran_type,"fortran_type")
      if (ierror /= 0) then
          err_cfml%flag = .true.
          err_cfml%ierr = -1
          err_cfml%msg  = 'Unwrap_Atlist_Type: Cannot determine fortran type'
      else
          if (fortran_type /= 'atlist_type') then
              ierror = -1
              err_cfml%flag = .true.
              err_cfml%ierr = -1
              err_cfml%msg  = 'Unwrap_Atlist_Type: Unknown fortran type'
          end if
      end if
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','active',py_var,my_list,ierror)
      if (ierror == 0) call list_to_array('Unwrap_Atlist_Type','active',my_list,for_var%active,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','atom',py_var,my_list,ierror)
      if (ierror == 0) call list_to_array('Unwrap_Atlist_Type','atom',my_list,for_var%atom,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','iph',py_var,p_int_1d,ierror)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atlist_Type','iph',p_int_1d,for_var%iph,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','mcomp',py_var,for_var%mcomp,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','natoms',py_var,for_var%natoms,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atlist_Type','symm_checked',py_var,for_var%symm_checked,ierror)
      if (ierror /= 0 .and. err_cfml%ierr == 0) then
          err_cfml%flag = .true.
          err_cfml%ierr = -1
          err_cfml%msg  = 'UnWrap_Atlist_Type: Unwrapping failed'
      end if

   End Subroutine Unwrap_Atlist_Type

   Module Subroutine Unwrap_Atm_Cell_Type(py_var,for_var,ierror)
      !---- Arguments ----!
      type(dict),                       intent(inout) :: py_var
      type(atm_cell_type), allocatable, intent(out)   :: for_var
      integer,                          intent(out)   :: ierror

      !---- Local Variables ----!
      integer, dimension(:), pointer :: p_int_1d
      integer, dimension(:,:), pointer :: p_int_2d
      real, dimension(:), pointer :: p_real_1d
      real, dimension(:,:), pointer :: p_real_2d
      real, dimension(:,:,:), pointer :: p_real_3d
      character(len=1) :: order
      character(len=:), allocatable :: fortran_type
      type(list) :: my_list

      ierror = 0

      ! fortran_type
      ierror = py_var%getitem(fortran_type,"fortran_type")
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = ierror
         err_cfml%msg  = 'Unwrap_Atm_Cell_Type: Cannot determine fortran type'
      else
         if (fortran_type /= 'atm_cell_type') then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_Atm_Cell_Type: Unknown fortran type'
         end if
      end if
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','charge',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','charge',p_real_1d,for_var%charge,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','ddist',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','ddist',p_real_1d,for_var%ddist,ierror)
      if (ierror == 0) ierror = list_create(my_list)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','ddlab',py_var,my_list,ierror)
      if (ierror == 0) call list_to_array('Unwrap_Atm_Cell_Type','ddlab',my_list,for_var%ddlab,ierror)
      if (ierror == 0) call my_list%destroy
      if (ierror == 0) ierror = list_create(my_list)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','lab',py_var,my_list,ierror)
      if (ierror == 0) call list_to_array('Unwrap_Atm_Cell_Type','lab',my_list,for_var%lab,ierror)
      if (ierror == 0) call my_list%destroy
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','distance',py_var,p_real_2d,ierror,order)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','distance',p_real_2d,for_var%distance,ierror,order)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','moment',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','moment',p_real_1d,for_var%moment,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','nat',py_var,for_var%nat,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','ndist',py_var,for_var%ndist,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','neighb',py_var,p_int_1d,ierror)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','neighb',p_int_1d,for_var%neighb,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','neighb_atom',py_var,p_int_2d,ierror,order)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','neighb_atom',p_int_2d,for_var%neighb_atom,ierror,order)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','trans',py_var,p_real_3d,ierror,order)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','trans',p_real_3d,for_var%trans,ierror,order)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','var_free',py_var,p_real_2d,ierror,order)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','var_free',p_real_2d,for_var%var_free,ierror,order)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Cell_Type','xyz',py_var,p_real_2d,ierror,order)
      if (ierror == 0) call pointer_to_array_alloc('Unwrap_Atm_Cell_Type','xyz',p_real_2d,for_var%xyz,ierror,order)
      if (ierror /= 0 .and. err_cfml%ierr == 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'Unwrap_Atm_Cell_Type: Unwrapping failed'
     end if

   End Subroutine Unwrap_Atm_Cell_Type

   Module Subroutine Unwrap_Atm_Type(py_var,for_var,ierror)
      !---- Arguments ----!
      type(dict),                    intent(inout) :: py_var
      class(atm_type), allocatable,  intent(out)   :: for_var
      integer,                       intent(out)   :: ierror

      !---- Local Variables ----!
      integer, dimension(:), pointer :: p_int_1d
      integer, dimension(:,:), pointer :: p_int_2d
      real, dimension(:), pointer :: p_real_1d
      real, dimension(:,:), pointer :: p_real_2d
      character(len=1) :: order
      character(len=:), allocatable :: fortran_type

      ierror = 0

      ! fortran_type
      ierror = py_var%getitem(fortran_type,"fortran_type")
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = ierror
         err_cfml%msg  = 'Unwrap_Atm_Type: Cannot determine fortran type'
      else
         if (fortran_type == 'atm_type') then
            allocate(atm_type :: for_var)
         else if (fortran_type == 'atm_std_type') then
            allocate(atm_std_type :: for_var)
         else if (fortran_type == 'modatm_std_type') then
            allocate(modatm_std_type :: for_var)
         else if (fortran_type == 'atm_ref_type') then
            allocate(atm_ref_type :: for_var)
         else if (fortran_type == 'modatm_ref_type') then
            allocate(modatm_ref_type :: for_var)
         else
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_Atm_Type: Unknown fortran type'
         end if
      end if
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','active',py_var,for_var%active,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','atminfo',py_var,for_var%atminfo,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','charge',py_var,for_var%charge,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','chemsymb',py_var,for_var%chemsymb,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ind_ff',py_var,p_int_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','lab',py_var,for_var%lab,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','magnetic',py_var,for_var%magnetic,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mom',py_var,for_var%mom,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moment',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moment',p_real_1d,for_var%moment,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mult',py_var,for_var%mult,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','occ',py_var,for_var%occ,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','sfacsymb',py_var,for_var%sfacsymb,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','thtype',py_var,for_var%thtype,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','u',p_real_1d,for_var%u,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','utype',py_var,for_var%utype,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_iso',py_var,for_var%u_iso,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','varf',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','varf',p_real_1d,for_var%varf,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','wyck',py_var,for_var%wyck,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','x',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','x',p_real_1d,for_var%x,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','z',py_var,for_var%z,ierror)
      if (ierror == 0) then
         select type (A => for_var)
            class is (atm_std_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moment_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moment_std',p_real_1d,A%moment_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','occ_std',py_var,A%occ_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_iso_std',py_var,A%u_iso_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','u_std',p_real_1d,A%u_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','x_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','x_std',p_real_1d,A%x_std,ierror)
         end select
         select type (A => for_var)
            class is (modatm_std_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','bcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','bcs',p_real_2d,A%bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','bcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','dcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','dcs',p_real_2d,A%dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','dcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','mcs',p_real_2d,A%mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moms',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moms',p_real_1d,A%moms,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_bc',py_var,A%n_bc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_dc',py_var,A%n_dc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_mc',py_var,A%n_mc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_oc',py_var,A%n_oc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_uc',py_var,A%n_uc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ocs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ocs',p_real_2d,A%ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ocs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pbc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pbc_q',p_int_1d,A%pbc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pdc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pdc_q',p_int_1d,A%pdc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pmc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pmc_q',p_int_1d,A%pmc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','poc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','poc_q',p_int_1d,A%poc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','puc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','puc_q',p_int_1d,A%puc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ucs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ucs',p_real_2d,A%ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ucs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','us',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','us',p_real_2d,A%us,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','xs',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','xs',p_real_1d,A%xs,ierror)
            class is (atm_ref_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_moment',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_moment',p_int_1d,A%l_moment,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_occ',py_var,A%l_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u_iso',py_var,A%l_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_u',p_int_1d,A%l_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_x',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_x',p_int_1d,A%l_x,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_moment',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_moment',p_real_1d,A%m_moment,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_occ',py_var,A%m_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u_iso',py_var,A%m_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_u',p_real_1d,A%m_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_x',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_x',p_real_1d,A%m_x,ierror)
         end select
         select type (A => for_var)
            class is (modatm_ref_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_bcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_dcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_mcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_occ',py_var,A%l_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_ocs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_u',p_int_1d,A%l_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_ucs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u_iso',py_var,A%l_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_x',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_x',p_int_1d,A%l_x,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_bcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_dcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_mcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_occ',py_var,A%m_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_ocs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_u',p_real_1d,A%m_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_ucs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u_iso',py_var,A%m_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_x',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_x',p_real_1d,A%m_x,ierror)
         end select
      end if
      if (ierror /= 0 .and. err_cfml%ierr == 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'UnWrap_Atm_Type: Unwrapping failed'
     end if

   End Subroutine Unwrap_Atm_Type

   Module Subroutine Unwrap_Atm_Type_No_Alloc(py_var,for_var,ierror)
      !---- Arguments ----!
      type(dict),                    intent(inout) :: py_var
      class(atm_type),               intent(out)   :: for_var
      integer,                       intent(out)   :: ierror

      !---- Local Variables ----!
      integer, dimension(:), pointer :: p_int_1d
      integer, dimension(:,:), pointer :: p_int_2d
      real, dimension(:), pointer :: p_real_1d
      real, dimension(:,:), pointer :: p_real_2d
      character(len=1) :: order
      character(len=:), allocatable :: fortran_type

      ierror = 0
      ! fortran_type
      ierror = py_var%getitem(fortran_type,"fortran_type")
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = ierror
         err_cfml%msg  = 'Unwrap_Atm_Type: Cannot determine fortran type'
      else
         if (fortran_type == 'atm_type' .and. .not. fortran_type == 'atm_std_type' &
            .and. .not. fortran_type == 'modatm_std_type' .and. .not. fortran_type == 'atm_ref_type' &
            .and. .not. fortran_type == 'modatm_ref_type') then
            ierror = -1
            err_cfml%flag = .true.
            err_cfml%ierr = ierror
            err_cfml%msg  = 'Unwrap_Atm_Type: Unknown fortran type'
         end if
      end if
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','active',py_var,for_var%active,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','atminfo',py_var,for_var%atminfo,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','charge',py_var,for_var%charge,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','chemsymb',py_var,for_var%chemsymb,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ind_ff',py_var,p_int_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ind_ff',p_int_1d,for_var%ind_ff,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','lab',py_var,for_var%lab,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','magnetic',py_var,for_var%magnetic,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mom',py_var,for_var%mom,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moment',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moment',p_real_1d,for_var%moment,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mult',py_var,for_var%mult,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','occ',py_var,for_var%occ,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','sfacsymb',py_var,for_var%sfacsymb,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','thtype',py_var,for_var%thtype,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','u',p_real_1d,for_var%u,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','utype',py_var,for_var%utype,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_iso',py_var,for_var%u_iso,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','varf',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','varf',p_real_1d,for_var%varf,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','wyck',py_var,for_var%wyck,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','x',py_var,p_real_1d,ierror)
      if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','x',p_real_1d,for_var%x,ierror)
      if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','z',py_var,for_var%z,ierror)
      if (ierror == 0) then
         select type (A => for_var)
            class is (atm_std_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moment_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moment_std',p_real_1d,A%moment_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','occ_std',py_var,A%occ_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_iso_std',py_var,A%u_iso_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','u_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','u_std',p_real_1d,A%u_std,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','x_std',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','x_std',p_real_1d,A%x_std,ierror)
         end select
         select type (A => for_var)
            class is (modatm_std_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','bcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','bcs',p_real_2d,A%bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','bcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','bcs_std',p_real_2d,A%bcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','dcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','dcs',p_real_2d,A%dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','dcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','dcs_std',p_real_2d,A%dcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','mcs',p_real_2d,A%mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','mcs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','mcs_std',p_real_2d,A%mcs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','moms',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','moms',p_real_1d,A%moms,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_bc',py_var,A%n_bc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_dc',py_var,A%n_dc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_mc',py_var,A%n_mc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_oc',py_var,A%n_oc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','n_uc',py_var,A%n_uc,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ocs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ocs',p_real_2d,A%ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ocs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ocs_std',p_real_2d,A%ocs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pbc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pbc_q',p_int_1d,A%pbc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pdc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pdc_q',p_int_1d,A%pdc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','pmc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','pmc_q',p_int_1d,A%pmc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','poc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','poc_q',p_int_1d,A%poc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','puc_q',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','puc_q',p_int_1d,A%puc_q,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ucs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ucs',p_real_2d,A%ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','ucs_std',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','ucs_std',p_real_2d,A%ucs_std,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','us',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','us',p_real_2d,A%us,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','xs',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','xs',p_real_1d,A%xs,ierror)
            class is (atm_ref_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_moment',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_moment',p_int_1d,A%l_moment,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_occ',py_var,A%l_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u_iso',py_var,A%l_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_u',p_int_1d,A%l_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_x',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_x',p_int_1d,A%l_x,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_moment',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_moment',p_real_1d,A%m_moment,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_occ',py_var,A%m_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u_iso',py_var,A%m_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_u',p_real_1d,A%m_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_x',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_x',p_real_1d,A%m_x,ierror)
         end select
         select type (A => for_var)
            class is (modatm_ref_type)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_bcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_bcs',p_int_2d,A%l_bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_dcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_dcs',p_int_2d,A%l_dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_mcs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_mcs',p_int_2d,A%l_mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_occ',py_var,A%l_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_ocs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_ocs',p_int_2d,A%l_ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_u',p_int_1d,A%l_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_ucs',py_var,p_int_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_ucs',p_int_2d,A%l_ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_u_iso',py_var,A%l_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','l_x',py_var,p_int_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','l_x',p_int_1d,A%l_x,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_bcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_bcs',p_real_2d,A%m_bcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_dcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_dcs',p_real_2d,A%m_dcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_mcs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_mcs',p_real_2d,A%m_mcs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_occ',py_var,A%m_occ,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_ocs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_ocs',p_real_2d,A%m_ocs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_u',p_real_1d,A%m_u,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_ucs',py_var,p_real_2d,ierror,order)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_ucs',p_real_2d,A%m_ucs,ierror,order)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_u_iso',py_var,A%m_u_iso,ierror)
               if (ierror == 0) call unwrap_dict_item('Unwrap_Atm_Type','m_x',py_var,p_real_1d,ierror)
               if (ierror == 0) call pointer_to_array('Unwrap_Atm_Type','m_x',p_real_1d,A%m_x,ierror)
         end select
      end if
      if (ierror /= 0 .and. err_cfml%ierr == 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'UnWrap_Atm_Type: Unwrapping failed'
     end if

   End Subroutine Unwrap_Atm_Type_No_Alloc

   Module Subroutine Wrap_Atlist_Type(for_var,py_var,ierror)
      !---- Arguments ----!
      type(atlist_type),  intent(in)    :: for_var
      type(dict),         intent(inout) :: py_var
      integer,            intent(out)   :: ierror

      !---- Local Variables ----!
      integer :: i
      type(ndarray) :: nd_iph
      type(dict), dimension(:), allocatable :: di_atom
      type(list) :: li_atom,li_active

      ierror = 0
      if (ierror == 0) ierror = py_var%setitem('fortran_type','atlist_type')
      if (ierror == 0) ierror = py_var%setitem('natoms',for_var%natoms)
      if (ierror == 0) ierror = py_var%setitem('mcomp',for_var%mcomp)
      if (ierror == 0) ierror = py_var%setitem('symm_checked',for_var%symm_checked)
      if (ierror == 0) ierror = list_create(li_active)
      do i = 1 , size(for_var%active)
          if (ierror == 0) ierror = li_active%append(for_var%active(i))
      end do
      if (ierror == 0) ierror = py_var%setitem('active',li_active)
      if (ierror == 0) ierror = ndarray_create(nd_iph,for_var%iph)
      if (ierror == 0) ierror = py_var%setitem('iph',nd_iph)
      if (ierror == 0) ierror = list_create(li_atom)
      if (ierror == 0) allocate(di_atom(for_var%natoms))
      if (ierror == 0) then
         do i = 1 , for_var%natoms
            ierror = dict_create(di_atom(i))
            if (ierror == 0) call wrap_atm_type(for_var%atom(i),di_atom(i),ierror)
            if (ierror == 0) ierror = li_atom%append(di_atom(i))
         end do
      end if
      if (ierror == 0) ierror = py_var%setitem('atom',li_atom)
      if (ierror /= 0) then
        err_cfml%flag = .true.
        err_cfml%ierr = -1
        err_cfml%msg  = 'Wrap_Atlist_Type: Wrapping failed'
     end if

   end subroutine Wrap_Atlist_Type

   Module Subroutine Wrap_Atm_Cell_Type(for_var,py_var,ierror)
      !---- Arguments ----!
      type(atm_cell_type), intent(in)    :: for_var
      type(dict),          intent(inout) :: py_var
      integer,             intent(out)   :: ierror

      !---- Local Variables ----!
      type(list) :: li_ddlab,li_lab
      type(ndarray) :: nd_charge,nd_ddist,nd_distance,nd_moment,nd_neighb,nd_neighb_atom,&
                       nd_trans,nd_var_free,nd_xyz

      ierror = 0
      if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_cell_type')
      if (ierror == 0) ierror = ndarray_create(nd_charge,for_var%charge)
      if (ierror == 0) ierror = py_var%setitem('charge',nd_charge)
      if (ierror == 0) ierror = ndarray_create(nd_ddist,for_var%ddist)
      if (ierror == 0) ierror = py_var%setitem('ddist',nd_ddist)
      if (ierror == 0) ierror = list_create(li_ddlab)
      if (ierror == 0) call array_to_list('Wrap_Atm_Cell_Type','ddlab',for_var%ddlab,li_ddlab,ierror)
      if (ierror == 0) ierror = py_var%setitem('ddlab',li_ddlab)
      if (ierror == 0) ierror = list_create(li_lab)
      if (ierror == 0) ierror = ndarray_create(nd_distance,for_var%distance)
      if (ierror == 0) ierror = py_var%setitem('distance',nd_distance)
      if (ierror == 0) call array_to_list('Wrap_Atm_Cell_Type','lab',for_var%lab,li_lab,ierror)
      if (ierror == 0) ierror = py_var%setitem('lab',li_lab)
      if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
      if (ierror == 0) ierror = py_var%setitem('moment',nd_moment)
      if (ierror == 0) ierror = py_var%setitem('nat',for_var%nat)
      if (ierror == 0) ierror = py_var%setitem('ndist',for_var%ndist)
      if (ierror == 0) ierror = ndarray_create(nd_neighb,for_var%neighb)
      if (ierror == 0) ierror = py_var%setitem('neighb',nd_neighb)
      if (ierror == 0) ierror = ndarray_create(nd_neighb_atom,for_var%neighb_atom)
      if (ierror == 0) ierror = py_var%setitem('neighb_atom',nd_neighb_atom)
      if (ierror == 0) ierror = ndarray_create(nd_trans,for_var%trans)
      if (ierror == 0) ierror = py_var%setitem('trans',nd_trans)
      if (ierror == 0) ierror = ndarray_create(nd_var_free,for_var%var_free)
      if (ierror == 0) ierror = py_var%setitem('var_free',nd_var_free)
      if (ierror == 0) ierror = ndarray_create(nd_xyz,for_var%xyz)
      if (ierror == 0) ierror = py_var%setitem('xyz',nd_xyz)
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'Wrap_Atm_Type: Wrapping failed'
      end if

   End Subroutine Wrap_Atm_Cell_Type

   Module Subroutine Wrap_Atm_Type(for_var,py_var,ierror)
      !---- Arguments ----!
      class(atm_type), intent(in)    :: for_var
      type(dict),      intent(inout) :: py_var
      integer,         intent(out)   :: ierror

      !---- Local Variables ----!
      type(ndarray) :: nd_x,nd_u,nd_moment,nd_ind_ff,nd_varf,nd_x_std,nd_u_std,&
                       nd_moment_std,nd_poc_q,nd_pbc_q,nd_pmc_q,nd_pdc_q,nd_puc_q,&
                       nd_ocs,nd_bcs,nd_mcs,nd_dcs,nd_ucs,nd_ocs_std,nd_bcs_std,&
                       nd_mcs_std,nd_dcs_std,nd_ucs_std,nd_xs,nd_moms,nd_us,nd_l_x,&
                       nd_l_moment,nd_l_u,nd_m_x,nd_m_moment,nd_m_u,nd_l_ocs,nd_l_bcs,&
                       nd_l_mcs,nd_l_dcs,nd_l_ucs,nd_m_ocs,nd_m_bcs,nd_m_mcs,nd_m_dcs,nd_m_ucs

      ierror = 0
      if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_type')
      if (ierror == 0) ierror = py_var%setitem('active',for_var%active)
      if (ierror == 0) ierror = py_var%setitem('atminfo',for_var%atminfo)
      if (ierror == 0) ierror = py_var%setitem('charge',for_var%charge)
      if (ierror == 0) ierror = py_var%setitem('chemsymb',for_var%chemsymb)
      if (ierror == 0) ierror = ndarray_create(nd_ind_ff,for_var%ind_ff)
      if (ierror == 0) ierror = py_var%setitem('ind_ff',nd_ind_ff)
      if (ierror == 0) ierror = py_var%setitem('lab',for_var%lab)
      if (ierror == 0) ierror = py_var%setitem('magnetic',for_var%magnetic)
      if (ierror == 0) ierror = py_var%setitem('mom',for_var%mom)
      if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
      if (ierror == 0) ierror = py_var%setitem('moment',nd_moment)
      if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
      if (ierror == 0) ierror = py_var%setitem('occ',for_var%occ)
      if (ierror == 0) ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
      if (ierror == 0) ierror = py_var%setitem('thtype',for_var%thtype)
      if (ierror == 0) ierror = ndarray_create(nd_u,for_var%u)
      if (ierror == 0) ierror = py_var%setitem('u',nd_u)
      if (ierror == 0) ierror = py_var%setitem('utype',for_var%utype)
      if (ierror == 0) ierror = py_var%setitem('u_iso',for_var%u_iso)
      if (ierror == 0) ierror = ndarray_create(nd_varf,for_var%varf)
      if (ierror == 0) ierror = py_var%setitem('varf',nd_varf)
      if (ierror == 0) ierror = py_var%setitem('wyck',for_var%wyck)
      if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
      if (ierror == 0) ierror = py_var%setitem('x',nd_x)
      if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
      select type (A => for_var)
         class is (atm_std_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_std_type')
            if (ierror == 0) ierror = ndarray_create(nd_moment_std,A%moment_std)
            if (ierror == 0) ierror = py_var%setitem('moment_std',nd_moment_std)
            if (ierror == 0) ierror = py_var%setitem('occ_std',A%occ_std)
            if (ierror == 0) ierror = py_var%setitem('u_iso_std',A%u_iso_std)
            if (ierror == 0) ierror = ndarray_create(nd_u_std,A%u_std)
            if (ierror == 0) ierror = py_var%setitem('u_std',nd_u_std)
            if (ierror == 0) ierror = ndarray_create(nd_x_std,A%x_std)
            if (ierror == 0) ierror = py_var%setitem('x_std',nd_x_std)
      end select
      select type (A => for_var)
         class is (modatm_std_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','modatm_std_type')
            if (ierror == 0) ierror = ndarray_create(nd_bcs,A%bcs)
            if (ierror == 0) ierror = py_var%setitem('bcs',nd_bcs)
            if (ierror == 0) ierror = ndarray_create(nd_bcs_std,A%bcs_std)
            if (ierror == 0) ierror = py_var%setitem('bcs_std',nd_bcs_std)
            if (ierror == 0) ierror = ndarray_create(nd_dcs,A%dcs)
            if (ierror == 0) ierror = py_var%setitem('dcs',nd_dcs)
            if (ierror == 0) ierror = ndarray_create(nd_dcs_std,A%dcs_std)
            if (ierror == 0) ierror = py_var%setitem('dcs_std',nd_dcs_std)
            if (ierror == 0) ierror = ndarray_create(nd_mcs,A%mcs)
            if (ierror == 0) ierror = py_var%setitem('mcs',nd_mcs)
            if (ierror == 0) ierror = ndarray_create(nd_mcs_std,A%mcs_std)
            if (ierror == 0) ierror = py_var%setitem('mcs_std',nd_mcs_std)
            if (ierror == 0) ierror = py_var%setitem('n_bc',A%n_bc)
            if (ierror == 0) ierror = py_var%setitem('n_dc',A%n_dc)
            if (ierror == 0) ierror = py_var%setitem('n_mc',A%n_mc)
            if (ierror == 0) ierror = py_var%setitem('n_oc',A%n_oc)
            if (ierror == 0) ierror = py_var%setitem('n_uc',A%n_uc)
            if (ierror == 0) ierror = ndarray_create(nd_moms,A%moms)
            if (ierror == 0) ierror = py_var%setitem('moms',nd_moms)
            if (ierror == 0) ierror = ndarray_create(nd_ocs,A%ocs)
            if (ierror == 0) ierror = py_var%setitem('ocs',nd_ocs)
            if (ierror == 0) ierror = ndarray_create(nd_ocs_std,A%ocs_std)
            if (ierror == 0) ierror = py_var%setitem('ocs_std',nd_ocs_std)
            if (ierror == 0) ierror = ndarray_create(nd_pbc_q,A%pbc_q)
            if (ierror == 0) ierror = py_var%setitem('pbc_q',nd_pbc_q)
            if (ierror == 0) ierror = ndarray_create(nd_pdc_q,A%pdc_q)
            if (ierror == 0) ierror = py_var%setitem('pdc_q',nd_pdc_q)
            if (ierror == 0) ierror = ndarray_create(nd_pmc_q,A%pmc_q)
            if (ierror == 0) ierror = py_var%setitem('pmc_q',nd_pmc_q)
            if (ierror == 0) ierror = ndarray_create(nd_poc_q,A%poc_q)
            if (ierror == 0) ierror = py_var%setitem('poc_q',nd_poc_q)
            if (ierror == 0) ierror = ndarray_create(nd_puc_q,A%puc_q)
            if (ierror == 0) ierror = py_var%setitem('puc_q',nd_puc_q)
            if (ierror == 0) ierror = ndarray_create(nd_ucs,A%ucs)
            if (ierror == 0) ierror = py_var%setitem('ucs',nd_ucs)
            if (ierror == 0) ierror = ndarray_create(nd_ucs_std,A%ucs_std)
            if (ierror == 0) ierror = py_var%setitem('ucs_std',nd_ucs_std)
            if (ierror == 0) ierror = ndarray_create(nd_us,A%us)
            if (ierror == 0) ierror = py_var%setitem('us',nd_us)
            if (ierror == 0) ierror = ndarray_create(nd_xs,A%xs)
            if (ierror == 0) ierror = py_var%setitem('xs',nd_xs)
      end select
      select type (A => for_var)
         class is (atm_ref_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','atm_ref_type')
            if (ierror == 0) ierror = ndarray_create(nd_l_moment,A%l_moment)
            if (ierror == 0) ierror = py_var%setitem('l_moment',nd_l_moment)
            if (ierror == 0) ierror = ndarray_create(nd_l_u,A%l_u)
            if (ierror == 0) ierror = py_var%setitem('l_u',nd_l_u)
            if (ierror == 0) ierror = py_var%setitem('l_occ',A%l_occ)
            if (ierror == 0) ierror = py_var%setitem('l_u_iso',A%l_u_iso)
            if (ierror == 0) ierror = ndarray_create(nd_l_x,A%l_x)
            if (ierror == 0) ierror = py_var%setitem('l_x',nd_l_x)
            if (ierror == 0) ierror = ndarray_create(nd_m_moment,A%m_moment)
            if (ierror == 0) ierror = py_var%setitem('m_moment',nd_m_moment)
            if (ierror == 0) ierror = py_var%setitem('m_occ',A%m_occ)
            if (ierror == 0) ierror = ndarray_create(nd_m_u,A%m_u)
            if (ierror == 0) ierror = py_var%setitem('m_u',nd_m_u)
            if (ierror == 0) ierror = py_var%setitem('m_u_iso',A%m_u_iso)
            if (ierror == 0) ierror = ndarray_create(nd_m_x,A%m_x)
            if (ierror == 0) ierror = py_var%setitem('m_x',nd_m_x)
         class is (modatm_ref_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','modatm_ref_type')
            if (ierror == 0) ierror = ndarray_create(nd_l_bcs,A%l_bcs)
            if (ierror == 0) ierror = py_var%setitem('l_bcs',nd_l_bcs)
            if (ierror == 0) ierror = ndarray_create(nd_l_dcs,A%l_dcs)
            if (ierror == 0) ierror = py_var%setitem('l_dcs',nd_l_dcs)
            if (ierror == 0) ierror = ndarray_create(nd_l_mcs,A%l_mcs)
            if (ierror == 0) ierror = py_var%setitem('l_mcs',nd_l_mcs)
            if (ierror == 0) ierror = py_var%setitem('l_occ',A%l_occ)
            if (ierror == 0) ierror = ndarray_create(nd_l_ocs,A%l_ocs)
            if (ierror == 0) ierror = py_var%setitem('l_ocs',nd_l_ocs)
            if (ierror == 0) ierror = ndarray_create(nd_l_u,A%l_u)
            if (ierror == 0) ierror = py_var%setitem('l_u',nd_l_u)
            if (ierror == 0) ierror = ndarray_create(nd_l_ucs,A%l_ucs)
            if (ierror == 0) ierror = py_var%setitem('l_ucs',nd_l_ucs)
            if (ierror == 0) ierror = py_var%setitem('l_u_iso',A%l_u_iso)
            if (ierror == 0) ierror = ndarray_create(nd_l_x,A%l_x)
            if (ierror == 0) ierror = py_var%setitem('l_x',nd_l_x)
            if (ierror == 0) ierror = ndarray_create(nd_m_bcs,A%m_bcs)
            if (ierror == 0) ierror = py_var%setitem('m_bcs',nd_m_bcs)
            if (ierror == 0) ierror = ndarray_create(nd_m_dcs,A%m_dcs)
            if (ierror == 0) ierror = py_var%setitem('m_dcs',nd_m_dcs)
            if (ierror == 0) ierror = ndarray_create(nd_m_mcs,A%m_mcs)
            if (ierror == 0) ierror = py_var%setitem('m_mcs',nd_m_mcs)
            if (ierror == 0) ierror = py_var%setitem('m_occ',A%m_occ)
            if (ierror == 0) ierror = ndarray_create(nd_m_ocs,A%m_ocs)
            if (ierror == 0) ierror = py_var%setitem('m_ocs',nd_m_ocs)
            if (ierror == 0) ierror = py_var%setitem('m_u_iso',A%m_u_iso)
            if (ierror == 0) ierror = ndarray_create(nd_m_u,A%m_u)
            if (ierror == 0) ierror = py_var%setitem('m_u',nd_m_u)
            if (ierror == 0) ierror = ndarray_create(nd_m_ucs,A%m_ucs)
            if (ierror == 0) ierror = py_var%setitem('m_ucs',nd_m_ucs)
            if (ierror == 0) ierror = ndarray_create(nd_m_x,A%m_x)
            if (ierror == 0) ierror = py_var%setitem('m_x',nd_m_x)
      end select
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'Wrap_Atm_Type: Wrapping failed'
      end if

   End Subroutine Wrap_Atm_Type

End SubModule Atm_Python_Wraps