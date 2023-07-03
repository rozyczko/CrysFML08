!!----
!!----
!!----
SubModule (CFML_Python)  Atm_Python_Wraps
  implicit none
  Contains

   !!----
   !!---- WRAP_ATOM_TYPE
   !!----
   !!---- 24/03/2023
   !!
   Module Subroutine Wrap_Atm_Type(for_var, py_var)
      !---- Arguments ----!
      class(atm_type), intent(in)    :: for_var
      type(dict),      intent(inout) :: py_var

      !---- Local Variables ----!
      integer :: ierror
      type(ndarray) :: nd_x,nd_u,nd_moment,nd_ind_ff,nd_varf

      ierror = 0
      if (ierror == 0) ierror = py_var%setitem('lab',for_var%lab)
      if (ierror == 0) ierror = py_var%setitem('chemsymb',for_var%chemsymb)
      if (ierror == 0) ierror = py_var%setitem('sfacsymb',for_var%sfacsymb)
      if (ierror == 0) ierror = py_var%setitem('z',for_var%z)
      if (ierror == 0) ierror = py_var%setitem('mult',for_var%mult)
      if (ierror == 0) ierror = py_var%setitem('charge',for_var%charge)
      if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
      if (ierror == 0) ierror = py_var%setitem('x',nd_x)
      if (ierror == 0) ierror = py_var%setitem('u_iso',for_var%u_iso)
      if (ierror == 0) ierror = py_var%setitem('occ',for_var%occ)
      if (ierror == 0) ierror = py_var%setitem('utype',for_var%utype)
      if (ierror == 0) ierror = py_var%setitem('thtype',for_var%thtype)
      if (ierror == 0) ierror = ndarray_create(nd_u,for_var%u)
      if (ierror == 0) ierror = py_var%setitem('u',nd_u)
      if (ierror == 0) ierror = py_var%setitem('magnetic',for_var%magnetic)
      if (ierror == 0) ierror = py_var%setitem('mom',for_var%mom)
      if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
      if (ierror == 0) ierror = py_var%setitem('moment',nd_moment)
      if (ierror == 0) ierror = ndarray_create(nd_ind_ff,for_var%ind_ff)
      if (ierror == 0) ierror = py_var%setitem('ind_ff',nd_ind_ff)
      if (ierror == 0) ierror = py_var%setitem('atminfo',for_var%atminfo)
      if (ierror == 0) ierror = py_var%setitem('wyck',for_var%wyck)
      if (ierror == 0) ierror = ndarray_create(nd_varf,for_var%varf)
      if (ierror == 0) ierror = py_var%setitem('varf',nd_varf)
      if (ierror == 0) ierror = py_var%setitem('active',for_var%active)
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'Wrap_Atm_Type: Wrapping failed'
      end if

   End Subroutine Wrap_Atm_Type

   !!----
   !!---- WRAP_ATLIST_TYPE
   !!----
   !!---- 24/03/2023
   !!
   Module Subroutine Wrap_Atlist_Type(for_var, py_var)
      !---- Arguments ----!
      type(atlist_type),  intent(in)    :: for_var
      type(dict),         intent(inout) :: py_var

      !---- Local Variables ----!
      integer :: ierror,i
      type(ndarray) :: nd_iph
      type(dict), dimension(:), allocatable :: di_atom
      type(list) :: li_atom,li_active

      ierror = 0
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
            if (ierror == 0) call wrap_atm_type(for_var%atom(i),di_atom(i))
            ierror = err_cfml%ierr
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

End SubModule Atm_Python_Wraps