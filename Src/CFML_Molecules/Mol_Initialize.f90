Submodule (CFML_Molecules) Mol_Initialize

   implicit none
 
 Contains
   !!----
   !!---- Subroutine Init_Molecule(Molecule,Natm)
   !!----
   !!----    Initialize the Variable Molecule
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_Molecule(Mol, Natm)
      !---- Argument ----!
      type(Molecule_Type), intent(in out) :: Mol
      integer, optional,   intent(in)     :: Natm

      mol%name_mol   =" "
      mol%natoms     =0

      mol%in_xtal    = .false.
      mol%is_eulerMat= .false.
      mol%is_connect = .false.
      mol%rot_type   =" "
      mol%coor_type  =" "
      mol%therm_type =" "

      mol%xcentre    =0.0_cp
      mol%mxcentre   =1.0_cp
      mol%lxcentre   =0

      mol%orient     =0.0_cp
      mol%mOrient    =1.0_cp
      mol%lorient    =0

      mol%t_tls      =0.0_cp
      mol%mT_TLS     =1.0_cp
      mol%lt_tls     =0

      mol%l_tls      =0.0_cp
      mol%mL_TLS     =1.0_cp
      mol%ll_tls     =0

      mol%s_tls      =0.0_cp
      mol%mS_TLS     =1.0_cp
      mol%ls_tls     =0

      mol%euler      =0.0_cp

      if (allocated(mol%AtName))  deallocate(mol%AtName)
      if (allocated(mol%AtSymb))  deallocate(mol%AtSymb)
      if (allocated(mol%AtZ))     deallocate(mol%AtZ)
      if (allocated(mol%Ptr))     deallocate(mol%Ptr)
      if (allocated(mol%I_Coor))  deallocate(mol%I_Coor)
      if (allocated(mol%mI_Coor)) deallocate(mol%mI_Coor)
      if (allocated(mol%lI_Coor)) deallocate(mol%lI_Coor)
      if (allocated(mol%U_iso))    deallocate(mol%U_iso)
      if (allocated(mol%mU_iso))   deallocate(mol%mU_iso)
      if (allocated(mol%lU_iso))   deallocate(mol%lU_iso)
      if (allocated(mol%Occ))     deallocate(mol%Occ)
      if (allocated(mol%mocc))    deallocate(mol%mocc)
      if (allocated(mol%lOcc))    deallocate(mol%lOcc)
      if (allocated(mol%Nb))      deallocate(mol%Nb)
      if (allocated(mol%INb))     deallocate(mol%INb)
      if (allocated(mol%Tb))      deallocate(mol%Tb)
      if (allocated(mol%Conn))    deallocate(mol%Conn)

      if (present(natm)) then
         if (natm > 0) then
            mol%natoms=natm

            allocate(mol%AtName(natm))
            allocate(mol%AtSymb(natm))
            allocate(mol%AtZ(natm))
            allocate(mol%Ptr(2,natm))
            allocate(mol%I_Coor(3,natm))
            allocate(mol%mI_Coor(3,natm))
            allocate(mol%lI_Coor(3,natm))
            allocate(mol%U_iso(natm))
            allocate(mol%mU_iso(natm))
            allocate(mol%lU_iso(natm))
            allocate(mol%Occ(natm))
            allocate(mol%mocc(natm))
            allocate(mol%lOcc(natm))
            allocate(mol%Nb(natm))
            allocate(mol%INb(10,natm))
            allocate(mol%Tb(10,natm))
            allocate(mol%Conn(3,natm))

            mol%AtName  =" "
            mol%AtSymb  =" "
            mol%AtZ     =0
            mol%Ptr     =0
            mol%I_Coor  =0.0_cp
            mol%mI_Coor =1.0_cp
            mol%lI_Coor =0
            mol%U_iso   =0.0_cp
            mol%mU_iso  =1.0_cp
            mol%lU_iso  =0
            mol%Occ     =0.0_cp
            mol%mocc    =1.0_cp
            mol%lOcc    =0
            mol%Nb      =0
            mol%INb     =0
            mol%Tb      =0
            mol%Conn    =0

         end if
      end if

   End Subroutine Init_Molecule
 
End Submodule Mol_Initialize 