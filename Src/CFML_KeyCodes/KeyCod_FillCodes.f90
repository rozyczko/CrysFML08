!!
Submodule (CFML_KeyCodes) KeyCod_FillCodes
   implicit none

   Contains
   !!--++
   !!--++ SUBROUTINE FILL_REFCODES_ATM
   !!--++
   !!--++   Write on Vectors the Information for Free Atoms
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fill_RefCodes_Atm(Keyword, Npar, Bounds, Ic, Natm, Spg, AtList)
      !---- Arguments ----!
      character(len=*),              intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                       intent(in)     :: NPar        ! Specific parameter X_,Y_,Occ_,...
      real(kind=cp), dimension(3),   intent(in)     :: Bounds      ! Lower, Upper and Step limits
      integer,                       intent(in)     :: Ic          ! 0/1 boundary conditions (0:fixed or 1:periodic)
      integer,                       intent(in)     :: Natm        ! Number of specific atom on the list
      type(Spg_Type),                intent(in)     :: Spg
      type(AtList_Type),             intent(in out) :: AtList

      !---- Local variables ----!
      integer          :: j,nc,np_ini
      character(len=4) :: cdire

      !> Init
      call clear_error()

      !> Check
      if (Natm <=0) then
         call set_error(1," The directive have to be apply on a specific atom!")
         return
      end if

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for specific Atom!")
                  return

               case ( 1) ! X
                  call Fix_XYZ_Atm(Atlist, Natm, 1)

               case ( 2) ! Y
                  call Fix_XYZ_Atm(Atlist, Natm, 2)

               case ( 3) ! Z
                  call Fix_XYZ_Atm(Atlist, Natm, 3)

               case ( 4) ! XYZ
                  call Fix_XYZ_Atm(Atlist, Natm, 0)

               case ( 5) ! OCC
                  call Fix_Occ_Atm(Atlist, Natm)

               case ( 6) ! U_ISO
                  call Fix_U_Atm(Atlist, Natm,0)

               case ( 7) ! U
                  call Fix_U_Atm(Atlist, Natm,-1)

               case ( 8) ! U11
                  call Fix_U_Atm(Atlist, Natm,1)

               case ( 9) ! U22
                  call Fix_U_Atm(Atlist, Natm,2)

               case (10) ! U33
                  call Fix_U_Atm(Atlist, Natm,3)

               case (11) ! U12
                  call Fix_U_Atm(Atlist, Natm,4)

               case (12) ! U13
                  call Fix_U_Atm(Atlist, Natm,5)

               case (13) ! U23
                  call Fix_U_Atm(Atlist, Natm,6)

               case (14) ! ALL
                  call Fix_XYZ_Atm(Atlist, Natm, 0)
                  call Fix_Occ_Atm(Atlist, Natm)
                  call Fix_U_Atm(Atlist,  Natm,0)
                  call Fix_U_Atm(Atlist,  Natm,-1)

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for specific Atom!")
                  return

               case ( 1:3) ! X
                  call Vary_XYZ_Atm(Atlist, NAtm, NPar, Spg, Bounds, Ic)

               case ( 4) ! XYZ
                  call Vary_XYZ_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)

               case ( 5) ! OCC
                  call Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)

               case ( 6) ! U_ISO
                  call Vary_U_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)

               case ( 7) ! U
                  call Vary_U_Atm(Atlist, NAtm, -1, Spg, Bounds, Ic)

               case ( 8:13) ! U's'
                  call Vary_U_Atm(Atlist, NAtm, NPar-7, Spg, Bounds, Ic)

               case (14) ! ALL
                  call Vary_XYZ_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)
                  call Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)
                  call Vary_U_Atm(Atlist,  NAtm, 0, Spg, Bounds, Ic)
                  call Vary_U_Atm(Atlist,  NAtm, -1, Spg, Bounds, Ic)

            end select ! Npar

      end select ! Directives

   End Subroutine Fill_RefCodes_Atm



End SubModule KeyCod_FillCodes
