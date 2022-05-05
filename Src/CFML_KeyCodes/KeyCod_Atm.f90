Submodule (CFML_KeyCodes) KeyCod_Atm
   implicit none

   Contains
   !!--++
   !!--++ Subroutine Fix_XYZ_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_XYZ_Atm(Atlist, NAtm, Ind)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm
      integer,           intent(in)     :: Ind ! 1:X, 2:Y, 3:Z, 0:XYZ


      !---- Local Variables ----!
      integer :: i,nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) /=0) then
                         nc=A%l_x(Ind)
                         call Del_RefCode_Atm(Atlist,nc)
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) /=0) then
                           nc=A%l_x(i)
                           call Del_RefCode_Atm(Atlist,nc)
                        end if
                     end do
               end select

            type is (MAtm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) /=0) then
                         nc=A%l_x(Ind)
                         call Del_RefCode_Atm(Atlist,nc)
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) /=0) then
                           nc=A%l_x(i)
                           call Del_RefCode_Atm(Atlist,nc)
                        end if
                     end do
               end select
         end select
      end associate

   End Subroutine Fix_XYZ_Atm

   !!--++
   !!--++ Subroutine Fix_OCC_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_OCC_Atm(Atlist, NAtm)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm

      !---- Local Variables ----!
      integer:: nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               if (A%l_occ /=0) then
                  nc=A%l_occ
                  call Del_RefCode_Atm(Atlist, nc)
               end if

            type is (MAtm_Ref_Type)
               if (A%l_occ /=0) then
                  nc=A%l_occ
                  call Del_RefCode_Atm(Atlist, nc)
               end if
         end select
      end associate

   End Subroutine Fix_OCC_Atm

   !!--++
   !!--++ Subroutine Fix_U_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_U_Atm(Atlist, NAtm, Ind)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm
      integer,           intent(in)     :: Ind  ! 0:U_iso, 1:U11, 2:U22, 3:U33, 4:U12
                                                ! 5: U13  6: U23, -1: All U's


      !---- Local Variables ----!
      integer :: i,nc
      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_u_iso /=0) then
                         nc=A%l_u_iso
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (1:6)
                     if (A%l_u(Ind) /=0) then
                         nc=A%l_u(Ind)
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) /=0) then
                           nc=A%l_u(i)
                           call Del_RefCode_Atm(Atlist, nc)
                        end if
                     end do
               end select

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_u_iso /=0) then
                         nc=A%l_u_iso
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (1:6)
                     if (A%l_u(Ind) /=0) then
                         nc=A%l_u(Ind)
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) /=0) then
                           nc=A%l_u(i)
                           call Del_RefCode_Atm(Atlist, nc)
                        end if
                     end do
               end select
         end select
      end associate

   End Subroutine Fix_U_Atm

   !!--++
   !!--++ Subroutine Vary_XYZ_Atm
   !!--++
   !!--++    Vary Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_XYZ_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      integer,                     intent(in)     :: Ind    ! 1:X, 2:Y, 3:Z, 0:XYZ
      class(SpG_Type),              intent(in)    :: Spg
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic


      !---- Local Variables ----!
      integer :: i,nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) ==0) then
                        A%m_x(Ind)=1.0_cp
                        call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                        if (A%l_x(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%X(Ind)
                           Vec_NamePar(NP_Ref)=trim(key_atm(Ind))//'_'//trim(A%Lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                           !Vec_RefParSTD=0.0_cp
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) ==0) then
                           A%m_x(i)=1.0_cp
                           call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                           if (A%l_x(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%X(i)
                              Vec_NamePar(NP_Ref)=trim(key_atm(i))//'_'//trim(A%Lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                              !Vec_RefParSTD=0.0_cp
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) ==0) then
                        A%m_x(Ind)=1.0_cp
                        call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                        if (A%l_x(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%X(Ind)
                           Vec_NamePar(NP_Ref)=trim(key_atm(Ind))//'_'//trim(A%Lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                           !Vec_RefParSTD=0.0_cp
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) ==0) then
                           A%m_x(i)=1.0_cp
                           call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                           if (A%l_x(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%X(i)
                              Vec_NamePar(NP_Ref)=trim(key_atm(i))//'_'//trim(A%Lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                              !Vec_RefParSTD=0.0_cp
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind
         end select ! A
      end associate

   End Subroutine Vary_XYZ_Atm

   !!--++
   !!--++ Subroutine Vary_OCC_Atm
   !!--++
   !!--++    Vary Occupancy factor Code
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic

      !---- Local Variables ----!
      integer:: nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               if (A%l_occ ==0) then
                  NP_Ref=NP_Ref+1

                  Vec_RefPar(NP_Ref)=A%occ
                  Vec_NamePar(NP_Ref)="Occ_"//trim(A%lab)
                  A%m_occ=1.0_cp
                  A%l_occ=NP_Ref
                  Vec_LimPar(:,NP_Ref)=Bounds
                  Vec_BCond(NP_Ref)=Ic
                  Vec_PointPar(NP_Ref)=Natm
               end if

            type is (Matm_Ref_Type)
               if (A%l_occ ==0) then
                  NP_Ref=NP_Ref+1

                  Vec_RefPar(NP_Ref)=A%occ
                  Vec_NamePar(NP_Ref)="Occ_"//trim(A%lab)
                  A%m_occ=1.0_cp
                  A%l_occ=NP_Ref
                  Vec_LimPar(:,NP_Ref)=Bounds
                  Vec_BCond(NP_Ref)=Ic
                  Vec_PointPar(NP_Ref)=Natm
               end if
         end select
      end associate

   End Subroutine Vary_OCC_Atm

   !!--++
   !!--++ Subroutine Vary_U_Atm
   !!--++
   !!--++    Vary Thermal factors  Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_U_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      integer,                     intent(in)     :: Ind    ! 0:Uiso, 1-6:Uij, -1:All thermal
      class(SpG_Type),              intent(in)     :: Spg
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic


      !---- Local Variables ----!
      integer :: i

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_U_iso ==0) then
                        NP_Ref=NP_Ref+1

                        Vec_RefPar(NP_Ref)=A%U_iso
                        Vec_NamePar(NP_Ref)="U_iso_"//trim(A%lab)
                        A%m_U_iso=1.0_cp
                        A%l_U_iso=NP_Ref
                        Vec_LimPar(:,NP_Ref)=Bounds
                        Vec_BCond(NP_Ref)=Ic
                        Vec_PointPar(NP_Ref)=Natm
                     end if

                  case (1:6)
                     if (A%l_u(Ind) ==0) then
                        A%m_u(Ind)=1.0_cp
                        call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                        if (A%l_u(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%U(Ind)
                           Vec_NamePar(NP_Ref)=trim(Key_atm(7+ind))//'_'//trim(A%lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) ==0) then
                           A%m_u(i)=1.0_cp
                           call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                           if (A%l_u(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%U(i)
                              Vec_NamePar(NP_Ref)=trim(Key_atm(7+i))//'_'//trim(A%lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_U_iso ==0) then
                        NP_Ref=NP_Ref+1

                        Vec_RefPar(NP_Ref)=A%U_iso
                        Vec_NamePar(NP_Ref)="U_iso_"//trim(A%lab)
                        A%m_U_iso=1.0_cp
                        A%l_U_iso=NP_Ref
                        Vec_LimPar(:,NP_Ref)=Bounds
                        Vec_BCond(NP_Ref)=Ic
                        Vec_PointPar(NP_Ref)=Natm
                     end if

                  case (1:6)
                     if (A%l_u(Ind) ==0) then
                        A%m_u(Ind)=1.0_cp
                        call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                        if (A%l_u(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%U(Ind)
                           Vec_NamePar(NP_Ref)=trim(Key_atm(7+ind))//'_'//trim(A%lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) ==0) then
                           A%m_u(i)=1.0_cp
                           call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                           if (A%l_u(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%U(i)
                              Vec_NamePar(NP_Ref)=trim(Key_atm(7+i))//'_'//trim(A%lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

         end select ! A
      end associate

   End Subroutine Vary_U_Atm

End SubModule KeyCod_Atm
