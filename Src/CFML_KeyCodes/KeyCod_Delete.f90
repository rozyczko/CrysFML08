!!
Submodule (CFML_KeyCodes) KeyCod_Delete
   implicit none

   Contains
   !!--++
   !!--++ Subroutine Del_RefCode_Atom
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the Atom list
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Del_RefCode_Atom(AtList, NPar)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NPar

      !---- Local Variables ----!
      logical :: deleted
      integer :: i,j

      deleted=.false.
      
      select type (A => AtList%Atom)
         type is (Atm_Ref_Type)
            !> Delete the NPar Parameter
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) == NPar) then
                     A(i)%l_x(j)=0
                     A(i)%m_x(j)=0.0_cp
                     deleted=.true.
                  end if
               end do

               if (A(i)%l_U_iso == NPar) then
                  A(i)%l_U_iso=0
                  A(i)%m_U_iso=0.0_cp
                  deleted=.true.
               end if

               if (A(i)%l_occ == NPar) then
                  A(i)%l_occ=0
                  A(i)%m_occ=0.0_cp
                  deleted=.true.
               end if

               do j=1,6
                  if (A(i)%l_u(j) == NPar) then
                     A(i)%l_u(j)=0
                     A(i)%m_u(j)=0.0_cp
                     deleted=.true.
                  end if
               end do
            end do

            !> Updating Variables
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) > NPar) then
                     A(i)%l_x(j)=A(i)%l_x(j)-1
                  end if
               end do

               if (A(i)%l_U_iso > NPar) then
                  A(i)%l_U_iso=A(i)%l_U_iso-1
               end if

               if (A(i)%l_occ > NPar) then
                  A(i)%l_occ=A(i)%l_occ-1
               end if

               do j=1,6
                  if (A(i)%l_u(j) > NPar) then
                     A(i)%l_u(j)=A(i)%l_u(j)-1
                  end if
               end do
            end do
            
         type is (MAtm_Ref_Type) 
            !! Faltan todavia partes magnéticas...
            !> Delete the NPar Parameter
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) == NPar) then
                     A(i)%l_x(j)=0
                     A(i)%m_x(j)=0.0_cp
                     deleted=.true.
                  end if
               end do

               if (A(i)%l_U_iso == NPar) then
                  A(i)%l_U_iso=0
                  A(i)%m_U_iso=0.0_cp
                  deleted=.true.
               end if

               if (A(i)%l_occ == NPar) then
                  A(i)%l_occ=0
                  A(i)%m_occ=0.0_cp
                  deleted=.true.
               end if

               do j=1,6
                  if (A(i)%l_u(j) == NPar) then
                     A(i)%l_u(j)=0
                     A(i)%m_u(j)=0.0_cp
                     deleted=.true.
                  end if
               end do
            end do

            !> Updating Variables
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) > NPar) then
                     A(i)%l_x(j)=A(i)%l_x(j)-1
                  end if
               end do

               if (A(i)%l_U_iso > NPar) then
                  A(i)%l_U_iso=A(i)%l_U_iso-1
               end if

               if (A(i)%l_occ > NPar) then
                  A(i)%l_occ=A(i)%l_occ-1
               end if

               do j=1,6
                  if (A(i)%l_u(j) > NPar) then
                     A(i)%l_u(j)=A(i)%l_u(j)-1
                  end if
               end do
            end do
            
      end select      

      !> Updating Vec_Vectors
      if (deleted) call Del_Element_in_VRef(NPar)

   End Subroutine Del_RefCode_Atom

   !!--++
   !!--++ Subroutine Del_Element_in_VRef
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Del_Element_in_VRef(N)
      !---- Arguments ----!
      integer, intent(in) :: N

      !---- Local Variables ----!
      integer :: i
      
      !> Check
      if (N <= 0 .or. N > NP_Ref) return

      if (N < NP_Ref) then
         do i=N+1,NP_Ref
            Vec_NamePar(i-1)   = Vec_NamePar(i)
            Vec_RefPar(i-1)    = Vec_RefPar(i)
            Vec_RefParSTD(i-1) = Vec_RefParSTD(i)
            Vec_RefSave(i-1)   = Vec_RefSave(i)
            Vec_RefShift(i-1)  = Vec_RefShift(i)
            Vec_LimPar(:,i-1)  = Vec_LimPar(:,i)
            Vec_BCond(i-1)     = Vec_BCond(i)
            Vec_PointPar(i-1)  = Vec_PointPar(i)
         end do
      end if

      Vec_Name(NP_Ref)     =" "
      Vec_RefPar(NP_Ref)   =0.0_cp
      Vec_RefParSTD(NP_Ref)=0.0_cp
      Vec_RefSave(NP_Ref)  =0.0_cp
      Vec_RefShift(NP_Ref) =0.0_cp
      Vec_LimPar(:,NP_Ref) =0.0_cp
      Vec_BCons(NP_Ref)    =0
      Vec_PointPar(NP_Ref) =0

      NP_Ref=NP_Ref-1

   End Subroutine Del_Element_in_VRef

End SubModule KeyCod_Delete
