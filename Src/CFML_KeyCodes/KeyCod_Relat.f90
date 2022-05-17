!!
Submodule (CFML_KeyCodes) KeyCod_Relat
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE Allocate_RelationsType
   !!----
   !!----
   !!---- Update: 13/05/2022
   !!
   Module Subroutine Allocate_RelationList(NDMax, R)
      !---- Arguments ----!
      integer,                 intent(in)     :: NDMax
      type(RelationList_Type), intent(in out) :: R

      !---- Local Arguments ----!
      integer :: i

      if (NDMax ==0) then
         R%ND_Max=0
         R%NPar=0
         if (allocated(R%Par)) deallocate(R%Par)
         return
      end if

      R%ND_Max=NDMax
      if (allocated(R%Par)) deallocate(R%Par)
      allocate(R%Par(NDMax))

      !> Initialize
      do i=1,NDMax
         R%Par(i)%Name=" "
         R%Par(i)%Ext=" "
         R%Par(i)%L=0
         R%Par(i)%M=0.0_cp
         R%Par(i)%Val=0.0_cp
         R%Par(i)%Sig=0.0_cp
      end do

      R%NPar=0

   End Subroutine Allocate_RelationList

   !!--++
   !!--++ Subroutine Del_RefCode_RelationList
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the RelationList type
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Del_RefCode_RelationList(R, NPar)
      !---- Arguments ----!
      type(RelationList_Type), intent(in out) :: R
      integer,                 intent(in)     :: NPar

      !---- Local Variables ----!
      logical :: deleted
      integer :: i,j,k

      deleted=.false.

      !> Delete the NPar Parameter
      k=0
      do i=1,R%NPar
         if (R%Par(i)%L == NPar) then
            R%Par(i)%L=0
            R%Par(i)%M=0.0_cp
            deleted=.true.
            k=k+1
         end if
      end do

      !> Updating Variables
      do i=1,R%Npar
         if (R%Par(i)%L > NPar) then
            R%Par(i)%L=R%Par(i)%L-1
         end if
      end do

      R%Npar=R%NPar-k

      !> Updating Vec_Vectors
      if (deleted) call Del_Element_in_VRef(NPar)

   End Subroutine Del_RefCode_RelationList

End SubModule KeyCod_Relat