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
   Module Subroutine Allocate_RelationList(Npar, R)
      !---- Arguments ----!
      integer,                 intent(in)     :: NPar
      type(RelationList_Type), intent(in out) :: R

      !---- Local Arguments ----!
      integer :: i

      if (Npar ==0) then
         R%NPar=0
         if allocated(R%Par) deallocate(R%Par)
         return
      end if

      R%Npar=NPar
      if allocated(R%Par) deallocate(R%Par)
      allocate(R%Par(NPar))

      !> Initialize
      do i=1,NPar
         Par(i)%Nam=" "
         Par(i)%L=0
         Par(i)%M=0.0_cp
         Par(i)%Val=0.0_cp
         Par(i)%Sig=0.0_cp
      end do

   End Subroutine Allocate_RelationList

End SubModule KeyCod_Relat