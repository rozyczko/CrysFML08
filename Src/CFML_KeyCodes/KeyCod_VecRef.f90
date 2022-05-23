!!
Submodule (CFML_KeyCodes) KeyCod_VecRef
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE ALLOCATE_VECREF
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Allocate_VecRef(N)
      !---- Arguments ----!
      integer, intent(in) :: N

      NP_Ref_Max=0
      if (allocated(Vec_RefPar))    deallocate(Vec_RefPar)
      if (allocated(Vec_RefSave))   deallocate(Vec_RefSave)
      if (allocated(Vec_RefParSTD)) deallocate(Vec_RefParSTD)
      if (allocated(Vec_RefShift))  deallocate(Vec_RefShift)
      if (allocated(Vec_NamePar))   deallocate(Vec_NamePar)
      if (allocated(Vec_LimPar))    deallocate(Vec_LimPar)

      if (allocated(Vec_BCond))     deallocate(Vec_BCond)
      if (allocated(Vec_PointPar))  deallocate(Vec_PointPar)
      if (N <= 0) return

      allocate(Vec_RefPar(n), Vec_RefSave(n), Vec_RefParSTD(n),Vec_RefShift(n), Vec_PointPar(n), Vec_BCond(n))
      Vec_RefPar   =0.0_cp
      Vec_RefSave  =0.0_cp
      Vec_RefParSTD=0.0_cp
      Vec_RefShift =0.0_cp
      Vec_PointPar=0

      allocate(Vec_NamePar(n))
      Vec_NamePar=" "

      allocate(Vec_LimPar(3,n))
      Vec_LimPar=0.0_cp

      NP_Ref_Max=N
   End Subroutine Allocate_VecRef

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
            if ( Vec_PointPar(i) < N) then
               Vec_PointPar(i-1)  = Vec_PointPar(i)
            else
               Vec_PointPar(i-1)  = Vec_PointPar(i)-1
            end if
         end do
      end if

      Vec_NamePar(NP_Ref)  =" "
      Vec_RefPar(NP_Ref)   =0.0_cp
      Vec_RefParSTD(NP_Ref)=0.0_cp
      Vec_RefSave(NP_Ref)  =0.0_cp
      Vec_RefShift(NP_Ref) =0.0_cp
      Vec_LimPar(:,NP_Ref) =0.0_cp
      Vec_BCond(NP_Ref)    =0
      Vec_PointPar(NP_Ref) =0

      NP_Ref=NP_Ref-1

   End Subroutine Del_Element_in_VRef


End SubModule KeyCod_VecRef
