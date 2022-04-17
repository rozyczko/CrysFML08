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

      allocate(Vec_RefPar(n), Vec_RefSave(n), Vec_RefParSTD(n),Vec_RefShift(n), Vec_RefPointPar(n), Vec_BCond(n))
      Vec_RefPar   =0.0_cp
      Vec_RefSave  =0.0_cp
      Vec_RefParSTD=0.0_cp
      Vec_RefShift =0.0_cp
      Vec_RefPointPar=0

      allocate(Vec_NamePar(n))
      Vec_NamePar=" "

      allocate(Vec_LimPar(3,n))
      Vec_LimPar=0.0_cp

      NP_Ref_Max=N
   End Subroutine Allocate_VecRef


End SubModule KeyCod_VecRef
