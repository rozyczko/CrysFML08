!!----
SubModule (CFML_gSpaceGroups) SPG_Apply_OP
   implicit none
   Contains
   !!----
   !!---- APPLY_OP_rat
   !!----    Apply a symmetry operator to a vector:  Vp = Apply_OP(Op,v)
   !!----
   !!---- 30/05/2019
   !!
   Pure Module Function Apply_OP_rat(Op, V) Result(S)
      !---- Arguments ----!
      Type(Symm_Oper_Type),         intent(in) :: Op    ! Symmetry Operator
      real(kind=cp), dimension(3),  intent(in) :: v     ! Vector
      real(kind=cp), dimension(3)              :: S     ! Output vector

      !---- Local Variables ----!
      type(rational), dimension(4) :: tr

      tr(1:3)=v
      tr(4)=1_LI//1_LI

      tr=matmul(Op%Mat,Tr)
      S=tr(1:3)
   End Function Apply_OP_rat

   !!----
   !!---- APPLY_OP_Real
   !!----    Apply a symmetry operator to a vector:  Vp = Apply_OP(Mat,v)
   !!----
   !!---- 7/10/2021
   !!
   Pure Module Function Apply_OP_real(Mat, V) Result(S)
      !---- Arguments ----!
      real(kind=cp), dimension(:,:),intent(in) :: Mat   ! Symmetry Operator in form of augmented matrix
      real(kind=cp), dimension(:),  intent(in) :: v     ! Vector
      real(kind=cp), dimension(size(v))        :: S     ! Output vector

      !---- Local Variables ----!
      real(kind=cp),dimension(size(v)+1) :: va
      va = [v,1.0_cp]
      va=matmul(Mat,va)
      S=va(1:size(v))
   End Function Apply_OP_real

End SubModule SPG_Apply_OP