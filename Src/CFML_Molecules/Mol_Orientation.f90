Submodule (CFML_Molecules) Mol_Orientation

   implicit none
 
 Contains
   !!----
   !!---- Subroutine Set_Euler_Matrix
   !!----
   !!----    Subroutine to obtain the Euler active matrix to transform a point
   !!----    to another point. For instance the internal coordinates of a molecule
   !!----    can be transformed to absolute positions using columns vectors.
   !!----
   !!----    If the Cartesian coordinates of an atom in the molecular frame is the
   !!----    column vector  Xm, the cartesian coordinates in the crystal frame X
   !!----    are obtained from:  X = Eu Xm
   !!----
   !!----    The internal coordinates of a point are obtained from Xm = EuT X.
   !!----    The character variable "rt" indicates the type of Euler angles provided.
   !!----    If rt="E", the angles PHI,THETA,CHI correspond to the conventional
   !!----    Euler angles ALPHA, BETA, GAMMA. Otherwise, they correspond to the
   !!----    2nd setting, allowing to interpret PHI and THETA as the polar angles of
   !!----    the molecular frame Zm-axis, and CHI a rotation around Zm.
   !!----
   !!----   Update: February - 2005
   !!
   Module Function Set_Euler_Matrix(Rt, Phi, Theta, Chi) Result(Eu)
      !---- Arguments ----!
      character(len=*),              intent ( in) :: Rt
      real(kind=cp),                 intent ( in) :: Phi,Theta,Chi
      real(kind=cp), dimension(3,3)               :: Eu

      !---- Local Variables ----!
      character(len=1) :: car
      real(kind=cp)    :: PH,TH,CH

      car=u_case(adjustl(rt))
      
      TH=THETA
      if (car == "E") then
         PH=PHI+90.0_cp
         CH=CHI-90.0_cp
      else
         PH=PHI
         CH=CHI
      end if
      Eu(1,1) =  cosd(PH)* cosd(TH)* cosd(CH) - sind(PH)* sind(CH)
      Eu(1,2) = -cosd(PH)* cosd(TH)* sind(CH) - sind(PH)* cosd(CH)
      Eu(1,3) =  cosd(PH)* sind(TH)
      Eu(2,1) =  sind(PH)* cosd(TH)* cosd(CH) + cosd(PH)* sind(CH)
      Eu(2,2) = -sind(PH)* cosd(TH)* sind(CH) + cosd(PH)* cosd(CH)
      Eu(2,3) =  sind(PH)* sind(TH)
      Eu(3,1) = -cosd(CH)* sind(TH)
      Eu(3,2) =  sind(CH)* sind(TH)
      Eu(3,3) =            cosd(TH)

   End Function Set_Euler_Matrix 
   
   !!----
   !!---- SUBROUTINE FIX_ORIENT_CARTESIAN
   !!----
   !!----    Subroutine to transform the Cartesian coordinates of the molecule choosing
   !!----    which atom is the origin, which define the X axis and which defines the XY Plane
   !!----    If the second argument is present the subroutine creates a new molecule
   !!----    preserving the input molecule in Cartesian. Otherwise the input molecule is
   !!----    changed on output.
   !!----
   !!----    If Natom_0 is absent, then the first atom on the molecule will be the origin.
   !!----    If Natom_X is absent, then the second atom on the molecule will define the X axis.
   !!----    If Natom_XY is absent, then the third atom on the molecule will define the XY Plane.
   !!----
   !!----    The optional output matrix Mat is the active rotation matrix passing from the old
   !!----    Cartesian frame to the new one. The transpose matrix has served to transform the
   !!----    original Cartesian coordinates.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Fix_Orient_Cartesian(Mol, NMol, NAtom_O, NAtom_X, NAtom_XY,Mat)
      !---- Arguments ----!
      type (Molecule_type),                    intent(in out) :: Mol
      type (Molecule_type),          optional, intent(   out) :: NMol
      integer,                       optional, intent(in)     :: NAtom_O
      integer,                       optional, intent(in)     :: NAtom_X
      integer,                       optional, intent(in)     :: NAtom_XY
      real(kind=cp), dimension(3,3), optional, intent(out)    :: Mat

      !---- Local variables ----!
      integer                       :: n_or, n_x, n_xy
      integer                       :: i
      real(kind=cp),dimension(3)    :: u1,u2,u3
      real(kind=cp),dimension(3,3)  :: R
      type (Molecule_type)          :: IMol

      n_or=1
      n_x =2
      n_xy=3
      if (present(natom_O))  n_or=natom_o
      if (present(natom_x))  n_x =natom_x
      if (present(natom_xy)) n_xy=natom_xy

      if (Mol%natoms > 0) call Init_Molecule(IMol,Mol%natoms)
      call Fix_Reference(Mol,IMol,n_or,n_x,n_xy)
      if (err_CFML%IErr /=0) return

      !> Traslation the Origin 
      do i=2,IMol%natoms
         IMol%I_coor(:,i)=IMol%I_coor(:,i)-IMol%I_coor(:,1)
      end do
      IMol%I_coor(:,1)=0.0

      u1=IMol%I_coor(:,2)
      u1=u1/sqrt(dot_product(u1,u1))
      u2=IMol%I_coor(:,3)
      u3=cross_product(u1,u2)
      u3=u3/sqrt(dot_product(u3,u3))
      u2=cross_product(u3,u1)
      R(1,:)=u1
      R(2,:)=u2  !Passive matrix needed to get the new coordinates
      R(3,:)=u3  !The active matrix can be output in the optional argument
      if (present(Mat)) Mat=transpose(R)

      do i=2,IMol%natoms
         IMol%I_coor(:,i)=matmul(R,IMol%I_coor(:,i))
      end do

      if (present(NMol)) then
         if (IMol%natoms <=0) then
            call set_error (1, "Fix_Orient_Cartesian: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Fix_Orient_Cartesian
 
End Submodule Mol_Orientation 