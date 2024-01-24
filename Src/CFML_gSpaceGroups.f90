!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_gSpaceGroups
!!----         Space Groups and their algebra
!!----
!!--.. Rational matrix of special type dimension (3+d+1,3+d+1). The matrix of the
!!--.. symmetry operator is extended with a column containing the translation in
!!--.. the 3+d space plus a zero 3+d+1 row and +1 at position (3+d+1,3+d+1).
!!--..
!!--.. In order to limit the operators to the factor group w.r.t. traslations, a
!!--.. modulo-1 is applied in the multiplication of two operators.
!!----
!!
Module CFML_gSpaceGroups
    !---- Use Modules ----!
    Use CFML_GlobalDeps,        only: CP,DP, LI, EPS, err_cfml, clear_error, CFML_Debug,TPI
    Use CFML_Rational
    Use CFML_Symmetry_Tables
    Use CFML_Magnetic_Database
    Use CFML_SuperSpace_Database
    Use CFML_Maths,             only: Set_eps_math, modulo_lat, determ3D, Get_eps_math, Zbelong,EPSS,Diagonalize_RGEN, &
                                      equal_vector,resolv_sist_3x3,trace,Equal_Matrix, Inverse_Matrix,Lat_modulo
    Use CFML_Strings,           only: u_case, l_case, pack_string, get_separator_pos, get_num, &
                                      get_words, String_Fraction_2Dig,Set_Symb_From_Mat, Get_Vec_from_FracStr

    !---- Variables ----!
    implicit none

    private

    !---- List of public operators ----!
    public :: operator (*)
    public :: operator (==)

    !---- List of public functions and subroutines ----!
    public :: Apply_OP, Symmetry_Symbol, Change_Setting_Generators, Get_MagPG_from_BNS,  &
              Get_Crystal_System, Get_Dimension_SymmOP, Get_Hall_from_Generators,        &
              Get_HM_Standard, Get_Lattice_Type, Get_Laue_Num, Get_Laue_Str,             &
              Get_OP_from_Symb, Get_PointGroup_Num, Get_PointGroup_Str,                  &
              Get_Rotation_Order, get_Symb_from_Mat, Get_Symb_from_OP,Inverse_Setting,   &
              Inverse_OP, Get_Orbit, Get_Moment_ctr, Get_TFourier_Ctr, Get_AtomBet_CTR,  &
              Get_AtomPos_CTR,ISO_to_jones_notation, Get_Symb_from_Rational_Mat

    public :: Allocate_OP, Allocate_SpaceGroup, Allocate_KVector, Change_Setting_SpaceG, &
              Get_Cosets, Get_Generators, Get_Laue_PG, Get_Magnetic_Lattice,             &
              Get_Mat_from_Symb, Get_Stabilizer, Get_SubGroups_gen, Group_Constructor,   &
              Identify_Group, Init_SpaceGroup,  Is_OP_Inversion_Centre, Get_Inv_OP,      &
              Set_Conditions_NumOP_EPS, Set_SpaceGroup,Is_OP_Lattice_Centring,           &
              Write_SpaceGroup_Info, Get_Multip_Pos, Is_Lattice_Vec,Is_OP_Anti_Lattice,  &
              Get_SubGroups_full, SearchOp, Write_SymTrans_Code, Read_SymTrans_Code,     &
              Write_SpaceGroup_bin, Set_gSpG_from_string

    !---- Types ----!

    Type, public :: Symm_Oper_Type
       integer                                     :: Time_Inv =1         ! Time inversion for Magnetic groups
       integer                                     :: Dt       =1         ! determinant of the submatrix (1:3,1:3), it should be 1 or -1
       type(rational), dimension(:,:), allocatable :: Mat                 ! Matrix operator
    End Type Symm_Oper_Type

    Type, public :: Group_Type
       integer                                         :: Multip =0       ! Multiplicity of the Group
       integer                                         :: D=0             ! Dimension of operator matrices (2:2D, 3:D3,...)
       integer,              dimension(:), allocatable :: inv             ! Pointer to the inverse of the symmetry operator
       type(Symm_Oper_Type), dimension(:), allocatable :: Op              ! Symmetry operator
       character(len=80),    dimension(:), allocatable :: Symb_Op         ! Symbol operator
    End Type Group_Type

    Type, public, extends(Group_Type) :: SPG_Type
       logical                                    :: magnetic = .true.
       logical                                    :: standard_setting=.true.  !true or false
       integer                                    :: numspg = 0        ! Spacegroup number (IT if standard)
       integer                                    :: numshu = 0        ! Shubnikov group number
       integer                                    :: numops = 0        ! Number of total symmetry operators
       integer                                    :: centred= 0        ! 0: Centric(-1 no at origin),  1: Acentric, 2: Centric(-1 at origin)
       integer                                    :: anticentred=0     ! 0: Anti-Centric(-1' no at origin), 1: Anti-Acentric, 2: Anti-Centric(-1' at origin)
       integer                                    :: mag_type = 0      ! Type of magnetic group: 1 (Colorless) 2 (Paramagnetic) 3 (Black & White:1) 4(Black & White: 2)
       integer                                    :: num_lat  = 0      ! Number of lattice points in cell
       integer                                    :: num_alat = 0      ! Number of centring anti-translations
       integer                                    :: Parent_num=0      ! Number of the parent Group
       integer                                    :: Bravais_num=0     ! Number of the Bravais class of the superspace (Stokes & Campbell database)
       character(len=1)                           :: spg_lat  =" "     ! Lattice type
       character(len=1), dimension(2)             :: shu_lat  =" "     ! Shubnikov lattice type
       character(len=:),              allocatable :: init_label        ! Symbol of the space group provided for generating it
       character(len=:),              allocatable :: Parent_spg        ! Symbol of the crystallographic space group
       character(len=:),              allocatable :: tfrom_parent      ! Transformation of the parent basis to get the actual one
       character(len=:),              allocatable :: Centre            ! Alphanumeric information about the center of symmetry
       character(len=:),              allocatable :: spg_symb          ! Space group symbol
       character(len=:),              allocatable :: BNS_num           ! Numerical Label in the data base of Stokes-Campbell
       character(len=:),              allocatable :: OG_num            ! Numerical Label in the data base of D. Litvin
       character(len=:),              allocatable :: BNS_symb          ! Belonov-Neronova-Smirnova Shubnikov group symbol
       character(len=:),              allocatable :: OG_symb           ! Opechowski-Guccione Shubnikov group symbol
       character(len=:),              allocatable :: Hall              ! Hall symbol
       character(len=:),              allocatable :: UNI               ! Unified Symbol
       character(len=:),              allocatable :: UNI_num           ! Unified number
       character(len=:),              allocatable :: crystalsys        ! Crystal system
       character(len=:),              allocatable :: pg                ! Point group
       character(len=:),              allocatable :: mag_pg            ! Magnetic PG
       character(len=:),              allocatable :: laue              ! Laue group
       character(len=:),              allocatable :: setting           ! Operators transformed by "setting" (e.g. -a+b,a+b,-c;1/2,0,0)
       character(len=:),              allocatable :: mat2std           ! To standard space group (Parent)
       character(len=:),              allocatable :: mat2std_shu       ! To standard Shubnikov space group
       character(len=:),              allocatable :: matfrom           ! From standard space group
       character(len=:),              allocatable :: generators_list   ! List of generators
       type(rational),dimension(:),   allocatable :: centre_coord      ! Fractional coordinates for inversion
       type(rational),dimension(:),   allocatable :: anticentre_coord  ! Fractional coordinates for time invesion
       type(rational),dimension(:,:), allocatable :: Lat_tr            ! Lattice traslations (3,12)
       type(rational),dimension(:,:), allocatable :: aLat_tr           ! Anti-translations
    End Type SPG_Type

    !Type, public, extends(Spg_Type):: Spg_Oreal_Type         ! Operator matrices (3+d+1,3+d+1,Multip) in real form to accelerate calculations
    !   real(kind=cp), allocatable,dimension(:,:,:):: Om      ! Suppressed class => Better to copy locally before doing long calculations
    !End Type Spg_Oreal_Type

      !                          / Rot   0   t  \
      !   Superspace operator:  |   M   ep   tI  |
      !                          \  0    0   1  /
    Type, public, extends(SpG_Type) :: SuperSpaceGroup_Type
       integer                                    :: nk=0      ! (nk=1,2,3, ...) number of k-vectors
       integer                                    :: nq=0      ! number of effective set of Q_coeff >= nk
       character(len=:),              allocatable :: SSG_symb    ! Symbol of the superspace  (if known)
       character(len=:),              allocatable :: SSG_Bravais ! Symbol of the superspace  Bravais class
       character(len=:),              allocatable :: SSG_nlabel  ! Label of the superspace  Bravais class (Stokes & Campbell database)
       real(kind=cp), allocatable,dimension(:,:)  :: kv        ! k-vectors (3,nk)
       real(kind=cp), allocatable,dimension(:,:)  :: kv_std    ! k-vectors sigmas(3,nk)
       real(kind=cp), allocatable,dimension(:)    :: sintlim   ! sintheta/lambda limits (nk)
       integer,       allocatable,dimension(:)    :: nharm     ! number of harmonics along each k-vector
       integer,       allocatable,dimension(:,:)  :: q_coeff   ! Q_coeff(nk,nq)
       integer,       allocatable,dimension(:,:,:):: Rot       ! Rotational Operator matrices (3,3,Multip) in integer form to accelerate calculations
       integer,       allocatable,dimension(:,:,:):: M         ! Reciprocal Operator matrices (d,3,Multip) in integer form to accelerate calculations
       integer,       allocatable,dimension(:,:,:):: Ep        ! Modulation vector transform matrices (d,d,Multip) in integer form to accelerate calculations
       real(kind=cp), allocatable,dimension(:,:)  :: t         ! Translation in external (physical) space (3,Multip)
       real(kind=cp), allocatable,dimension(:,:)  :: tI        ! Translation in internal space (d,Multip)
    End Type SuperSpaceGroup_Type

    !!----
    !!---- TYPE :: KVECT_INFO_TYPE
    !!--..
    !!
    Type, public :: Kvect_Info_Type
       integer                                      :: nk=0        ! Number of independent k-vectors
       real(kind=cp),allocatable,dimension(:,:)     :: kv          ! k-vectors (3,nk)
       real(kind=cp),allocatable,dimension(:,:)     :: kv_std      ! k-vectors sigmas(3,nk)
       real(kind=cp),allocatable,dimension(:)       :: sintlim     ! sintheta/lambda limits (nk)
       integer,allocatable,dimension(:)             :: nharm       ! number of harmonics along each k-vector
       integer                                      :: nq=0        ! number of effective set of Q_coeff > nk
       integer,allocatable,dimension(:,:)           :: q_coeff     ! number of q_coeff(nk,nq)
    End Type kvect_info_type

    !!----
    !!---- TYPE :: Point_Orbit
    !!--..
    !!
    Type, public :: Point_Orbit
       integer                                      :: Mult=0      ! Multiplicity of the orbit
       real(kind=cp),allocatable,dimension(:,:)     :: pos         ! (d,Mult) Positions of the points
       real(kind=cp),allocatable,dimension(:,:)     :: mom         ! (d,Mult) Associated moments
       integer,      allocatable,dimension(:)       :: pts         ! (  Mult) Pointer to symmetry operator
       integer,      allocatable,dimension(:,:)     :: Lat         ! (d,Mult) lattice translation used to
    End Type Point_Orbit                                           ! put the atom  g(i).pos(:,1) within the cell
                                                                   ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)
    !---- Private Variables ----!
    integer,          dimension(0:2), parameter :: CENT=[2,1,2]       ! Multiplier for calculating the total multiplicity
    character(len=1), dimension(10),  parameter :: XYZ=["x","y","z","t","u","v","w","p","q","r"]
    character(len=1), dimension(10),  parameter :: ABC=["a","b","c","d","e","f","g","h","i","j"]
    character(len=3), dimension(10),  parameter :: X1X2X3=["x1 ","x2 ","x3 ","x4 ","x5 ","x6 ","x7 ","x8 ","x9 ","x10"]
    character(len=3), dimension(10),  parameter :: A1A2A3=["a1 ","a2 ","a3 ","a4 ","a5 ","a6 ","a7 ","a8 ","a9 ","a10"]

    integer                                         :: MaxNum_OP=2048     ! Maximum number of Operators
    character(len=120), dimension(230)              :: it_spg_gen=" "     ! Generator of space groups in the standard setting
    type(rational),     dimension(:,:), allocatable :: Identity_Matrix    ! Identity matrix
    logical                                         :: Hexa=.false.       ! Identify hexagonal groups


    !-------------------!
    !---- Operators ----!
    !-------------------!

    Interface operator (*)
       module procedure Multiply_Symm_Oper
    End interface

    Interface operator (==)
       module procedure Equal_Symm_Oper
       module procedure Equal_Group
    End interface

    !------------------!
    !---- Overload ----!
    !------------------!

    Interface Apply_OP
       module procedure Apply_OP_rat
       module procedure Apply_OP_real
    End Interface Apply_OP

    Interface Group_Constructor
       module procedure SpaceG_Constructor_GenV
       module procedure SpaceG_Constructor_Str
    End Interface Group_Constructor

    Interface Get_Generators
       module procedure Get_Generators_from_Hall
       module procedure Get_Generators_from_Str
    End Interface Get_Generators

    Interface Get_Lattice_Type
       module procedure Get_Lattice_Type_L
       module procedure Get_Lattice_Type_from_Mat
       module procedure Get_Lattice_Type_from_Gener
    End Interface Get_Lattice_Type

    Interface Get_Symb_from_OP
       module procedure String_from_MAT_TR_R
       module procedure String_from_MAT_TR_I
       module procedure String_from_Op
    End Interface Get_Symb_from_OP

    Interface Get_Symb_from_Mat
       module procedure Get_Symb_from_Mat_Tr_R
       module procedure Get_Symb_from_Mat_Tr_I
       module procedure Get_Symb_from_Rational_Mat
    end Interface Get_Symb_from_Mat

    Interface Get_Crystal_System
       module procedure Get_Crystal_System_Str
       module procedure Get_Crystal_System_from_Laue
    End Interface Get_Crystal_System

    Interface Is_Lattice_Vec
       module procedure Is_Lattice_Vec_rat
       module procedure Is_Lattice_Vec_real
       module procedure Is_Vec_Lattice_Centring
    End Interface Is_Lattice_Vec

    Interface Set_SpaceGroup
       module procedure Set_SpaceGroup_DBase
       module procedure Set_SpaceGroup_gen
    End Interface Set_SpaceGroup

    !------------------------!
    !---- Interface Zone ----!
    !------------------------!
    Interface
       Module Subroutine Allocate_KVector(nk, nq, Kvec)
          !---- Arguments ----!
          integer,               intent(in)     :: nk
          integer,               intent(in)     :: nq
          type(Kvect_Info_Type), intent(in out) :: Kvec
       End Subroutine Allocate_KVector

       Pure Module Function Apply_OP_rat(Op, V) Result(S)
          !---- Arguments ----!
          Type(Symm_Oper_Type),         intent(in) :: Op    ! Symmetry Operator
          real(kind=cp), dimension(3),  intent(in) :: v     ! Vector
          real(kind=cp), dimension(3)              :: S     ! Output vector
       End Function Apply_OP_rat

       Pure Module Function Apply_OP_real(Mat, V) Result(S)
          !---- Arguments ----!
          real(kind=cp), dimension(:,:),intent(in) :: Mat   ! Symmetry Operator in matrix form
          real(kind=cp), dimension(:),  intent(in) :: v     ! Vector
          real(kind=cp), dimension(size(v))        :: S     ! Output vector
       End Function Apply_OP_real

       Module Subroutine Allocate_Operators(D, NMax, Op)
          !---- Arguments ----!
          integer,                                         intent(in)     :: d       ! Dimension
          integer,                                         intent(in)     :: NMax    ! is the expected maximum number of operators
          type(Symm_Oper_Type), dimension(:), allocatable, intent(in out) :: Op
       End Subroutine Allocate_Operators

       Module Subroutine Allocate_SpaceGroup(D, Multip, Grp)
          !---- Arguments ----!
          integer,         intent(in)     :: d
          integer,         intent(in)     :: multip
          class(Spg_Type), intent(in out) :: Grp
       End Subroutine Allocate_SpaceGroup

       Module Subroutine Allocate_Op(d, Op)
          !---- Arguments ----!
          integer,              intent(in)    :: d
          type(Symm_Oper_Type), intent(inout) :: Op
       End Subroutine Allocate_Op

       Module Subroutine Check_Gener(Gen_in, Gen_out)
          !---- Arguments ----!
          character(len=*), dimension(:),              intent(in)  :: gen_in
          character(len=*), dimension(:), allocatable, intent(out) :: gen_out
       End Subroutine Check_Gener

       Module Function Equal_Group(Gr1, Gr2) Result(info)
          !---- Arguments ----!
          class(Spg_Type), intent(in) :: Gr1
          class(Spg_Type), intent(in) :: Gr2
          logical                    :: info
       End Function Equal_Group

       Pure Module Function Equal_Symm_Oper(Op1, Op2) Result(info)
          !---- Arguments ----!
          type(Symm_Oper_Type), intent(in) :: Op1,Op2
          logical                          :: info
       End Function Equal_Symm_Oper

       Module Subroutine Get_A_Matrix_Crys(Laueclass,A,N)
          !---- Arguments ----!
          character(len=*),                 intent(in)  :: LaueClass
          type(rational), dimension(3,3,6), intent(out) :: A
          integer,                          intent(out) :: n
       End Subroutine Get_A_Matrix_Crys

       Module Subroutine Get_A_Matrix_Shub(Laueclass,A,N)
          !---- Arguments ----!
          character(len=*),                 intent(in)  :: laueClass
          type(rational), dimension(3,3,6), intent(out) :: A
          integer,                          intent(out) :: n
       End Subroutine Get_A_Matrix_Shub

       Module Subroutine Get_AtomBet_CTR(X, Betas, Spgr, Codini, Icodes, Multip, Ord, Ss, Ipr)
          !---- Arguments ----!
          real(kind=cp), dimension(3),             intent(in    ) :: X
          real(kind=cp), dimension(6),             intent(in out) :: Betas
          type(SpG_type),                          intent(in    ) :: Spgr
          integer,                                 intent(in out) :: Codini
          integer, dimension(6),                   intent(in out) :: Icodes
          real(kind=cp), dimension(6),             intent(in out) :: Multip
          integer,                       optional, intent(in    ) :: Ord
          integer, dimension(:),         optional, intent(in    ) :: Ss
          integer,                       optional, intent(in    ) :: Ipr
       End Subroutine Get_AtomBet_CTR

       Module Subroutine Get_AtomPos_CTR(X, Spgr, Codini, ICodes, Multip, Ord, Ss, Att,Ipr)
          !---- Arguments ----!
          real(kind=cp), dimension(3),            intent(in)     :: X
          type(SpG_type),                         intent(in)     :: Spgr
          integer,                                intent(in out) :: Codini
          integer,       dimension(3),            intent(in out) :: ICodes
          real(kind=cp), dimension(3),            intent(in out) :: Multip
          integer,                       optional,intent(in)     :: Ord
          integer, dimension(:),         optional,intent(in)     :: Ss
          real(kind=cp), dimension(:,:), optional,intent(in)     :: Att
          integer,                       optional,intent(in)     :: Ipr
       End Subroutine Get_AtomPos_CTR

       Module Subroutine Get_Cosets(G,H, cosets)
          !---- Arguments ----!
          class(Spg_Type),                     intent(in)  :: G
          class(Spg_Type),                     intent(in)  :: H
          integer, dimension(:), allocatable, intent(out) :: cosets
       End Subroutine Get_Cosets

       Module Function Get_Crystal_System_from_Laue(Laue) Result(Str)
          !---- Arguments ----!
          character(len=*), intent(in)  :: Laue
          character(len=:), allocatable :: Str
       End Function Get_Crystal_System_from_Laue

       Module Function Get_Crystal_System_Str(Ops, nops) Result(Str)
          !---- Arguments ----!
          type(Symm_Oper_Type), dimension(:), intent(in) :: Ops
          integer,                            intent(in) :: NOps
          character(len=:), allocatable                  :: Str
       End Function Get_Crystal_System_Str

       Module Function Get_Dimension_SymmOp(Symb) Result(d)
          !---- Arguments ----!
          character(len=*), intent(in) :: Symb
          integer                      :: d
       End Function Get_Dimension_SymmOp

       Module Subroutine Get_Generators_from_Hall(Hall, Gen, Ngen, R_shift)
          !---- Arguments ----!
          character(len=*),                            intent(in)  :: Hall
          character(len=*), dimension(:), allocatable, intent(out) :: Gen
          integer,                                     intent(out) :: Ngen
          logical, optional,                           intent(in)  :: R_Shift
       End Subroutine Get_Generators_from_Hall

       Module Subroutine Get_Generators_from_Str(StrGen, d, gen, ngen,time_given)
          !---- Arguments ----!
          character(len=*),                            intent(in)  :: StrGen
          integer,                                     intent(out) :: d
          character(len=*), dimension(:), allocatable, intent(out) :: gen
          integer,                                     intent(out) :: ngen
          logical, optional,                           intent(out) :: time_given
       End Subroutine Get_Generators_from_Str

       Module Subroutine Get_Generators_L(laueClass,nSymOp,symOp,Gen,nGen)
          !---- Arguments ----!
          character(len=*),                        intent(in)  :: laueClass ! Laue class
          integer,                                 intent(in)  :: nSymOp    ! number of symmetry operations
          type(Symm_Oper_Type), dimension(nSymOp), intent(in)  :: symOp     ! symmetry operations
          type(Symm_Oper_Type), dimension(3),      intent(out) :: Gen       ! generators
          integer,                                 intent(out) :: nGen      ! number of generators
       End Subroutine Get_Generators_L

       Module Function Get_Hall_from_Generators(Ngen, Gen, Ishift) Result(Hall)
          !---- Arguments ----!
          integer,                         intent(in) :: NGen
          character(len=*), dimension(:),  intent(in) :: Gen
          integer, dimension(3), optional, intent(in) :: ishift
          character(len=:), allocatable               :: Hall
       End Function Get_Hall_from_Generators

       Module Function Get_HM_Standard(NumSpg) Result(SymbolHM)
          !---- Arguments ----!
          integer, intent(in) :: numSpg
          character(len=:), allocatable :: symbolHM
       End Function Get_HM_Standard

       Module Subroutine ISO_to_jones_notation(gen_string)
          !---- Arguments ----!
          character(len=*), intent(in out) :: gen_string
       End Subroutine ISO_to_jones_notation

       Module Function Get_Lattice_Type_from_Gener(Ngen,Gen) Result(Latt)
          !---- Arguments ----!
          integer,                        intent(in) :: Ngen
          character(len=*), dimension(:), intent(in) :: Gen
          character(len=:), allocatable              :: Latt
       End Function Get_Lattice_Type_from_Gener

       Module Function Get_Lattice_Type_from_MAT(M) Result(lattyp)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: M
          character(len=1)                            :: lattyp
       End Function Get_Lattice_Type_from_MAT

       Module Function Get_Lattice_Type_L(N, Latc) Result(lattyp)
          !---- Arguments ----!
          integer,                        intent( in) :: N
          type(rational), dimension(:,:), intent( in) :: Latc
          character(len=1)                            :: lattyp
       End Function Get_Lattice_Type_L

       Module Function Get_Laue_Num(Str_Laue) Result(N)
          !---- Arguments ----!
          character(len=*), intent (in) :: Str_Laue
          integer                       :: N
       End Function Get_Laue_Num

       Module Subroutine Get_Laue_PG(Ops, nops, Centro, Laue, Pg)
          !---- Arguments ----!
          type(Symm_Oper_Type), dimension(:), intent(in) :: Ops
          integer,                            intent(in) :: NOps
          logical,                            intent(in) :: Centro
          character(len=*),                   intent(out):: Laue
          character(len=*),                   intent(out):: Pg
       End Subroutine Get_Laue_PG

       Module Function Get_Laue_Str(N) Result(Str_Laue)
          !---- Arguments ----!
          integer,          intent( in) :: N
          character(len=:), allocatable :: Str_Laue
       End Function Get_Laue_Str

       Module Subroutine Get_Magnetic_Lattice(G)
          !---- Arguments ----!
          type(spg_type), intent(in out) :: G
       End Subroutine Get_Magnetic_Lattice

       Module Subroutine Get_Mat_From_Symb(Symb, Mat, Invt)
          !---- Arguments ----!
          character(len=*),                intent(in)  :: Symb
          type(rational), dimension(:,:),  intent(out) :: Mat
          integer, optional,               intent(out) :: invt
       End Subroutine Get_Mat_From_Symb

       Module Function Get_Mc_Matrix(LaueClass, Mp) Result(Mc)
          !---- Arguments ----!
          character(len=*),               intent(in)  :: LaueClass
          type(rational), dimension(3,3), intent(in)  :: Mp
          type(rational), dimension(3,3)              :: Mc
       End Function Get_Mc_Matrix

       Module Function Get_Mp_Matrix(G,P) Result(Mp)
          !---- Arguments ----!
          class(spg_type),                 intent(in)  :: G
          type(rational), dimension(3,3), intent(in)  :: P
          type(rational), dimension(3,3)              :: Mp
       End Function Get_Mp_Matrix

       Module Subroutine Get_Moment_CTR(xnr,moment,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
          real(kind=cp), dimension(3),            intent(in)     :: xnr
          real(kind=cp), dimension(:),            intent(in out) :: moment
          class(SpG_type),                        intent(in)     :: Spg
          Integer,                                intent(in out) :: codini
          real(kind=cp), dimension(:),            intent(in out) :: codes
          integer,                       optional,intent(in)     :: ord
          integer, dimension(:),         optional,intent(in)     :: ss
          real(kind=cp), dimension(:,:), optional,intent(in)     :: att
          integer,                       optional,intent(in)     :: Ipr
          character(len=*),              optional,intent(out)    :: ctr_code
       End Subroutine Get_Moment_CTR

       Module Subroutine Get_Multip_OP_Table(Op,Table)
          !---- Arguments ----!
          type(Symm_Oper_Type), dimension(:), intent(in) :: Op
          integer, dimension(:,:),allocatable,intent(out):: Table
       End Subroutine Get_Multip_OP_Table

       Pure Module Function Get_Inv_OP(Op) Result(inv)
          !---- Arguments ----!
          type(Symm_Oper_Type), dimension(:),   intent(in) :: Op
          integer, dimension(:), allocatable               :: inv
       End Function Get_Inv_OP

       Module Function Get_Multip_Pos(x,SpG) Result(mult)
          !---- Arguments ----!
          real(kind=cp), dimension(:), intent(in) :: x
          class(SPG_Type),             intent(in) :: SpG
          integer                                 :: mult
       End Function Get_Multip_Pos

       Module Function Get_Op_from_Symb(Symb) Result(Op)
          !---- Arguments ----!
          character(len=*),     intent(in) :: symb
          type(Symm_Oper_Type)             :: Op
       End Function Get_Op_from_Symb

       Module Subroutine Get_OPS_from_Generators(Ngen, Ops, Multip, Table)
          !---- Arguments ----!
          integer,                                        intent(in)     :: ngen
          type(Symm_Oper_Type), dimension(:),             intent(in out) :: Ops
          integer,                                        intent(out)    :: multip
          integer, dimension(:,:), allocatable, optional, intent(out)    :: table
       End Subroutine Get_OPS_from_Generators

       Module Subroutine Get_Orbit(x,Spg,orbit,mom,orb3D,convl,Tbox)
          !---- Arguments ----!
          real(kind=cp), dimension(:),          intent(in)  :: x
          class(SpG_Type),                      intent(in)  :: spg
          type(Point_Orbit),                    intent(out) :: orbit
          real(kind=cp), dimension(:),optional, intent(in)  :: mom
          type(Point_Orbit),          optional, intent(out) :: orb3D
          logical,                    optional, intent(in)  :: convl
          integer,  dimension(3),     optional, intent(in)  :: Tbox
       End Subroutine Get_Orbit

       Module Subroutine Get_Origin_Shift(ng, G, G_,  P_, origShift, shift)
          !---- Arguments ----!
          integer,                             intent(in)  :: ng
          type(symm_oper_type), dimension(ng), intent(in)  :: G
          type(symm_oper_type), dimension(ng), intent(in)  :: G_
          type(rational), dimension(3,3),      intent(in)  :: P_
          type(rational), dimension(3),        intent(out) :: origShift
          logical,                             intent(out) :: shift
       End Subroutine Get_Origin_Shift

       Module Function Get_P_Matrix(G,Nospin) Result(P)
          !---- Arguments ----!
          class(spg_type),                 intent(in)  :: G
          logical, optional,              intent(in)  :: nospin
          type(rational), dimension(3,3)              :: P
       End Function Get_P_Matrix

       Module Function Get_PointGroup_Num(Str_PG) Result(N)
          !---- Arguments ----!
          character(len=*), intent (in) :: Str_PG
          integer                       :: N
       End Function Get_PointGroup_Num

       Module Function Get_PointGroup_Str(N) Result(Str_PG)
          !---- Arguments ----!
          integer,          intent( in) :: N
          character(len=:), allocatable :: Str_PG
       End Function Get_PointGroup_Str

       Module Subroutine Get_Pseudo_Standard_Base(W,perpAxis,bz,bx,by)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W
          type(rational), dimension(3,4), intent(in)  :: perpAxis
          type(rational), dimension(3),   intent(in)  :: bz
          type(rational), dimension(3),   intent(out) :: bx
          type(rational), dimension(3),   intent(out) :: by
       End Subroutine Get_Pseudo_Standard_Base

       Module Function Get_Rotation_Axis(W) Result(axis)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W       !rotation matrix
          type(rational), dimension(3)                :: axis    !shortest vector along the rotation axisP
       End Function Get_Rotation_Axis

       Module Function Get_Rotation_Order(W) Result(N)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W
          integer                                     :: N
       End Function Get_Rotation_Order

       Module Subroutine Get_Rotations(nSymOP, symOP, n, nso, idd)
          !---- Arguments ----!
          integer,                                 intent(in)  :: nSymOP
          type(Symm_Oper_Type), dimension(nSymOP), intent(in)  :: symOP
          integer,                                 intent(in)  :: n
          integer,                                 intent(out) :: nso
          integer, dimension(nSymOP,2),            intent(out) :: idd
       End Subroutine Get_Rotations

       Module Function Get_S_Matrix(W) Result(S)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W
          type(rational), dimension(3,3)               :: S
       End Function Get_S_Matrix

       Module Function SearchOp(Sim,I1,I2) Result(Isl)
          !---- Arguments ----!
          integer , dimension(3,3), Intent(in) :: sim
          integer , Intent(in)                 :: i1,i2
          integer                              :: Isl
       End Function SearchOp

       Module Function Get_MagPG_from_BNS(BNS_Symb,mag_type) Result(mag_pg)
          character(len=*), intent(in) :: BNS_Symb
          integer,          intent(in) :: mag_type
          character(len=:), allocatable:: mag_pg
       End Function Get_MagPG_from_BNS

       Module Subroutine Get_Stabilizer(X, Spg,Order,Ptr,Atr)
          !---- Arguments ----!
          real(kind=cp), dimension(3),  intent (in)  :: x
          class(Spg_Type),              intent (in)  :: Spg
          integer,                      intent(out)  :: order
          integer, dimension(:),        intent(out)  :: ptr
          real(kind=cp), dimension(:,:),intent(out)  :: atr
       End Subroutine Get_Stabilizer

       Module Subroutine Get_SubGroups_gen(SpG, SubG, nsg, point,printd)
          !---- Arguments ----!
          type(Spg_Type),                   intent( in) :: SpG
          type(Spg_Type),dimension(:),      intent(out) :: SubG
          integer,                          intent(out) :: nsg
          logical, dimension(:,:), optional,intent(out) :: point
          logical,                 optional,intent(in)  :: printd
       End Subroutine Get_SubGroups_gen

       Module Subroutine Get_SubGroups_full(SpG, SubG, nsg, indexg, point,printd)
          !---- Arguments ----!
          type(Spg_Type),                    intent( in) :: SpG
          type(Spg_Type),dimension(:),       intent(out) :: SubG
          integer,                           intent(out) :: nsg
          integer,                  optional,intent(in)  :: indexg
          logical, dimension(:,:),  optional,intent(out) :: point
          logical,                  optional,intent(in)  :: printd
       End Subroutine Get_SubGroups_full

       Module Function Get_Symb_from_Mat_Tr_I(Mat, tr, opposite) Result(Str)
          !---- Arguments ----!
          integer,       dimension(3,3), intent(in) :: Mat
          real(kind=cp), dimension(3),   intent(in) :: tr
          logical, optional,             intent(in) :: opposite
          character(len=:), allocatable             :: Str
       End Function Get_Symb_from_Mat_Tr_I

       Module Function Get_Symb_from_Mat_Tr_R(Mat, tr, opposite) Result(Str)
          !---- Arguments ----!
          real(kind=cp), dimension(3,3), intent(in) :: Mat
          real(kind=cp), dimension(3),   intent(in) :: tr
          logical, optional,             intent(in) :: opposite
          character(len=:),  allocatable            :: Str
       End Function Get_Symb_from_Mat_Tr_R

       Module Function Get_Symb_from_Rational_Mat(Matt, Strcode, Invt) Result(Symb)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: Matt
          character(len=*), optional,     intent(in) :: strcode
          integer,          optional,     intent(in) :: invt
          character(len=:), allocatable              :: symb
       End Function Get_Symb_from_Rational_Mat

       Module Function Inverse_Setting(setting,D) Result(inv_sett)
          Character(len=*), intent(in) :: setting
          integer,          intent(in) :: D
          Character(len=:), allocatable:: inv_sett
       End Function Inverse_Setting

       Module Function Search_Hall_Operators(G, Ishift) Result(Str)
          !---- Arguments ----!
          class(spg_type),                 intent(in)  :: G
          integer, dimension(3), optional, intent(in)  :: Ishift
          character(len=:), allocatable                :: Str
       End Function Search_Hall_Operators

       Module Function Search_OnePrime_Operator(G) Result(Prime)
          !---- Arguments ----!
          class(spg_type), intent(in) :: G
          integer                     :: Prime
       End Function Search_OnePrime_Operator

       Module Function String_from_Op(Op, Strcode) Result(symb)
          !---- Arguments ----!
          type(Symm_Oper_Type),       intent(in) :: Op
          character(len=*), optional, intent(in) :: Strcode
          character(len=:), allocatable          :: symb
       End Function String_from_Op

       Module Function String_from_MAT_TR_I(MAT,T) Result(Symb)
          !---- Arguments ----!
          integer,       dimension(3,3), intent( in) :: Mat
          real(kind=cp), dimension(3),   intent( in) :: T
          character(len=:), allocatable              :: Symb
       End Function String_from_MAT_TR_I

       Module Function String_from_MAT_TR_R(Mat,T) Result(Symb)
          !---- Arguments ----!
          real(kind=cp),    dimension(3,3), intent( in) :: Mat
          real(kind=cp),    dimension(3),   intent( in) :: t
          character(len=:), allocatable                 :: symb
       End Function String_from_MAT_TR_R

       Module Function Symmetry_Symbol(S,T) Result(Symbol)
          !---- Arguments ----!
          integer,       dimension(3,3),    intent( in) :: s
          real(kind=cp), dimension(3),      intent( in) :: t
          character (len=:), allocatable                :: Symbol
       End Function Symmetry_Symbol

       Module Function Get_VecPerp_To_RotAxis(W) Result(vPerp)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W
          type(rational), dimension(3,4)              :: vPerp
       End Function Get_VecPerp_To_RotAxis

       Module Subroutine Identify_PointGroup(G)
          !---- Arguments ----!
          type(spg_type), intent(in out) :: G
       End Subroutine Identify_PointGroup

       Module Subroutine Get_TFourier_CTR(xnr,TFourier,codes,SpG,codini,mode,ord,ss,att,Ipr,ctr_code)
          real(kind=cp), dimension(3),            intent(in)     :: xnr
          real(kind=cp), dimension(:,:),          intent(in out) :: TFourier
          real(kind=cp), dimension(:,:),          intent(in out) :: codes
          class(SuperSpaceGroup_Type),            intent(in)     :: SpG
          Integer,                                intent(in out) :: codini
          character(len=*),                       intent(in)     :: mode
          integer,                       optional,intent(in)     :: ord
          integer, dimension(:),         optional,intent(in)     :: ss
          real(kind=cp), dimension(:,:), optional,intent(in)     :: att
          integer,                       optional,intent(in)     :: Ipr
          character(len=*),dimension(:), optional,intent(out)    :: ctr_code
       End Subroutine Get_TFourier_CTR

       Module Subroutine Identify_Crystal_System(G)
          !---- Arguments ----!
          type(spg_type), intent(in out) :: G
       End Subroutine Identify_Crystal_System

       Module Subroutine Identify_Group(G)
          !---- Arguments ----!
          type(spg_type),    intent(in out) :: G
       End Subroutine Identify_Group

       Module Subroutine Identify_LaueClass(G)
          !---- Arguments ----!
          type(spg_type), intent(inout) :: G
       End Subroutine Identify_LaueClass

       Module Subroutine Identify_Shubnikov_Group(G)
          !---- Arguments ----!
          type(spg_type),    intent(in out) :: G
       End Subroutine Identify_Shubnikov_Group

       Module Subroutine Match_SpaceGroup_3D(G,P,M,n,A)
          !---- Arguments ----!
          type(spg_type),                   intent(inout) :: G
          type(rational), dimension(3,3),   intent(in)    :: P
          type(rational), dimension(3,3),   intent(in)    :: M
          integer,                          intent(in)    :: n
          type(rational), dimension(3,3,n), intent(in)    :: A
       End Subroutine Match_SpaceGroup_3D

       Module Subroutine Match_Shubnikov_Group(G,P,M)
          !---- Arguments ----!
          type(spg_type),                   intent(in out):: G
          type(rational), dimension(3,3),   intent(in)    :: P
          type(rational), dimension(3,3),   intent(in)    :: M
       End Subroutine Match_Shubnikov_Group

       Module Subroutine SpaceG_Constructor_GenV(GenV, Spg, StrCode, set_inv)
          !---- Arguments ----!
          character(len=*),dimension(:),intent(in)     :: GenV
          class(Spg_Type),              intent(in out) :: Spg
          character(len=*),optional,    intent(in)     :: StrCode
          logical,         optional,    intent(in)     :: set_inv
       End Subroutine SpaceG_Constructor_GenV

       Module Subroutine SpaceG_Constructor_Str(ListGen, Spg, Strcode, set_inv)
          !---- Arguments ----!
          character(len=*),           intent(in)     :: ListGen
          class(Spg_Type),            intent(in out) :: Spg
          character(len=*), optional, intent(in)     :: Strcode
          logical,          optional, intent(in)     :: set_inv
       End Subroutine SpaceG_Constructor_Str

       Module Subroutine Init_SpaceGroup(Grp)
          !---- Arguments ----!
          class(Group_type),  intent(in out) :: Grp
       End Subroutine Init_SpaceGroup

       Module Function Is_OP_Anti_Lattice(Op) Result(Info)
          !---- Arguments ----!
          type(Symm_Oper_Type),intent(in) :: Op
          logical                         :: info
       End Function Is_OP_Anti_Lattice

       Module Function Is_OP_Inversion_Centre(Op) Result(Info)
          !---- Arguments ----!
          type(Symm_Oper_Type),intent(in) :: Op
          logical                         :: info
       End Function Is_OP_Inversion_Centre

       Module Function Is_OP_Lattice_Centring(Op) Result(Info)
          !---- Arguments ----!
          type(Symm_Oper_Type), intent(in) :: Op
          logical                          :: info
       End Function Is_OP_Lattice_Centring

       Module Function Is_Lattice_Vec_rat(V,Ltr) Result(Lattice)
          !---- Argument ----!
          type(rational), dimension(:),   intent( in) :: v
          type(rational), dimension(:,:), intent( in) :: Ltr
          logical                                     :: Lattice
       End Function Is_Lattice_Vec_rat

       Module Function is_Lattice_vec_real(v,Ltr) Result(Lattice)
          !---- Arguments ----!
          real(kind=cp), dimension(:),   intent(in) :: v
          type(rational),dimension(:,:), intent(in) :: Ltr
          logical                                   :: Lattice
       End Function is_Lattice_vec_real

       Pure Module Function Is_Vec_Lattice_Centring(vec,SpG,Prim) Result(Info)
          !---- Arguments ----!
          real(kind=cp), dimension(:), intent(in) :: Vec
          Class(SpG_Type), optional,   intent(in) :: SpG
          logical, optional,           intent(in) :: Prim
          logical                                 :: info
       End Function Is_Vec_Lattice_Centring

       Module Function Is_OP_Minus_1_Prime(Op) Result(Info)
          !---- Arguments ----!
          type(Symm_Oper_Type),intent(in) :: Op
          logical                         :: info
       End Function Is_OP_Minus_1_Prime

       Module Function Is_OP_1_Prime(Op) Result(Info)
          !---- Arguments ----!
          type(Symm_Oper_Type),intent(in) :: Op
          logical                         :: info
       End Function Is_OP_1_Prime

       Module Function Inverse_OP(Op) Result(i_OP)
          !---- Arguments ----!
          type(Symm_Oper_Type), intent(in) :: Op
          type(Symm_Oper_Type)             :: i_Op
       End Function Inverse_OP

       Pure Module Function Multiply_Symm_Oper(Op1, Op2) Result (Op3)
          !---- Arguments ----!
          type(Symm_Oper_Type), intent(in) :: Op1,Op2
          type(Symm_Oper_Type)             :: Op3
       End Function Multiply_Symm_Oper

       Module Function Positive_SenseRot(W, Axis) Result(Positive)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(in)  :: W
          type(rational), dimension(3),   intent(in)  :: axis
          logical                                     :: positive
       End Function Positive_SenseRot

       Pure Module Function Write_SymTrans_Code(N,Tr) Result(Code)
          !---- Arguments ----!
          integer,                    intent(in)  :: N
          real(kind=cp),dimension(3), intent(in)  :: Tr
          character (len=:), allocatable          :: Code
       End Function Write_SymTrans_Code

       Module Subroutine Read_SymTrans_Code(Code,N,Tr)
          !---- Arguments ----!
          character (len=*),          intent( in) :: Code
          integer,                    intent(out) :: N
          real(kind=cp),dimension(3), intent(out) :: Tr
       End Subroutine Read_SymTrans_Code

       Module Subroutine Reduced_Translation(Mat)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in out) :: Mat
       End Subroutine Reduced_Translation

       Module Subroutine Reorder_Operators(Multip, Op, Centred, Centre_Coord, Anticentred, &
                                           Anticentre_Coord, Numops, Num_Lat, Num_Alat,    &
                                           Lat_Tr, Alat_Tr, Mag_Type)
          !---- Arguments ----!
          integer,                            intent(in)     :: multip
          type(Symm_Oper_Type), dimension(:), intent(in out) :: Op
          integer,                            intent(out)    :: num_lat
          integer,                            intent(out)    :: num_alat
          integer,                            intent(out)    :: Numops
          integer,                            intent(out)    :: centred
          integer,                            intent(out)    :: anticentred
          integer,                            intent(out)    :: mag_type
          type(rational),dimension(:,:),      intent(out)    :: Lat_tr
          type(rational),dimension(:,:),      intent(out)    :: aLat_tr
          type(rational),dimension(:),        intent(out)    :: centre_coord
          type(rational),dimension(:),        intent(out)    :: anticentre_coord
       End Subroutine Reorder_Operators

       Module Subroutine Set_Conditions_NumOP_EPS(maxop,epsg)
          !---- Arguments ----!
          integer,       optional, intent(in) :: maxop
          real(kind=cp), optional, intent(in) :: epsg
       End Subroutine Set_Conditions_NumOP_EPS

       Module Subroutine Set_Identity_Matrix(D)
          !---- Arguments ----!
          integer, intent(in) :: D
       End Subroutine Set_Identity_Matrix

       Module Subroutine Set_Right_Handedness(A)
          !---- Arguments ----!
          type(rational), dimension(3,3), intent(inout) :: A
       End Subroutine Set_Right_Handedness

       Module Subroutine Change_Setting_Generators(setting,ngen,gen,xyz_type)
          !---- Arguments ----!
          character(len=*),               intent(in )    :: setting
          integer,                        intent(in )    :: ngen
          character(len=*), dimension(:), intent(in out) :: gen
          character(len=*), optional,     intent(in )    :: xyz_type
       End Subroutine Change_Setting_Generators

       Module Subroutine Change_Setting_SpaceG(setting, SpaceG,xyz_type)
          !---- Arguments ----!
          character(len=*),           intent(in )    :: setting
          class(spg_type),            intent(in out) :: SpaceG
          character(len=*), optional, intent(in )    :: xyz_type
       End Subroutine Change_Setting_SpaceG

       Module Subroutine Set_SpaceGroup_DBase(Str,mode,SpaceG,xyz_type,Setting,keepdb,parent,database_path,trn_to)
          !---- Arguments ----!
          character(len=*),           intent(in ) :: Str
          character(len=*),           intent(in ) :: mode
          class(spg_type),allocatable,intent(out) :: SpaceG
          character(len=*), optional, intent(in ) :: xyz_type
          character(len=*), optional, intent(in ) :: Setting
          logical,          optional, intent(in ) :: keepdb
          character(len=*), optional, intent(in ) :: parent
          character(len=*), optional, intent(in ) :: database_path
          logical,          optional, intent(in ) :: trn_to
       End Subroutine Set_SpaceGroup_DBase

       Module Subroutine Set_SpaceGroup_gen(Str, SpaceG, NGen, Gen, set_inv)
          !---- Arguments ----!
          character(len=*),                          intent(in ) :: Str
          class(spg_type),allocatable,               intent(out) :: SpaceG
          integer,                         optional, intent(in ) :: NGen
          character(len=*),  dimension(:), optional, intent(in ) :: Gen
          logical,                         optional, intent(in ) :: set_inv
       End Subroutine Set_SpaceGroup_gen

       Module Subroutine Set_gSpG_from_string(str,SpG,Setting_Search)
          !---- Arguments ----!
          character(len=*),           intent(in) :: str
          Class(SpG_Type),allocatable,intent(out):: SpG
          logical, optional,          intent(in) :: Setting_Search
       End Subroutine Set_gSpG_from_string

       !Module Subroutine Set_SpaceGroup_symb(Str, SpaceG, NGen, Gen)
       !   !---- Arguments ----!
       !   character(len=*),                          intent(in ) :: Str
       !   class(spg_type),                           intent(out) :: SpaceG
       !   integer,                         optional, intent(in ) :: NGen
       !   character(len=*),  dimension(:), optional, intent(in ) :: Gen
       !End Subroutine Set_SpaceGroup_symb

       Module Subroutine Smallest_Integral_Vector(v)
          !---- Arguments ----!
          type(rational), dimension(:), intent(inout) :: v
       End Subroutine Smallest_Integral_Vector

       Module Subroutine Sort_Oper(N, Op, Cod, perm)
          !---- Arguments ----!
          integer,                           intent(in)     :: n
          type(Symm_Oper_Type),dimension(n), intent(in out) :: Op
          character(len=*),                  intent(in)     :: cod
          integer, optional,   dimension(n), intent(out)    :: perm
       End Subroutine Sort_Oper

       Module Subroutine Write_SpaceGroup_Info(Grp,Lun)
          !---- Arguments ----!
          class(Spg_Type),    intent(in)   :: Grp
          integer, optional,  intent(in)   :: lun
       End Subroutine Write_SpaceGroup_Info

       Module Subroutine Write_SpaceGroup_bin(Grp,Lun)
          !---- Arguments ----!
          class(Spg_Type),    intent(in)   :: Grp
          integer, optional,  intent(in)   :: lun
       End Subroutine Write_SpaceGroup_bin

    End Interface

End Module CFML_gSpaceGroups
