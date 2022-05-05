!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2012  Institut Laue-Langevin (ILL), Grenoble, FRANCE
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
!!---- MODULE: CFML_KeyCodes
!!----   INFO: Routines for Codes refinement
!!----
!!---- HISTORY
!!----    Update: 11/04/2022
!!----
!!----
!!
Module CFML_KeyCodes
   !---- Use Modules ----!
   Use CFML_GlobalDeps,   only: CP, Clear_error, Set_Error
   Use CFML_Atoms,        only: MAX_MOD, AtList_Type, Atm_Ref_Type, MAtm_Ref_Type, Index_AtLab_on_AtList
   Use CFML_Strings,      only: File_Type, Get_Num, Cut_String, Get_Words, U_Case
   Use CFML_gSpaceGroups, only: Spg_Type, Symm_Oper_Type, Get_Stabilizer, Get_Symb_from_OP, &
                                Get_OP_from_Symb, Symmetry_symbol
   Use CFML_Rational

   Implicit none

   private

   !---- List of public Functions ----!

   !---- List of public Subroutines ----!
   public :: Allocate_VecRef, Allocate_Restraints_Vec,    &
             Del_RefCode_ATM, &
             Fill_RefCodes_Atm, &
             Get_AFIX_Line, Get_DFIX_Line, Get_TFIX_Line, &
             ReadCode_FIX_ATM, ReadCode_VARY_ATM, &
             Split_GenRefCod_ATM, Split_LocRefCod_ATM, &
             WriteInfo_RefParams, WriteInfo_Restraints, WriteInfo_Constraints

   !---- Definitions ----!

   !!----
   !!---- TYPE :: ANGLE_RESTRAINT_TYPE
   !!--..
   !!---- April - 2022
   Type, public :: Angle_Restraint_Type
      integer                       :: IPh =0         ! Phase identification
      real(kind=cp)                 :: Obs =0.0_cp
      real(kind=cp)                 :: Cal =0.0_cp
      real(kind=cp)                 :: Sig =0.0_cp
      integer,dimension(3)          :: P   =0
      character(len=8),dimension(2) :: Code=" "
   End Type Angle_Restraint_Type

   !!----
   !!---- TYPE :: DISTANCE_RESTRAINT_TYPE
   !!--..
   !!---- Update: April - 2022
   Type, public :: Distance_Restraint_Type
      integer              :: IPh =0             ! Phase identification
      real(kind=cp)        :: Obs =0.0_cp
      real(kind=cp)        :: Cal =0.0_cp
      real(kind=cp)        :: Sig =0.0_cp
      integer,dimension(2) :: P   =0
      character(len=8)     :: Code=" "
   End Type Distance_Restraint_Type

   !!----
   !!---- TYPE :: TORSION_RESTRAINT_TYPE
   !!--..
   !!----
   !!---- Update: April - 2022
   Type, public :: Torsion_Restraint_Type
      integer                       :: IPh =0        ! Phase identification
      real(kind=cp)                 :: Obs =0.0_cp
      real(kind=cp)                 :: Cal =0.0_cp
      real(kind=cp)                 :: Sig =0.0_cp
      integer,dimension(4)          :: P   =0
      character(len=8),dimension(3) :: Code=" "
   End Type Torsion_Restraint_Type

   !!----
   !!---- TYPE :: RELATION_TYPE
   !!--..
   !!----
   !!---- Update: April - 2022
   Type, public :: Relation_Type
      character(len=30) :: Nam=" "
      integer           :: L=0            ! Code number
      real(kind=cp)     :: M=0.0_cp       ! Multiplicator
      real(kind=cp)     :: Val=0.0_cp
      real(kind=cp)     :: Sig=0.0_cp
   End Type Relation_Type

   !!----
   !!---- TYPE :: RELATIONLIST_TYPE
   !!--..
   !!----
   !!---- Update: April - 2022
   Type, public :: RelationList_Type
      integer                                        :: Npar=0
      type(Relation_Type), allocatable, dimension(:) :: Par
   End Type RelationList_Type


   !---- Parameters ----!
   integer, private, parameter :: NKEY_ATM =14      ! Number of Keywords for Atoms
   integer, private, parameter :: NKEY_MATM=25      ! Number of Keywords for Magnetic atoms
   integer, private, parameter :: NKEY_MOL =8       ! Number of Keywords for Molecule
   integer, private, parameter :: NKEY_RGB =5       ! Number of Keywords for Rigid body (RGB)
   integer, private, parameter :: NKEY_PHAS=8       ! Number of Keywords for Phases
   integer, private, parameter :: NKEY_PATT=8       ! Number of Keywords for Patterns


   character(len=*), dimension(NKEY_ATM), public, parameter :: KEY_ATM=[                      &
                     "X    ", "Y    ", "Z    ", "XYZ  ", "OCC  ",                             &
                     "UISO ", "U    ", "U11  ", "U22  ", "U33  ", "U12  ", "U13  ", "U23  ",  &
                     "ALL  "]

   character(len=*), dimension(NKEY_MATM), public, parameter :: KEY_MATM=[ &
                     "RX   ", "RY   ", "RZ   ", "IX   ", "IY   ", "IZ   ", &
                     "RM   ", "RPHI ", "RTHE ", "IM   ", "IPHI ", "ITHE ", &
                     "MAGPH",                                              &
                     "C1   ", "C2   ", "C3   ", "C4   ", "C5   ", "C6   ", &
                     "C7   ", "C8   ", "C9   ", "C10  ", "C11  ", "C12  "]

   character(len=*), dimension(NKEY_MOL), public, parameter :: KEY_MOL=[ &
                     "XC   ", "YC   ", "ZC   ", "CENTE",                 &
                     "THE  ", "PHI  ", "CHI  ", "ORIEN"]

   character(len=*), dimension(NKEY_RGB), public, parameter :: KEY_RGB=[ &
                     "T    ", "L    ", "S    ", "TL   ", "TLS  "]

   character(len=*), dimension(NKEY_PHAS), public, parameter :: KEY_PHAS=[          &
                     "A    ", "B    ", "C    ", "ALP  ", "BET  ", "GAM  ", "CELL ", &
                     "PH   "]

   character(len=*), dimension(NKEY_PATT), public, parameter :: KEY_PATT=[ &
                     "U    ", "V    ", "W    ", "UVW   ",                  &
                     "BKG  ", "SC   ", "EXTI ", "PAT   "]

   !-------------------!
   !---- Variables ----!
   !-------------------!

   !---- Private ----!
   character(len=132), private                :: line=" "
   character(len=40),  private, dimension(40) :: dire=" "
   integer,            private, dimension(10) :: ivet=0
   real(kind=cp),      private, dimension(10) :: vet=0.0_cp

   !---- Public ----!
   integer, public :: NP_Constr  =0  ! Number of Constraints relations

   integer, public :: NP_Ref_Max =0  ! Number of Maximum refinable Parameters
   integer, public :: NP_Ref     =0  ! Number of Refinable parameters

   integer, public :: NP_Rest_Ang=0  ! Number of Angle restraints relations
   integer, public :: NP_Rest_Dis=0  ! Number of Distance restraints relations
   integer, public :: NP_Rest_Tor=0  ! Number of Torsional angles restraints

   integer,           public, dimension(:)  , allocatable :: Vec_BCond    ! Vector of Boundary Conditions
   integer,           public, dimension(:)  , allocatable :: Vec_PointPar ! Vector of indices pointing to the parameter number
   real(kind=cp),     public, dimension(:,:), allocatable :: Vec_LimPar   ! Vector of Lower, Upper limits and Step for Parameters
   real(kind=cp),     public, dimension(:)  , allocatable :: Vec_RefPar   ! Vector of refined parameters (values)
   real(kind=cp),     public, dimension(:)  , allocatable :: Vec_RefParSTD! Vector of STF refined paramaters
   real(kind=cp),     public, dimension(:)  , allocatable :: Vec_RefSave  ! Vector of refined paramaters (save values)
   real(kind=cp),     public, dimension(:)  , allocatable :: Vec_RefShift ! Vector of Shifted paramaters
   character(len=40), public, dimension(:)  , allocatable :: Vec_NamePar  ! Vector of names for all refinable parameters

   type(Angle_Restraint_Type),    public, dimension(:), allocatable :: Ang_Rest     ! Relations for Angle restraints
   type(Distance_Restraint_Type), public, dimension(:), allocatable :: Dis_Rest     ! Relations for Distance restraints
   type(Torsion_Restraint_Type),  public, dimension(:), allocatable :: Tor_Rest     ! Relations for Torsional angles restraints


   !---- Overload Zone ----!

   !---- Interface Zone ----!
   Interface
      Module Subroutine Allocate_Restraints_Vec(Ffile, N_ini, N_end, NDfix, NAfix, NTFix)
         !---- Arguments ----!
         Type(file_type),  intent( in) :: Ffile
         integer, optional,intent( in) :: N_Ini
         integer, optional,intent( in) :: N_End
         integer, optional,intent(out) :: NDfix
         integer, optional,intent(out) :: NAfix
         integer, optional,intent(out) :: NTfix
      End Subroutine Allocate_Restraints_Vec

      Module Subroutine Allocate_VecRef(N)
         !---- Arguments ----!
         integer, intent(in) :: N
      End Subroutine Allocate_VecRef

      Module Subroutine Del_Element_in_VRef(N)
         !---- Arguments ----!
         integer, intent(in) :: N
      End Subroutine Del_Element_in_VRef

      Module Subroutine Del_RefCode_Atm(AtList, NPar)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NPar
      End Subroutine Del_RefCode_Atm

      Module Subroutine Fill_RefCodes_Atm(Keyword, Npar, Bounds, Ic, Natm, Spg, AtList)
         !---- Arguments ----!
         character(len=*),              intent(in)     :: Keyword
         integer,                       intent(in)     :: NPar
         real(kind=cp), dimension(3),   intent(in)     :: Bounds
         integer,                       intent(in)     :: Ic
         integer,                       intent(in)     :: Natm
         type(Spg_Type),                intent(in)     :: Spg
         type(AtList_Type),             intent(in out) :: AtList
      End Subroutine Fill_RefCodes_Atm

      Module Subroutine Fix_OCC_Atm(Atlist, NAtm)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
      End Subroutine Fix_OCC_Atm

      Module Subroutine Fix_U_Atm(Atlist, NAtm, Ind)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
         integer,           intent(in)     :: Ind
      End Subroutine Fix_U_Atm

      Module Subroutine Fix_XYZ_Atm(Atlist, NAtm, Ind)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
         integer,           intent(in)     :: Ind
      End Subroutine Fix_XYZ_Atm

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

      Module Subroutine Get_AFIX_Line(String, AtList, IPhase)
         !---- Arguments ----!
         character(len=*),  intent(in) :: String
         type(AtList_Type), intent(in) :: AtList
         integer, optional, intent(in) :: IPhase
      End Subroutine Get_AFIX_Line

      Module Subroutine Get_DFIX_Line(String, AtList, IPhase)
         !---- Arguments ----!
         character(len=*),  intent(in) :: String
         type(AtList_Type), intent(in) :: AtList
         integer, optional, intent(in) :: IPhase
      End Subroutine Get_DFIX_Line

      Module Subroutine Get_TFIX_Line(String, AtList, IPhase)
         !---- Arguments ----!
         character(len=*),  intent(in) :: String
         type(AtList_Type), intent(in) :: AtList
         integer, optional, intent(in) :: IPhase
      End Subroutine Get_TFIX_Line

      Module Subroutine Split_GenRefCod_ATM(String, Nc, Ikeys, IPhas, Keys)
         !---- Arguments ----!
         character(len=*),                         intent(in)  :: String
         integer,                                  intent(out) :: Nc
         integer, dimension(:),                    intent(out) :: IKeys
         integer, dimension(:),                    intent(out) :: IPhas
         character(len=*), dimension(:), optional, intent(out) :: Keys
      End Subroutine Split_GenRefCod_ATM

      Module Subroutine Split_LocRefCod_ATM(String, Nc, Keys, Ikeys, IPhas, AtLab)
         !---- Arguments ----!
         character(len=*),               intent(in)  :: String
         integer,                        intent(out) :: Nc
         character(len=*), dimension(:), intent(out) :: Keys
         integer,          dimension(:), intent(out) :: Ikeys
         integer,          dimension(:), intent(out) :: IPhas
         character(len=*), dimension(:), intent(out) :: AtLab
      End Subroutine Split_LocRefCod_ATM

      Module Subroutine Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)
         !---- Arguments ----!
         type(AtList_Type),           intent(in out) :: AtList
         integer,                     intent(in)     :: NAtm
         real(kind=cp), dimension(3), intent(in)     :: Bounds
         integer,                     intent(in)     :: Ic
      End Subroutine Vary_OCC_Atm

      Module Subroutine Vary_U_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
         !---- Arguments ----!
         type(AtList_Type),           intent(in out) :: AtList
         integer,                     intent(in)     :: NAtm
         integer,                     intent(in)     :: Ind
         class(SpG_Type),              intent(in)     :: Spg
         real(kind=cp), dimension(3), intent(in)     :: Bounds
         integer,                     intent(in)     :: Ic
      End Subroutine Vary_U_Atm

      Module Subroutine Vary_XYZ_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
         !---- Arguments ----!
         type(AtList_Type),           intent(in out) :: AtList
         integer,                     intent(in)     :: NAtm
         integer,                     intent(in)     :: Ind
         class(SpG_Type),              intent(in)     :: Spg
         real(kind=cp), dimension(3), intent(in)     :: Bounds
         integer,                     intent(in)     :: Ic
      End Subroutine Vary_XYZ_Atm

      Module Subroutine WriteInfo_RefParams(Iunit)
         !---- Arguments ----!
         integer, optional,   intent(in) :: Iunit
      End Subroutine WriteInfo_RefParams

      Module Subroutine WriteInfo_Restraints(AtList, Calc, Iunit)
         !---- Arguments ----!
         type(AtList_Type), intent(in) :: AtList
         logical, optional, intent(in) :: Calc
         integer, optional, intent(in) :: iunit
      End Subroutine WriteInfo_Restraints

      Module Subroutine WriteInfo_Constraints(AtList, Iunit)
         !---- Arguments ----!
         type(AtList_Type), intent(in) :: AtList
         integer, optional, intent(in) :: Iunit
      End Subroutine WriteInfo_Constraints

      Module Subroutine ReadCode_FIX_ATM(String, AtList, Spg)
         !---- Arguments ----!
         character(len=*),   intent(in)     :: String
         type(AtList_Type),  intent(in out) :: AtList
         class (SpG_type),   intent(in)     :: Spg
      End Subroutine ReadCode_FIX_ATM

      Module Subroutine ReadCode_VARY_ATM(String, AtList, Spg)
         !---- Arguments ----!
         character(len=*),   intent(in)     :: String
         type(AtList_Type),  intent(in out) :: AtList
         class (SpG_type),   intent(in)     :: Spg
      End Subroutine ReadCode_VARY_ATM

   End Interface

End Module CFML_KeyCodes