!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2019  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
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
!!---- MODULE: CFML_HDF5
!!----         Module for working with HDF5 files.
!!----
!!
Module CFML_HDF5
    !---- Use Modules ----!
    use hdf5
    use CFML_GlobalDeps
    use CFML_Strings,         only: l_case, Get_Filename
    use CFML_ILL_Instrm_Data, only: SXTAL_Numor_type,Read_Numor

    implicit none

    private

    Type, public :: Nexus_Type
        integer                                         :: nanodes
        integer                                         :: ncathodes
        integer                                         :: nframes
        integer                                         :: scan_angle
        integer, dimension(:,:,:), allocatable          :: counts
        real(kind=cp)                                   :: gamma_step
        real(kind=cp)                                   :: temperature
        real(kind=cp), dimension(3)                     :: scan_info
        real(kind=cp), dimension(:), allocatable        :: monitor
        real(kind=cp), dimension(:,:), allocatable      :: angles ! phi,chi,omega,gamma,psi,nu
        logical                                         :: is_name
        logical                                         :: is_scantype
        logical                                         :: is_gamma
        logical                                         :: is_omega
        logical                                         :: is_chi
        logical                                         :: is_phi
        logical                                         :: is_mode
        logical                                         :: is_monitor
        logical                                         :: gamma_coupling
        character(len=2)                                :: geometry
        character(len=10)                               :: instrument_name
        character(len=20)                               :: scan_type
        character(len=512)                              :: filename
        character(len=512)                              :: filcod
    End Type Nexus_Type

    type, public :: Cfl_Type
        integer, dimension(:,:), allocatable :: numor_list
        character(len=10)  :: instrument_name
        character(len=512) :: numor_path
    end type Cfl_Type

    ! List of public variables
    logical,            public :: err_nexus
    character(len=256), public :: err_nexus_mess

    ! List of public subroutines
    public :: Initialize_Nexus,Read_Nexus, Ascii_to_Nexus

    Interface

        Module Subroutine Read_Nexus(nexus)
          type(nexus_type), intent(in out)  :: nexus
        End Subroutine Read_Nexus

        Module Subroutine Ascii_to_Nexus(cfl,n,ierr)
          type(cfl_type), intent(in)  :: cfl
          integer,        intent(in)  :: n
          integer,        intent(out) :: ierr
        End Subroutine Ascii_to_Nexus

    End Interface

    Contains

    subroutine Initialize_Nexus(nexus)
        !---- Arguments ----!
        type(Nexus_Type), intent(inout)  :: nexus

        nexus%instrument_name = ''
        nexus%scan_type       = ''
        nexus%geometry        = ''
        nexus%filename        = ''
        nexus%filcod          = ''
        nexus%nanodes         = 0
        nexus%ncathodes       = 0
        nexus%nframes         = 0
        nexus%scan_angle      = 0
        nexus%scan_info(:)    = 0.0
        nexus%temperature     = 0.0
        nexus%gamma_step      = 0.0
        nexus%is_name         = .false.
        nexus%is_scantype     = .true.
        nexus%is_gamma        = .true.
        nexus%is_omega        = .true.
        nexus%is_chi          = .true.
        nexus%is_phi          = .true.
        nexus%is_mode         = .true.
        nexus%is_monitor      = .false.
        nexus%gamma_coupling  = .false.
        if (allocated(nexus%counts)) deallocate(nexus%counts)
        if (allocated(nexus%angles)) deallocate(nexus%angles)

    end subroutine Initialize_Nexus

End Module CFML_HDF5