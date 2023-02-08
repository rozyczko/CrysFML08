!-------------------------------------------------------------
! Int3D08
! -------------------------------------------------------------
! This file is part of Int3d08
!
! The Int3D project is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!
! Authors: Nebil A. Katcho (ILL)
!          Juan Rodriguez-Carvajal (ILL)
!
!
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! -------------------------------------------------------------

module nexus_mod

    ! ------
    ! Public
    ! ------
    ! subroutine read_nexus(filename,nexus)
    !
    ! -------
    ! Private
    ! -------
    ! subroutine get_source(nexus)
    ! subroutine read_nexus_ill(nexus)
    ! subroutine read_nexus_ill_d19(nexus)
    ! subroutine read_nexus_ill_d1b(nexus)
    ! subroutine read_nexus_ill_d2b(nexus)
    ! subroutine read_nexus_ill_d20(nexus)
    ! subroutine read_nexus_ill_d9(nexus)
    ! subroutine set_data_ordering(data_ordering_,data_ordering)

    use hdf5
    use CFML_GlobalDeps, only: ops_sep
    use CFML_Strings, only: L_Case,Get_Filename

    implicit none

    private

    ! List of public variables
    logical, public :: err_nexus, war_nexus
    character(len=:), allocatable, public :: err_nexus_mess, war_nexus_mess

    type, public :: nexus_type

        integer                                :: nz
        integer                                :: nx
        integer                                :: nf
        real                                   :: wave
        real                                   :: scan_step
        real                                   :: gamma_step
        real                                   :: virtual_cgap
        real, dimension(3)                     :: reflection
        real, dimension(5)                     :: conditions ! Temp-s.pt,Temp-Regul,Temp-sample,Voltmeter,Mag.field
        real, dimension(3,3)                   :: ub
        real, dimension(:), allocatable        :: monitor
        real, dimension(:), allocatable        :: timef
        real, dimension(:), allocatable        :: total_counts
        real, dimension(:,:), allocatable      :: angles     ! phi,chi,omega,gamma,psi,canne,nu,2theta
        integer, dimension(:,:,:), allocatable :: counts     ! (nz,nx,nf)
        character(len=:), allocatable          :: data_ordering
        character(len=:), allocatable          :: end_time
        character(len=:), allocatable          :: filcod
        character(len=:), allocatable          :: filename
        character(len=:), allocatable          :: geometry
        character(len=:), allocatable          :: header
        character(len=:), allocatable          :: instrument_name
        character(len=:), allocatable          :: scan_type
        character(len=:), allocatable          :: source
        character(len=:), allocatable          :: title
        character(len=:), allocatable          :: user_name
        character(len=:), allocatable          :: local_contact
        logical                                :: is_gamma
        logical                                :: is_omega
        logical                                :: is_chi
        logical                                :: is_phi
        logical                                :: is_psi
        logical                                :: is_canne
        logical                                :: is_tth
        logical                                :: is_mode
        logical                                :: is_monitor
        logical                                :: is_timef
        logical                                :: is_total_counts
        logical                                :: is_ub
        logical                                :: is_virtual
        logical                                :: gamma_coupling

    end type nexus_type

    ! Parameters
    real, parameter :: MIN_DANGLE = 0.01

    ! Local variables with hdf5 types
    integer :: hdferr
    integer(HID_T) :: file_id,dset,filetype,dset2,dset3,space,space2,space3
    integer(SIZE_T), parameter :: STR_MAX_LEN = 20 ! maximum string length
    integer(HSIZE_T), dimension(1) :: scalar
    integer(HSIZE_T), dimension(2) :: maxdims,maxdims3,data_dims
    integer(HSIZE_T), dimension(3) :: dims,dims3

    ! List of public subroutines
    public :: read_nexus

    contains

    subroutine read_nexus(filename,nexus)

        ! Arguments
        character(len=*), intent(in)  :: filename
        type(nexus_type), intent(out) :: nexus

        ! Local variables
        integer :: i
        logical :: exist
        character(len=:), allocatable :: basename

        ! Initialize variables
        err_nexus  = .false.
        nexus%is_gamma          = .false.
        nexus%is_omega          = .false.
        nexus%is_chi            = .false.
        nexus%is_phi            = .false.
        nexus%is_psi            = .false.
        nexus%is_tth            = .false.
        nexus%is_mode           = .false.
        nexus%is_monitor        = .false.
        nexus%is_timef          = .false.
        nexus%is_total_counts   = .false.
        nexus%is_ub             = .false.
        nexus%is_virtual        = .false.
        nexus%gamma_coupling    = .false.

        ! Check that nexus file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus: file '//trim(filename)//' not found'
            return
        end if

        ! Extract the file code
        nexus%filename = filename
        basename = Get_Filename(nexus%filename)
        i = index(basename,'.',back=.true.)
        if (i == 0) then
            nexus%filcod = basename
        else
            nexus%filcod = basename(1:i-1)
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = "read_nexus: error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        if (hdferr /= -1) call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        if (hdferr /= -1) then
            call h5fopen_f(trim(nexus%filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = "read_nexus: error opening nexus file"
            end if
        end if

        ! Determine the source: ILL | Zebra
        if (hdferr /= -1) call get_source(nexus)

        ! Read nexus
        if (hdferr /= -1) then
            if (nexus%source == 'ill') then
                call read_nexus_ILL(nexus)
            else if (nexus%source == 'zebra') then
                !call read_nexus_Zebra(file_id,nexus)
            end if
        end if

        ! Close NEXUS file anf FORTRAN interface
        call h5fclose_f(file_id,hdferr)
        call h5close_f(hdferr)

    end subroutine read_nexus

    subroutine get_source(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        logical :: source =.false.

        call h5dopen_f(file_id,'entry0/instrument/name',dset,hdferr)
        if (hdferr /= -1) then
            source = .true.
            nexus%source = 'ill'
        end if
        if (.not. source) then
            call h5dopen_f(file_id,'entry1/ZEBRA/SINQ/name',dset,hdferr)
            if (hdferr /= -1) then
                source = .true.
                nexus%source = 'zebra'
            end if
        end if
        if (.not. source) then
            nexus%source = 'unknown'
            err_nexus = .true.
            err_nexus_mess = "get_source: Unknown source"
        end if

    end subroutine get_source

    subroutine read_nexus_ill(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        character(len=STR_MAX_LEN) :: name

        call h5dopen_f(file_id,'entry0/instrument/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,name,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%instrument_name = L_Case(adjustl(trim(name)))
        else
            err_nexus = .true.
            err_nexus_mess = "read_nexus_ill: error getting instrument name"
            return
        end if
        if (nexus%instrument_name(1:3) == 'd19') then
            call read_nexus_ill_d19(nexus)
        else if (nexus%instrument_name(1:3) == 'd1b') then
            call read_nexus_ill_d1b(nexus)
        else if (nexus%instrument_name(1:3) == 'd2b') then
            call read_nexus_ill_d2b(nexus)
        else if (nexus%instrument_name(1:2) == 'd9') then
            call read_nexus_ill_d9(nexus)
        else
            err_nexus = .true.
            err_nexus_mess = "read_nexus_ill: this program is not ready for processing data from "//nexus%instrument_name
            return
        end if

    end subroutine read_nexus_ill

    subroutine read_nexus_ill_d19(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        integer :: i,i_moni,nvar,mode
        integer, dimension(7,3) :: motors ! phi, chi, omega, gamma, psi, canne, nu
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real :: ga_val,nu_val,ome_val,chi_val,phi_val,dgamma
        real, dimension(:,:), allocatable :: datos
        character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr == -1) then
            nexus%wave = 0.0
        end if
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Det1/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if
        call h5dclose_f(dset,hdferr)

        ! Check if this nexus corresponds to a virtual detector
        call h5dopen_f(file_id,'entry0/instrument/Det1/virtual_cgap',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%virtual_cgap,scalar,hdferr)
        if (hdferr /= -1) then
            nexus%is_virtual = .true.
        else
            nexus%is_virtual = .false.
        end if
        call h5dclose_f(dset,hdferr)

        ! Get counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d19: data not found in nexus.'
            return
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        !   Assign memory to arrays and read counts
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d19: error reading counts.'
            return
        end if

        ! Read angles
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then ! Scan
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)

            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)

            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = (/ STR_MAX_LEN , dims(1) /)

            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)

            !   Motors rows: phi,chi,omega,gamma,psi,canne
            !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
            !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
            !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
            !   in the nexus file. This is not a problem because the values of gamma for every frame are
            !   accesible.
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('phi')
                            motors(1,1) = 1
                            motors(1,3) = i
                            if (scanned(i) == 1) motors(1,2) = 1
                            nexus%is_phi = .true.
                        case('chi')
                            motors(2,1) = 1
                            motors(2,3) = i
                            if (scanned(i) == 1) motors(2,2) = 1
                            nexus%is_chi = .true.
                        case('omega','flyomega')
                            motors(3,1) = 1
                            motors(3,3) = i
                            if (scanned(i) == 1) motors(3,2) = 1
                            nexus%is_omega = .true.
                        case('gamma')
                            motors(4,1) = 1
                            motors(4,3) = i
                            if (scanned(i) == 1) motors(4,2) = 1
                            nexus%is_gamma = .true.
                        case('psi')
                            motors(5,1) = 1
                            motors(5,3) = i
                            if (scanned(i) == 1) motors(5,2) = 1
                            nexus%is_psi = .true.
                        case('canne')
                            motors(6,3) = i
                            motors(6,1) = 1
                            if (scanned(i) == 1) motors(6,2) = 1
                            nexus%is_canne = .true.
                        case('nu')
                            motors(7,1) = 1
                            motors(7,3) = i
                            if (scanned(i) == 1) motors(7,2) = 1
                        case('monitor1')
                            nexus%is_monitor = .true.
                            i_moni = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nf))
                    nexus%monitor(:) = datos(:,i_moni)
                    nexus%is_monitor = .true.
                end if
                !   Check that gamma, nu, omega, chi and phi have been read. If not,
                !   read them from entry0/instrument.
                !
                !   Gamma
                if (motors(4,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_gamma = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,ga_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Nu
                if (motors(7,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                    if (hdferr == -1) then
                        ! If nu is not specified, assume nu = 0. This is the case
                        ! for example for nexus from D19
                        nu_val = 0.0
                        hdferr = 0
                    else
                        call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                    end if
                    call h5dclose_f(dset,hdferr)
                end if
                !   Omega
                if (motors(3,1) == 0 .and. motors(6,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/omega/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_omega = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,ome_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Chi
                if (motors(2,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/chi/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_chi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Phi
                if (motors(1,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/phi/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_phi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if

                !   Store angles
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(7,nexus%nf))
                if (motors(1,1) == 1) then ! Phi
                    nexus%angles(1,:) = datos(:,motors(1,3))
                    if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) < MIN_DANGLE) motors(1,2) = 0
                else
                    nexus%angles(1,:) = phi_val
                end if
                if (motors(2,1) == 1) then ! Chi
                    nexus%angles(2,:) = datos(:,motors(2,3))
                    if (abs(nexus%angles(2,nexus%nf)-nexus%angles(2,1)) < MIN_DANGLE) motors(2,2) = 0
                else
                    nexus%angles(2,:) = chi_val
                end if
                if (motors(3,1) == 1) then ! Omega
                    if (motors(6,1) == 0) then
                        nexus%angles(3,:) = datos(:,motors(3,3))
                        if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) < MIN_DANGLE) motors(3,2) = 0
                    end if
                end if
                if (motors(4,1) == 1) then ! Gamma
                    nexus%angles(4,:) = datos(:,motors(4,3))
                    dgamma = nexus%angles(4,nexus%nf) - nexus%angles(4,1)
                    if (abs(dgamma) > 0.001) then
                        nexus%gamma_coupling = .true.
                        nexus%gamma_step = dgamma / (nexus%nf-1)
                    end if
                else
                    nexus%angles(4,:) = ga_val
                end if
                if (motors(5,1) == 1) then ! Psi
                    nexus%angles(5,:) = datos(:,motors(5,3))
                    if (abs(nexus%angles(5,nexus%nf)-nexus%angles(5,1)) < MIN_DANGLE) motors(5,2) = 0
                end if
                if (motors(6,1) == 1) then ! Canne == omega
                    nexus%angles(3,:) = datos(:,motors(6,3))
                    if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) < MIN_DANGLE) motors(6,2) = 0
                else if (motors(3,1) == 0) then
                    nexus%angles(3,:) = ome_val
                end if
                if (motors(7,1) == 1) then ! Nu
                    nexus%angles(7,:) = datos(:,motors(7,3))
                    if (abs(nexus%angles(7,nexus%nf)-nexus%angles(7,1)) < MIN_DANGLE) motors(7,2) = 0
                else
                    nexus%angles(7,:) = nu_val
                end if

                !   Deduce the scan type
                if      (motors(1,2) == 1 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'phi'
                    nexus%scan_step = abs(nexus%angles(1,nexus%nf) - nexus%angles(1,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 1 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'chi'
                    nexus%scan_step = abs(nexus%angles(2,nexus%nf) - nexus%angles(2,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'omega'
                    nexus%scan_step = abs(nexus%angles(3,nexus%nf) - nexus%angles(3,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'omega'
                    nexus%scan_step = abs(nexus%angles(3,nexus%nf) - nexus%angles(3,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
                    nexus%scan_type = 'canne'
                    nexus%scan_step = abs(nexus%angles(6,nexus%nf) - nexus%angles(6,1)) / max(1,nexus%nf-1)
                    ! Set chi and phi angles to zero
                    nexus%angles(1,:) = 0.0 ! phi
                    nexus%angles(2,:) = 0.0 ! chi
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
                    nexus%scan_type = 'canne'
                    nexus%scan_step = abs(nexus%angles(6,nexus%nf) - nexus%angles(6,1)) / max(1,nexus%nf-1)
                    ! Set chi and phi angles to zero
                    nexus%angles(1,:) = 0.0 ! phi
                    nexus%angles(2,:) = 0.0 ! chi
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'gamma'
                    nexus%scan_step = abs(nexus%angles(4,nexus%nf) - nexus%angles(4,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
                    nexus%scan_type = 'psi'
                    nexus%scan_step = abs(nexus%angles(5,nexus%nf) - nexus%angles(5,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
                    nexus%scan_type = 'renninger'
                else
                    nexus%scan_type = 'q-scan'
                end if

                if (allocated(datos)) deallocate(datos)
            end if

            !   Close the datasets
            call h5dclose_f(dset,hdferr)
            call h5dclose_f(dset2,hdferr)
            call h5dclose_f(dset3,hdferr)

            ! Get mode
            call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/mode',dset,hdferr)
            if (hdferr == -1) then
                nexus%is_mode = .false.
                nexus%geometry = '??'
            else
                !   Read mode
                call h5dread_f(dset,H5T_NATIVE_INTEGER,mode,scalar,hdferr)
                !   Close the dataset
                call h5dclose_f(dset,hdferr)
                !   Set geometry
                if (mode == 0) then
                    nexus%geometry = 'NB' ! Normal Beam
                else if (mode == 1) then
                    nexus%geometry = '4C' ! Four Circle
                else
                    nexus%geometry = '??'
                end if
            end if

        else ! No scan. This case corresponds to a virtual nexus written by program gamma_scan
            ! Read gamma
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(7,nexus%nf))
            call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ga_val,scalar,hdferr)
            if (hdferr /= -1) nexus%angles(4,:) = ga_val
            if (hdferr /= -1) call h5dclose_f(dset,hdferr)
            ! Read nu
            if (hdferr /= -1) then
                call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                if (hdferr /= -1) then
                    nexus%angles(7,:) = nu_val
                else
                    nexus%angles(7,:) = 0.0
                    hdferr = 0
                end if
            end if
        end if

        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d19: error reading angles.'
            return
        end if

    end subroutine read_nexus_ill_d19

    subroutine read_nexus_ill_d1b(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        integer :: i,i_moni,i_time,i_total_counts,nvar
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real, dimension(:,:), allocatable :: datos
        character(len=80) :: title,user_name,local_contact,end_time
        character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr == -1) then
            nexus%wave = 0.0
        end if
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Title
        call h5dopen_f(file_id,'entry0/experiment_identifier',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,title,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%title = L_Case(adjustl(trim(title)))
        else
            nexus%title = ''
        end if
        ! User name
        call h5dopen_f(file_id,'entry0/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user_name,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%user_name = L_Case(adjustl(trim(user_name)))
        else
            nexus%user_name = ''
        end if
        ! Local contact
        call h5dopen_f(file_id,'entry0/user/namelocalcontact',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,local_contact,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%local_contact = L_Case(adjustl(trim(local_contact)))
        else
            nexus%local_contact = ''
        end if
        ! Title
        call h5dopen_f(file_id,'entry0/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%end_time = L_Case(adjustl(trim(end_time)))
        else
            nexus%end_time = ''
        end if
        ! Header
        nexus%header = nexus%user_name//nexus%local_contact//nexus%end_time

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Detector/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if

        ! Get counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/raw_data',dset,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill_d1b: data not found in nexus.'
                return
            end if
        end if
        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        !   Assign memory to arrays and read counts
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d1b: error reading counts.'
            return
        end if

        ! Read gamma values
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)

            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)

            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = (/ STR_MAX_LEN , dims(1) /)

            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)

            !   Motors rows: phi,chi,omega,gamma,psi,canne
            !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
            !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
            !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
            !   in the nexus file. This is not a problem because the values of gamma for every frame are
            !   accesible.
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('monitor1')
                            nexus%is_monitor = .true.
                            i_moni = i
                        case('acquisitionspy')
                            nexus%is_timef = .true.
                            i_time = i
                        case('detector','multi')
                            nexus%is_total_counts = .true.
                            i_total_counts = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nf))
                    nexus%monitor(:) = datos(:,i_moni)
                end if
                ! Store time
                if (nexus%is_timef) then
                    if (allocated(nexus%timef)) deallocate(nexus%timef)
                    allocate(nexus%timef(nexus%nf))
                    nexus%timef(:) = datos(:,i_time)
                end if
                ! Store total counts
                if (nexus%is_total_counts) then
                    if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
                    allocate(nexus%total_counts(nexus%nf))
                    nexus%total_counts(:) = datos(:,i_total_counts)
                end if
                call h5dclose_f(dset,hdferr)
            end if

            !   Close the datasets
            call h5dclose_f(dset,hdferr)
            call h5dclose_f(dset2,hdferr)
            call h5dclose_f(dset3,hdferr)
        end if

        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d1b: error reading scan.'
            return
        end if

    end subroutine read_nexus_ill_d1b

    subroutine read_nexus_ill_d2b(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local parameters
        integer, parameter :: NPIXELS_HORIZ = 128
        real,    parameter :: WIDTH_HORIZ = 1.25 ! degrees

        ! Local variables
        integer :: i,i_moni,nvar
        integer, dimension(8,3) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real :: ga_val,nu_val
        real, dimension(:,:), allocatable :: datos
        character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr == -1) then
            nexus%wave = 0.0
        end if
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Detector/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if

        ! Check if this nexus corresponds to a virtual detector
        call h5dopen_f(file_id,'entry0/instrument/Detector/virtual_cgap',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%virtual_cgap,scalar,hdferr)
        if (hdferr /= -1) then
            nexus%is_virtual = .true.
        else
            nexus%is_virtual = .false.
        end if
        call h5dclose_f(dset,hdferr)

        ! Get counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/raw_data',dset,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill_d2b: data not found in nexus.'
                return
            end if
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        !   Assign memory to arrays and read counts
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d2b: error reading counts.'
            return
        end if

        ! Read gamma values
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)

            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)

            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = (/ STR_MAX_LEN , dims(1) /)

            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)

            !   Motors rows: phi,chi,omega,gamma,psi,canne
            !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
            !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
            !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
            !   in the nexus file. This is not a problem because the values of gamma for every frame are
            !   accesible.
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('2theta')
                            motors(8,1) = 1
                            motors(8,3) = i
                            if (scanned(i) == 1) motors(8,2) = 1
                            nexus%is_tth = .true.
                        case('nu')
                            motors(7,1) = 1
                            motors(7,3) = i
                            if (scanned(i) == 1) motors(7,2) = 1
                        case('monitor1','Monitor_1')
                            nexus%is_monitor = .true.
                            i_moni = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nf))
                    nexus%monitor(:) = datos(:,i_moni)
                end if
                ! Check that this is a gamma scan, and store gamma values
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(8,nexus%nf))
                if (motors(8,2) == 0) then
                    err_nexus = .true.
                    err_nexus_mess = 'read_nexus_ill_d2b: this is not a 2theta scan.'
                    return
                else
                    nexus%angles(8,:) = datos(:,motors(8,3))
                end if
                ! Store nu
                call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                if (hdferr == -1) then
                    ! If nu is not specified, assume nu = 0.
                    nu_val = 0.0
                    hdferr = 0
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                    if (hdferr == -1) then
                        err_nexus = .true.
                        err_nexus_mess = 'read_nexus_ill_d2b: error reading nu value.'
                        return
                    end if
                end if
                nexus%angles(7,:) = nu_val
                call h5dclose_f(dset,hdferr)
            end if

            !   Close the datasets
            call h5dclose_f(dset,hdferr)
            call h5dclose_f(dset2,hdferr)
            call h5dclose_f(dset3,hdferr)
        else ! No scan. This case corresponds to a virtual nexus written by program gamma_scan
            ! Read gamma
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(7,nexus%nf))
            call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ga_val,scalar,hdferr)
            if (hdferr /= -1) nexus%angles(4,:) = ga_val
            if (hdferr /= -1) call h5dclose_f(dset,hdferr)
            ! Read nu
            if (hdferr /= -1) then
                call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                if (hdferr /= -1) then
                    nexus%angles(7,:) = nu_val
                else
                    nexus%angles(7,:) = 0.0
                    hdferr = 0
                end if
            end if
        end if

        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d2b: error reading angles.'
            return
        end if

        ! Transform 2theta into gamma values
        if (nexus%is_tth) then
            do i = 1 , nexus%nf
                nexus%angles(4,i) = nexus%angles(8,i) - 0.5 * (NPIXELS_HORIZ-1) * WIDTH_HORIZ + 0.5 * WIDTH_HORIZ
            end do
        end if

    end subroutine read_nexus_ill_d2b

    subroutine read_nexus_ill_d20(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        integer :: i,i_moni,i_time,i_total_counts,nvar
        integer, dimension(8,3) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real, dimension(:,:), allocatable :: datos
        character(len=80) :: title,user_name,local_contact,end_time
        character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr == -1) then
            nexus%wave = 0.0
        end if
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Title
        call h5dopen_f(file_id,'entry0/experiment_identifier',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,title,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%title = L_Case(adjustl(trim(title)))
        else
            nexus%title = ''
        end if
        ! User name
        call h5dopen_f(file_id,'entry0/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user_name,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%user_name = L_Case(adjustl(trim(user_name)))
        else
            nexus%user_name = ''
        end if
        ! Local contact
        call h5dopen_f(file_id,'entry0/user/namelocalcontact',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,local_contact,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%local_contact = L_Case(adjustl(trim(local_contact)))
        else
            nexus%local_contact = ''
        end if
        ! Title
        call h5dopen_f(file_id,'entry0/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            nexus%end_time = L_Case(adjustl(trim(end_time)))
        else
            nexus%end_time = ''
        end if
        ! Header
        nexus%header = nexus%user_name//nexus%local_contact//nexus%end_time

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Detector/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if

        ! Get counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/raw_data',dset,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill_d20: data not found in nexus.'
                return
            end if
        end if
        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        !   Assign memory to arrays and read counts
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d1b: error reading counts.'
            return
        end if

        ! Read gamma values
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)

            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)

            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = (/ STR_MAX_LEN , dims(1) /)

            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)

            !   Motors rows: phi,chi,omega,gamma,psi,canne
            !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
            !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
            !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
            !   in the nexus file. This is not a problem because the values of gamma for every frame are
            !   accesible.
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('2theta')
                            motors(8,1) = 1
                            motors(8,3) = i
                            if (scanned(i) == 1) motors(8,2) = 1
                            nexus%is_tth = .true.
                        case('monitor1')
                            nexus%is_monitor = .true.
                            i_moni = i
                        case('acquisitionspy')
                            nexus%is_timef = .true.
                            i_time = i
                        case('detector','multi')
                            nexus%is_total_counts = .true.
                            i_total_counts = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nf))
                    nexus%monitor(:) = datos(:,i_moni)
                end if
                ! Store time
                if (nexus%is_timef) then
                    if (allocated(nexus%timef)) deallocate(nexus%timef)
                    allocate(nexus%timef(nexus%nf))
                    nexus%timef(:) = datos(:,i_time)
                end if
                ! Store total counts
                if (nexus%is_total_counts) then
                    if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
                    allocate(nexus%total_counts(nexus%nf))
                    nexus%total_counts(:) = datos(:,i_total_counts)
                end if
                ! Check that 2theta was given
                if (motors(8,2) == 0) then
                    err_nexus = .true.
                    err_nexus_mess = 'read_nexus_ill_d20: 2theta angle not given.'
                    return
                end if
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(8,nexus%nf))
                nexus%angles(8,:) = datos(:,motors(8,3))
                call h5dclose_f(dset,hdferr)
            end if

            !   Close the datasets
            call h5dclose_f(dset,hdferr)
            call h5dclose_f(dset2,hdferr)
            call h5dclose_f(dset3,hdferr)
        end if

        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d20: error reading scan.'
            return
        end if

    end subroutine read_nexus_ill_d20

    subroutine read_nexus_ill_d9(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        integer :: i,i_moni,i_time,i_total_counts,nvar,mode
        integer, dimension(7,3) :: motors ! phi, chi, omega, gamma, psi, canne, nu
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real :: ga_val,nu_val,ome_val,chi_val,phi_val,dgamma
        real, dimension(9) :: ub
        real, dimension(:,:), allocatable :: datos
        character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr == -1) then
            nexus%wave = 0.0
        end if
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Det1/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if
        call h5dclose_f(dset,hdferr)

        ! No virtual detector for D9
        nexus%is_virtual = .false.

        ! Read reflection
        nexus%reflection(:) = 0.0
        call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/reflection',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection,dims,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Read UB-matrix
        call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/orientation_matrix',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ub,dims,hdferr)
        if (hdferr /= -1) then
            nexus%ub = transpose(reshape(ub,(/3,3/)))
            nexus%is_ub = .true.
        end if
        call h5dclose_f(dset,hdferr)

        ! Read Conditions
        nexus%conditions = 0.0
        !   Setpoint temperature
        call h5dopen_f(file_id,'entry0/sample/setpoint_temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%conditions(1),scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        !   Regulation temperature
        call h5dopen_f(file_id,'entry0/sample/regulation_temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%conditions(2),scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        !   Sample temperature
        call h5dopen_f(file_id,'entry0/sample/temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%conditions(3),scalar,hdferr)
        call h5dclose_f(dset,hdferr)
        !   Magnetic field
        call h5dopen_f(file_id,'entry0/sample/field',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%conditions(5),scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Get counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d9: data not found in nexus.'
            return
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
        !   Assign memory to arrays and read counts
        if (hdferr /= -1) then
            nexus%nz = dims(1)
            nexus%nx = dims(2)
            nexus%nf = dims(3)
            if (allocated(nexus%counts)) deallocate(nexus%counts)
            allocate(nexus%counts(nexus%nz,nexus%nx,nexus%nf))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
            call h5dclose_f(dset,hdferr)
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d9: error reading counts.'
            return
        end if

        ! Read angles
        call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
        if (hdferr /= -1) call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
        if (hdferr /= -1) then ! Scan
            !   Get the data type
            call h5dget_type_f(dset,filetype,hdferr)

            !   Get dimensions of the dataset
            if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset2,space2,hdferr)
            if (hdferr /= -1) call h5dget_space_f(dset3,space3,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            if (hdferr /= -1) call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
            if (hdferr /= -1) nvar = dims(1)

            !   Assign memory to arrays and initalize them
            if (hdferr /= -1) allocate(var_names(nvar))
            if (hdferr /= -1) allocate(str_len(nvar))
            if (hdferr /= -1) allocate(scanned(nvar))
            if (hdferr /= -1) allocate(datos(nexus%nf,nvar))
            if (hdferr /= -1) str_len(:) = STR_MAX_LEN
            if (hdferr /= -1) scanned(:) = 0
            if (hdferr /= -1) data_dims = (/ STR_MAX_LEN , dims(1) /)

            !   Read data
            if (hdferr /= -1) call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
            if (hdferr /= -1) call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
            if (hdferr /= -1) call h5dread_f(dset3,H5T_NATIVE_REAL,datos,dims3,hdferr)

            !   Motors rows: phi,chi,omega,gamma,psi,canne
            !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
            !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
            !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
            !   in the nexus file. This is not a problem because the values of gamma for every frame are
            !   accesible.
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('phi')
                            motors(1,1) = 1
                            motors(1,3) = i
                            if (scanned(i) == 1) motors(1,2) = 1
                            nexus%is_phi = .true.
                        case('chi')
                            motors(2,1) = 1
                            motors(2,3) = i
                            if (scanned(i) == 1) motors(2,2) = 1
                            nexus%is_chi = .true.
                        case('omega')
                            motors(3,1) = 1
                            motors(3,3) = i
                            if (scanned(i) == 1) motors(3,2) = 1
                            nexus%is_omega = .true.
                        case('gamma')
                            motors(4,1) = 1
                            motors(4,3) = i
                            if (scanned(i) == 1) motors(4,2) = 1
                            nexus%is_gamma = .true.
                        case('psi')
                            motors(5,1) = 1
                            motors(5,3) = i
                            if (scanned(i) == 1) motors(5,2) = 1
                            nexus%is_psi = .true.
                        case('canne')
                            motors(6,3) = i
                            motors(6,1) = 1
                            if (scanned(i) == 1) motors(6,2) = 1
                            nexus%is_canne = .true.
                        case('nu')
                            motors(7,1) = 1
                            motors(7,3) = i
                            if (scanned(i) == 1) motors(7,2) = 1
                        case('monitor1')
                            nexus%is_monitor = .true.
                            i_moni = i
                        case('acquisitionspy')
                            nexus%is_timef = .true.
                            i_time = i
                        case('detector','multi')
                            nexus%is_total_counts = .true.
                            i_total_counts = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nf))
                    nexus%monitor(:) = datos(:,i_moni)
                end if
                ! Store time
                if (nexus%is_timef) then
                    if (allocated(nexus%timef)) deallocate(nexus%timef)
                    allocate(nexus%timef(nexus%nf))
                    nexus%timef(:) = datos(:,i_time)
                end if
                ! Store total counts
                if (nexus%is_total_counts) then
                    if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
                    allocate(nexus%total_counts(nexus%nf))
                    nexus%total_counts(:) = datos(:,i_total_counts)
                end if
                !   Check that gamma, nu, omega, chi and phi have been read. If not,
                !   read them from entry0/instrument.
                !   Gamma
                if (motors(4,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_gamma = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,ga_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Nu
                if (motors(7,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                    if (hdferr == -1) then
                        ! If nu is not specified, assume nu = 0.
                        nu_val = 0.0
                        hdferr = 0
                    else
                        call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                    end if
                    call h5dclose_f(dset,hdferr)
                end if
                !   Omega
                if (motors(3,1) == 0 .and. motors(6,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/omega/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_omega = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,ome_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Chi
                if (motors(2,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/chi/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_chi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if
                !   Phi
                if (motors(1,1) == 0) then
                    call h5dopen_f(file_id,'entry0/instrument/phi/value',dset,hdferr)
                    if (hdferr == -1) then
                        hdferr = 0
                    else
                        nexus%is_phi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                    end if
                end if

                !   Store angles
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(7,nexus%nf))
                if (motors(1,1) == 1) then ! Phi
                    nexus%is_phi = .true.
                    nexus%angles(1,:) = datos(:,motors(1,3))
                    if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) < MIN_DANGLE) motors(1,2) = 0
                else
                    nexus%angles(1,:) = phi_val
                end if
                if (motors(2,1) == 1) then ! Chi
                    nexus%is_chi = .true.
                    nexus%angles(2,:) = datos(:,motors(2,3))
                    if (abs(nexus%angles(2,nexus%nf)-nexus%angles(2,1)) < MIN_DANGLE) motors(2,2) = 0
                else
                    nexus%angles(2,:) = chi_val
                end if
                if (motors(3,1) == 1) then ! Omega
                    nexus%is_omega = .true.
                    if (motors(6,1) == 0) then
                        nexus%angles(3,:) = datos(:,motors(3,3))
                        if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) < MIN_DANGLE) motors(3,2) = 0
                    end if
                end if
                nexus%gamma_coupling = .false.
                if (motors(4,1) == 1) then ! Gamma
                    nexus%is_gamma = .true.
                    nexus%angles(4,:) = datos(:,motors(4,3))
                    dgamma = nexus%angles(4,nexus%nf) - nexus%angles(4,1)
                    if (abs(dgamma) > 0.001) then
                        nexus%gamma_coupling = .true.
                        nexus%gamma_step = dgamma / (nexus%nf-1)
                    end if
                else
                    nexus%angles(4,:) = ga_val
                end if
                if (motors(5,1) == 1) then ! Psi
                    nexus%is_psi = .true.
                    nexus%angles(5,:) = datos(:,motors(5,3))
                    if (abs(nexus%angles(5,nexus%nf)-nexus%angles(5,1)) < MIN_DANGLE) motors(5,2) = 0
                end if
                if (motors(6,1) == 1) then ! Canne == omega
                    nexus%is_canne = .true.
                    nexus%angles(3,:) = datos(:,motors(6,3))
                    if (abs(nexus%angles(3,nexus%nf)-nexus%angles(3,1)) < MIN_DANGLE) motors(6,2) = 0
                else if (motors(3,1) == 0) then
                    nexus%angles(3,:) = ome_val
                end if
                if (motors(7,1) == 1) then ! Nu
                    nexus%angles(7,:) = datos(:,motors(7,3))
                    if (abs(nexus%angles(7,nexus%nf)-nexus%angles(7,1)) < MIN_DANGLE) motors(7,2) = 0
                else
                    nexus%angles(7,:) = nu_val
                end if

                !   Deduce the scan type
                if      (motors(1,2) == 1 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'phi'
                    nexus%scan_step = abs(nexus%angles(1,nexus%nf) - nexus%angles(1,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 1 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'chi'
                    nexus%scan_step = abs(nexus%angles(2,nexus%nf) - nexus%angles(2,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'omega'
                    nexus%scan_step = abs(nexus%angles(3,nexus%nf) - nexus%angles(3,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'omega'
                    nexus%scan_step = abs(nexus%angles(3,nexus%nf) - nexus%angles(3,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
                    nexus%scan_type = 'canne'
                    nexus%scan_step = abs(nexus%angles(6,nexus%nf) - nexus%angles(6,1)) / max(1,nexus%nf-1)
                    ! Set chi and phi angles to zero
                    nexus%angles(1,:) = 0.0 ! phi
                    nexus%angles(2,:) = 0.0 ! chi
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
                    nexus%scan_type = 'canne'
                    nexus%scan_step = abs(nexus%angles(6,nexus%nf) - nexus%angles(6,1)) / max(1,nexus%nf-1)
                    ! Set chi and phi angles to zero
                    nexus%angles(1,:) = 0.0 ! phi
                    nexus%angles(2,:) = 0.0 ! chi
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
                    nexus%scan_type = 'gamma'
                    nexus%scan_step = abs(nexus%angles(4,nexus%nf) - nexus%angles(4,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
                    nexus%scan_type = 'psi'
                    nexus%scan_step = abs(nexus%angles(5,nexus%nf) - nexus%angles(5,1)) / max(1,nexus%nf-1)
                else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
                    nexus%scan_type = 'renninger'
                else
                    nexus%scan_type = 'q-scan'
                end if
                if (allocated(datos)) deallocate(datos)
            end if

            !   Close the datasets
            call h5dclose_f(dset,hdferr)
            call h5dclose_f(dset2,hdferr)
            call h5dclose_f(dset3,hdferr)

            ! Get mode
            call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/mode',dset,hdferr)
            if (hdferr == -1) then
                nexus%is_mode = .false.
                nexus%geometry = '??'
            else
                !   Read mode
                call h5dread_f(dset,H5T_NATIVE_INTEGER,mode,scalar,hdferr)
                !   Close the dataset
                call h5dclose_f(dset,hdferr)
                !   Set geometry
                if (mode == 0) then
                    nexus%geometry = 'NB' ! Normal Beam
                else if (mode == 1) then
                    nexus%geometry = '4C' ! Four Circle
                else
                    nexus%geometry = '??'
                end if
            end if

        else ! No scan. This case corresponds to a virtual nexus written by program gamma_scan
            ! Read gamma
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(7,nexus%nf))
            call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
            if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ga_val,scalar,hdferr)
            if (hdferr /= -1) nexus%angles(4,:) = ga_val
            if (hdferr /= -1) call h5dclose_f(dset,hdferr)
            ! Read nu
            if (hdferr /= -1) then
                call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                if (hdferr /= -1) then
                    nexus%angles(7,:) = nu_val
                else
                    nexus%angles(7,:) = 0.0
                    hdferr = 0
                end if
            end if
        end if
        if (hdferr == -1) then
            err_nexus = .true.
            err_nexus_mess = 'read_nexus_ill_d9: error reading angles.'
            return
        end if

    end subroutine read_nexus_ill_d9

    subroutine set_data_ordering(data_ordering_,data_ordering)

        ! Arguments
        character(len=:), allocatable, intent(in)  :: data_ordering_
        character(len=:), allocatable, intent(out) :: data_ordering

        ! Local variables
        integer :: i
        character(len=:), allocatable :: key1,key2,key3

        i=index(data_ordering_,'top')
        if (i > 0) then
            key1 = 'top'
        else
            key1 = 'bottom'
        end if
        i=index(data_ordering_,'left')
        if (i > 0) then
            key2 = 'left'
        else
            key2 = 'right'
        end if
        i=index(data_ordering_,'row')
        if (i > 0) then
            key3 = 'row'
        else
            key3 = 'col'
        end if
        data_ordering=key1//key2//key3//'major'

    end subroutine set_data_ordering

end module nexus_mod