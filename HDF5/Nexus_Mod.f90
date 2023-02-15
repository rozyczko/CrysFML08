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
    ! subroutine set_data_ordering(data_ordering_,data_ordering)

    use hdf5
    use CFML_GlobalDeps, only: ops_sep
    use CFML_ILL_Instrm_Data, only: Powder_Numor_Type,Initialize_Numor
    use CFML_Strings, only: L_Case,Get_Filename

    implicit none

    private

    ! List of public variables
    logical, public :: err_nexus, war_nexus
    character(len=:), allocatable, public :: err_nexus_mess, war_nexus_mess

    type, public :: nexus_type

        integer                                :: manip
        integer                                :: nbang  ! Total number of angles moved during the scan
        integer                                :: nbdata ! nz * nx
        integer                                :: nz     ! number of vertical pixels
        integer                                :: nx     ! number of horizontal pixels
        integer                                :: nf     ! number of frames
        integer                                :: run_number
        real                                   :: fcoupling
        real                                   :: magnetic_field
        real                                   :: reg_temperature
        real                                   :: scan_start
        real                                   :: scan_step
        real                                   :: scan_width
        real                                   :: setp_temperature
        real                                   :: temperature
        real                                   :: virtual_cgap
        real                                   :: wave
        real, dimension(3)                     :: reflection
        real, dimension(3,3)                   :: ub
        real, dimension(:), allocatable        :: monitor
        real, dimension(:), allocatable        :: timef
        real, dimension(:), allocatable        :: total_counts
        real, dimension(:,:), allocatable      :: angles     ! phi,chi,omega,gamma,psi,canne,nu,2theta
        integer, dimension(:,:,:), allocatable :: counts     ! (nz,nx,nf)
        character(len=:), allocatable          :: data_ordering
        character(len=:), allocatable          :: end_time
        character(len=:), allocatable          :: experiment_id
        character(len=:), allocatable          :: filcod
        character(len=:), allocatable          :: filename
        character(len=:), allocatable          :: geometry
        character(len=:), allocatable          :: instrument_name
        character(len=:), allocatable          :: scan_type
        character(len=:), allocatable          :: source
        character(len=:), allocatable          :: user
        character(len=:), allocatable          :: local_contact
        logical                                :: is_canne
        logical                                :: is_chi
        logical                                :: is_gamma
        logical                                :: is_mode
        logical                                :: is_monitor
        logical                                :: is_nu
        logical                                :: is_omega
        logical                                :: is_phi
        logical                                :: is_psi
        logical                                :: is_timef
        logical                                :: is_total_counts
        logical                                :: is_tth
        logical                                :: is_ub
        logical                                :: is_virtual

    end type nexus_type

    ! Parameters
    real, parameter :: MIN_SCAN_ANGLE = 0.01

    ! Local variables with hdf5 types
    integer :: hdferr
    integer(HID_T) :: file_id,dset,filetype,dset2,dset3,space,space2,space3
    integer(SIZE_T), parameter :: STR_MAX_LEN = 20 ! maximum string length
    integer(HSIZE_T), dimension(1) :: scalar
    integer(HSIZE_T), dimension(2) :: maxdims,maxdims3,data_dims
    integer(HSIZE_T), dimension(3) :: dims,dims3

    ! List of public subroutines
    public :: nxs_to_powder_numor,initialize_nexus,read_nexus

    contains

    ! Public procedures

    function nxs_to_powder_numor(nexus) Result(pnum)

        ! Arguments
        type(nexus_type), intent(in) :: nexus
        type(Powder_Numor_type)      :: pnum

        ! Local variables
        integer :: i,n,ia,ic,nf

        call Initialize_Numor(pnum,nexus%nbang,nexus%nbdata,nexus%nf)
        pnum%title = trim(nexus%experiment_id)
        pnum%header = trim(nexus%user)//trim(nexus%local_contact)//trim(nexus%end_time)
        pnum%instrm = trim(nexus%instrument_name)
        pnum%numor = nexus%run_number
        pnum%manip = nexus%manip
        pnum%scantype = nexus%scan_type
        pnum%wave = nexus%wave
        pnum%conditions(1) = nexus%setp_temperature
        pnum%conditions(2) = nexus%reg_temperature
        pnum%conditions(3) = nexus%temperature
        pnum%conditions(5) = nexus%magnetic_field
        pnum%scans(1) = nexus%scan_start
        pnum%scans(2) = nexus%scan_step
        pnum%scans(3) = nexus%scan_width
        if (nexus%is_timef) pnum%tmc_ang(1,:) = nexus%timef(:)
        if (nexus%is_monitor) pnum%tmc_ang(2,:) = nexus%monitor(:)
        if (nexus%is_total_counts) pnum%tmc_ang(3,:) = nexus%total_counts(:)
        pnum%time = sum(pnum%tmc_ang(1,:))
        pnum%monitor = sum(pnum%tmc_ang(2,:)) / pnum%nframes
        if (nexus%is_phi) pnum%angles(1) = 0.5*(nexus%angles(1,1) + nexus%angles(1,nexus%nf))
        if (nexus%is_chi) pnum%angles(2) = 0.5*(nexus%angles(2,1) + nexus%angles(2,nexus%nf))
        if (nexus%is_omega) then
            pnum%angles(3) = 0.5*(nexus%angles(3,1) + nexus%angles(3,nexus%nf))
        else if (nexus%is_canne) then
            pnum%angles(3) = 0.5*(nexus%angles(6,1) + nexus%angles(6,nexus%nf))
        end if
        if (nexus%is_tth) then
            pnum%angles(4) = 0.5*(nexus%angles(8,1) + nexus%angles(8,nexus%nf))
        else if (nexus%is_gamma) then
            pnum%angles(4) = 0.5*(nexus%angles(4,1) + nexus%angles(4,nexus%nf))
        end if
        if (nexus%is_phi) pnum%angles(5) = 0.5*(nexus%angles(5,1) + nexus%angles(5,nexus%nf))
        do n = 1 , nf
            i = 0
            do ic = 1 , nexus%nx
                do ia = 1 , nexus%nz
                    i = i + 1
                    pnum%counts(i,n)= nexus%counts(ia,ic,n) ! (nz,nx,nf)
                end do
            end do
        end do
        if (nexus%nbang == 1) then
            if (nexus%scan_type == 'phi') then
                pnum%tmc_ang(4,:) = nexus%angles(1,:)
            else if (nexus%scan_type == 'chi') then
                pnum%tmc_ang(4,:) = nexus%angles(2,:)
            else if (nexus%scan_type == 'omega') then
                pnum%tmc_ang(4,:) = nexus%angles(3,:)
            else if (nexus%scan_type == 'gamma') then
                pnum%tmc_ang(4,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'psi') then
                pnum%tmc_ang(4,:) = nexus%angles(5,:)
            else if (nexus%scan_type == 'canne') then
                pnum%tmc_ang(4,:) = nexus%angles(6,:)
            else if (nexus%scan_type == 'nu') then
                pnum%tmc_ang(4,:) = nexus%angles(7,:)
            else if (nexus%scan_type == '2theta') then
                pnum%tmc_ang(4,:) = nexus%angles(8,:)
            end if
        else if (nexus%nbang == 2) then
            if (nexus%scan_type == 'omega') then
                pnum%tmc_ang(4,:) = nexus%angles(3,:)
                pnum%tmc_ang(5,:) = nexus%angles(4,:)
            else if (nexus%scan_type == 'canne') then
                pnum%tmc_ang(4,:) = nexus%angles(6,:)
                pnum%tmc_ang(5,:) = nexus%angles(4,:)
            end if
        end if
        if (nexus%geometry == 'NB') pnum%icalc = 1

    end function nxs_to_powder_numor

    subroutine initialize_nexus(nexus)

        !---- Arguments ----!
        type(nexus_type), intent(inout)  :: nexus

        nexus%manip             = 0
        nexus%nz                = 0
        nexus%nx                = 0
        nexus%nf                = 0
        nexus%run_number        = 0
        nexus%fcoupling         = 0.0
        nexus%magnetic_field    = 0.0
        nexus%reflection(:)     = 0.0
        nexus%reg_temperature   = 0.0
        nexus%scan_start        = 0.0
        nexus%scan_step         = 0.0
        nexus%scan_width        = 0.0
        nexus%setp_temperature  = 0.0
        nexus%temperature       = 0.0
        nexus%ub(:,:)           = 0.0
        nexus%virtual_cgap      = 0.0
        nexus%wave              = 0.0
        nexus%data_ordering     = ''
        nexus%end_time          = ''
        nexus%experiment_id     = ''
        nexus%filcod            = ''
        nexus%filename          = ''
        nexus%geometry          = '4C'
        nexus%instrument_name   = ''
        nexus%local_contact     = ''
        nexus%scan_type         = ''
        nexus%source            = ''
        nexus%user              = ''
        nexus%is_canne          = .false.
        nexus%is_chi            = .false.
        nexus%is_gamma          = .false.
        nexus%is_mode           = .false.
        nexus%is_monitor        = .false.
        nexus%is_nu             = .false.
        nexus%is_omega          = .false.
        nexus%is_phi            = .false.
        nexus%is_psi            = .false.
        nexus%is_timef          = .false.
        nexus%is_tth            = .false.
        nexus%is_total_counts   = .false.
        nexus%is_ub             = .false.
        nexus%is_virtual        = .false.
        if (allocated(nexus%monitor))      deallocate(nexus%monitor)
        if (allocated(nexus%timef))        deallocate(nexus%timef)
        if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
        if (allocated(nexus%counts))       deallocate(nexus%counts)
        if (allocated(nexus%angles))       deallocate(nexus%angles)

    end subroutine initialize_nexus

    subroutine read_nexus(filename,nexus,source)

        ! Arguments
        character(len=*), intent(in)  :: filename
        type(nexus_type), intent(out) :: nexus
        character(len=*), intent(in), optional :: source

        ! Local variables
        integer :: i
        logical :: exist
        character(len=:), allocatable :: basename,src

        ! Initialize variables
        err_nexus  = .false.
        call initialize_nexus(nexus)

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
                call h5close_f(hdferr)
                return
            end if
        end if

        if (present(source)) then
            src = source
        else
            src = 'ill'
        end if

        if (source == 'ill') then
            call read_nexus_ILL(nexus)
        else
            err_nexus = .true.
            err_nexus_mess = "read_nexus: unknown source"
            call h5fclose_f(file_id,hdferr)
            call h5close_f(hdferr)
            return
        end if

        ! Close NEXUS file anf FORTRAN interface
        call h5fclose_f(file_id,hdferr)
        call h5close_f(hdferr)

    end subroutine read_nexus

    subroutine read_nexus_ill(nexus)

        ! Arguments
        type(nexus_type), intent(inout) :: nexus

        ! Local variables
        integer :: i,mode,nvar
        integer, dimension(8) :: motors ! phi, chi, omega, gamma, psi, canne, nu, 2theta
        integer, dimension(:), allocatable :: scanned
        integer(SIZE_T), dimension(:), allocatable :: str_len
        real :: phi_val,chi_val,omegamma_val,gamma_val,psi_val,canne_val,nu_val,tth_val
        real, dimension(9) :: ub
        real, dimension(:,:), allocatable :: datos
        !character(len=30) :: data_ordering
        character(len=STR_MAX_LEN) :: key
        character(len=STR_MAX_LEN), dimension(:), allocatable :: var_names
        character(len=STR_MAX_LEN) :: name
        character(len=30) :: data_ordering
        character(len=80) :: user,local_contact,end_time,experiment_id

        ! Instrument name
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

        ! Run number
        call h5dopen_f(file_id,'entry0/run_number',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%run_number,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Wavelength
        call h5dopen_f(file_id,'entry0/wavelength',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wave,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! User name
        call h5dopen_f(file_id,'entry0/user/name',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,user,dims,hdferr)
        if (hdferr /= -1) nexus%user = user
        call h5dclose_f(dset,hdferr)

        ! Local contact
        call h5dopen_f(file_id,'entry0/user/namelocalcontact',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,local_contact,dims,hdferr)
        if (hdferr /= -1) nexus%local_contact = local_contact
        call h5dclose_f(dset,hdferr)

        ! End time
        call h5dopen_f(file_id,'entry0/end_time',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,end_time,dims,hdferr)
        if (hdferr /= -1) nexus%end_time = end_time
        call h5dclose_f(dset,hdferr)

        ! Experiment identifier
        call h5dopen_f(file_id,'entry0/experiment_identifier',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,experiment_id,dims,hdferr)
        if (hdferr /= -1) nexus%experiment_id = experiment_id
        call h5dclose_f(dset,hdferr)

        ! Setpoint temperature
        call h5dopen_f(file_id,'entry0/sample/setp_temperatureerature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%setp_temperature,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Regulation temperature
        call h5dopen_f(file_id,'entry0/sample/reg_temperatureerature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reg_temperature,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Sample temperature
        call h5dopen_f(file_id,'entry0/sample/temperature',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%temperature,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Magnetic field
        call h5dopen_f(file_id,'entry0/sample/field',dset,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%magnetic_field,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Geometry
        call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/mode',dset,hdferr)
        if (hdferr /= -1) then
            call h5dread_f(dset,H5T_NATIVE_INTEGER,mode,scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            if (mode == 0) then
                nexus%geometry = 'NB' ! Normal Beam
            else if (mode == 1) then
                nexus%geometry = '4C' ! Four Circle
            else
                nexus%geometry = '??'
            end if
        end if

        ! Data ordering
        call h5dopen_f(file_id,'entry0/instrument/Detector/data_ordering',dset,hdferr)
        if (hdferr == -1) call h5dopen_f(file_id,'entry0/instrument/Det1/data_ordering',dset,hdferr)
        if (hdferr /= -1) call h5dget_type_f(dset,filetype,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,filetype,data_ordering,dims,hdferr)
        call h5dclose_f(dset,hdferr)
        if (hdferr /= -1) then
            call set_data_ordering(L_Case(data_ordering),nexus%data_ordering)
        else
            nexus%data_ordering = 'unknown'
        end if

        ! Reflection
        call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/reflection',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nexus%reflection,dims,hdferr)
        call h5dclose_f(dset,hdferr)

        ! UB-matrix
        call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/orientation_matrix',dset,hdferr)
        if (hdferr /= -1) call h5dget_space_f(dset,space,hdferr)
        if (hdferr /= -1) call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
        if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,ub,dims,hdferr)
        if (hdferr /= -1) then
            nexus%ub = transpose(reshape(ub,(/3,3/)))
            nexus%is_ub = .true.
        end if
        call h5dclose_f(dset,hdferr)

        ! Counts
        call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
        if (hdferr == -1) then
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/raw_data',dset,hdferr)
            if (hdferr == -1) then
                err_nexus = .true.
                err_nexus_mess = 'read_nexus_ill: data not found in nexus.'
                return
            end if
        end if
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims3,hdferr)
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
            err_nexus_mess = 'read_nexus_ill: error reading counts.'
            return
        end if

        ! Scan variables
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
            !   motors: phi,chi,omega,gamma,psi,canne,nu,2theta
            !           if motors(i) == 1, this motor is moved during the scan
            motors(:) = 0
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(8,nexus%nf))
            if (hdferr /= -1) then
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('phi')
                            nexus%is_phi = .true.
                            nexus%angles(1,:) = datos(:,i)
                            if (abs(nexus%angles(1,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(1) = 1
                        case('chi')
                            nexus%is_chi = .true.
                            nexus%angles(2,:) = datos(:,i)
                            if (abs(nexus%angles(2,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(2) = 1
                        case('omega')
                            nexus%is_omega = .true.
                            nexus%angles(3,:) = datos(:,i)
                            if (abs(nexus%angles(3,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(3) = 1
                        case('gamma')
                            nexus%is_gamma = .true.
                            nexus%angles(4,:) = datos(:,i)
                            if (abs(nexus%angles(4,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(4) = 1
                        case('psi')
                            nexus%is_psi = .true.
                            nexus%angles(5,:) = datos(:,i)
                            if (abs(nexus%angles(5,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(5) = 1
                        case('canne')
                            nexus%is_canne = .true.
                            nexus%angles(6,:) = datos(:,i)
                            if (abs(nexus%angles(6,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(6) = 1
                        case('nu')
                            nexus%is_nu = .true.
                            nexus%angles(7,:) = datos(:,i)
                            if (abs(nexus%angles(7,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(7) = 1
                        case('2theta')
                            nexus%is_tth = .true.
                            nexus%angles(8,:) = datos(:,i)
                            if (abs(nexus%angles(8,nexus%nf)-nexus%angles(1,1)) > MIN_SCAN_ANGLE) motors(8) = 1
                        case('monitor1','Monitor_1')
                            nexus%is_monitor = .true.
                            if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                            allocate(nexus%monitor(nexus%nf))
                            nexus%monitor(:) = datos(:,i)
                        case('acquisitionspy')
                            nexus%is_timef = .true.
                            if (allocated(nexus%timef)) deallocate(nexus%timef)
                            allocate(nexus%timef(nexus%nf))
                            nexus%timef(:) = datos(:,i)
                        case('detector','multi')
                            nexus%is_total_counts = .true.
                            if (allocated(nexus%total_counts)) deallocate(nexus%total_counts)
                            allocate(nexus%total_counts(nexus%nf))
                            nexus%total_counts(:) = datos(:,i)
                    end select
                end do
                if (.not. nexus%is_phi) then
                    call h5dopen_f(file_id,'entry0/instrument/phi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_phi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(1,:) = phi_val
                    end if
                end if
                if (.not. nexus%is_chi) then
                    call h5dopen_f(file_id,'entry0/instrument/chi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_chi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(2,:) = chi_val
                    end if
                end if
                if (.not. nexus%is_omega) then
                    call h5dopen_f(file_id,'entry0/instrument/omega/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_omega = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,omegamma_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(3,:) = omegamma_val
                    end if
                end if
                if (.not. nexus%is_gamma) then
                    call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_gamma = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(4,:) = gamma_val
                    end if
                end if
                if (.not. nexus%is_psi) then
                    call h5dopen_f(file_id,'entry0/instrument/psi/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_psi = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,psi_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(5,:) = psi_val
                    end if
                end if
                if (.not. nexus%is_canne) then
                    call h5dopen_f(file_id,'entry0/instrument/canne/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_canne = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,canne_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(6,:) = canne_val
                    end if
                end if
                if (.not. nexus%is_nu) then
                    call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_nu = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(7,:) = nu_val
                    end if
                end if
                if (.not. nexus%is_tth) then
                    call h5dopen_f(file_id,'entry0/instrument/2theta/value',dset,hdferr)
                    if (hdferr /= -1) then
                        nexus%is_tth = .true.
                        call h5dread_f(dset,H5T_NATIVE_REAL,tth_val,scalar,hdferr)
                        call h5dclose_f(dset,hdferr)
                        nexus%angles(8,:) = tth_val
                    end if
                end if

                !   Deduce the scan type
                do i = 1 , 8
                    if (motors(i) == 1) nexus%nbang = nexus%nbang + 1
                end do
                if (nexus%nbang == 1) then
                    if (motors(1) == 1) then
                        nexus%manip = 4
                        nexus%scan_type  = 'phi'
                        nexus%scan_start = nexus%angles(1,1)
                        nexus%scan_width = nexus%angles(1,nexus%nf) - nexus%angles(1,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(2) == 1) then
                        nexus%manip = 3
                        nexus%scan_type = 'chi'
                        nexus%scan_start = nexus%angles(2,1)
                        nexus%scan_width = nexus%angles(2,nexus%nf) - nexus%angles(2,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(3) == 1) then
                        nexus%manip = 2
                        nexus%scan_type = 'omega'
                        nexus%scan_start = nexus%angles(3,1)
                        nexus%scan_width = nexus%angles(3,nexus%nf) - nexus%angles(3,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(4) == 1) then
                        nexus%manip = 1
                        nexus%scan_type = 'gamma'
                        nexus%scan_start = nexus%angles(4,1)
                        nexus%scan_width = nexus%angles(4,nexus%nf) - nexus%angles(4,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(5) == 1) then
                        nexus%scan_type = 'psi'
                        nexus%scan_start = nexus%angles(5,1)
                        nexus%scan_width = nexus%angles(5,nexus%nf) - nexus%angles(5,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(6) == 1) then
                        nexus%scan_type = 'canne'
                        nexus%scan_start = nexus%angles(6,1)
                        nexus%scan_width = nexus%angles(6,nexus%nf) - nexus%angles(6,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(7) == 1) then
                        nexus%scan_type = 'nu'
                        nexus%scan_start = nexus%angles(7,1)
                        nexus%scan_width = nexus%angles(7,nexus%nf) - nexus%angles(7,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    else if (motors(8) == 1) then
                        nexus%manip = 1
                        nexus%scan_type = '2theta'
                        nexus%scan_start = nexus%angles(8,1)
                        nexus%scan_width = nexus%angles(8,nexus%nf) - nexus%angles(8,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                    end if
                else if (nexus%nbang == 2) then
                    if (motors(3) == 1 .and. motors(4) == 1) then
                        nexus%manip = 2
                        nexus%scan_type = 'omega'
                        nexus%scan_start = nexus%angles(3,1)
                        nexus%scan_width = nexus%angles(3,nexus%nf) - nexus%angles(3,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                        nexus%fcoupling = (nexus%angles(3,nexus%nf)-nexus%angles(3,1)) / (nexus%angles(4,nexus%nf)-nexus%angles(4,1))
                    else if (motors(3) == 1 .and. motors(4) == 1) then
                        nexus%manip = 2
                        nexus%scan_type = 'canne'
                        nexus%scan_start = nexus%angles(6,1)
                        nexus%scan_width = nexus%angles(6,nexus%nf) - nexus%angles(6,1)
                        if (nexus%nf > 1) nexus%scan_step = nexus%scan_width / (nexus%nf - 1)
                        nexus%fcoupling = (nexus%angles(6,nexus%nf)-nexus%angles(6,1)) / (nexus%angles(4,nexus%nf)-nexus%angles(4,1))
                    end if
                end if
                if (allocated(datos)) deallocate(datos)
            end if
        else ! Virtual detector?
            call h5dopen_f(file_id,'entry0/instrument/Detector/virtual_cgap',dset,hdferr)
            if (hdferr == -1) call h5dopen_f(file_id,'entry0/instrument/Det1/virtual_cgap',dset,hdferr)
            if (hdferr /= 1) then
                call h5dclose_f(dset,hdferr)
                nexus%is_virtual = .true.
                ! Read gamma
                if (allocated(nexus%angles)) deallocate(nexus%angles)
                allocate(nexus%angles(8,nexus%nf))
                call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
                if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                if (hdferr /= -1) nexus%angles(4,:) = gamma_val
                if (hdferr /= -1) call h5dclose_f(dset,hdferr)
                ! Read nu
                if (hdferr /= -1) then
                    call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                    if (hdferr /= -1) call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                    if (hdferr /= -1) then
                        nexus%angles(7,:) = nu_val
                    else
                        nexus%angles(7,:) = 0.0
                    end if
                end if
            else
                call h5dclose_f(dset,hdferr)
            end if
        end if

        !   Close the datasets
        call h5dclose_f(dset,hdferr)
        call h5dclose_f(dset2,hdferr)
        call h5dclose_f(dset3,hdferr)

    end subroutine read_nexus_ill

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