module Nexus_Mod

    ! Module for reading nexus data
    !
    !   Variables:
    !
    !       Public:
    !           err_nexus
    !           err_nexus_mess
    !
    !   Subroutines:
    !
    !       Public:
    !           initialize_nexus
    !           read_nexus
    !
    !   Comment: Normal beam geometry not implemented yet. For the time being, it is
    !   mandatory to specify omega,chi,phi,gamma in nexus. Revise this when normal
    !   beam is implemented.
    use hdf5
    use CFML_GlobalDeps, only: ops_sep
    use CFML_String_Utilities, only: l_case,Get_Basename

    implicit none

    private

    type, public :: nexus_type
        integer                                :: nanodes
        integer                                :: ncathodes
        integer                                :: nframes
        integer                                :: scan_angle
        integer, dimension(:,:,:), allocatable :: counts
        real                                   :: wavelength
        real                                   :: gamma_step
        real                                   :: temperature
        real, dimension(3)                     :: scan_info
        real, dimension(3)                     :: actual_reflection
        real, dimension(:), allocatable        :: monitor
        real, dimension(:,:), allocatable      :: angles ! phi,chi,omega,gamma,psi,nu
        logical                                :: is_name
        logical                                :: is_scantype
        logical                                :: is_gamma
        logical                                :: is_omega
        logical                                :: is_chi
        logical                                :: is_phi
        logical                                :: is_mode
        logical                                :: is_monitor
        logical                                :: is_actual_reflection
        logical                                :: gamma_coupling
        character(len=2)                       :: geometry
        character(len=10)                      :: instrument_name
        character(len=20)                      :: scan_type
        character(len=512)                     :: filename
        character(len=512)                     :: filcod
    end type nexus_type

    ! Parameters
    real, parameter :: MIN_DANGLE = 0.01

    ! List of public variables
    logical, public :: err_nexus, wrn_nexus
    character(len=256), public :: err_nexus_mess, wrn_nexus_mess

    ! List of public subroutines
    public :: initialize_nexus,read_nexus

    contains

    subroutine initialize_nexus(nexus)

        !---- Arguments ----!
        type(nexus_type), intent(inout)  :: nexus

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
        nexus%wavelength      = 0.0
        nexus%is_name         = .true.
        nexus%is_scantype     = .true.
        nexus%is_gamma        = .true.
        nexus%is_omega        = .true.
        nexus%is_chi          = .true.
        nexus%is_phi          = .true.
        nexus%is_mode         = .true.
        nexus%is_monitor      = .false.
        nexus%is_actual_reflection = .false.
        nexus%gamma_coupling  = .false.
        if (allocated(nexus%counts)) deallocate(nexus%counts)
        if (allocated(nexus%angles)) deallocate(nexus%angles)

    end subroutine initialize_nexus

    subroutine read_nexus(nexus)

        ! Arguments
        type(nexus_type), intent(inout)  :: nexus

        ! Local variables with hdf5 types
        integer(SIZE_T), PARAMETER :: str_max_len = 20 ! maximum string length
        integer(SIZE_T), dimension(:), allocatable :: str_len
        integer(HID_T) :: file_id,dset,dset2,dset3,space,space2,space3,filetype,filetype2
        integer(HSIZE_T), dimension(1) :: scalar
        integer(HSIZE_T), dimension(2) :: maxdims,maxdims3,data_dims
        integer(HSIZE_T), dimension(3) :: dims,dims3

        ! Local variables
        integer :: hdferr,i,j,k,ii,jj,kk,n,m,ni,nk,d12,nvar,mode,i_moni,source
        integer, dimension(7,3) :: motors ! phi, chi, omega, gamma, psi, canne, nu
        integer, dimension(:), allocatable :: scanned
        integer, dimension(:,:,:), allocatable :: counts
        real :: gamma_val,nu_val,omega_val,chi_val,phi_val,temperature,dgamma
        real, dimension(:,:), allocatable :: data
        logical :: kine
        character(len=2) :: geo
        character(len=512) :: basename,path
        character(len=str_max_len) :: name,key,scantype
        character(len=str_max_len), dimension(:), allocatable :: var_names

        ! Initialize variables
        err_nexus  = .false.
        kine = .false.
        motors(:,:) = 0
        source = -1

        ! Extract the numor code and create the numor files
        call  Get_Basename(nexus%filename,ops_sep,basename)
        i = index(basename,'.',back=.true.)
        if (i == 0) then
            nexus%filcod = basename
        else
            nexus%filcod = basename(1:i-1)
            if (basename(1:1) == 'z' .or. basename(1:1) == 'Z') then ! zebra files
                i = len_trim(nexus%filcod)
                nexus%filcod = nexus%filcod(i-5:i)
            end if
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr /= 0) then
            err_nexus = .true.
            err_nexus_mess = "Error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        call h5fopen_f(trim(nexus%filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
        if (hdferr /= 0) then
            err_nexus = .true.
            err_nexus_mess = "Error opening nexus file"
            return
        end if

        ! Determine the source by getting instrument name: ILL (0), PSI (1)
        call h5dopen_f(file_id,'entry0/instrument/name',dset,hdferr)
        if (hdferr == 0) source = 0
        if (hdferr /= 0) then
            call h5dopen_f(file_id,'entry1/ZEBRA/SINQ/name',dset,hdferr)
            if (hdferr == 0) source = 1
        end if
        if (source == -1) then
            err_nexus = .true.
            err_nexus_mess = "Unknown source"
            return
        end if

        !   Get the data type and its size
        call h5dget_type_f(dset,filetype,hdferr)

        !   Read instrument name
        if (source == 0) then
            call h5dread_f(dset,filetype,name,dims,hdferr)
            nexus%instrument_name = l_case(trim(name))
        else if (source == 1) then
            nexus%instrument_name = 'zebra'
        end if

        !   Close the dataset
        call h5dclose_f(dset,hdferr)

        !* Get wavelength
        if (source == 0) then
            path = 'entry0/wavelength'
        else if (source == 1) then
            path = 'entry1/ZEBRA/monochromator/wavelength'
        end if
        call h5dopen_f(file_id,path,dset,hdferr)
            if (hdferr /= 0) then
                wrn_nexus = .true.
                wrn_nexus_mess = "wavelength not found in nexus"
                !return
            end if
        call h5dread_f(dset,H5T_NATIVE_REAL,nexus%wavelength,scalar,hdferr)
        call h5dclose_f(dset,hdferr)

        ! Get actual reflection
        if (source == 0) then
            call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/actual_reflection',dset,hdferr)
            if (hdferr == 0) then
                call h5dget_space_f(dset,space,hdferr)
                call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
                call h5dread_f(dset,H5T_NATIVE_REAL,nexus%actual_reflection,dims,hdferr)
                call h5dclose_f(dset,hdferr)
                nexus%is_actual_reflection = .true.
            end if
        end if

        ! * Get counts
        if (source == 0) then
            call h5dopen_f(file_id,'entry0/data_scan/detector_data/data',dset,hdferr)
            if (hdferr /= 0) then
                call h5dopen_f(file_id,'entry0/data_kine/detector_data/data',dset,hdferr)
                if (hdferr == 0) then
                    kine = .true.
                else
                    err_nexus = .true.
                    err_nexus_mess = "data not found in nexus"
                    return
                end if
            end if
        else if (source == 1) then
            call h5dopen_f(file_id,'entry1/area_detector2/data',dset,hdferr)
            if (hdferr /= 0) then
                err_nexus = .true.
                err_nexus_mess = "data not found in nexus"
                return
            end if
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)

        !   Assign memory to arrays
        nexus%nanodes = dims(1)
        nexus%ncathodes = dims(2)
        nexus%nframes = dims(3)
        if (allocated(nexus%counts)) deallocate(nexus%counts)
        allocate(nexus%counts(nexus%nanodes,nexus%ncathodes,nexus%nframes))

        !   Read counts
        if (source == 0) then
            call h5dread_f(dset,H5T_NATIVE_INTEGER,nexus%counts,dims,hdferr)
        else if (source == 1) then
            allocate(counts(dims(1),dims(2),dims(3)))
            call h5dread_f(dset,H5T_NATIVE_INTEGER,counts,dims,hdferr)
            n = 0
            d12 = dims(1) * dims(2)
            do kk = 1 , dims(3)
                do jj = 1 , dims(2)
                    do ii = 1 , dims(1)
                        nk = n / d12
                        k = nk + 1
                        m = n - nk * d12
                        ni = m / dims(2)
                        i = ni + 1
                        m = n - nk * d12 - ni * dims(2)
                        j = mod(m,dims(2)) + 1
                        nexus%counts(i,j,k) = counts(ii,jj,kk)
                        n = n + 1
                    end do
                end do
            end do
            deallocate(counts)
        end if

        !   Close the datasets
        call h5dclose_f(dset,hdferr)

        ! * Get angles
        if (source == 0) then
            if (.not. kine) then
                call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/name',dset,hdferr)
            else
                call h5dopen_f(file_id,'entry0/data_kine/scanned_variables/variables_names/name',dset,hdferr)
            end if
            if (hdferr /= 0) nexus%is_scantype = .false.

            if (.not. kine) then
                call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/variables_names/scanned',dset2,hdferr)
            else
                call h5dopen_f(file_id,'entry0/data_kine/scanned_variables/variables_names/scanned',dset2,hdferr)
            end if
            if (hdferr /= 0) nexus%is_scantype = .false.

            if (.not. kine) then
                call h5dopen_f(file_id,'entry0/data_scan/scanned_variables/data',dset3,hdferr)
            else
                call h5dopen_f(file_id,'entry0/data_kine/scanned_variables/data',dset3,hdferr)
            end if
            if (hdferr /= 0) nexus%is_scantype = .false.

            if (nexus%is_scantype) then
                !   Get the data type
                call h5dget_type_f(dset,filetype,hdferr)

                !   Get dimensions of the dataset
                call h5dget_space_f(dset,space,hdferr)
                call h5dget_space_f(dset2,space2,hdferr)
                call h5dget_space_f(dset3,space3,hdferr)
                call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
                call h5sget_simple_extent_dims_f(space3,dims3,maxdims3,hdferr)
                nvar = dims(1)

                !   Assign memory to arrays and initalize them
                allocate(var_names(nvar))
                allocate(str_len(nvar))
                allocate(scanned(nvar))
                allocate(data(nexus%nframes,nvar))
                str_len(:) = str_max_len
                scanned(:) = 0
                data_dims = (/ str_max_len , dims(1) /)

                !   Read data
                call h5dread_vl_f(dset,filetype,var_names,data_dims,str_len,hdferr,space)
                call h5dread_f(dset2,H5T_NATIVE_INTEGER,scanned,dims,hdferr)
                call h5dread_f(dset3,H5T_NATIVE_REAL,data,dims3,hdferr)

                !   Motors rows: phi,chi,omega,gamma,psi,canne
                !   Motors columns: firts column,  1 -> motor read, 0 -> motor not found in nexus
                !                   second column, 1 -> motor moves during the scan, 0 -> motor does not move
                !   There is a specific case, where gamma moves with omega, where scanned is cero for gamma
                !   in the nexus file. This is not a problem because the values of gamma for every frame are
                !   accesible.
                do i = 1 , dims(1)
                    key = l_case(var_names(i))
                    select case(key)
                        case('phi')
                            motors(1,1) = 1
                            motors(1,3) = i
                            if (scanned(i) == 1) motors(1,2) = 1
                        case('chi')
                            motors(2,1) = 1
                            motors(2,3) = i
                            if (scanned(i) == 1) motors(2,2) = 1
                        case('omega','flyomega')
                            motors(3,1) = 1
                            motors(3,3) = i
                            if (scanned(i) == 1) motors(3,2) = 1
                        case('gamma')
                            motors(4,1) = 1
                            motors(4,3) = i
                            if (scanned(i) == 1) motors(4,2) = 1
                        case('psi')
                            motors(5,1) = 1
                            motors(5,3) = i
                            if (scanned(i) == 1) motors(5,2) = 1
                        case('canne')
                            motors(6,3) = i
                            motors(6,1) = 1
                            if (scanned(i) == 1) motors(6,2) = 1
                        case('nu')
                            motors(7,3) = i
                            motors(7,1) = 1
                            if (scanned(i) == 1) motors(7,2) = 1
                        case('monitor1')
                            nexus%is_monitor = .true.
                            i_moni = i
                    end select
                end do
                ! Store monitor counts
                if (nexus%is_monitor) then
                    if (allocated(nexus%monitor)) deallocate(nexus%monitor)
                    allocate(nexus%monitor(nexus%nframes))
                    nexus%monitor(:) = data(:,i_moni)
                end if

                !   Close the datasets
                call h5dclose_f(dset,hdferr)
                call h5dclose_f(dset2,hdferr)
                call h5dclose_f(dset3,hdferr)
            end if

            !   Check that gamma, nu, omega, chi and phi have been read. If not,
            !   read them from entry0/instrument.
            !   Gamma
            if (motors(4,1) == 0) then
                call h5dopen_f(file_id,'entry0/instrument/gamma/value',dset,hdferr)
                if (hdferr /= 0) then
                    nexus%is_gamma = .false.
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,gamma_val,scalar,hdferr)
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            !   Nu
            if (motors(7,1) == 0) then
                call h5dopen_f(file_id,'entry0/instrument/nu/value',dset,hdferr)
                if (hdferr /= 0) then
                    ! If nu is not specified, assume nu = 0. This is the case
                    ! for example for nexus from D19
                    nu_val = 0.0
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,nu_val,scalar,hdferr)
                end if
                call h5dclose_f(dset,hdferr)
            end if
            !   Omega
            if (motors(3,1) == 0 .and. motors(6,1) == 0) then
                call h5dopen_f(file_id,'entry0/instrument/omega/value',dset,hdferr)
                if (hdferr /= 0) then
                    nexus%is_omega = .false.
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,omega_val,scalar,hdferr)
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            !   Chi
            if (motors(2,1) == 0) then
                call h5dopen_f(file_id,'entry0/instrument/chi/value',dset,hdferr)
                if (hdferr /= 0) then
                    nexus%is_chi = .false.
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,chi_val,scalar,hdferr)
                    call h5dclose_f(dset,hdferr)
                end if
            end if
            !   Phi
            if (motors(1,1) == 0) then
                call h5dopen_f(file_id,'entry0/instrument/phi/value',dset,hdferr)
                if (hdferr /= 0) then
                    nexus%is_phi = .false.
                else
                    call h5dread_f(dset,H5T_NATIVE_REAL,phi_val,scalar,hdferr)
                    call h5dclose_f(dset,hdferr)
                end if
            end if

            !   Store angles
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(nexus%nframes,7))
            if (motors(1,1) == 1) then ! Phi
                nexus%angles(:,1) = data(:,motors(1,3))
                if (abs(nexus%angles(nexus%nframes,1)-nexus%angles(1,1)) < MIN_DANGLE) motors(1,2) = 0
            else
                nexus%angles(:,1) = phi_val
            end if
            if (motors(2,1) == 1) then ! Chi
                nexus%angles(:,2) = data(:,motors(2,3))
                if (abs(nexus%angles(nexus%nframes,2)-nexus%angles(1,2)) < MIN_DANGLE) motors(2,2) = 0
            else
                nexus%angles(:,2) = chi_val
            end if
            if (motors(3,1) == 1) then ! Omega
                if (motors(6,1) == 0) then
                    nexus%angles(:,3) = data(:,motors(3,3))
                    if (abs(nexus%angles(nexus%nframes,3)-nexus%angles(1,3)) < MIN_DANGLE) motors(3,2) = 0
                end if
            end if
            nexus%gamma_coupling = .false.
            if (motors(4,1) == 1) then ! Gamma
                nexus%angles(:,4) = data(:,motors(4,3))
                dgamma = nexus%angles(nexus%nframes,4) - nexus%angles(1,4)
                if (abs(dgamma) > 0.001) then
                    nexus%gamma_coupling = .true.
                    nexus%gamma_step = dgamma / (nexus%nframes-1)
                end if
            else
                nexus%angles(:,4) = gamma_val
            end if
            if (motors(5,1) == 1) then ! Psi
                nexus%angles(:,5) = data(:,motors(5,3))
                if (abs(nexus%angles(nexus%nframes,5)-nexus%angles(1,5)) < MIN_DANGLE) motors(5,2) = 0
            end if
            if (motors(6,1) == 1) then ! Canne == omega
                nexus%angles(:,3) = data(:,motors(6,3))
                if (abs(nexus%angles(nexus%nframes,3)-nexus%angles(1,3)) < MIN_DANGLE) motors(6,2) = 0
            else if (motors(3,1) == 0) then
                nexus%angles(:,3) = omega_val
            end if
            if (motors(7,1) == 1) then ! Nu
                nexus%angles(:,7) = data(:,motors(7,3))
                if (abs(nexus%angles(nexus%nframes,7)-nexus%angles(1,7)) < MIN_DANGLE) motors(7,2) = 0
            else
                nexus%angles(:,7) = nu_val
            end if
        else if (source == 1) then
            if (allocated(nexus%angles)) deallocate(nexus%angles)
            allocate(nexus%angles(nexus%nframes,7))
            ! Gamma
            call h5dopen_f(file_id,'entry1/ZEBRA/area_detector2/polar_angle',dset,hdferr)
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(:,4),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            motors(4,1) = 1
            if (abs(nexus%angles(nexus%nframes,4)-nexus%angles(1,4)) > MIN_DANGLE) motors(4,2) = 1
            !   Nu
            call h5dopen_f(file_id,'entry1/ZEBRA/area_detector2/tilt_angle',dset,hdferr)
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(1,7),scalar,hdferr)
            nexus%angles(:,7) = nexus%angles(1,7)
            call h5dclose_f(dset,hdferr)
            motors(7,1) = 1
            if (abs(nexus%angles(nexus%nframes,7)-nexus%angles(1,7)) > MIN_DANGLE) motors(7,2) = 1
            !   Omega
            call h5dopen_f(file_id,'entry1/sample/rotation_angle',dset,hdferr)
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(:,3),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            motors(3,1) = 1
            if (abs(nexus%angles(nexus%nframes,3)-nexus%angles(1,3)) > MIN_DANGLE) motors(3,2) = 1
            !   Chi
            call h5dopen_f(file_id,'entry1/sample/chi',dset,hdferr)
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(:,2),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            motors(2,1) = 1
            if (abs(nexus%angles(nexus%nframes,2)-nexus%angles(1,2)) > MIN_DANGLE) motors(2,2) = 1
            !   Phi
            call h5dopen_f(file_id,'entry1/sample/phi',dset,hdferr)
            call h5dget_space_f(dset,space,hdferr)
            call h5sget_simple_extent_dims_f(space,dims,maxdims,hdferr)
            call h5dread_f(dset,H5T_NATIVE_REAL,nexus%angles(:,1),scalar,hdferr)
            call h5dclose_f(dset,hdferr)
            motors(1,1) = 1
            if (abs(nexus%angles(nexus%nframes,1)-nexus%angles(1,1)) > MIN_DANGLE) motors(1,2) = 1
        end if

        !   Deduce the scan type
        if      (motors(1,2) == 1 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
            scantype = 'phi'
        else if (motors(1,2) == 0 .and. motors(2,2) == 1 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
            scantype = 'chi'
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 0) then
            scantype = 'omega'
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
            scantype = 'omega'
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
            scantype = 'canne'
            ! Set chi and phi angles to zero
            nexus%angles(:,1) = 0.0 ! phi
            nexus%angles(:,2) = 0.0 ! chi
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0 .and. motors(6,2) == 1) then
            scantype = 'canne'
            ! Set chi and phi angles to zero
            nexus%angles(:,1) = 0.0 ! phi
            nexus%angles(:,2) = 0.0 ! chi
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 1 .and. motors(5,2) == 0) then
            scantype = 'gamma'
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 0 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
            scantype = 'psi'
        else if (motors(1,2) == 0 .and. motors(2,2) == 0 .and. motors(3,2) == 1 .and. motors(4,2) == 0 .and. motors(5,2) == 1) then
            scantype = 'renninger'
        else
            scantype = 'q-scan'
        end if
        nexus%scan_type = scantype

        if (allocated(data)) deallocate(data)

        ! * Get mode
        if (source == 0) then
            call h5dopen_f(file_id,'entry0/instrument/SingleCrystalSettings/mode',dset,hdferr)
            if (hdferr /= 0) then
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
        else if (source == 1) then
            call h5dopen_f(file_id,'entry1/zebra_mode',dset,hdferr)
            call h5dget_type_f(dset,filetype,hdferr)
            call h5dread_f(dset,filetype,name,dims,hdferr)
            write(geo,'(a2)') trim(name)
            if (geo == 'nb') then
                nexus%geometry = 'NB' ! Normal Beam
            else if (geo == 'bi') then
                nexus%geometry = '4C' ! Four Circle
            else
                nexus%geometry = '??'
            end if
            call h5dclose_f(dset,hdferr)
        end if
        ! * Get temperature
        if (source == 0) then
            call h5dopen_f(file_id,'entry0/sample/temperature',dset,hdferr)
            if (hdferr == 0) then
                call h5dread_f(dset,H5T_NATIVE_REAL,temperature,scalar,hdferr)
                nexus%temperature = temperature
                call h5dclose_f(dset,hdferr)
            end if
        end if

        ! Close NEXUS file.
        call h5fclose_f(file_id,hdferr)

        ! Close FORTRAN interface.
        call h5close_f(hdferr)

        ! Set scan parameters
        if (nexus%is_scantype) then
            if (nexus%scan_type == 'phi') then
                nexus%scan_angle = 1
            else if (nexus%scan_type == 'chi') then
                nexus%scan_angle = 2
            else if (nexus%scan_type == 'omega' .or. nexus%scan_type == 'canne') then
                nexus%scan_angle = 3
            else if (nexus%scan_type == 'psi') then
                nexus%scan_angle = 5
            end if
            if (nexus%scan_angle > 0) then
                nexus%scan_info(1) = min(nexus%angles(1,nexus%scan_angle),nexus%angles(nexus%nframes,nexus%scan_angle))
                nexus%scan_info(2) = max(nexus%angles(1,nexus%scan_angle),nexus%angles(nexus%nframes,nexus%scan_angle))
                if (nexus%nframes > 1) then
                    nexus%scan_info(3) = (nexus%angles(nexus%nframes,nexus%scan_angle) - nexus%angles(1,nexus%scan_angle)) / (nexus%nframes- 1)
                else
                    nexus%scan_info(3) = 0.0
                end if
            end if
        end if

    end subroutine read_nexus

end module Nexus_Mod