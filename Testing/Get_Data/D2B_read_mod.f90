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

module D2B_read_mod

    ! Subroutines:
    !
    !   Public:
    !       read_cfl

    use hdf5
    use CFML_GlobalDeps,      only: Clear_Error,Err_CFML,OPS_Sep
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Read_Current_Instrm,Calibration_Detector_Type, &
                                    Read_Calibration_File
    use CFML_Strings,         only: L_Case,Reading_File,File_Type
    use CFML_SXTAL_Geom,      only: Set_PSD
    use nexus_mod

    implicit none

    private

    ! List of public subroutines
    public :: read_calibration_lamp,read_calibration_mantid,read_cfl_D2B

    type, public :: cfl_D2B_type

        integer            :: nscans
        integer            :: nz_int
        integer            :: num1,num2
        integer            :: kc1,kc2
        real               :: scale_fac
        real               :: tth_min
        real               :: tth_max
        real               :: nsigma
        real               :: Norm_Monitor
        real               :: ef_cutoff
        logical            :: is_tth_min = .false.
        logical            :: is_tth_max = .false.
        logical            :: is_label   = .false.
        logical            :: calibration,combine,raw
        logical            :: suma
        logical            :: is_numor     = .false.
        logical            :: is_ef_cutoff = .false.
        logical            :: single
        logical            :: align
        logical            :: Apply_Shifts
        logical            :: verbose =.false.
        logical            :: monitor ! if true, a given Norm_Monitor is used for nomalization
        character(len=6)   :: calib_gen
        character(len=12)  :: instrument_name
        character(len=20)  :: suffix
        character(len=512) :: scan_path,calib_path,calib_file,combine_name
        character(len=512), dimension(:), allocatable :: scans
        character(len=:),                 allocatable :: label_sum
        character(len=:),                 allocatable :: label

    end type cfl_D2B_type

    character(len=3) :: suffix_DEFAULT = 'd2b'
    real, parameter  :: NSIGMA_DEFAULT = 3.0
    logical          :: CALIBRATION_DEFAULT = .false.
    logical, public  :: is_d2b = .false.

    ! Parameters
    integer,          parameter :: NZ_INT_DEFAULT     = 100
    real,             parameter :: SCALE_FAC_DEFAULT  = 1.0
    logical,          parameter :: ALIGN_DEFAULT      = .false.
    logical,          parameter :: SINGLE_DEFAULT     = .false.
    logical,          parameter :: SUM_DEFAULT        = .false.
    character(len=3), parameter :: LABEL_SUM_DEFAULT  = 'sum'
    character(len=3), parameter :: LABEL_DEFAULT      = 'w80'
    ! Error message
    character(len=1024), public :: war_D2B_mess, err_D2B_mess


    contains

    subroutine read_calibration_lamp(filename,Cal,ierr)
        ! Arguments
        character(len=*),                intent(in)  :: filename
        type(calibration_detector_type), intent(out) :: Cal
        integer,                         intent(out) :: ierr

        ! Local variables
        integer :: i
        character(len=10) :: namef
        logical :: exist

        ierr = 0

        ! Check that lamp file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            ierr = 1
            war_D2B_mess = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        namef = L_Case(current_instrm%name_inst)
        i = index(namef,'d2b')
        if (i < 1) then
            ierr = 1
            war_D2B_mess = 'read_calibration: calibration for instrument '//trim(namef)//' not implemented'
            return
        else
            !call read_calibration_lamp_d2b(filename,Cal,ierr)
            call Read_Calibration_File(filename, "D2B", Cal)
            if(Cal%Pos_read) write(*,"(12f10.4)") Cal%PosX

            if(Err_CFML%Flag) then
                write(*,"(a)") trim(Err_CFML%Msg)
                ierr=1
            end if
            war_D2B_mess = Err_CFML%Msg
        end if

    end subroutine read_calibration_lamp

    subroutine read_calibration_lamp_d2b(filename,Cal,ierr)

        ! Arguments
        character(len=*),               intent(in)  :: filename
        type(Calibration_Detector_Type),intent(out) :: Cal
        integer,                        intent(out) :: ierr

        ! Local variables
        integer :: i,j,m,n,i1,i2,i1_,i2_,i_tubo, i_cal,ier
        integer, dimension(2,128) :: active_pixels
        character(len=256)        :: line
        real, dimension(128,128)  :: calibration
        ierr = 0

        Cal%Name_Instrm="D2B-Lamp"
        Cal%NDet=128                ! Number of detectors
        Cal%NPointsDet=128          ! Number of Points per Detector

        allocate( Cal%PosX(Cal%NDet), Cal%Effic(Cal%NPointsDet,Cal%NDet), Cal%Active(Cal%NPointsDet,Cal%NDet) ) ! Relative angular positions of detectors, efficiencies, mask
        Cal%PosX(:)=0.0
        open(newunit=i_cal,file=filename,status='old',action='read')
        read(unit=i_cal,fmt=*)
        read(unit=i_cal,fmt=*)
        ! read active pixels
        do i = 0 , 63
            i_tubo = 2*i + 1
            read(unit=i_cal,fmt='(a)') line
            j = index(line,'*')
            if (j > 0) then
                read(unit=line(:j-1),fmt=*) active_pixels(1:2,i_tubo),active_pixels(1:2,i_tubo+1)
            else
                read(unit=line(:),fmt=*) active_pixels(1:2,i_tubo),active_pixels(1:2,i_tubo+1)
            end if
        end do
        active_pixels(:,:) = active_pixels(:,:) + 1
        ! read efficiencies
        read(i_cal,*)
        n = 0
        do i = 0,127
            n = i + 1
            if (mod(i,2) == 0) then
                m = 1
                do j = 1 , 21
                    read(unit=i_cal,fmt=*) calibration(m:m+5,n)
                    m = m + 6
                end do
                read(unit=i_cal,fmt=*) calibration(127:128,n)
            else
                m = 128
                do j = 1 , 21
                    read(unit=i_cal,fmt=*) calibration(m:m-5:-1,n)
                    m = m - 6
                end do
                read(unit=i_cal,fmt=*) calibration(2:1:-1,n)
            end if
        end do
        read(i_cal,*)
        read(i_cal,*,iostat=ier) Cal%PosX
        if(ier == 0) then
           Cal%Pos_read=.true.
        else
           Cal%Pos_read=.false.
        end if
        ! replace efficiencies of non-active pixels by a negative number
        do i = 0 , 127
            n = i + 1
            i1_ = active_pixels(1,n)
            i2_ = active_pixels(2,n)
            if (mod(i,2) == 0) then
                i1 = i1_
                i2 = i2_
            else
                i2 = 256 - i1_
                i1 = 256 - i2_
            end if
            if (i1 > 1) calibration(:i1-1,n) = -1
            if (i2 < 128) calibration(i2+1:,n) = -1
        end do

        where(calibration <= 0.0)
            Cal%Active=.false.
        elsewhere
            Cal%effic=1.0/calibration  !convert efficiencies to alphas
        end where
    end subroutine read_calibration_lamp_d2b

    subroutine read_calibration_mantid(filename,path,Cal,ierr)

        ! Arguments
        character(len=*),               intent(in)  :: filename
        character(len=*),               intent(in)  :: path
        type(Calibration_Detector_Type),intent(out) :: Cal
        integer,                        intent(out) :: ierr

        ! Local variables
        integer :: i,hdferr,nx,nz
        integer(HID_T) :: file_id,dset,space
        integer(HSIZE_T), dimension(3) :: dims,dims_
        logical :: exist
        real, dimension(:,:), allocatable :: calibration
        ierr = 0

        ! Check that nexus file exists
        inquire(file=filename,exist=exist)
        if (.not. exist) then
            ierr = 1
            war_D2B_mess = 'read_calibration: file '//trim(filename)//' not found'
            return
        end if

        ! Initialize fortran interface
        call h5open_f(hdferr)
        if (hdferr == -1) then
            ierr = 1
            war_D2B_mess = "read_calibration: error opening hdf5 fortran interface"
            return
        end if

        ! Prevent error messages
        if (hdferr /= -1) call h5eset_auto_f(0,hdferr)

        ! Open NEXUS file
        if (hdferr /= -1) then
            call h5fopen_f(trim(filename),H5F_ACC_RdoNLY_F,file_id,hdferr)
            if (hdferr == -1) then
                ierr = 1
                err_D2B_mess = "read_calibration: error opening nexus file"
                return
            end if
        end if

        ! Get calibration
        call h5dopen_f(file_id,path,dset,hdferr)
        if (hdferr == -1) then
            ierr = 1
            err_D2B_mess = 'read_calibration: wrong path.'
            return
        end if

        !   Get dimensions of the dataset
        call h5dget_space_f(dset,space,hdferr)
        call h5sget_simple_extent_dims_f(space,dims,dims_,hdferr)
        !   Assign memory to arrays and read data
        if (hdferr /= -1) then
            i = index(current_instrm%name_inst,'d2b')
            if (i > 0) then
                ! for d2b, the calibration matrix produced by mantid is
                ! transposed with respect to the counts in nexus files
                nx = dims(1)
                nz = dims(2)
                allocate(calibration(nx,nz))
                call h5dread_f(dset,H5T_NATIVE_REAL,calibration,dims,hdferr)
                if (hdferr /= -1) calibration = transpose(calibration)
                call h5dclose_f(dset,hdferr)
            else
                ierr = 1
                err_D2B_mess = 'read_calibration: calibration cannot be applied, only implemented for d2b.'
                return
            end if
        end if
        if (hdferr == -1) then
            ierr = 1
            err_D2B_mess = 'read_calibration: error reading calibration data.'
            return
        end if

        ! Close NEXUS file.
        call h5fclose_f(file_id,hdferr)

        ! Close FORTRAN interface.
        call h5close_f(hdferr)

        !Construction of Cal object
        Cal%Name_Instrm="D2B-Mantid"
        Cal%NDet=128                ! Number of detectors
        Cal%NPointsDet=128          ! Number of Points per Detector

        allocate( Cal%PosX(Cal%NDet), Cal%Effic(Cal%NPointsDet,Cal%NDet), Cal%Active(Cal%NPointsDet,Cal%NDet) ) ! Relative angular positions of detectors, efficiencies, mask
        Cal%PosX(:)=[(-158.750+(i-1)*1.25,i=1,128)]
        Cal%Pos_read=.true.; Cal%effic=1.0; Cal%Active=.true.
        where(calibration <= 0.0)
            Cal%Active=.false.
        elsewhere
           where(Cal%effic > 0.0) Cal%effic=1.0/calibration
        end where

    end subroutine read_calibration_mantid

    subroutine read_cfl_D2B(cfl_file,cfl,ierr)

        ! Read and process the cfl file

        ! Arguments
        character(len=*),    intent(in)  :: cfl_file
        type(cfl_D2B_type),  intent(out) :: cfl
        integer,             intent(out) :: ierr

        ! Local variables
        integer                       :: i,j,k,n,ierror,num1,num2
        character(len=100)            :: keyword,gen_calib
        character(len=1024)           :: path_calib,combine_name,code_path
        character(len=:), allocatable :: line,file_inst,file_calib,namef
        logical                       :: is_file,is_calib_file,is_calib_path
        type(File_Type)               :: cfl_file_type

        ierr = 0
        is_calib_file = .false.
        is_calib_path = .false.
        call Clear_Error()

        ! Set defaults
        cfl%calibration = CALIBRATION_DEFAULT
        cfl%combine = .false.
        cfl%monitor = .false.
        cfl%align   = .false.
        cfl%raw     = .false.
        cfl%verbose = .false.
        cfl%apply_shifts  = .false.
        cfl%suffix  = suffix_DEFAULT
        cfl%nsigma  = NSIGMA_DEFAULT
        cfl%nscans  = 0
        cfl%num1  = 0
        cfl%num2  = 0
        cfl%kc1  = 56   ! 56-72 w=17,  54-74 w=21,   51-77 w=27,  59-69  w=11
        cfl%kc2  = 72
        cfl%scan_path=" "

        ! Put the content in cfl_file_type
        cfl_file_type = Reading_File(cfl_file)

        ! Read content
        do i = 1 , cfl_file_type%nlines
            line = adjustl(cfl_file_type%line(i)%str)
            if (len_trim(line) == 0) cycle
            j = index(line,' ')
            if (j > 0) then
                keyword = L_Case(line(1:j-1))
            else
                keyword = L_Case(line)
            end if

            select case (keyword)

                case('instrm')
                    read(line(j+1:),*,iostat=ierror) cfl%instrument_name
                    if (ierror == 0) then
                        cfl%instrument_name = "d2b"
                    end if

                case('nz1-nz2')
                    read(line(j+1:),*,iostat=ierror) cfl%kc1,cfl%kc2
                    if (ierror /= 0) then
                       cfl%kc1 = 56; cfl%kc2=72
                    end if

                case('monitor')
                    read(line(j+1:),*,iostat=ierror) cfl%Norm_monitor
                    if (ierror == 0) then
                        cfl%monitor = .true.
                    end if

                case('ef_cutoff')
                    read(line(j+1:),*,iostat=ierror) cfl%ef_cutoff
                    if (ierror == 0) cfl%is_ef_cutoff = .true.

                case('scan_path')
                    if (j > 0) read(unit=line(j:),fmt='(a)',iostat=ierror) code_path
                    if(ierror /= 0 .or. j == 0) then
                        ierr = 1
                        err_D2B_mess = 'read_cfl: error reading the scan_path name'
                        return
                    else
                       cfl%scan_path=trim(adjustl(code_path))
                       k=len_trim(cfl%scan_path)
                       if(cfl%scan_path(k:k) /= OPS_SEP) cfl%scan_path(k+1:k+1)=OPS_SEP
                    end if

                case('numors')
                    if(len_trim(cfl%scan_path) == 0) then
                        ierr = 1
                        err_D2B_mess = 'read_cfl: error, scan_path should be provided before reading numors'
                        return
                    end if
                    read(line(j+1:),*,iostat=ierror) num1,num2
                    if (ierror == 0) then
                        n = num2-num1+1
                        cfl%nscans = num2-num1+1
                        allocate(cfl%scans(n))
                        cfl%num1=num1
                        cfl%num2=num2
                        cfl%is_numor=.true.
                        n=0
                        do k = num1,num2
                            n=n+1
                            write(cfl%scans(n),"(a,i6.6,a)") trim(cfl%scan_path),k,".nxs"
                        end do
                    else
                        ierr = 1
                        err_D2B_mess = 'read_cfl: error reading numors'
                        return
                    end if

                case('combine')
                    cfl%combine = .true.
                    ierror = 0
                    if (j > 0) read(unit=line(j:),fmt='(a)',iostat=ierror) combine_name
                    if (ierror /= 0 .or. j == 0) then
                        cfl%combine_name = 'gsc'
                    else
                        cfl%combine_name = adjustl(combine_name)
                    end if


                case('instrument_file')
                    file_inst = adjustl(trim(line(j+1:)))
                    inquire(file = file_inst, exist = is_file)
                    if (.not. is_file) then
                        ierr = 1
                        err_D2B_mess = 'read_cfl: instrument file '//file_inst//' not found'
                        return
                    end if
                    call Read_Current_Instrm(trim(file_inst))
                    if (Err_CFML%Flag) then
                        ierr = 1
                        err_D2B_mess = Err_CFML%Msg
                        return
                    end if
                    namef = L_Case(current_instrm%name_inst)
                    k = index(namef,'d2b')
                    if (k > -1) is_d2b = .true.
                    call Set_PSD()

                case('calibration_file')
                    file_calib = adjustl(trim(line(j+1:)))
                    inquire(file = file_calib, exist = is_file)
                    if (.not. is_file) then
                        ierr = 1
                        err_D2B_mess = 'read_cfl: calibration file '//file_calib//' not found'
                        return
                    end if
                    cfl%calib_file = file_calib
                    is_calib_file = .true.

                case('calibration_gen')
                    read(unit=line(j+1:),fmt='(a)',iostat=ierror) gen_calib
                    if (ierror == 0) cfl%calib_gen = adjustl(trim(gen_calib))

                case('calibration_path')
                    read(unit=line(j+1:),fmt='(a)',iostat=ierror) path_calib
                    cfl%calib_path = adjustl(path_calib)
                    if (ierror == 0) is_calib_path = .true.

                case('suffix')
                    read(line(j+1:),*,iostat=ierror) cfl%suffix
                    if (ierror /= 0) cfl%suffix = suffix_DEFAULT

                case('nsigma')
                    read(line(j+1:),*,iostat=ierror) cfl%nsigma
                    if (ierror /= 0) cfl%nsigma = NSIGMA_DEFAULT

                case ('raw')
                    cfl%raw = .true.

                case ('apply_shifts')
                    cfl%apply_shifts = .true.

                case ('verbose')
                    cfl%verbose = .true.

                case('scans')
                    read(line(j+1:),*,iostat=ierror) n
                    if (ierror == 0) then
                        allocate(cfl%scans(n))
                        do k = 1 , n
                            line = adjustl(cfl_file_type%line(i+k)%str)
                            read(line,"(a)",iostat=ierror) cfl%scans(k)
                            if (ierror /= 0) exit
                        end do
                    end if
                    if (ierror /= 0) then
                        ierr = 1
                        err_D2B_mess = 'read_cfl: error reading scan list'
                        return
                    else
                        cfl%nscans = n
                    end if

                case('align')
                    cfl%align = .true.

                case('nz_int')
                    read(line(j+1:),*,iostat=ierror) cfl%nz_int
                    if (ierror /= 0) cfl%nz_int = NZ_INT_DEFAULT

                case('scale_fac')
                    read(line(j+1:),*,iostat=ierror) cfl%scale_fac
                    if (ierror /= 0) cfl%scale_fac = SCALE_FAC_DEFAULT

                case('single')
                    cfl%single = .true.

                case('sum')
                    if (len(trim(line(j+1:))) > 0) cfl%label_sum = adjustl(trim(line(j+1:)))
                    cfl%suma = .true.

                case('label')
                    if (len(trim(line(j+1:))) > 0) cfl%label = adjustl(trim(line(j+1:)))
                    cfl%is_label = .true.

                case('tth_min')
                    read(line(j+1:),*,iostat=ierror) cfl%tth_min
                    if (ierror == 0) cfl%is_tth_min = .true.

                case('tth_max')
                    read(line(j+1:),*,iostat=ierror) cfl%tth_max
                    if (ierror == 0) cfl%is_tth_max = .true.

            end select
        end do

        if (trim(cfl%calib_gen) == 'mantid') then
            if (is_calib_file .and. .not. is_calib_path) then
                write(*,'(4x,a,1x,a)') ' => Warning: Calibration path not given, calibration cannot be applied'
            else if (.not. is_calib_file .and. is_calib_path) then
                write(*,'(4x,a,1x,a)') ' => Warning: Calibration file not given, calibration cannot be applied'
            else if (is_calib_file .and. is_calib_path) then
                cfl%calibration = .true.
            end if
        else if (trim(cfl%calib_gen) == 'lamp' .and. is_calib_file) then
            cfl%calibration = .true.
        end if

    end subroutine read_cfl_D2B

end module D2B_read_mod