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

program D2B_int

    use CFML_GlobalDeps,      only: to_deg
    use CFML_ILL_Instrm_Data, only: Current_Instrm
    use CFML_Strings,         only: L_Case
    use CFML_DiffPatt,        only: DiffPat_E_Type,write_pattern
    use D2B_data_mod
    use D2B_read_mod
    use nexus_mod

    implicit none

    ! Local parameters
    real,    parameter :: EPSIL = 0.0001
    integer, parameter :: D2B_NPIXELS_HORIZ = 128
    real,    parameter :: D2B_WIDTH_HORIZ = 1.25 ! degrees

    ! Local variables
    integer            :: ierr,i,j,nscans
    real               :: t_ini,dga_i,dga,ga_D_min_i,ga_D_max_i,ga_D_min,ga_D_max,ga_1,ga_N
    character(len=512) :: scanf, cfl_file
    type(cfl_D2B_type) :: cfl
    type(nexus_type)   :: nexus

    integer :: np_horiz,np_horiz_virtual
    integer, dimension(:,:), allocatable :: data2D
    real    :: ga_D,nu_D,xmin,cgap
    logical :: is_virtual_detector_set
    type(DiffPat_E_Type) :: pow_pat

    ! Starting time
    call cpu_time(t_ini)
    call write_header(6)

    ! Read the cfl file
    call Get_Command_Argument(1,cfl_file)
    write(*,'(a)') ' => Reading cfl file: '//trim(cfl_file)
    call read_cfl_D2B(cfl_file,cfl,ierr)
    if (ierr > 0) call finish(t_ini)

    ! Read calibration file if it was given
    if (cfl%calibration) then
        if (trim(L_Case(cfl%calib_gen)) == 'mantid') then
            write(*,'(4x,a)') ' => Calibration generator: mantid'
            write(*,'(8x,a,1x,a)') 'Reading calibration file:', trim(cfl%calib_file)
            write(*,'(8x,a,1x,a)') 'Data path:', trim(adjustl(cfl%calib_path))
            call read_calibration_mantid(cfl%calib_file,cfl%calib_path,ierr)
            if (ierr > 0) then
                write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_D2B_mess)
                write(*,'(8x,a)') 'Calibration will not be applied'
                cfl%calibration = .false.
            end if
        else if (trim(L_Case(cfl%calib_gen)) == 'lamp') then
            write(*,'(4x,a)') ' => Calibration generator: lamp'
            write(*,'(8x,a,1x,a)') 'Reading calibration file:', trim(cfl%calib_file)
            call read_calibration_lamp(cfl%calib_file,ierr)
            if (ierr > 0) then
                write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_D2B_mess)
                write(*,'(8x,a)') 'Calibration will not be applied'
                cfl%calibration = .false.
            end if
        else
            write(*,'(4x,a)') ' => Calibration generator: unknown'
            write(*,'(8x,a)') 'Calibration will not be applied'
            cfl%calibration = .false.
        end if
    else
        if (.not. allocated(calibration)) allocate(calibration(current_instrm%np_vert,current_instrm%np_horiz))
        calibration(:,:) = 1.0
    end if

    ga_range_real = to_deg * current_instrm%cgap * (current_instrm%np_horiz - 1) / current_instrm%dist_samp_detector
    if (cfl%combine) then
        ! Determine the virtual detector common for all scans
        write(*,'(4x,a)') ' => Computing virtual detector common for all scans'
        do i = 1 , cfl%nscans
            write(*,'(8x,a,1x,a)') 'Reading nexus file', trim(cfl%scans(i))
            call read_nexus(trim(cfl%scans(i)),nexus)
            if (err_nexus) then
                write(*,'(8x,a)') trim(err_nexus_mess)
                cycle
            end if
            if (is_d2b) then
                ! Transform 2theta into gamma values
                if (nexus%is_tth) then
                    do j = 1 , nexus%nf
                        nexus%angles(4,j) = nexus%angles(8,j) - 0.5 * (D2B_NPIXELS_HORIZ-1) * D2B_WIDTH_HORIZ + 0.5 * D2B_WIDTH_HORIZ
                    end do
                end if
            end if
            ! dga_i -> scan step of the scan i
            ! ga_D_min_i -> minimum value of the gamma detector value for frames in the scan
            ! ga_D_max_i -> maximum value of the gamma detector value for frames in the scan
            dga_i = abs(nexus%angles(4,nexus%nf)-nexus%angles(4,1)) / (nexus%nf - 1)
            ga_D_min_i = min(nexus%angles(4,nexus%nf),nexus%angles(4,1))
            ga_D_max_i = max(nexus%angles(4,nexus%nf),nexus%angles(4,1))
            if (i == 1) then
                dga = dga_i
                ga_D_min = ga_D_min_i
                ga_D_max = ga_D_max_i
            else
                if (abs(dga-dga_i) > EPSIL) then
                    err_D2B_mess = 'Scans with different scan steps cannot be combined'
                    call finish(t_ini)
                end if
                if (ga_D_min_i < ga_D_min) ga_D_min = ga_D_min_i
                if (ga_D_max_i > ga_D_max) ga_D_max = ga_D_max_i
            end if
        end do
        ! ga_1 -> gamma value of the first pixel of the virtual detector
        ! ga_N -> gamma value of the last  pixel of the virtual detector
        ga_1 = ga_D_min - 0.5 * ga_range_real
        ga_N = ga_D_max + 0.5 * ga_range_real
        call set_virtual_detector(ga_1,ga_N,dga)
    end if

    ! Start looping over scans
    do i = 1 , cfl%nscans
        write(*,'(4x,a,1x,a)') ' => Reading nexus file', trim(cfl%scans(i))
        if (cfl%raw) then
            call read_nexus(trim(cfl%scans(i)),nexus,raw=cfl%raw)
        else
            call read_nexus(trim(cfl%scans(i)),nexus)
        end if
        if (err_nexus) then
            write(*,'(8x,a)') trim(err_nexus_mess)
            cycle
        end if
        if (is_d2b) then
            ! Transform 2theta into gamma values
            if (nexus%is_tth) then
                do j = 1 , nexus%nf
                    nexus%angles(4,j) = nexus%angles(8,j) - 0.5 * (D2B_NPIXELS_HORIZ-1) * D2B_WIDTH_HORIZ + 0.5 * D2B_WIDTH_HORIZ
                end do
            end if
        end if
        if (abs(current_instrm%wave-nexus%wave) > EPSIL) then
            write(*,'(8x,a)') 'Warning! Wavelengths from instrument and nexus differ! Instrument wavelength will be used'
            write(*,'(8x,a,f6.4)') 'Wavelength (instrument): ',current_instrm%wave
            write(*,'(8x,a,f6.4)') 'Wavelength (nexus): ',nexus%wave
        else
            write(*,'(8x,a,f6.4)') 'Wavelength: ',nexus%wave
        end if
        if (trim(current_instrm%data_ordering) /= nexus%data_ordering) then
            write(*,'(8x,a)') 'Warning! Data ordering from instrument and nexus differ! Instrument data ordering will be used'
            write(*,'(8x,a,a)') 'Data ordering (instrument): ',trim(current_instrm%data_ordering)
            write(*,'(8x,a,a)') 'Data ordering (nexus)     : ',trim(nexus%data_ordering)
        else
            write(*,'(8x,2a)') 'Data ordering: ',nexus%data_ordering
        end if
        if (.not. cfl%combine) then
            write(*,'(4x,a)') ' => Building virtual detector'
            dga = abs(nexus%angles(4,nexus%nf)-nexus%angles(4,1)) / (nexus%nf - 1)
            ga_D_min = min(nexus%angles(4,nexus%nf),nexus%angles(4,1))
            ga_D_max = max(nexus%angles(4,nexus%nf),nexus%angles(4,1))
            ga_1 = ga_D_min - 0.5 * ga_range_real
            ga_N = ga_D_max + 0.5 * ga_range_real
            call set_virtual_detector(ga_1,ga_N,dga)
        end if
        write(*,'(4x,a)') ' => Putting counts in the virtual detector'
        call fill_virtual_detector(nexus)

        if (.not. cfl%combine) then
            write(*,'(4x,a)') ' => Averaging counts for virtual pixels'
            call average_virtual_counts(cfl%nsigma)
            write(scanf,'(2a)') trim(nexus%filcod),'_'//trim(cfl%suffix)//'.nxs'
            write(*,'(4x,2a)') ' => Writing nexus file ', trim(scanf)
            !Setting nexus%virtual_cgap
            nexus%virtual_cgap=virtual_instrm%cgap
            call write_vnexus(trim(scanf),ga_D_virtual,nu_D_virtual,ave_counts_virtual,nexus)
        end if
    end do

    if (cfl%combine) then
        write(*,'(4x,a)') ' => Averaging counts for virtual pixels'
        call average_virtual_counts(cfl%nsigma)
        write(scanf,'(2a)') trim(adjustl(cfl%combine_name))//'.nxs'
        write(*,'(4x,2a)') ' => Writing nexus file ', trim(scanf)
        !Setting nexus%virtual_cgap
        nexus%virtual_cgap=virtual_instrm%cgap
        call write_vnexus(scanf,ga_D_virtual,nu_D_virtual,ave_counts_virtual,nexus)
    end if

    !Continue with integration

    call display_nexus(6,nexus)

    nscans = 0
    is_virtual_detector_set = .false.

        write(*,'(a,1x,a)') ' => Reading nexus file', trim(scanf)
        call read_nexus(scanf,nexus)
        if (err_nexus) then
            write(*,'(4x,a)') trim(err_nexus_mess)//"  -> "//trim(scanf)
            call finish(t_ini)
        end if
        ga_D = nexus%angles(4,1)
        nu_D = nexus%angles(7,1)
        np_horiz = size(nexus%counts,2)
        if (.not. is_virtual_detector_set) then
            ga_D_virtual = ga_D
            nu_D_virtual = nu_D
            np_horiz_virtual = np_horiz
            is_virtual_detector_set = .true.
            if (nexus%is_virtual) then
                cgap = nexus%virtual_cgap
            else
                cgap = current_instrm%cgap
            end if
            write(*,'(4x,a)') 'Setting parameters of the virtual detector'
            write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels:',np_horiz_virtual
            write(*,'(8x,a,1x,f6.2)') 'Horizontal pixel size (mm): ',cgap
            write(*,'(8x,a,1x,f6.2)') 'Gamma:',ga_D_virtual
            write(*,'(8x,a,1x,f6.2)') 'Nu:   ',nu_D_virtual
        end if

        if (cfl%single) then
            write(*,'(4x,a)') 'Integrating'
            call get_powder_pattern(cfl,np_horiz_virtual,cgap,ga_D,nu_D,nexus%counts(:,:,1),pow_pat)
            if (cfl%is_tth_min) then
                xmin = cfl%tth_min
            else
                xmin = pow_pat%xmin
            end if
            if (cfl%is_tth_max) then
                call write_pattern(trim(nexus%filcod)//'.xys',pow_pat,'xys',xmin=xmin,xmax=cfl%tth_max)
            else
                call write_pattern(trim(nexus%filcod)//'.xys',pow_pat,'xys',xmin=xmin)
            end if
        end if

        if (cfl%suma) then
            if (.not. allocated(data2D)) then
                allocate(data2D(nexus%nz,nexus%nx))
                data2D(:,:) = 0
            end if
            if (abs(ga_D-ga_D_virtual) > EPSIL .or. abs(nu_D-nu_D_virtual) > EPSIL .or. &
                np_horiz /= np_horiz_virtual) then
                write(*,'(8x,a)') 'This scan cannot be averaged. Inconsistent dimensions'
            else
                data2D(:,:) = data2D(:,:) + nexus%counts(:,:,1)
                nscans = nscans + 1
            end if
        end if


    if (cfl%suma .and. nscans > 0) then
        write(*,'(a)') ' => Integrating the sum'
        call get_powder_pattern(cfl,np_horiz_virtual,cgap,ga_D,nu_D,data2D,pow_pat)
        if (cfl%is_tth_min) then
            xmin = cfl%tth_min
        else
            xmin = pow_pat%xmin
        end if
        if (cfl%is_tth_max) then
            call write_pattern(trim(cfl%label_sum)//'.xys',pow_pat,'xys',xmin=xmin,xmax=cfl%tth_max)
        else
            call write_pattern(trim(cfl%label_sum)//'.xys',pow_pat,'xys',xmin=xmin)
        end if
    end if

end program D2B_int