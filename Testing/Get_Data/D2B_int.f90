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

    use CFML_GlobalDeps,      only: to_deg,err_CFML
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Calibration_Detector_Type
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
    integer            :: ierr,i,j,last
    real               :: t_ini,dga_i,dga,ga_D_min_i,ga_D_max_i,ga_D_min,ga_D_max,ga_1,ga_N,t_end
    character(len=512) :: scanf, cfl_file
    type(cfl_D2B_type) :: cfl
    type(nexus_type),dimension(:),allocatable   :: nexus
    type(nexus_type)   :: snexus  !Single nexus

    integer :: np_horiz
    integer, dimension(3) :: dims
    real    :: ga_D,nu_D,xmin,cgap
    type(DiffPat_E_Type) :: pow_pat

    ! Starting time
    call cpu_time(t_ini)
    call write_header(6)

    ! Read the cfl file
    call Get_Command_Argument(1,cfl_file)
    write(*,'(a)') ' => Reading cfl file: '//trim(cfl_file)
    call read_cfl_D2B(cfl_file,cfl,ierr)
    if (ierr > 0) call finish(t_ini)

    if(allocated(nexus)) deallocate(nexus)
    allocate(nexus(cfl%nscans))

    ! Read calibration file if it was given
    if(.not. (current_instrm%alpha_correct)) then
       if (cfl%calibration ) then
           if (trim(L_Case(cfl%calib_gen)) == 'mantid') then
               write(*,'(4x,a)') ' => Calibration generator: mantid'
               write(*,'(8x,a,1x,a)') 'Reading calibration file:', trim(cfl%calib_file)
               write(*,'(8x,a,1x,a)') 'Data path:', trim(adjustl(cfl%calib_path))
               call read_calibration_mantid(cfl%calib_file,cfl%calib_path,Cal,ierr)
               if (ierr > 0) then
                   write(*,'(8x,2a)') 'Warning! Error reading calibration: -> ',trim(err_D2B_mess)
                   write(*,'(8x,a)') 'Calibration will not be applied'
                   cfl%calibration = .false.
               end if
           else if (trim(L_Case(cfl%calib_gen)) == 'lamp') then
               write(*,'(4x,a)') ' => Calibration generator: lamp'
               write(*,'(8x,a,1x,a)') 'Reading calibration file:', trim(cfl%calib_file)
               call read_calibration_lamp(cfl%calib_file,Cal,ierr)
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
           if (.not. allocated(Cal%Effic)) allocate(Cal%Effic(current_instrm%np_vert,current_instrm%np_horiz))
           if (.not. allocated(Cal%Active)) allocate(Cal%Active(current_instrm%np_vert,current_instrm%np_horiz))
           if (.not. allocated(Cal%PosX)) allocate(Cal%PosX(current_instrm%np_horiz))
           Cal%Effic = 1.0
           Cal%Active = .true.
           Cal%PosX=0.0
       end if

       !Calculate the horizontal shifts in mm or the detector tubes in put them, as well as alphas, in (Current_instrm%alphas,Current_instrm%shifts)
       call set_alphas_shifts_D2B(Cal)

    end if

    ga_range_real = to_deg * current_instrm%cgap * (current_instrm%np_horiz - 1) / current_instrm%dist_samp_detector
    ! Determine the virtual detector common for all scans
    write(*,'(4x,a)') ' => Computing virtual detector common for all scans'
    do i = 1 , cfl%nscans
        write(*,'(8x,a,1x,a)') 'Reading nexus file', trim(cfl%scans(i))
        if (cfl%raw) then
            call read_nexus(trim(cfl%scans(i)),nexus(i),raw=cfl%raw)
        else
            call read_nexus(trim(cfl%scans(i)),nexus(i))
        end if
        if (err_nexus) then
            write(*,'(8x,a)') trim(err_nexus_mess)
            cycle
        end if
        if (is_d2b) then
            ! Transform 2theta into gamma values
            if (nexus(i)%is_tth) then
                do j = 1 , nexus(i)%nf
                    nexus(i)%angles(4,j) = nexus(i)%angles(8,j) - 0.5 * (D2B_NPIXELS_HORIZ-1) * D2B_WIDTH_HORIZ + 0.5 * D2B_WIDTH_HORIZ
                end do
            end if
        end if
        ! dga_i -> scan step of the scan i
        ! ga_D_min_i -> minimum value of the gamma detector value for frames in the scan
        ! ga_D_max_i -> maximum value of the gamma detector value for frames in the scan
        dga_i = abs(nexus(i)%angles(4,nexus(i)%nf)-nexus(i)%angles(4,1)) / (nexus(i)%nf - 1)
        ga_D_min_i = min(nexus(i)%angles(4,nexus(i)%nf),nexus(i)%angles(4,1))
        ga_D_max_i = max(nexus(i)%angles(4,nexus(i)%nf),nexus(i)%angles(4,1))
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

    !Construction of "virtual_instrm" of type diffractometer_type
    call set_virtual_detector(ga_1,ga_N,dga)

    ! Start looping over scans
    do i = 1 , cfl%nscans
        if (is_d2b) then
            ! Transform 2theta into gamma values
            if (nexus(i)%is_tth) then
                do j = 1 , nexus(i)%nf
                    nexus(i)%angles(4,j) = nexus(i)%angles(8,j) - 0.5 * (D2B_NPIXELS_HORIZ-1) * D2B_WIDTH_HORIZ + 0.5 * D2B_WIDTH_HORIZ
                end do
            end if
        end if
        if (abs(current_instrm%wave-nexus(i)%wave) > EPSIL) then
            write(*,'(8x,a)') 'Warning! Wavelengths from instrument and nexus differ! Instrument wavelength will be used'
            write(*,'(8x,a,f6.4)') 'Wavelength (instrument): ',current_instrm%wave
            write(*,'(8x,a,f6.4)') 'Wavelength (nexus): ',nexus(i)%wave
        end if
        if (trim(current_instrm%data_ordering) /= nexus(i)%data_ordering) then
            write(*,'(8x,a)') 'Warning! Data ordering from instrument and nexus differ! Instrument data ordering will be used'
            write(*,'(8x,a,a)') 'Data ordering (instrument): ',trim(current_instrm%data_ordering)
            write(*,'(8x,a,a)') 'Data ordering (nexus)     : ',trim(nexus(i)%data_ordering)
        end if
        call fill_virtual_detector(nexus(i),cal)
    end do

    last=cfl%nscans
    write(*,'(4x,a)') ' => Averaging counts for virtual pixels'
    call average_virtual_counts(cfl%nsigma)
    write(scanf,'(2a)') trim(adjustl(cfl%combine_name))//'.nxs'
    write(*,'(4x,2a)') ' => Writing nexus file ', trim(scanf)

    !Setting snexus%virtual_cgap
    dims(1)=size(ave_counts_virtual,1)
    dims(2)=size(ave_counts_virtual,2)
    dims(3)=size(ave_counts_virtual,3)
    call initialize_nexus(snexus,dims)
    call partial_nexus_copy(nexus(last),snexus)
    snexus%virtual_cgap=virtual_instrm%cgap
    snexus%angles(4,1)=ga_D_virtual
    snexus%angles(7,1)=nu_D_virtual
    snexus%counts=nint(ave_counts_virtual)
    call write_simple_nexus(trim(scanf),snexus) !Corresponds to the virtual instrument

    !Integration

    if(cfl%verbose) call display_nexus(6,snexus)

    ga_D = snexus%angles(4,1)
    nu_D = snexus%angles(7,1)
    np_horiz = size(snexus%counts,2)
    cgap = snexus%virtual_cgap
    if(cfl%verbose) then
       write(*,'(4x,a)') 'Setting parameters of the virtual detector'
       write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels:',np_horiz
       write(*,'(8x,a,1x,f6.2)') 'Horizontal pixel size (mm): ',cgap
       write(*,'(8x,a,1x,f6.2)') 'Gamma:',ga_D
       write(*,'(8x,a,1x,f6.2)') 'Nu:   ',nu_D
    end if

    call get_powder_pattern(cfl%nz_int,cfl%scale_fac,cfl%align,np_horiz,cgap,ga_D,nu_D,snexus%counts(:,:,1),pow_pat)
    if (cfl%is_tth_min) then
        xmin = cfl%tth_min
    else
        xmin = pow_pat%xmin
    end if
    write(*,'(4x,2a)') ' => Writing xys file ', trim(cfl%combine_name)//'.xys'
    if (cfl%is_tth_max) then
        call write_pattern(trim(cfl%combine_name)//'.xys',pow_pat,'xys',xmin=xmin,xmax=cfl%tth_max)
    else
        call write_pattern(trim(cfl%combine_name)//'.xys',pow_pat,'xys',xmin=xmin)
    end if

    call cpu_time(t_end)
    write(unit=*, fmt='(/,a)')       ' => D2B_int finished normally  '
    write(unit=*, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_end-t_ini,' seconds'

  contains

    subroutine finish(t_ini)

        ! Finish the program due to an error

        ! Arguments
        real, intent(in) :: t_ini

        call write_error_message(6,t_ini)
        stop

    end subroutine finish

    subroutine write_header(iout)

        ! Arguments
        integer, intent(in), optional :: iout

        ! Local variables
        integer :: i,lun

        lun=6
        if(present(iout)) lun=iout

        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)
        write(unit=lun,fmt='(13x,a)') ' Integrating D2B data'
        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)
        write(unit=lun,fmt='(1x,a)') ' Program: D2B_int, February 2023'
        write(unit=lun,fmt='(1x,a)') ' Authors: Nebil A. Katcho and J. Rodriguez-Carvajal'
        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)

    end subroutine write_header

    subroutine write_error_message(lun,t_ini)

        ! Stop the program, printing out an error message

        ! Arguments
        integer, intent(in) :: lun
        real,    intent(in) :: t_ini

        ! Local variables
        real :: t_fin

        call cpu_time(t_fin)
        write(unit=lun, fmt='(a,a)')       ' => D2B_int stopped!: ', trim(err_CFML%Msg)
        write(unit=lun, fmt='(a,f10.4,a)') ' => Total CPU-time: ',t_fin-t_ini,' seconds'

    end subroutine write_error_message

    subroutine write_warning_message(lun)

        ! Print a warning message

        ! Arguments
        integer, intent(in) :: lun

        write(unit=lun, fmt='(a,a)') ' => Warning!: ', trim(war_D2B_mess)

    end subroutine write_warning_message

end program D2B_int