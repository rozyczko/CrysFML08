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

module D2B_data_mod

    use CFML_GlobalDeps,      only: To_Rad,To_Deg,Err_CFML,clear_error
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Diffractometer_Type
    use CFML_SXTAL_Geom,      only: diffractometer => psd, psd_convert
    use CFML_DiffPatt,        only: DiffPat_E_Type,Allocate_Pattern
    use HDF5
    use nexus_mod

    implicit none

    private

    ! List of public subroutines
    public :: average_virtual_counts,fill_virtual_detector,set_virtual_detector, &
              finish,write_header,write_warning_message,write_error_message, &
              get_powder_pattern

    integer, parameter :: NSAMPLES_MAX = 1000
    real,    parameter :: EPSIL = 0.01

    integer, dimension(:,:),   allocatable, public :: nsamples
    integer, dimension(:,:,:), allocatable, public :: counts_virtual
    integer, dimension(:,:,:), allocatable, public :: ave_counts_virtual
    real,    dimension(:,:),   allocatable, public :: calibration
    real, public :: nu_D_virtual = 0.0
    real, public :: ga_D_virtual
    real, public :: ga_range_real

    ! Error message
    character(len=1024), public :: war_D2B_mess, err_D2B_mess
    logical :: pattern_allocated = .false.

    type(diffractometer_type), public :: virtual_instrm

    type, public :: cfl_D2B_type

        integer            :: nscans
        integer            :: nz_int
        real               :: scale_fac
        real               :: tth_min
        real               :: tth_max
        real               :: nsigma
        logical            :: is_tth_min = .false.
        logical            :: is_tth_max = .false.
        logical            :: calibration,combine,raw
        logical            :: suma
        logical            :: single
        logical            :: align
        character(len=6)   :: calib_gen
        character(len=12)  :: instrument_name
        character(len=20)  :: suffix
        character(len=512) :: scan_path,calib_path,calib_file,combine_name
        integer, dimension(:,:),          allocatable :: scan_list
        character(len=512), dimension(:), allocatable :: scans
        character(len=:),                 allocatable :: label_sum

    end type cfl_D2B_type

    contains

    subroutine allocate_virtual_arrays()

        if (allocated(ave_counts_virtual)) deallocate(ave_counts_virtual)
        if (allocated(counts_virtual)) deallocate(counts_virtual)
        if (allocated(nsamples)) deallocate(nsamples)
        ! Assign memory to arrays
        allocate(ave_counts_virtual(virtual_instrm%np_vert,virtual_instrm%np_horiz,1))
        allocate(counts_virtual(virtual_instrm%np_vert,virtual_instrm%np_horiz,NSAMPLES_MAX))
        allocate(nsamples(virtual_instrm%np_vert,virtual_instrm%np_horiz))
        ave_counts_virtual(:,:,:) = 0
        counts_virtual(:,:,:) = 0
        nsamples(:,:) = 0

    end subroutine allocate_virtual_arrays

    subroutine average_virtual_counts(nsigma)

        ! Arguments
        real, intent(in) :: nsigma

        ! Local variables
        integer :: i,j,k,n
        real :: ave,sigma,dsigma

        do j = 1 , size(nsamples,2)
            do i = 1 , size(nsamples,1)
                if (nsamples(i,j) == 0) cycle
                if (nsamples(i,j) == 1) then
                    ave_counts_virtual(i,j,1) = counts_virtual(i,j,1)
                    cycle
                end if
                ! Compute the average
                ave = 0.0
                do k = 1 , nsamples(i,j)
                    ave = ave + counts_virtual(i,j,k)
                end do
                ave = ave / nsamples(i,j)
                ! Compute standard deviation
                sigma = 0.0
                do k = 1 , nsamples(i,j)
                    sigma = sigma + (counts_virtual(i,j,k) - ave)**2
                end do
                sigma = sqrt(sigma / nsamples(i,j))
                ! Compute the final value
                n = 0
                dsigma = nsigma * sigma
                ave_counts_virtual(i,j,1) = 0.0
                do k = 1 , nsamples(i,j)
                    if (abs(counts_virtual(i,j,k) - ave) < dsigma) then
                        n = n + 1
                        ave_counts_virtual(i,j,1) = &
                            ave_counts_virtual(i,j,1) + counts_virtual(i,j,k)
                    end if
                end do
                if (n > 0) ave_counts_virtual(i,j,1) = nint(ave_counts_virtual(i,j,1)  / float(n))
            end do
        end do

    end subroutine average_virtual_counts

    subroutine fill_virtual_detector(nexus)

        ! Arguments
        type(nexus_type), intent(in) :: nexus

        ! Local variables
        integer :: i,j,k,ii,jj
        real :: ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P

        do k = 1 , nexus%nf
            ga_D = nexus%angles(4,k)
            nu_D = nexus%angles(7,k)
            do j = 1 , nexus%nx
                do i = 1 , nexus%nz
                    px = j - 1
                    pz = i - 1
                    call psd_convert(current_instrm,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P)
                    call psd_convert(virtual_instrm,1,1,ga_D_virtual,nu_D,px,pz,x_D,z_D,ga_P,nu_P)
                    if (Err_CFML%ierr == 0) then
                        ii = nint(pz) + 1
                        jj = nint(px) + 1
                        if (ii > 0 .and. ii <= virtual_instrm%np_vert .and. jj > 0 .and. jj <= virtual_instrm%np_horiz) then
                            nsamples(ii,jj) = nsamples(ii,jj) + 1
                            if (nsamples(ii,jj) <= NSAMPLES_MAX) then
                                counts_virtual(ii,jj,nsamples(ii,jj)) = &
                                    counts_virtual(ii,jj,nsamples(ii,jj)) + nexus%counts(i,j,k) * calibration(i,j)
                            else
                                write(*,'(8x,a)') 'Warning! Nsamples reached its maximum allowed value!'
                                nsamples(ii,jj) = NSAMPLES_MAX
                            end if
                        end if
                    end if
                end do
            end do
        end do

    end subroutine fill_virtual_detector

    subroutine set_virtual_detector(ga_1,ga_N,dga)

        ! Arguments
        real, intent(in) :: ga_1  ! gamma of the first pixel
        real, intent(in) :: ga_N  ! gamma of the last pixel
        real, intent(in) :: dga   ! gamma step

        virtual_instrm%ipsd     = current_instrm%ipsd
        virtual_instrm%np_horiz = nint((ga_N-ga_1) / dga) + 1
        virtual_instrm%cgap     = current_instrm%dist_samp_detector * dga * to_rad
        virtual_instrm%np_vert  = current_instrm%np_vert
        virtual_instrm%agap     = current_instrm%agap
        virtual_instrm%det_offsets(:)     = current_instrm%det_offsets(:)
        virtual_instrm%dist_samp_detector = current_instrm%dist_samp_detector
        virtual_instrm%data_ordering      = current_instrm%data_ordering
        ga_D_virtual = 0.5 * (ga_1 + ga_N)
        call allocate_virtual_arrays()
        write(*,'(8x,a,1x,f8.3)') 'Gamma step: ', dga
        write(*,'(8x,a,1x,f8.3)') 'Gamma range of the real    detector: ', ga_range_real
        write(*,'(8x,a,1x,f8.3)') 'Gamma range of the virtual detector: ', ga_N - ga_1
        write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels of the real    detector: ', current_instrm%np_horiz
        write(*,'(8x,a,1x,i6)')   'Number of horizontal pixels of the virtual detector: ', virtual_instrm%np_horiz
        write(*,'(8x,a,1x,f8.3)') 'Size of horizontal pixel of the real    detector (mm): ', current_instrm%cgap
        write(*,'(8x,a,1x,f8.3)') 'Size of horizontal pixel of the virtual detector (mm): ', virtual_instrm%cgap
        write(*,'(8x,a,1x,f8.3)') 'Gamma value for the first pixel of the virtual detector: ', ga_1
        write(*,'(8x,a,1x,f8.3)') 'Gamma value for the last  pixel of the virtual detector: ', ga_N

    end subroutine set_virtual_detector

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

    subroutine get_powder_pattern(cfl,np_horiz,virtual_cgap,ga_D,nu_D,data2D,pat)

        ! Arguments
        type(cfl_D2B_type),      intent(in)    :: cfl
        integer,                 intent(in)    :: np_horiz
        real,                    intent(in)    :: virtual_cgap
        real,                    intent(in)    :: ga_D
        real,                    intent(in)    :: nu_D
        integer, dimension(:,:), intent(in)    :: data2D
        type(DiffPat_E_Type),    intent(inout) :: pat

        ! Local variables
        integer :: i,k,k1,k2,nc,ith
        real :: span_angle,tth,fac
        real :: px,pz,x_D,z_D,ga_P,nu_P

        call clear_Error()
        current_instrm%np_horiz = np_horiz
        current_instrm%cgap = virtual_cgap
        span_angle = (((current_instrm%np_horiz-1) * current_instrm%cgap) / current_instrm%dist_samp_detector) * to_deg

        if (.not. cfl%align .or. .not. pattern_allocated) then
            ! Allocating a 1D powder diffraction pattern
            call Allocate_Pattern(pat,np_horiz)
            pattern_allocated = .true.

            ! Set diffraction pattern attributes
            pat%kindrad = 'neutron'
            pat%scatvar = '2theta'
            pat%npts    = current_instrm%np_horiz - 1
            pat%xmin    = ga_d - span_angle * 0.5
            pat%xmax    = ga_d + span_angle * 0.5
            pat%step    = span_angle / (current_instrm%np_horiz - 1)
            pat%wave(1)   = current_instrm%wave
            ! Initialize pattern
            do i = 1 , current_instrm%np_horiz
                pat%x(i) = pat%xmin + (i-1) * pat%step
            end do
        end if

        pat%y     = 0.0
        pat%sigma = 0.0
        pat%nd    = 0

        ! Compute integration limits
        nc = max(1,cfl%nz_int/2)
        nc = min(current_instrm%np_vert/2,nc)
        k1 = current_instrm%np_vert/2 - nc + 1
        k2 = current_instrm%np_vert/2 + nc
        write(unit=*,fmt="(12x,a,2i4)") "Vertical Integration between cells: ",k1,k2

        ! Map Data2D -> Pat
        do i = 1 , np_horiz
            do k = k1 , k2
                pz = k - 1
                px = i - 1
                call psd_convert(current_instrm,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P)
                if (err_CFML%ierr == 0) then
                    tth = acosd(cosd(nu_P)*cosd(ga_P))
                    ith = nint((tth-pat%xmin) / pat%step) + 1
                    if (ith > 0 .and. ith <= Pat%npts) then
                        pat%y(ith)  = pat%y(ith) + data2D(k,i)
                        pat%nd(ith) = pat%nd(ith) + 1
                    end if
                end if
            end do
        end do

        ! Average
        do i = 1 , pat%npts
            fac = cfl%scale_fac / max(1,pat%nd(i))
            pat%y(i) = pat%y(i) * fac
            if (pat%y(i) < 1.00) pat%y(i) = 1.0
            pat%sigma(i) = pat%y(i)
            if (pat%nd(i) > 0) pat%sigma(i) = pat%sigma(i) / pat%nd(i)
        end do

    end subroutine get_powder_pattern

end module D2B_data_mod