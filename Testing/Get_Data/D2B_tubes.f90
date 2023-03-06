
program D2B_tubes

    use CFML_GlobalDeps,      only: err_CFML
    use CFML_Strings,         only: L_Case
    use CFML_DiffPatt,        only: DiffPat_E_Type,Write_Pattern,Allocate_Pattern,Add_patterns
    use D2B_read_mod,         only: cfl_D2B_type,read_cfl_D2B
    use nexus_mod

    implicit none

    ! Local parameters

    ! Local variables
    integer            :: ierr,i,j,k,nmax,nf,n
    integer, parameter :: nt=128 !Number of tubes
    real               :: t_ini,t_end,xmin,step,xmax
    character(len=132) :: fname,straux,cfl_file
    type(cfl_D2B_type) :: cfl
    type(nexus_type),    dimension(:), allocatable  :: nexus
    type(DiffPat_E_Type)     :: pat
    type(DiffPat_E_Type),dimension(nt) :: patterns
    logical, dimension(nt)  :: Active=.true.
    real,   dimension (nt)  :: PosX
    real,   dimension (:,:), allocatable :: IntTubes
    real,   dimension (:,:), allocatable :: TwoThetaTubes
    integer,dimension (:),   allocatable :: np


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

    !PosX=[(-158.750+(i-1)*1.25,i=1,nt)] !theoretical positions with respect to the last detector colected in nexus%angle(8,nf)
    if(cfl%apply_shifts) then
       open(unit=1,file="calib_d2b.pos",status="old", action= "read",position="rewind")
       do i=1,nt
         read(unit=1,fmt="(i4,2f12.4)") j,xmin,xmax !In reality theoretical position and shift
         PosX(i)= xmin - xmax  !New positions
       end do
       close(unit=1)
    else
       PosX=[(-158.750+(i-1)*1.25,i=1,128)]
    end if
    write(*,'(4x,a)') ' => Reading all scans'
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
    end do

    nmax= cfl%nscans * maxval(nexus(:)%nf)
    step=0.05
    allocate(IntTubes(nmax,nt),TwoThetaTubes(nmax,nt),np(nmax))
    IntTubes=0.0; TwoThetaTubes=0.0
    !Integration of the central part just suming the counts along each tube
    write(*,"(/,a,2i4,a,i3,/)") " => Integrating individual tubes between vertical pixels: ",cfl%kc1,cfl%kc2, &
                              " -> width: ",cfl%kc2-cfl%kc1+1
    do j=1,nt
       np(j)=0
       do i=1, cfl%nscans
         do nf=1,nexus(i)%nf
            np(j)=np(j)+1
            TwoThetaTubes(np(j),j)=nexus(i)%angles(8,nf)+PosX(j)
            IntTubes(np(j),j)=0.0
            do k=cfl%kc1,cfl%kc2
              IntTubes(np(j),j)=IntTubes(np(j),j)+nexus(i)%counts(k,j,nf)
            end do
         end do
       end do
       call Allocate_Pattern(Patterns(j),np(j))
       write(straux,"(2(a,i4),a,2i4)") " Tube #",j,"    Number of points: ",np(j), " -> Integration between vertical pixels: ",cfl%kc1,cfl%kc2
       Patterns(j)%kindrad = 'Neutrons'
       Patterns(j)%scatvar = '2theta'
       Patterns(j)%Title=trim(straux)
       Patterns(j)%x(1:np(j))=TwoThetaTubes(1:np(j),j)
       Patterns(j)%npts=np(j)
       Patterns(j)%y(1:np(j))=cfl%scale_fac*IntTubes(1:np(j),j)
       n=Patterns(j)%npts
       Patterns(j)%xmin=Patterns(j)%x(1)
       Patterns(j)%ymin=minval(Patterns(j)%y)
       Patterns(j)%xmax=Patterns(j)%x(n)
       Patterns(j)%ymax=maxval(Patterns(j)%y)
       if(.not. cfl%apply_shifts) then
          write(fname,"(a,i3.3,a)") "tube_",j,".xys"
          write(*,'(a)') ' => Writing xys file '//trim(fname)
          call write_pattern(trim(fname),Patterns(j),'xys')
       end if

    end do
    if(cfl%apply_shifts) then
       write(*,'(/,a)') ' => Output of individual tubes suppressed! '
       write(*,'(a)')   '    It is supposed that the shifts of detectors are '
       write(*,'(a)')   '    already refined and we output only the final integrated pattern. '
    else
       write(*,'(/,a)') ' => The individual tube patterns are output with ideal positions '
       write(*,'(a)')   '    they are prepared to be used by FullProf in sequential mode, '
       write(*,'(a)')   '    for refining the Zero-shifts of each tube '
    end if
    call Add_Patterns(Patterns, nt, Active, Pat, step_int=step)
    if(err_CFML%Ierr /= 0) then
        write(*,"") "  ERROR! "//trim(err_CFML%Msg)
    end if
    write(*,"(a)") " => Writing combined integrated file: "//trim(cfl%combine_name)//".xys"
    call write_pattern(trim(cfl%combine_name)//".xys",Pat,'xys')

    call cpu_time(t_end)
    write(unit=*, fmt='(/,a)')       ' => D2B_tubes finished normally  '
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
        write(unit=lun,fmt='(13x,a)') ' Integrating individual D2B tubes'
        write(unit=lun,fmt='(1x,60a)') ('-',i=1,52)
        write(unit=lun,fmt='(1x,a)') ' Program: D2B_tubes, February 2023'
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

end program D2B_tubes