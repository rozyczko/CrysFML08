     Module Input_output_data_mod
      use TOF_diffraction
      use CFML_Optimization_LSQ
      use CFML_DiffPatt

      implicit none
      private
      !Public procedures
      public  :: INPUT_data, get_texte, output_plot
      private :: Backa
      ! Global variables
      real,    public :: thmin,step,thmax,ain,afin
      integer, public :: icont,imeth, NPEAKX

      contains

      Function Backa(tth) result(back)
       real, intent(in)    :: tth
       Real                :: Back
       integer             :: ib1,ib2,i1,i2, ib
       real                :: tang

       !  Calculation of the background
       i1=1
       i2=n_ba         ! nb de points de bruit de fond
       ib1=ngl_tof+1   ! nb de parametres globaux
       ib2=ib1+1       ! nb de parametres globaux + 1

        do ib=1,n_ba-1
          if(tth >= BackGroundPoint(ib)%x .and. tth <= BackGroundPoint(ib+1)%x) then
            i1=ib
            i2=ib+1
            ib1=ngl_tof+i1
            ib2=ib1+1
            exit
          end if
        end do
        tang=(tth-BackGroundPoint(i1)%x)/(-BackGroundPoint(i2)%x--BackGroundPoint(i1)%x)
        Back=vs%pv(ib1)+(vs%pv(ib2)-vs%pv(ib1))*tang
        return
      End Function Backa


      Subroutine Get_Texte(lun,texte,ok)
        integer,intent(in)               :: lun
        character(len=*), intent(in out) :: texte
        logical,          intent(out)    :: ok
        integer :: ier
        ok=.false.
        do
          read(unit=lun,fmt="(a)",iostat=ier) texte
          if( ier /= 0) return
          texte=adjustl(texte)
          if(texte(1:1) == "!") cycle
          exit
        end do
        ok=.true.
        return
      End Subroutine Get_Texte

!--------------------------------------------------------------------
      Subroutine output_plot(nob,xx,fobs,sig,fcalc,chi2)
      integer, intent(in)              :: nob
      real,    intent(in),dimension(:) :: xx,fobs,fcalc,sig
      real,    intent(in)              :: chi2
      ! Local variables
      integer                          :: i,j,k,l,ico,ifinal
      real                             :: shb,shd,dif,yma,ymi,tofi,tofb
      real                             :: iposr

 !    Rewriting the input file
      open(Unit=8,file=trim(filecode)//".new",status="replace")
      ico=0
      if(c%constr) ico=1
      write(unit=8,fmt="(a)")   trim(title)
      write(unit=8,fmt="(a)")       &
      "!    TOF_init       TOF_fin      Nbac Npeak  Ncyc  Inst  Jobt  Cont Weight Corr Constr Percent "

      write(unit=8,fmt="(2f15.4,10i6)")  &
              ain,afin,n_ba,npeakx,c%icyc,itype,jobtyp,icont,c%iw,c%corrmax,ico,nint(c%percent)

      write(unit=8,fmt="(f14.6,a)") d2tof , "  <=  d to T.O.F. coefficient"
      write(unit=8,fmt="(a)") "!  Global Profile Parameters:"
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(1),vs%code(1), " <= Global-Alpha &  Flag "
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(2),vs%code(2), " <= Global-Beta  &       "
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(3),vs%code(3), " <= Sig-2        &       "
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(4),vs%code(4), " <= Sig-1        &       "
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(5),vs%code(5), " <= Sig-0        &       "
      write(unit=8,fmt="(f14.4,4x,i2,5x,a)")    vs%pv(6),vs%code(6), " <= Eta          &       "
      write(unit=8,fmt="(a)") "!  Background Parameters:"
      write(unit=8,fmt="(a)") "!       TOF        Background   Flag "
      do j=1,n_ba   !write background paramers
       write(unit=8,fmt="(2f14.4,i6)") BackGroundPoint(j)%x, vs%pv(j+ngl_tof), vs%code(j+ngl_tof)
      end do
      l=ngl_tof+n_ba+1
      write(unit=8,fmt="(a)") "!  Reflection Parameters:"
      write(unit=8,fmt="(a)") "!    TOF-Bragg     Intensity   Shift-sigma   Shift-alpha    Shift-beta    Shift-eta   Flags"
      do i=1,npeakx    !write peak parameters
        write(unit=8,fmt="(6f14.4,2x,6i2)") &
             vs%pv(l),vs%pv(l+1),vs%pv(l+2),vs%pv(l+3),vs%pv(l+4),vs%pv(l+5),(vs%code(l+k),k=0,5)
        l=l+6
      end do
      write(unit=8,fmt="(a,g14.6)") "!  Chi2 = ",chi2
      close(unit=8)

      yma= -1.E9   !
      ymi=  1.E9   !
      do i=1,nob
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      end do

       open(unit=22,file=trim(filecode)//".xys",status="replace")

        write(unit=22,fmt='(a)') 'XYDATA'
        write(unit=22,fmt='(a)') 'TITLE: '//trim(Title)
        write(unit=22,fmt='(a)') '! File saved from TOT-FIT'
        write(unit=22,fmt='(a)') '! File name: '//trim(filecode)
        write(unit=22,fmt='(a)') '! Scattering variable: TOF'
        write(unit=22,fmt='(a)') '! '//" Time-of-flight"//'     '//'Intensity'
        do  i=1,nob
          write(unit=22,fmt="(4f14.4)") xx(i),fobs(i),sig(i), fcalc(i)
        end do
      close (unit=22)
      return
      End subroutine output_plot

      Subroutine Input_Data(filename,iform,dif)
        character(len=*),    intent(in) :: filename
        Type(DiffPat_E_Type),intent(out):: dif
        integer,             intent(in) :: iform
        character (len=10) :: modem
        integer :: i
        Select Case(iform)
          case(0)
            modem="DEFAULT"
          case(1)
            modem="D1AOLD"
          case(2)
            modem="D1BOLD"
          case(3)
            modem="D1B"
          case(4)
            modem="NLS"
          case(5)
            modem="G41"
          case(6)
            modem="D2B"
          case(7)
            modem="3T2"
          case(8)
            modem="DMC"
          case(9,10)
            modem="XYSIGMA"
          case(12)
            modem="GSASTOF"
        End Select
        Call Read_Pattern(filename,dif,modem)
        if(Err_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
          if(Err_CFML%nl /= 0) then
            do i=1, Err_CFML%nl
              write(unit=*,fmt="(a)") trim(Err_CFML%txt(i))
            end do
          end if
         stop
        end if
      End Subroutine Input_Data

     End Module Input_output_data_mod
!----------------------------------------------------------------------------------
!------------------------------------------------------------------
!     Program to fit TOF powder patterns
!     Version 3.0  J.Rodriguez-Carvajal (June-2015)
!     The program fits selected profile zones
!     Marquardt fitting procedure with analytical derivatives
!------------------------------------------------------------------
!------------------------------------------------------------------
   Program TOF_fit
      use CFML_GlobalDeps, only: cp, Err_CFML
      use CFML_Optimization_LSQ
      use TOF_diffraction
      use Input_output_data_mod
      use CFML_DiffPatt,  only : DiffPat_E_Type

      Implicit None

      Integer               :: ll,ier,no,ifail
      Character (Len=256)   :: texte
      Character (Len=4)     :: ext
      Logical               :: esta, numeric,ok
      Integer               :: i,j,k,npts,L,ico

     ! character(len=120), ALLOCATABLE, DIMENSION(:)   :: scroll_lines
      Real                           :: timi,timf
      real,dimension(:), allocatable :: ww
      Integer                        :: narg,lr,ln
      Type(DiffPat_E_Type)           :: df  !Diffraction pattern

      !---- Arguments on the command line ----!
      lr=0
      ln=0
      narg=COMMAND_ARGUMENT_COUNT()
      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filecode)
              i=index(filecode,".pik")
              if(i /= 0) then
                 filecode=filecode(1:i-1)
                 ext=".pik"
              else
                 j=index(filecode,".new")
                 if(j /= 0) then
                    filecode=filecode(1:j-1)
                    ext=".new"
                 else
                    ext=".pik"
                 end if
              end if
              ln=len_trim(filecode)
              filedat=trim(filecode)//".dat"
              lr=ln+4
      end if
      if(narg > 1) then
              call GET_COMMAND_ARGUMENT(2,filedat)
              i=index(filedat,".")
              if(i == 0) filedat=trim(filedat)//".dat"
              lr=len_trim(filedat)
      end if
      numeric=.false.
      if(narg > 2) numeric=.true.
      !   Header and openning of files

      write(unit=*,fmt="(a)")" "
      WRITE(unit=*,fmt='(a)')'            ------------------------------------ '
      WRITE(unit=*,fmt='(a)')'                  --- PROGRAM: TOF-FIT ---'
      WRITE(unit=*,fmt='(a)')'            (Author: J. Rodriguez-Carvajal, ILL)'
      WRITE(unit=*,fmt='(a)')'                 (version 4.0 June - 2015) '
      WRITE(unit=*,fmt='(a)')'            ------------------------------------ '
      write(unit=*,fmt="(a)") " "

      if( lr == 0) then
         write(unit=*,fmt="(a)") " => Give the name of the input file (e.g. XX.pik or XX.new, <cr>=stop): "
         read(unit=*,fmt="(a)") filecode
         IF(len_trim(filecode) == 0) STOP
         i=index(filecode,".pik")
         if(i /= 0) then
            filecode=filecode(1:i-1)
            ext=".pik"
         else
            j=index(filecode,".new")
            if(j /= 0) then
               filecode=filecode(1:j-1)
               ext=".new"
            else
               ext=".pik"
            end if
         end if
         write(unit=*,fmt="(a)")  " => Give the name of the data file (e.g. xx.dat) "
         write(unit=*,fmt="(2a)") "             (<cr> =",trim(filecode)//".dat): "
         read(unit=*,fmt="(a)") texte
         if(len_trim(texte) == 0) then
            filedat=trim(filecode)//".dat"
         else
            filedat=texte
         end if
      end if
      Open(Unit=1,File=Trim(Filecode)//ext,STATUS="old",iostat=ier,action="read",position="rewind")
      if( ier /= 0 ) then
        write(unit=*,fmt="(a,a)") " => Error openning ",trim(filecode)//ext
        STOP
      end if
      IF(len_trim(filedat) == 0) filedat=trim(filecode)//".dat"
      !-------------------Start Program
      c%percent=50.0
      !      Read the input control file and write in output files

   Do   ! repeat until icont = 0 !

     call get_texte(1,texte,ok)
     if(ok) then
       read(unit=texte,fmt="(a)",iostat=ier)title
       if(ier /= 0) then
          write(unit=*,fmt="(a)") " => End of pik input file at reading text: title! "
          stop
       end if
     else
          write(unit=*,fmt="(a)") " => Error in pik input file at reading TITLE ! "
          stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     call get_texte(1,texte,ok)
     if(ok) then
         read(unit=texte,fmt=*,iostat=ier) ain,afin,n_ba,npeakx,c%icyc,itype,jobtyp,icont,c%iw, c%corrmax, ico, c%percent
         if(ier /= 0) then
           write(unit=*,fmt="(a)") " => Error at reading text: tof_ini, tof_fin, n_ba, etc ! "
           stop
         end if
     else
         write(unit=*,fmt="(a)") " => Error in pik input file at reading: tof_ini, tof_fin, n_ba, etc ! "
         stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     if(c%corrmax < 1.0) c%corrmax=50.0
     if (ico/=0) c%constr = .true.
     npeaks=npeakx
     call get_texte(1,texte,ok)
     if(ok) then
         read(unit=texte,fmt=*,iostat=ier) d2tof
         if(ier /= 0) then
           write(unit=*,fmt="(a)") " => Error at reading text: d2tof ! "
           stop
         end if
     else
         write(unit=*,fmt="(a)") " => Error in pik input file at reading: d2tof ! "
         stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     if(n_ba > nbac) then
       write(unit=*,fmt="(a)") "  ! Warning: too many background parameters !"
       stop
     end if

     do j=1,ngl_tof         !read global parameters
       call get_texte(1,texte,ok)
       if(ok) then
           read(unit=texte,fmt=*,iostat=ier) vs%pv(j), vs%code(j)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i2)") " => Error at reading text: global parameter #",j
             stop
           end if
       else
         write(unit=*,fmt="(a,i2)") " => Error in pik input file at reading global parameter #",j
         stop
       end if
       write(*,"(a)") " => Line: "//trim(texte)
     end do
     do j=1,n_ba        !read background parameters
        call get_texte(1,texte,ok)
        if(ok) then
           read(unit=texte,fmt=*,iostat=ier) BackGroundPoint(j)%x, vs%pv(j+ngl_tof), vs%code(j+ngl_tof)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i3)") " => Error at reading text: background point #",j
             stop
           end if
           BackGroundPoint(j)%y = vs%pv(j+ngl_tof)
        else
           write(unit=*,fmt="(a,i3)") " => Error in pik input file at reading background point #",j
           stop
        end if
        write(*,"(a,i3)") " => Line: "//trim(texte)//"   Back#",j
     end do
     if(ain > BackGroundPoint(1)%x )     ain=BackGroundPoint(1)%x
     if(afin < BackGroundPoint(n_ba)%x ) afin=BackGroundPoint(n_ba)%x

     j=ngl_tof+n_ba+1
     npeaks_rf=0
     DO i=1,npeaks       !read peak parameters
      call get_texte(1,texte,ok)
      if(ok) then
           read(unit=texte,fmt=*,iostat=ier) vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),vs%pv(j+4),vs%pv(j+5),(vs%code(j+k),k=0,5)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i3)") " => Error at reading text: peak #",i
             stop
           end if
      else
           write(unit=*,fmt="(a,i3)") " => Error in pik input file at reading peak #",i
           stop
      end if
      write(*,"(a,i3)") " => Line: "//trim(texte)//"   Peak#",i

      if(sum(vs%code(j:j+5)) > 0) npeaks_rf=npeaks_rf+1
      j=j+6
     END DO

     call set_nampar_tof(n_ba,npeaks)

      !   Read intensity data

      Call Input_Data(filedat,itype,df)
      call cpu_time(timi)
      Call Tof_Profile_Fitting(Filedat, ain,afin,df, Ifail)
      call cpu_time(timf)
      call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,d%sw,7,c,vs)
      close(unit=1)
      call Output_Plot(d%nobs,d%x,d%y,d%sw,d%yc,chi2)
      if(icont == 0) exit
   End Do  !icont
   stop
   End Program TOF_fit
!------------------------------------------------------------------------
