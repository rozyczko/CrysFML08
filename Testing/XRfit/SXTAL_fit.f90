  Module Input_output_data_mod
    use CW_diffraction_PV, only: rla,ngl,n_ba,bac_pos, jobtyp, inter, vs, c,  &
                                 icont, rla1,rla2, filedat, NPEAKX,title, eta1,eta2, &
                                 fwhm1,fwhm2, filecode, use_asymm,use_hps
    use CFML_GlobalDeps,   only: Err_CFML,cp
    use CFML_Optimization_LSQ

    implicit none
    private
    !Public procedures
    public  :: get_texte, output_plot
    private :: Backa
    ! Global variables
    real,    public :: thmin,step,thmax,ain,afin

    contains

    Function Backa(tth) result(back)
       real(kind=cp), intent(in)    :: tth
       Real(kind=cp)                :: Back
       integer                      :: ib1,ib2,i1,i2, ib
       real(kind=cp)                :: tang

       !  Calculation of the background
       i1=1
       i2=n_ba         ! nb de points de bruit de fond
       ib1=ngl+1       ! nb de parametres globaux
       ib2=ib1+1       ! nb de parametres globaux + 1

       do ib=1,n_ba-1
         if(tth >= bac_pos(ib) .and. tth <= bac_pos(ib+1)) then
           i1=ib
           i2=ib+1
           ib1=ngl+i1
           ib2=ib1+1
           exit
         end if
       end do
       tang=(tth-bac_pos(i1))/(bac_pos(i2)-bac_pos(i1))
       Back=vs%pv(ib1)+(vs%pv(ib2)-vs%pv(ib1))*tang
    End Function Backa

    Subroutine Get_Texte(lun,texte)
      integer,intent(in)               :: lun
      character(len=*), intent(in out) :: texte
      do
        read(unit=lun,fmt="(a)") texte
        texte=adjustl(texte)
        if(texte(1:1) == "!") cycle
        exit
      end do
    End Subroutine Get_Texte

    Subroutine Output_Plot(nobs,xx,fobs,fcalc,nref,chi2,refl)
      integer,                     intent(in) :: nobs, nref
      real,dimension(:),           intent(in) :: xx,fobs,fcalc
      real,                        intent(in) :: chi2
      character (len=*),           intent(in) :: refl
      ! Local variables
      integer                          :: i,j,k,l,inum,ki,kj,ico,ifinal,i_plot=22
      real                             :: shb,shd,dif,yma,ymi,twtet,twtetb
      real                             :: iposr
      character (len=90)  :: refplot


      yma= -1.E9   !
      ymi=  1.E9   !
      Do i=1,nobs
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      End Do
      write(refplot,"(i6.6)") nref
       Open(Unit=i_plot,File=trim(refplot)//".xrf",status="replace",action="write")

        shb   = 0.0                         ! idem wpl_pfit.f90
        shd   = ymi - 0.2*(yma-ymi)
        iposr = ymi - 0.1*(yma-ymi)

        !write(unit=i_plot,fmt="(2A)") " ",TRIM(title)
        write(unit=i_plot,fmt="(a)") trim(refl)
        write(unit=i_plot,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
        write(unit=i_plot,fmt="(a,I4)")    " => Instrm        : ", 0
        write(unit=i_plot,fmt="(a,2f9.5)") " => Lambda(1&2)   : ", rla1,rla2
        write(unit=i_plot,fmt="(a,I9)")    " => Numb.of.points: ", nobs
        write(unit=i_plot,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
        write(unit=i_plot,fmt="(a)")   &
        "    2Theta     Yobs     Ycalc  Yobs-Ycal    Backg    Bragg     Posr  Intensity   FWHM    Eta"
        IF(npeakx > nobs) THEN
          ifinal=nobs
        ELSE
          ifinal=npeakx
        END IF
        j=ngl+n_ba+1
        Do  i=1,ifinal
          twtet=xx(i)
          dif=fobs(i)-fcalc(i)+shd
          twtetb=vs%pv(j)
          write(unit=i_plot,fmt="(f10.4,4f10.2,f10.4,f10.2,tr1,f10.2,2f8.4)")  &
               twtet,fobs(i),fcalc(i), dif, backa(twtet)-shb,  &
               twtetb,iposr, vs%pv(j+1), fwhm1(i), eta1(i)
          j=j+4
        End Do

        If(npeakx+1 < nobs) THEN
          Do  i=npeakx+1,nobs
            twtet=xx(i)
            dif=fobs(i)-fcalc(i)+shd       ! shd < 0.
            write(unit=i_plot,fmt="(f10.4,4f10.2)") twtet,fobs(i),fcalc(i),dif,  &
                backa(twtet)-shb
          End Do
        End If
      close (UNIT=i_plot)
    End Subroutine Output_Plot


  End Module Input_output_data_mod
  !----------------------------------------------------------------------------------
  !------------------------------------------------------------------
  !     PROGRAM TO FIT POWDER PATTERNS
  !     VERSION 2.1  J.RODRIGUEZ-CARVAJAL (APRIL-1988)
  !     THE PROGRAM FITS SELECTED PROFILE ZONES
  !     MARQUARDT FITTING PROCEDURE WITH ANALYTICAL DERIVATIVES
  !------------------------------------------------------------------
  !------------------------------------------------------------------
  Program SXTAL_fit
    use CFML_GlobalDeps, only: cp, Err_CFML
    use CFML_Optimization_LSQ
    use CW_diffraction_PV
    use Input_output_data_mod
    use CFML_Profiles, only : Init_Prof_Val

    Implicit None

    Integer                   :: ll,ier,no,np,i,j,k,npeak,npts,L,narg
    Integer                   :: lun=1, i_out=7, i_hkl=8
    integer, dimension (256)  :: num_pix
    Logical                   :: esta
    Real(kind=cp)             :: chi2, timi,timf,sumpix,aver,normp, norm_mon, norma
    real(kind=cp)             :: gamm, omega, nu, chi, phi, monitor, inten, sigma,aux1,aux2
    real(kind=cp),dimension(3):: hkl

    Character (Len=512)                     :: texte
    Character (Len=90)                      :: refl,testchi2,line
    Character (Len=80), dimension(256)      :: profile
    real(kind=cp),dimension(:), allocatable :: ww

    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
            call GET_COMMAND_ARGUMENT(1,filecode)
            i=index(filecode,".ref")
            if(i /= 0) filecode=filecode(1:i-1)
            filedat=trim(filecode)//".ref"
    else
          write(unit=*,fmt="(a,a)") " => The program should be invoked with an argument like MyRef_File.ref"
          stop
    end if
    !   Header and openning of files

    write(unit=*,fmt="(a)")" "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)")"                --- PROGRAM: SXTAL_FIT ---"
    write(unit=*,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=*,fmt="(a)")"                    (Created April 2021) "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)") " "

    Open(Unit=lun,File=Trim(Filecode)//".ref",STATUS="old",iostat=ier,action="read",position="rewind")
    if( ier /= 0 ) then
      write(unit=*,fmt="(a,a)") " => Error openning ",trim(filecode)//".ref"
      Stop
    end if
    Open(Unit=i_out, File=trim(filecode)//".out",status="replace",action="write")
    Open(Unit=i_hkl, File=trim(filecode)//".hkl",status="replace",action="write")
    !-------------------Start Program
    c%percent=50.0
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "
    write(unit=i_out,fmt="(a)")"                --- PROGRAM: SXTAL_FIT ---"
    write(unit=i_out,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=i_out,fmt="(a)")"                    (Created April 2021) "
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "

    !      Read the input control file and write in output files
    np=0
    title="SXTAL integration for reflections in file: "//Trim(Filecode)//".ref"
    call cpu_time(timi)
    do
      read(unit=lun,fmt="(a)",iostat=ier) texte
      texte=adjustl(texte)
      if(texte(1:12) == "NORM_MONITOR") then
         read(texte(13:),*,iostat=ier)  norm_mon
         if(ier /= 0) norm_mon=100000.0
         exit
      end if
    end do
    Do ! repeat until icont = 0 !

      read(unit=lun,fmt="(a)",iostat=ier) texte
      if(ier /= 0) exit
      if(trim(texte) == "REFLECTION") then
         read(unit=lun,fmt="(a)",iostat=ier) texte
         if(ier /= 0) exit
         read(unit=texte,fmt=*,iostat=ier) hkl,gamm,omega,nu,chi,phi,monitor
         if(ier /= 0) then
            write(unit=*,fmt="(a)") " => Problem reading hkl, gamma, omega, nu, chi, phi, monitor ! "
            exit
         else
            np=np+1
            write(unit=i_out,fmt="(/,a,i6,a)") " => Reflection: #",np,"    hkl gamma, omega, nu, chi, phi, monitor: "//trim(texte)
         end if
         no=0
         do
            read(lun,"(a)",iostat=ier) line
            if(ier /= 0)  exit
            if(trim(line) == "REFLECTION") then
              no=no-1
              backspace(unit=lun)
              exit
            end if
            read(line,*) aux1,aux2,L  !Eliminate points with zero pixels
            if(L == 0) cycle
            no=no+1
            profile(no)=line
         end do
         if(no < 10) cycle
         if (Allocated(d%x ))   deallocate (d%x)
         if (Allocated(d%sw))   deallocate (d%sw)
         if (Allocated(d%y ))   deallocate (d%y)
         if (Allocated(d%yc))   deallocate (d%yc)
         if (Allocated(ww))     deallocate (ww)
         allocate ( d%x(no),d%sw(no),d%y(no),d%yc(no),ww(no) )
         d%nobs=no
         ww=0.0
         sumpix=0.0
         do i=1,d%nobs
            read(unit=profile(i),fmt=*) d%x(i),d%y(i),num_pix(i)
            d%sw(i)=sqrt(d%y(i))
            sumpix=sumpix+num_pix(i)
         end do
         aver=sumpix/no
         step=0.0
         do i=1,d%nobs
           if(i > 1)step=step + abs(d%x(i)-d%x(i-1))
           normp=aver / num_pix(i)
           d%y(i) =  d%y(i) * normp
           d%sw(i)= d%sw(i) * normp
           if(abs(d%sw(i)) > 0.001) ww(i)=1.0/(d%sw(i)*d%sw(i))
         end do
         step=step/no
         ! Initial values
         norma=abs(sind(gamm) * cosd(nu) * (d%nobs-1)*step) * norm_mon/monitor
         ain=d%x(1)
         afin=d%x(no)
         n_ba=2
         bac_pos(1)    = d%x(1)
         bac_pos(n_ba) = d%x(no)
         npeakx=1
         c%icyc=50
         c%iw=0
         c%corrmax=50.0
         c%constr = .true.
         c%percent=50.0
         npeak=npeakx

         vs%pv(1:ngl)=0.0
         vs%code(1:ngl)=0
         vs%code(ngl+1:ngl+2)=1

         j=ngl+n_ba+1
         vs%pv(j)  = omega                 !Position
         vs%pv(j+1)= d%y(no/2)             !Intensity
         vs%pv(j+2)= 0.25*(d%x(no)-d%x(1)) !Fwhm
         vs%pv(j+3)= 0.0                   !Eta
         vs%code(j:j+2)=1
         !Do i=1,npeak       !read peak parameters
         ! call get_texte(1,texte)
         ! read(unit=texte,fmt=*) vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),(vs%code(j+k),k=0,3)
         ! j=j+4
         !End Do
         write(unit=i_out,fmt="(/,a)")        " => Omega range, peaks and cycles read "
         write(unit=i_out,fmt="(a,tr1,f12.6)")" => Omega(init)    :",ain
         write(unit=i_out,fmt="(a,tr1,f12.6)")" => Omega(fin )    :",afin
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of peaks :",npeak
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of background points :",n_ba
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of cycles:",c%icyc
         rla2=0.0
         rla1=0.0
         vs%np = ngl+ n_ba + 4 * npeak
         use_asymm=.false.
         use_hps=.false.

         !save input data
         j = 0
         do i=1,vs%np
          if (vs%code(i) /=0) j = j + 1
         end do
         c%npvar  = j    ! number of refined parameters

         !write(unit=i_out,fmt="(/,a)")          " => Global parameters                 Flag"
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Kalph2/Kalph1 ratio :", vs%pv(1),vs%code(1)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Asymmetry-1(S_L)    :", vs%pv(2),vs%code(2)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Asymmetry-2(D_L)    :", vs%pv(3),vs%code(3)
         !write(unit=i_out,fmt="(/,a)")          "      Profile Parameters for Pseudo-Voigt"
         !write(unit=i_out,fmt="(a)")            "        pV(x) = ETA* L(x) + (1-ETA)* G(x)"
         !write(unit=i_out,fmt="(a)")            "    FWHM = SQRT((U tanT + V) tanT + W) + Z/cosT"
         !write(unit=i_out,fmt="(a,/)")          "    ETA  = Eta0 + X * 2T "
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  U        :", vs%pv(4),vs%code(4)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  V        :", vs%pv(5),vs%code(5)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  W        :", vs%pv(6),vs%code(6)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  Z        :", vs%pv(7),vs%code(7)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter Eta0      :", vs%pv(8),vs%code(8)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  X        :", vs%pv(9),vs%code(9)
         !write(unit=i_out,fmt="(/,a)")          " => Background parameters"
         !write(unit=i_out,fmt="(a)")            "      Scatt. Variable     Background  Flag"
         do j=1,n_ba
           write(unit=i_out,fmt="(2f18.4,i4)")   bac_pos(j), vs%pv(j+ngl),vs%code(j+ngl)
         end do

         write(unit=i_out,fmt="(/,a,/)")                                                    &
              "      Position      Intensity      D-Gamma         D-Eta            Flags"
         j=ngl+n_ba+1

         DO i=1,npeak
           write(unit=i_out,fmt="(f14.6,f14.2,2F14.6,tr5,4i3)") vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),   &
                                       vs%code(j),vs%code(j+1),vs%code(j+2),vs%code(j+3)
           j=j+4
         END DO
         call set_nampar(n_ba,npeak,vs)
         write(unit=i_out,fmt= "(/,a,i4,/)") " => Total number of refined parameters: ", c%npvar
         d%iw=0
         c%tol=epsilon(0.0_cp)
         !write(unit=*,fmt="(a,i6    )") " => Number of points: ",no
         !write(unit=*,fmt="(a,2f14.4)") " => Treating data in range: ",ain,afin
         !write(unit=*,fmt="(a,2f14.4)") " => Background in range: ",bac_pos(1),bac_pos(n_ba)

         call Init_Prof_Val()
         call Levenberg_Marquardt_Fit(powder_patt_der, d%nobs, c, Vs, chi2, .true., texte)
         !call Marquardt_Fit(Sum_PV_Peaks,d,c,vs,i_out,chi2)
         !write(unit=*,fmt="(a)") " => "//texte(1:41)
         !write(unit=*,fmt="(a)") " => "//texte(41:133)
         !write(unit=*,fmt="(a)") " => "//trim(texte(133:))
         write(unit=testchi2,fmt="(a,i6,a,f15.5)") "    Reflection: ",np," => Final   Chi2:", chi2
         !write(unit=*,fmt=*) " => Function and Jacobian evaluations: ",c%nfev,c%njev
         !if(Err_CFML%Ierr /= 0) stop
         call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,"Analytical Levenberg-Marquardt")
         !call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,"CURFIT")
         j=ngl+n_ba+1
         inten=vs%pv(j+1)*norma
         sigma=vs%spv(j+1)*norma*sqrt(chi2)
         write(unit=*,fmt="(a,3i4, 2f14.4)")    trim(testchi2)//"    hkl, Intens, Sigma: ",nint(hkl),Inten,sigma
         write(unit=i_hkl,fmt="(3i4,2f14.4,a)") nint(hkl),Inten,sigma,  "          # "//trim(testchi2)
         write(refl,"(a,3i4,2(a,f12.2))") "Reflection:",nint(hkl),",   Intensity:",Inten,",  Sigma:",sigma
         call Output_Plot(d%nobs,d%x,d%y,d%yc,np,chi2,refl)
      end if

    End Do  !icont
    call cpu_time(timf)
    write(unit=*,fmt="(a,f10.3,a)") " => CPU-time:", timf-timi," seconds"

 End Program SXTAL_fit