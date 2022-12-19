!!----
!!----
!!----
!!----
 Module Atoms_in_BOX
   use CFML_Globaldeps
   use CFML_Atoms,        only: AtList_Type, ModAtm_Std_Type
   use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Point_Orbit, get_Orbit, Is_Lattice_Vec
   use CFML_Strings,      only: pack_string
   use CFML_rational
   use CFML_Maths,       only: equal_vector,Lat_Modulo,EPSS, Zbelong

   implicit none
   public

   !!  Type, public :: Point_Orbit
   !!     integer                                      :: Mult=0 ! Multiplicity of the orbit
   !!     real(kind=cp),allocatable,dimension(:,:)     :: pos    ! (d,Mult) Positions of the points
   !!     real(kind=cp),allocatable,dimension(:,:)     :: mom    ! (d,Mult) Associated moments
   !!     integer,      allocatable,dimension(:)       :: pts    ! (  Mult) Pointer to symmetry operator
   !!     integer,      allocatable,dimension(:,:)     :: Lat    ! (d,Mult) lattice translation used to
   !!  End Type Point_Orbit                                      ! put the atom  g(i).pos(:,1) within the cell
   !!                                                            ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)


   Type, public :: Orbit_type
      integer                                       :: Mult=0      ! Multiplicity of the orbit
      character(len=2)                              :: ChemSymb    ! Chemical symbol
      character(len=20),   allocatable, dimension(:):: lab         ! Atom label
      real(kind=cp),allocatable,dimension(:,:)      :: pos         ! (3,Mult) Positions of the points
      integer,      allocatable,dimension(:)        :: pts         ! (  Mult) Pointer to symmetry operator
      integer,      allocatable,dimension(:,:)      :: Lat         ! (3,Mult) lattice translation used to
   End Type Orbit_type                                                  ! put the atom  g(i).pos(:,1) within the cell
                                                                   ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)

   type, Extends (Point_Orbit) :: orbit_ssp
     character(len=20), dimension(:),   allocatable :: lab         !Atom label
     character(len=2) , dimension(:),   allocatable :: ChemSymb    !Chemical symbol
     integer,           dimension(:),   allocatable :: Ls          !Pointer to a lattice translation
     integer,           dimension(:,:), allocatable :: Latt        !Additional lattice translation to the operator
   end type orbit_ssp

   type :: orbit_list
     integer :: num_orbs
     type(orbit_ssp), dimension(:), allocatable :: orbit
   end type orbit_list

   Contains

   !! Module Subroutine Get_Orbita(x,Spg,orbit,mom,orb3D,convl,Tbox)
   !!
   !!---- Subroutine to generate the orbit of a point x in space/superspace
   !!---- If mom is not present then the orbit%mom components remain unallocated
   !!---- If orb3D is present, the restriction to 3D space is also output.
   !!---- If convl is present and convl=.false., only the orbit within a primitive
   !!---- cell is output. If Tbox is present, the orbit is extended to a supercell
   !!---- that is [ Tbox(1) x a, Tbox(2) x b, Tbox(3) x c ]  by adding all possible
   !!---- translations to the Zero-cell orbit
   !!----
   !!---- The orb object is of type Point Orbit defined in the general module
   !!---- and repeated here for the sake of proper documentation.
   !!  Type, public :: Point_Orbit
   !!     integer                                      :: Mult=0 ! Multiplicity of the orbit
   !!     real(kind=cp),allocatable,dimension(:,:)     :: pos    ! (d,Mult) Positions of the points
   !!     real(kind=cp),allocatable,dimension(:,:)     :: mom    ! (d,Mult) Associated moments
   !!     integer,      allocatable,dimension(:)       :: pts    ! (  Mult) Pointer to symmetry operator
   !!     integer,      allocatable,dimension(:,:)     :: Lat    ! (d,Mult) lattice translation used to
   !!  End Type Point_Orbit                                      ! put the atom  g(i).pos(:,1) within the cell
   !!                                                            ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)
   !!
   Subroutine Get_Orbita(x,Spg,orbit,tbox)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      class(SpG_Type),                      intent(in)  :: spg
      type(Point_Orbit),                    intent(out) :: orbit
      integer,       dimension(3),optional, intent(in)  :: tbox

      !---- Local variables ----!
      integer                                          :: j, nt,d,mult,L,mtot,i1,i2,i3,k
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),   allocatable       :: ptr
      integer,       dimension(:,:), allocatable       :: trn

      d=Spg%d-1
      xs(Spg%d)=1.0_cp
      xs(1:3)=x(1:3)
      orb=0.0_cp

      do j=1,Spg%Multip
        Om(:,:,j)=Spg%Op(j)%Mat
      end do

      allocate(ptr(Spg%multip))
      ptr=0
      mult=1
      orb(:,mult)=xs(1:d)
      Lat(:,mult)=0
      ptr(mult) = 1   !Pointer to symmetry operator
      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:3)=orb(1:3,nt)-xsp(1:3)
            if(sum(abs(v(1:3))) < 2.0 * EPSS) cycle do_ext
            if (Zbelong(v(1:3))) cycle do_ext
         end do
         mult=mult+1
         orb(:,mult)=xsp(1:d)
         Lat(:,mult)=Latt(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(tbox)) then
         nt=tbox(1)*tbox(2)*tbox(3)
         allocate(trn(3,nt))
         trn=0
         nt=0
         do i1=0,tbox(1)-1
           do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
               nt=nt+1
               trn(:,nt)=[i1,i2,i3]
             end do
           end do
         end do

         mtot=mult*nt
         orbit%Mult=mtot
         allocate (orbit%pos(3,mtot),orbit%pts(mtot),orbit%Lat(3,mtot))
         orbit%pos=0.0; orbit%Lat=0
         k=0
         do L=1,nt
           do j=1,Mult
             k=k+1
             orbit%pos(1:3,k) = orb(1:3,j) + trn(:,L)
             orbit%pts(    k) = ptr(j)
             orbit%Lat(1:3,k) = Lat(1:3,j) + trn(:,L)
           end do
         end do
      else
         orbit%Mult=mult
         orbit%pos=orb(:,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(:,1:mult)
      end if

   End Subroutine Get_Orbita

   Subroutine Get_Orbit3D(x,Lab,Chem,Spg,orbit,tbox)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      character(len=*),                     intent(in)  :: Lab,Chem
      class(SpG_Type),                      intent(in)  :: spg
      type(Orbit_type),                     intent(out) :: orbit
      integer,       dimension(3),optional, intent(in)  :: tbox

      !---- Local variables ----!
      integer                                          :: j, nt,d,mult,L,mtot,i1,i2,i3,k
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),   allocatable       :: ptr
      integer,       dimension(:,:), allocatable       :: trn
      character(len=20)                                :: label

      d=Spg%d-1
      xs=0.0
      xs(Spg%d)=1.0_cp
      xs(1:3)=x(1:3)
      orb=0.0_cp

      do j=1,Spg%Multip
        Om(:,:,j)=Spg%Op(j)%Mat
      end do
      Write(*,"(a)") " Operators assigned"
      allocate(ptr(Spg%multip))
      ptr=0
      mult=1
      orb(:,mult)=xs(1:d)
      Lat(:,mult)=0
      ptr(mult) = 1   !Pointer to symmetry operator
      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:3)=orb(1:3,nt)-xsp(1:3)
            if(sum(abs(v(1:3))) < 1.0 * EPSS) cycle do_ext
         end do
         mult=mult+1
         orb(:,mult)=xsp(1:d)
         Lat(:,mult)=Latt(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(tbox)) then
         nt=tbox(1)*tbox(2)*tbox(3)
         allocate(trn(3,nt))
         trn=0
         nt=0
         do i1=0,tbox(1)-1
           do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
               nt=nt+1
               trn(:,nt)=[i1,i2,i3]
             end do
           end do
         end do

         mtot=mult*nt
         orbit%Mult=mtot
         allocate (orbit%pos(3,mtot),orbit%pts(mtot),orbit%lab(mtot),orbit%Lat(3,mtot))
         orbit%pos=0.0; orbit%Lat=0; orbit%lab=" "; orbit%ChemSymb=Chem

         k=0
         do L=1,nt
           do j=1,Mult
             k=k+1
             orbit%pos(1:3,k) = orb(1:3,j) + trn(:,L)
             orbit%pts(    k) = ptr(j)
             orbit%Lat(1:3,k) = Lat(1:3,j) + trn(:,L)
             label=" "
             write(unit=label,fmt="(a,i5)")  trim(Lab)//"_",k
             orbit%lab(k)=pack_string(label)
           end do
         end do
      else
         allocate(orbit%lab(mult))
         do j=1,Mult
           label=" "
           write(unit=label,fmt="(a,i5)")  trim(Lab)//"_",j
           orbit%lab(j)=pack_string(label)
         end do
         orbit%ChemSymb=Chem
         orbit%Mult=mult
         orbit%pos=orb(1:3,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(1:3,1:mult)
      end if

   End Subroutine Get_Orbit3D


    Function modulation_function(At,SpG,s,Lat,time_v,tshift) result(v)
      type(ModAtm_Std_Type),        intent(in) :: At     !Modulated atom type
      type(SuperSpaceGroup_type),   intent(in) :: SpG
      integer,                      intent(in) :: s      !Pointer to the symmetry operator relating x and x0: x= g(s) x0 + t(s) + Lat
      integer,       dimension(:),  intent(in) :: Lat    !Additional lattice translation
      logical,                      intent(in) :: time_v !if .true. magnetic modulation
      real(kind=cp), dimension(3)              :: v      !Modulation at position x= g(s) At%x + ts + Lat
      real(kind=cp), dimension(:), optional, intent(in) :: tshift      !tshift   xI= tshift + (mE, rI)

      !---- Local variables ----!
      real(kind=cp)                      :: xarg
      real(kind=cp), dimension(SpG%nk)   :: rI,tI,xI     !rI = Op^(-1) . xS   Internal transformed space vector from xI=(x4, x5, ... x3+d) at new position
      real(kind=cp), dimension(SpG%d-1)  :: ts           ! ts=(t,tI) Total translation in superspace operator
      integer                            :: j,s_inv, d, ds
                                                                                          !                         /  g    0   t  \
                                                         ![m].inv_Eg  nk-vector           !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(SpG%nk,3)       :: Hg           !Hg(nk,3)                        !                         \  0    0   1  /
      integer, dimension(3,3)            :: g, magm      !g, magm= delta * det(g) * g     !                         /  inv_g    0      it  \
      integer, dimension(SpG%nk,SpG%nk)  :: inv_Eg       !E (nk,nk)                       ! Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
      real(kind=cp), dimension(SpG%nk)   :: tsh          !  xsi= ki (x + L) + tsh         !                         \   0       0       1  /
      real(kind=cp), dimension(3)        :: x


      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices
      tsh=0.0
      if(present(tshift)) tsh=tshift
      g(:,:) = SpG%Op(s)%Mat(1:3,1:3)
      if(time_v) magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
      ts(:) = SpG%Op(s)%Mat(1:d,ds)
      s_inv = SpG%inv(s)                         !   Pointer to Inverse operator
      inv_Eg(:,:) = SpG%Op(s_inv)%Mat(4:d,4:d)   ! See the expression of the inverse operator
          Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3)
      tI(:) = ts(4:d)            !Internal translation of the current operator
      x=matmul(g,At%x)+ts(1:3)+Lat
      xI  = matmul(x,SpG%kv)-matmul(Hg,At%x)-ts(4:d) + tsh
      rI  = matmul(inv_Eg, xI)
      xarg=0.0; v=0.0
      if(time_v) then
        do j=1,At%n_mc !Moment amplitudes
           xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
           v(1:3)=v(1:3)+at%Mcs(1:3,j)*cos(xarg) + at%Mcs(4:6,j)*sin(xarg)
        end do
        v=matmul(magm,v)
      else
        do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
           xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
           v(1:3)=v(1:3)+at%Dcs(1:3,j)*cos(xarg) + at%Dcs(4:6,j)*sin(xarg)
        end do
        v=matmul(g,v)
      end if
    End Function modulation_function

    Subroutine Get_ModAtoms_inBOX(A,SpG,TBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3), intent(in)    :: TBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, s,nt,d, ds, i1,i2,i3,n_mO, &
                                                          s_inv,n_tr, n_orb
      real(kind=cp)                                    :: xarg
      real(kind=cp), dimension(3)                      :: xs,xsp,xst,v
      real(kind=cp), dimension(3)                      :: u_mu,m_mu, u_nu,m_nu
      real(kind=cp), dimension(3)                      :: ms,msp
      integer,      dimension(:,:),   allocatable      :: Lat,Lst !lattice translations
      real(kind=cp), dimension(:,:) ,  allocatable     :: orb,morb,orb3D !Temporal orbits
      real(kind=cp), dimension(:),     allocatable     :: rI, xI,Ls,tI  !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)     :: ts            ! ts=(t,tI)  Total translation in superspace
      integer, dimension(3)                 :: Lati       ![m].M  3-components vector      !                         /  g    0   t  \
      integer, dimension(:),   allocatable  :: mE,pt         ![m].Eg  nk-vector               !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(:,:), allocatable  :: Hg            !M (nk,3)                        !                         \   0    0   1 /
      integer, dimension(3,3)               :: g, magm       !g, magm= delta * det(g) * g
      integer, dimension(:,:), allocatable  :: Eg,inv_Eg     !Eg (nk,nk)

      ! Transformation Equation for general vector modulation function
      !
      !   p(nu)[ Hg . rE(mu)+Eg rI(mu) + tI] = g_a p(mu)[rI(mu)]
      !   p(nu)[ rI(nu) ] = g_a p(mu)[inv_Og rI(mu)] = g_a p(mu) [Ng (rE-t) + inv_Eg (rI(mu)-tI) ]
      !
      ! Where g_a= g for polar displacement modulations and g_a=magm for magnetic moment modulations
      !
      !   rI=(x4, x5, ... x3+d)
      !
      !   xj=qj.r(nu)    r(nu,L) = x(nu)+L    xjL= qj ( x(nu)+L )  j=4,...3+d
      !
      !  The general expression of the modulation functions in terms of Fourier series is:
      !
      !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]) cos[2pi [m](Eg^-1).(rI(mu)-Hg rE(m) - tI )] + Ps(mu)^([m]) sin[2pi [m](Eg^-1).(rI(mu)-Hg rE(m) - tI )] ]
      !
      !Calculation of the maximum number of additional lattice translations
      nt=TBOX(1)*TBOX(2)*TBOX(3)
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices

      !Allocation of main arrays
      Select Type(SpG)
        type is(SuperSpaceGroup_Type)
          allocate(mE(SpG%nk),    Hg(SpG%nk,3),    Eg(SpG%nk,SpG%nk), rI(SpG%nk))
          allocate(inv_Eg(SpG%nk,SpG%nk),xI(SpG%nk),tI(SpG%nk))
      End Select
      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      allocate(Lat(3,nt)) !Allocate lattice translations in superspace
      nt=0
      do i1=0,TBOX(1)-1
        do i2=0,TBOX(2)-1
           do i3=0,TBOX(3)-1
              nt=nt+1
              Lat(:,nt) = real([i1,i2,i3],kind=cp)  !External space
           end do
        end do
      end do
      n_mO=2*Spg%multip*nt
      n_tr=nt

      !Generate the orbits of all atoms

      do i=1,A%natoms
        xs=0.0; ms=0.0
        xsp=0.0; msp=0.0
        if(allocated(orb)) deallocate(orb)
        if(allocated(orb3D)) deallocate(orb3D)
        if(allocated(morb)) deallocate(morb)
        if(allocated(pt)) deallocate(pt)
        if(allocated(Ls)) deallocate(Ls)
        if(allocated(Lst)) deallocate(Lst)
        allocate(orb(3,n_mO),morb(3,n_mO),pt(n_mO),Ls(n_mO),orb3D(3,n_mO),Lst(3,n_mO))
        orb=0.0; orb3D=0.0; Ls=0; pt=0
        morb=0.0
        pt=1
        Select Type(at => A%Atom(i))

          class is (ModAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
                n_orb= 0
                xs   = At%x
                ms=At%Moment
                do_s: do s=1,SpG%Multip
                    s_inv=SpG%inv(s)
                    g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
                    magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                                     !                         /  g    0   t  \
                   ts(:) = SpG%Op(s)%Mat(1:d,ds)     !   Superspace operator: |  Hg   Eg   tI  |    ts=(t,tI)
                   Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)  !                         \  0    0   1  /
                   Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3)
                 inv_Eg(:,:) = SpG%Op(s_inv)%Mat(4:d,4:d)

                   xsp = matmul(g,xs)+ts(1:3)
                   call Lat_Modulo(xsp,v,lati)
                   xsp=v
                   msp = matmul(magm,ms)

                   !Applying translations
                   do_L:do L=1,n_tr
                      u_mu= 0.0; m_mu=0.0
                      xarg= 0.0
                      xst = xsp  + Lat(:,L)
                      do m=1,3
                        if(xst(m) < 0.0 .or. xst(m) > tbox(m)) cycle do_L
                      end do
                      do m=n_orb,1,-1
                        if(sum(abs(xst-orb3D(:,m))) < 0.002) cycle do_L
                      end do
                      n_orb = n_orb+1
                      orb3D(:,n_orb) = xst
                      Ls(n_orb) = L
                      Lst(:,n_orb) = Lat(:,L) + Lati
                      pt(n_orb) = s
                      xI  = matmul(xst,SpG%kv)-matmul(Hg,xs)-ts(4:d)
                      rI = matmul(inv_Eg, xI)
                      do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                         xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
                         u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + at%Dcs(4:6,j)*sin(xarg)
                      end do
                      u_nu = matmul(g,u_mu)
                      orb(:,n_orb) = orb3D(:,n_orb) + u_nu
                      if(A%Atom(i)%Magnetic) then
                        xarg=0.0; m_mu=0.0
                        do j=1,At%n_mc !Moment amplitudes
                           xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
                           m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + at%Mcs(4:6,j)*sin(xarg)
                        end do
                        m_nu=matmul(magm,m_mu)
                        !Homogeneous moment + modulation
                        morb(:,n_orb) = msp + m_nu
                      end if
                   end do do_L
                end do do_s
                nt=n_orb
                allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt),Ol%orbit(i)%Ls(nt),Ol%orbit(i)%Latt(3,nt))
                allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%ChemSymb(nt),Ol%orbit(i)%pts(nt))
                Ol%orbit(i)%ChemSymb(1:nt)=A%Atom(i)%ChemSymb
                Ol%orbit(i)%mult=nt
                do k=1,n_orb
                  Ol%orbit(i)%pos(:,k)=orb(:,k)
                  Ol%orbit(i)%mom(:,k)=morb(:,k)
                  Ol%orbit(i)%Ls(k)=Ls(k)
                  Ol%orbit(i)%pts(k)=pt(k)
                  Ol%orbit(i)%Latt(:,k)=Lst(:,k) !Lat(:,Ls(k))
                  write(unit=Ol%orbit(i)%lab(k),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",k
                  Ol%orbit(i)%lab(k)=pack_string(Ol%orbit(i)%lab(k))
                end do
            End select

        End Select

      end do !Atoms

    End Subroutine Get_ModAtoms_inBOX

 End Module Atoms_in_BOX

 Program Test_CIF_CFL_SHX
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Maths,        only: Set_EPS_Math
    use CFML_Strings,      only: File_type, u_case, Get_extension
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell, Change_Setting_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info, &
                                 Get_moment_ctr, Get_TFourier_Ctr, Get_Orbit, point_orbit,Get_Inv_OP
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List, ModAtm_Std_Type
    use CFML_IOForm
    use Atoms_in_BOX

    !---- Local Variables ----!
    implicit none

    character(len=:), allocatable       :: line
    character(len=512)                  :: fname,cmdline
    integer                             :: nlong,narg
    real(kind=cp)                       :: start, fin
    type(AtList_Type)                   :: Atm
    type(orbit_list)                    :: Ol
    integer, dimension(3)               :: TBOX=[1,1,1]
    integer, dimension(3)               :: mcell

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    character(len=256)                  :: ctr_code,forma,formb !setting,
    character(len=256),dimension(26)    :: tctr_code
    type(Cell_G_Type)                   :: Cell !,Celln
    class(Spg_Type), allocatable        :: Grp
    type(File_type)                     :: flist
    type(Point_Orbit)                   :: orb, orb3
    !type(Orbit_type)                    :: orb3D
    integer :: i, j, d, codini, s, nt, ier
    real(kind=cp), dimension(3)               :: codes=1.0, v
    real(kind=cp), dimension(:,:),allocatable :: codeT
    logical :: box_given

    !> Init
    narg=COMMAND_ARGUMENT_COUNT()
    cmdline=" "; nlong=0; box_given=.false.
    if (narg ==0) then
       write(unit=*,fmt='(/,a)',advance='no') " => Introduce the name of the file: "
       read(unit=*,fmt='(a)') fname
       if (len_trim(fname) <=0 ) call CloseProgram()
       cmdline=trim(fname)
    else
       call GET_COMMAND_ARGUMENT(1, cmdline)
    end if
    nlong=len_trim(cmdline)
    fname=cmdline
    !> Start
    call CPU_TIME(start)

    !> Type of Files

    call Read_Xtal_Structure(fname,Cell,Grp,Atm,Ftype=flist)
    if(Err_CFML%Ierr == 0) then
       mcell=0
       do i=1,flist%nlines
         line=adjustl(flist%line(i)%Str_tmp)
         if(len_trim(line) < 6) cycle
         if(line(1:5) =="MCELL") then
           read(line(6:),*) mcell
           tbox(:)=mcell
           box_given=.true.
           exit
         end if
       end do
       if(.not. box_given) then
          write(*,"(a)",advance="no") " => Please enter the box for calculations (3 integers): "
          read(*,*,iostat=ier) tbox
          if(ier /= 0) tbox=[1,1,1]
       end if
       call Write_Crystal_Cell(Cell)

       !if(len_trim(Grp%setting) /= 0) then  !This has been suppressed because the change of setting is done in Read_Xtal_structure
       !  write(*,"(/,a)") " => Transformed Cell"
       !  if(Grp%D > 4) then
       !    i=index(Grp%setting,"d")
       !    setting=Grp%setting(1:d-2)//";0,0,0"
       !  else
       !    setting=Grp%setting
       !  end if
       !  call Change_Setting_Cell(Cell,setting,Celln)
       !  call Write_Crystal_Cell(Celln)
       !end if

       !Determine the inverse operators
       Grp%Inv= Get_Inv_OP(Grp%Op)

       call Write_SpaceGroup_Info(Grp)


       call Set_Eps_Math(0.0002_cp)
       if(Atm%natoms > 0) then
          !First Check symmetry constraints in magnetic moments and Fourier coefficients
          !call Check_Symmetry_Constraints(Grp,Atm)
          write(*,"(//a,i5)") "  Number of atoms:",Atm%natoms
          Select Type (Grp)
            Type is (Spg_Type)
               call Write_Atom_List(Atm)
            Type is (SuperSpaceGroup_Type)
               call Write_Atom_List(Atm,SpG=Grp)
               formb="(a, i3,a,6f10.5,a)"
               write(unit=formb(4:4),fmt="(i1)") Grp%nk
          End Select
          !Calculate all atoms in the unit cell
                !1234567890123456789012345678901234567890
          forma="(i5, f10.5,tr8, f10.5,i8,tr5, i4,3f12.5)"
          d=Grp%d-1
          write(forma(5:5),"(i1)") d
          write(forma(16:16),"(i1)") d
          write(forma(30:30),"(i1)") d
          write(*,"(//a)") "  Orbits of atoms after applying constraints on moments:"
          write(*,"(  a)") "  ======================================================"

          Ol%num_orbs=Atm%natoms
          allocate(Ol%orbit(Atm%natoms))
          do i=1,Atm%natoms
            codini=1; codes=1.0
            call Get_moment_ctr(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,codes,ctr_code=ctr_code)!,Ipr=6)
            write(*,"(a,3f10.5,a)") " => Moment of atom "//trim(Atm%Atom(i)%Lab)//": ",Atm%Atom(i)%moment,"    CtrCode: "//trim(ctr_code)

            call Get_Orbit(Atm%Atom(i)%x,Grp,orb,Atm%Atom(i)%moment,tbox=tbox)  !,orb3D)
            !call Get_Orbit(Atm%Atom(i)%x,Grp,orb,Atm%Atom(i)%moment)  !,orb3D)

            write(*,"(a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)

            Select Case(Grp%d-1)
              Case(3)
                write(*,"(a)") "    N      X         Y         Z                 Mx        My       Mz      PointoOP"
              Case(4)
                write(*,"(a)") "    N     X1        X2        X3        X4                 M1        M2         M3        M4      PointoOP"
              Case(5)
                write(*,"(a)") "    N     X1        X2        X3        X4        X5                 M1        M2        M3        M4        M5      PointoOP"
              Case(6)
                write(*,"(a)") "    N     X1        X2        X3        X4        X5        X6                 M1        M2        M3        M4        M5        M6      PointoOP"
            End Select

            Select Type (Grp)
              Type is (SuperSpaceGroup_Type)

                 Select Type(at => Atm%Atom(i))

                   type is (ModAtm_Std_Type)

                      do j=1,orb%Mult
                          !Calculate modulation functions to write properly the superspace coordinates
                          s=orb%pts(j)
                          v=modulation_function(At,Grp,s,orb%Lat(1:3,j),.true.) !,tshift)
                          orb%mom(1:3,j)=orb%mom(1:3,j)+v
                          write(*,forma) j,orb%pos(1:d,j),orb%mom(1:d,j),s,orb%Lat(1:d,j),orb%mom(1:3,j)
                      end do
                      !write(*,"(/,a)") " => Orbit of atom (restricted to 3D): "//trim(Atm%Atom(i)%Lab)
                      !do j=1,orb3%Mult
                      !    !Calculate modulation functions to write properly the superspace coordinates
                      !    s=orb3%pts(j)
                      !    v=modulation_function(At,Grp,s,orb3%Lat(1:3,j),.true.) !,tshift)
                      !    orb%mom(1:3,j)=orb%mom(1:3,j)+v
                      !    write(*,"(i5,3f10.5,tr8,3f10.5,i8,tr5,3i4,3f12.5)") j,orb3%pos(1:3,j),orb3%mom(1:3,j),s,orb3%Lat(1:3,j),orb3%mom(1:3,j)
                      !end do

                      nt=orb3%Mult
                      allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt),Ol%orbit(i)%Ls(nt),Ol%orbit(i)%Latt(3,nt))
                      allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%ChemSymb(nt),Ol%orbit(i)%pts(nt))
                      Ol%orbit(i)%ChemSymb(1:nt)=At%ChemSymb
                      Ol%orbit(i)%mult=nt
                      do j=1,nt
                        Ol%orbit(i)%pos(:,j) = orb%pos(1:3,j)
                        Ol%orbit(i)%mom(:,j) = orb%mom(1:3,j)
                        Ol%orbit(i)%Ls(j)    = j
                        Ol%orbit(i)%pts(j)   = orb%pts(j)
                        Ol%orbit(i)%Latt(:,j)= orb%Lat(1:3,j)
                        write(unit=Ol%orbit(i)%lab(j),fmt="(a,i5)")  trim(At%Lab)//"_",j
                        Ol%orbit(i)%lab(j)=pack_string(Ol%orbit(i)%lab(j))
                      end do

                 End Select

              Type is(Spg_Type)

                 do j=1,orb%Mult
                     s=orb%pts(j)
                     write(*,forma) j,orb%pos(1:d,j),orb%mom(1:d,j),s,orb%Lat(1:d,j),orb%mom(1:3,j)
                 end do
                 !write(*,"(/,a)") " => Orbit of atom (restricted to 3D): "//trim(Atm%Atom(i)%Lab)
            End Select
            !write(*,"(/,a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)//"  restricted to 3D but within TBOX"
            !call Get_Orbit3D(Atm%Atom(i)%x,Atm%Atom(i)%Lab,Atm%Atom(i)%ChemSymb,Grp,orb3D,tbox)
            !
            !do j=1,orb3D%Mult
            !  posi=orb3D%pos(1:3,j)
            !  if(any(posi >= 1.00000) ) cycle
            !  write(*,"(i5,3f12.5,i8,tr5,a,3i4,a)") j,posi,orb3D%pts(j),"[",orb3D%Lat(1:3,j),"]"
            !end do
            !
           Select Type(at => Atm%Atom(i))

             class is (ModAtm_Std_Type)
               write(*,"(a)") " => Modulation amplitudes of atom: "//trim(Atm%Atom(i)%Lab)
               if(allocated(CodeT)) deallocate(CodeT)
               allocate(CodeT(6,at%n_mc))
               CodeT=1.0
               Select Type (Grp)
                 Type is (SuperSpaceGroup_Type)
                    do j=1,At%n_mc
                      write(*,formb) "     Mcs: [",Grp%Q_coeff(:,j),"]",At%Mcs(:,j),"    CtrCode: "
                    end do
                    call Get_TFourier_Ctr(At%x,At%Mcs(:,1:at%n_mc),codeT,Grp,codini,"M",Ipr=6,ctr_code=tctr_code)
                    do j=1,At%n_mc
                      write(*,formb) "     Mcs: [",Grp%Q_coeff(:,j),"]",At%Mcs(:,j),"    CtrCode: "//trim(tctr_code(j))
                      !Atm%Atom(i)%Mcs(:,j) = At%Mcs(:,j)  !!!! ERROR
                    end do
                    if(allocated(CodeT)) deallocate(CodeT)
                    allocate(CodeT(6,at%n_dc))
                    CodeT=1.0
                    call Get_TFourier_Ctr(At%x,At%Dcs(:,1:at%n_dc),codeT,Grp,codini,"D",ctr_code=tctr_code)
                    do j=1,At%n_dc
                      write(*,formb) "     Dcs: [",Grp%Q_coeff(:,j),"]",At%Dcs(:,j),"    CtrCode: "//trim(tctr_code(j))
                    end do
               end select
           end select
          end do

          Select Type (Grp)

              Type is (SuperSpaceGroup_Type)
                ! Testing atoms in BOX
                call Get_ModAtoms_inBOX(Atm,Grp,TBOX,Ol)
                formb="(a15,tr5,a, 3f14.6,2i4,a,3i4,a)"
                do i=1,Ol%num_orbs
                  write(*,"(/,a,i4)") " Orbit of atom: ",i
                  do j=1,Ol%orbit(i)%mult
                     write(*,formb) Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb(j),Ol%orbit(i)%pos(1:3,j),Ol%orbit(i)%pts(j),Ol%orbit(i)%Ls(j),"  [",Ol%orbit(i)%Latt(:,j)," ]"
                     write(*,"(tr22,3f14.6)") Ol%orbit(i)%mom(1:3,j)
                  end do
                end do
                call Write_CIF_P1()
                i=index(fname,".")
                call Write_MCIF_Template(fname(1:i-1)//"_mod.mcif",Cell,Grp,Atm,"Testing Write_ssg_MCIF")

              Type is (SPG_Type)
                i=index(fname,".")
                call Write_Cif_Template(fname(1:i)//"cif", Cell, Grp, Atm, 2, "Testing WriteCIF")
                call Write_MCIF_Template(fname(1:i-1)//"_mod.mcif",Cell,Grp,Atm,"Testing Write_MCIF")

          End Select
       end if
    else
      write(*,"(a)") " => Error found!"
      write(*,"(a)") " => "//trim(Err_CFML%Msg)
    end if
    call CPU_TIME(fin)
    write(unit=*,fmt="(/,a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"

 contains
    !!----
    !!---- CLOSEPROGRAM
    !!----
    !!---- 09/05/2020
    Subroutine CloseProgram()
       !---- Local Variables ----!
       character(len=1) :: ans

       write(unit=*,fmt="(a)")   " "
       write(unit=*,fmt="(a)")   " => Press <cr> to finish ..."
       read(unit=*,fmt="(a)") ans

       stop
    End Subroutine CloseProgram

    Subroutine Write_CIF_P1()
      integer :: Ipr=1
      real    :: pos(3)
       !---- Local Variables ----!
      i=index(fname,".",back=.true.)
        open(newunit=ipr,file=fname(1:i-1)//"_P1.mcif", status="replace",action="write")
        write(unit=Ipr,fmt="(a)") "#  -----------------------------------------------"
        write(unit=Ipr,fmt="(a)") "#  Magnetic CIF file generated by CrysFML08 in P1"
        write(unit=Ipr,fmt="(a)") "#  -----------------------------------------------"
        write(unit=Ipr,fmt="(a)") "# This is a simple mCIF in P1 in a BOX (multiple CELL)"
        write(unit=Ipr,fmt="(a)") "# MagCIF file for: "//trim(fname)
        write(unit=Ipr,fmt="(a)") " "
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "_magnetic_space_group_standard_setting  'no'"
        write(unit=Ipr,fmt="(a)") '_parent_space_group.name_H-M  "'//"P 1"//'"'
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        mcell(:)  = tbox(:)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_a    ",cell%cell(1)*mcell(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_b    ",cell%cell(2)*mcell(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_c    ",cell%cell(3)*mcell(3)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_alpha ",cell%ang(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_beta  ",cell%ang(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_gamma ",cell%ang(3)
        write(unit=Ipr,fmt="(a,f10.5)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")  "loop_"
        write(unit=Ipr,fmt="(a)")  "_space_group_symop_magn_operation.id"
        write(unit=Ipr,fmt="(a)")  "_space_group_symop_magn_operation.xyz"
        write(unit=Ipr,fmt="(a)")  "1   x,y,z,+1"
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "_atom_site_label"
        write(unit=Ipr,fmt="(a)") "_atom_site_type_symbol"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_x"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_y"
        write(unit=Ipr,fmt="(a)") "_atom_site_fract_z"
        do i=1,Ol%num_orbs
          do j=1,Ol%orbit(i)%mult
             pos=Ol%orbit(i)%pos(1:3,j)/mcell
             if(any(pos < 0.0 ) .or. any(pos >= 1.0 )) cycle
             write(Ipr,"(a15,tr5,a,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb(j),pos
          end do
        end do
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.label"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_x"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_y"
        write(unit=Ipr,fmt="(a)") "_atom_site_moment.crystalaxis_z"
        do i=1,Ol%num_orbs
          do j=1,Ol%orbit(i)%mult
             pos=Ol%orbit(i)%pos(1:3,j)/mcell
             if(any(pos < 0.0 ) .or. any(pos >= 1.0 )) cycle
             write(Ipr,"(a15,tr5,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%mom(1:3,j)
          end do
        end do
        close(unit=Ipr)
    End Subroutine Write_CIF_P1

End Program Test_CIF_CFL_SHX