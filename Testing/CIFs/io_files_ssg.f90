!!----
!!----
!!----
!!----
 Module Atoms_in_BOX
   use CFML_Globaldeps
   use CFML_Atoms,        only: AtList_Type, MAtm_Std_Type
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
   Subroutine Get_Orbita(x,Spg,orbit,mom,orb3D,convl,Tbox)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      class(SpG_Type),                      intent(in)  :: spg
      type(Point_Orbit),                    intent(out) :: orbit
      real(kind=cp), dimension(:),optional, intent(in)  :: mom
      type(Point_Orbit),          optional, intent(out) :: orb3D
      logical,                    optional, intent(in)  :: convl
      integer,       dimension(3),optional, intent(in)  :: Tbox

      !---- Local variables ----!
      integer                                          :: i, j, n, nt,d,mult,n_tr,L,i1,i2,i3
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d)                  :: ms,msp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb,morb
      real(kind=cp), dimension(:,:), allocatable       :: OrbE
      integer,       dimension(:,:), allocatable       :: LatE
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),  allocatable        :: ptr
      logical                                          :: conv
      integer,       dimension(:,:), allocatable       :: add_Lat

      conv=.true.
      if(present(convl)) conv=convl
      d=Spg%d-1
      xs(Spg%d)=1.0_cp
      ms(Spg%d)=1.0_cp
      orb=0.0_cp; morb=0.0_cp
      ptr=0

      Select Type(SpG)

        type is (SuperSpaceGroup_Type)
           Om=SpG%Om
           xs(1:3)=x   !Extend the position and moment to superspace
           if(present(mom)) ms(1:3)=mom
           do i=1,SpG%nk
             xs(3+i)=dot_product(x,SpG%kv(:,i))
             if(present(mom)) ms(3+i)=dot_product(mom,SpG%kv(:,i))
           end do
        class default
           do i=1,Spg%Multip
             Om(:,:,i)=Spg%Op(i)%Mat
           end do
           xs(1:d)=x
           if(present(mom)) ms(1:d)=mom
      End Select

      allocate(ptr(Spg%multip))
      mult=1
      orb(:,1)=xs(1:d)
      Lat(:,1)=0
      ptr(mult) = 1
      if(present(mom)) morb(1:d,1)=ms(1:d)

      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:d)=orb(1:d,nt)-xsp(1:d)
            if(sum(abs(v(1:d))) < 2.0 * EPSS) cycle do_ext
            if(Spg%Num_lat > 0 .and. .not. conv) then
              if (is_Lattice_vec(v(1:d),Spg%Lat_tr(1:d,:))) cycle do_ext
            else
              if (Zbelong(v(1:d))) cycle do_ext
            end if
         end do
         msp(1:d)=Spg%Op(j)%dt*Spg%Op(j)%time_inv*matmul(Om(1:d,1:d,j),ms(1:d))
         mult=mult+1
         orb(1:d,mult)=xsp(1:d)
         Lat(1:d,mult)=Latt
         if(present(mom)) morb(1:d,mult)=msp(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(Tbox)) then  !Add supplemental cells
        n_tr=(tbox(1)+1)*(tbox(2)+1)*(tbox(3)+1); L=0
        allocate(add_Lat(d,n_tr))
        add_Lat=0
        do i1=0,tbox(1)
          do i2=0,tbox(2)
             do i3=0,tbox(3)
                if(i1 == 0 .and. i2 == 0 .and. i3 == 0 ) cycle
                L=L+1
                add_Lat(1:3,L)=[i1,i2,i3]
             end do
          end do
        end do
        n_tr=L
        nt=mult*(n_tr+1)
        orbit%Mult=nt
        allocate(orbit%pos(d,nt), orbit%pts(nt), orbit%Lat(d,nt))
        if(present(mom))  allocate(orbit%mom(d,nt))
        orbit%pos(:,1:mult) = orb(:,1:mult)
        orbit%pts(1:mult)   = ptr(1:mult)
        orbit%Lat(:,1:mult) = Lat(:,1:mult)
        nt=mult
        do L=1,n_tr
          do n=1,mult
            orbit%pos(:,nt+n)=orb(:,n)+ add_Lat(:,L)
            orbit%Lat(:,nt+n)=Lat(:,n)+ add_Lat(:,L)
            orbit%pts(nt+n)= ptr(n)
            if(present(mom)) orbit%mom(:,nt+n)=morb(:,n)
          end do
          nt=nt+mult
        end do
      else
         orbit%Mult=mult
         orbit%pos=orb(:,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(:,1:mult)
         if(present(mom)) orbit%mom=morb(:,1:mult)
      end if

      if(present(orb3D)) then !Restriction to 3D
        mult=orbit%Mult
        allocate(orbe(3,Mult),LatE(3,Mult))
        if(allocated(ptr)) deallocate(ptr)
        allocate(ptr(Mult))
        ptr=0;orbe=0.0;late=0
        nt=1
        orbE(:,1)=orb(1:3,1)
        ptr(1)=1
        LatE(:,1)=0
        do_i:do i=2,mult
          do j=1,nt
            if(sum(abs(orbE(:,j)-orbit%pos(1:3,i))) < 2.0 * EPSS ) cycle do_i
          end do
          nt=nt+1
          orbE(:,nt) = orbit%pos(1:3,i)
          LatE(:,nt) = orbit%Lat(1:3,i)
          ptr(nt)=i    !This points to the i-atom of the superspace orbit
        end do do_i
        orb3D%mult=nt

        allocate(orb3D%pos(3,nt),orb3D%Lat(3,nt),orb3D%pts(nt))
        if(present(mom)) allocate(orb3D%mom(3,nt))
        do i=1,nt
          orb3D%pos(:,i)= orbE(:,i)
          orb3D%Lat(:,i)= LatE(:,i)
          orb3D%pts(i)  = orbit%pts(ptr(i)) !This points to the symmetry operator
          if(present(mom)) orb3D%mom(:,i) = orbit%mom(1:3,ptr(i))
        end do
      end if

   End Subroutine Get_Orbita


    !Subroutine generate_P1(A,SpG,TBOX,Ol)
    !  !---- Arguments ----!
    !  type(AtList_Type),       intent(in)  :: A
    !  class(SpG_Type),         intent(in)  :: spg
    !  integer, dimension(3), intent(in)    :: TBOX
    !  type(orbit_list),        intent(out) :: Ol
    !  !---- Local Variables ----!
    !  character(len=256)                             :: line
    !  character(len=60),dimension(80)                :: string,text
    !  character(len=8)                               :: chemsym
    !  character(len=15)                              :: name_at
    !  logical                                        :: basf
    !  integer                                        :: i,j,ipiof,L,nv,m,n,nt,nlatt, ncon,ir, ipr,n_kv, natoms, &
    !                                                    iss,ii, ic, iq, iqt,d
    !  real(kind=cp)                                  :: pph,x
    !  real(kind=cp),    dimension(3)                 :: xi,yr,yi,ayr,ayi,side, car, cars
    !  real(kind=cp),    dimension(9)                 :: cm
    !  real(kind=cp),    dimension(3)                 :: kvec,mcell,tras
    !  real(kind=cp),    dimension(:,:), allocatable  :: ltr
    !  real(kind=cp),    dimension(:,:), allocatable  :: coord
    !  real(kind=cp),    dimension(:,:), allocatable  :: moments
    !  character(len=15),dimension(:),   allocatable  :: mLab
    !  character(len=1)                               :: lats
    !  integer(kind=1),  dimension(:),   allocatable  :: pt
    !  integer,          dimension(3)                 :: magcel
    !  ! Superspace variables
    !  real(kind=cp),    dimension(6,0:6)             :: Tfourier
    !  real(kind=cp),    dimension(6)                 :: tfou
    !  integer,          dimension(spg%nk)            :: brack_m  ! hs=(H,[m])
    !  real(kind=cp),    dimension(spg%d-1)           :: ts  ! ts=(t,tI)
    !  real(kind=cp),    dimension(spg%nk)            :: ri  ! internal coordinates xi*kv
    !  real(kind=cp),    dimension(3)                 :: Mcos, Msin
    !  integer,          dimension(ssg(k)%nk)         :: mE  ![m].E
    !  integer,          dimension(3)                 :: mM  ![m].H
    !  integer,          dimension(spg%nk,3)          :: Mx  !M
    !  integer,          dimension(3,3)               :: g, magm   !g, magm= delta * det(g) * g
    !  integer,          dimension(spg%nk,spg%nk)     :: ep  !E
    !  logical :: k0_K
    !
    !  magcel=tbox+1
    !  allocate(Ltr(3,2*magcel(1)*magcel(2)*magcel(3)))
    !  m=0
    !  Do i=0,magcel(1)-1
    !    Do j=0,magcel(2)-1
    !      Do L=0,magcel(3)-1
    !         tras=real([i,j,L])
    !         m=m+1
    !         ltr(:,m)= tras
    !      End do
    !    End Do
    !  End Do
    !  nlatt=m !Total number of translations to be applied to the content of the cell
    !  !Generation of all atoms and magnetic moments within the magnetic cell
    !  if(allocated(coord)) deallocate(coord)
    !  if(allocated(moments)) deallocate(moments)
    !  if(allocated(mLab)) deallocate(mLab)
    !  if(allocated(pt))deallocate(pt)
    !  natoms=nlatt*A%natoms*spg%Multip
    !  allocate(coord(3,natoms),moments(3,natoms),mLab(natoms),pt(natoms))
    !  pt=1
    !  moments=0.0
    !
    !  nt=0
    !  d=spg%d-1
    !  do i=1,A%natoms
    !    xi = A%atom(i)%x
    !    ic=0
    !    tfourier(:,0)= [xl(ipiof,6:8),[0.0,0.0,0.0]]     !homogeneus moment
    !    sTfourier=0.0
    !    tfourier(:,1)= [xl(ipiof,21:23),xl(ipiof,24:26)]
    !    if(kinfo(k)%nq > 1) tfourier(:,2)=0.5*[xl(ipiof,27:29),xl(ipiof,30:32)]
    !    if(kinfo(k)%nq > 2) tfourier(:,3)=0.5*[xl(ipiof,33:35),xl(ipiof,36:38)]
    !    if(kinfo(k)%nq > 3) tfourier(:,4)=0.5*[xl(ipiof,39:41),xl(ipiof,42:44)]
    !    if(kinfo(k)%nq > 4) tfourier(:,5)=0.5*[xl(ipiof,45:47),xl(ipiof,48:50)]
    !    if(kinfo(k)%nq > 5) tfourier(:,6)=0.5*[xl(ipiof,51:53),xl(ipiof,54:56)]
    !    !Generate all atoms in the zero cell and then all the atoms outside the unit cell accoring to MULTCELL
    !    !+++++++++++++++++++++++++
    !    do ir=1,ssg(k)%Multip   !Loop over symmetry operators
    !    !+++++++++++++++++++++++++
    !         g(:,:) = spg%Op(ir)%Mat(1:3,1:3) !                         /  g    0   t  \
    !          ts(:) = spg%Op(ir)%Mat(1:d,d+1) !   Superspace operator:  |  Mx   ep   tI |    ts=(t,tI)
    !        Ep(:,:) = spg%Op(ir)%Mat(4:d,4:d) !                         \   0    0   1 /
    !      magm(:,:) = g(:,:)*spg%Op(ir)%time_inv*spg%Op(ir)%dt
    !       ! Mx(:,:) = ssg(k)%Op(ir)%Mat(4:d,1:3)
    !
    !       !      Mx(:,:) = ssg(iph)%Op(ir)%Mat(4:d,1:3)
    !       !      Ep(:,:) = ssg(iph)%Op(ir)%Mat(4:d,4:d)
    !       !      gM(:,:) = ssg(iph)%Op(ir)%Mat(1:d,1:3)
    !       !    magm(:,:) = g(:,:)*ssg(iph)%Op(ir)%time_inv*ssg(iph)%Op(ir)%dt
    !       !
    !       ! Transformation Equation for magnetic moments
    !       !
    !       !   M(mu)[ Mx . rE(nu)+Ep rI(nu) + tI] = magm M(nu)[rI(nu)]
    !       !
    !       !   rI=(x4, x5, ... x3+d)
    !       !
    !
    !      !Coordinates of a new Atom
    !      car(:)=matmul(g,xi)+ts(1:3)
    !      where(car < 0.0) car=car+1.0
    !      where(car > 1.0) car=car-1.0
    !      Do L=1,nlatt
    !        nt=nt+1
    !        ic=ic+1
    !        coord(:,nt)=car + Ltr(:,L)
    !        moments(:,nt)=matmul(magm,Tfourier(1:3,0))  !need to add modulations
    !        mLab(nt) = trim(atext(ipiof))
    !        msfac(nt)= ntyp(ipiof)
    !        do iq=1,spg%nk
    !          ri(iq)=dot_product(xi+Ltr(:,L),pvk(k,iq,:))
    !        end do
    !        !Identify the q_coefficients of mM to select the proper Tfourier
    !        do iqt=1,kinfo(k)%nq
    !          brack_m(:)=kinfo(k)%q_coeff(:,iqt) !nk-components vector
    !          mE=matmul(brack_m,Ep)              ![m].Ep  nk-components vector
    !          !mM=matmul(brack_m,Mx)             ! 3-components vector
    !          if(equal_vector(mE,kinfo(k)%q_coeff(:,iqt))) then
    !            tfou=Tfourier(:,iqt)
    !          else if(equal_vector(mE,-kinfo(k)%q_coeff(:,iqt))) then
    !            tfou=[Tfourier(1:3,iqt),-Tfourier(4:6,iqt)]
    !          end if
    !          pph=-2.0*pi*(dot_product(mE,ri)) !+dot_product(mM,coord(:,nt)))
    !          Mcos=matmul(magm,tfou(1:3))
    !          Msin=matmul(magm,tfou(4:6))
    !          moments(:,nt)= moments(:,nt)+Mcos*cos(pph)+Msin*sin(pph)
    !        end do
    !      End Do  !nlatt
    !    !+++++++++++++++++++++++++
    !    end do   !  end loop over symmetry operators
    !    !+++++++++++++++++++++++++
    !  end do !i=1,natoms
    !
    !  m=0
    !  do i=1,nt
    !     coord(:,i)=coord(:,i)/real(mul_cell)
    !     if(any(coord(:,i) > 1.0) ) pt(i)=0
    !     if(any(coord(:,i) < 0.0) ) pt(i)=0
    !  end do
    !
    !  !Eliminate duplicates
    !  do i=1,nt-1
    !     if(pt(i) == 0) cycle
    !     tras=coord(:,i)
    !     do j=i+1,nt
    !        if(pt(j) == 0) cycle
    !        if (sum(abs(tras-coord(:,j))) <= 0.001)  then
    !          if(k0_K) then
    !            write(*,"(2(a,3f8.4))") "  "//trim(mLab(i)), moments(:,i),  "  ->  "//trim(mLab(j)),moments(:,j)
    !            moments(:,i)=moments(:,i) + moments(:,j)
    !            yr=matmul(cellp(k)%Cr_Orth_cel(:,:),moments(:,i)/cellp(k)%cell)
    !            mod_mom(i) = sqrt(dot_product(yr,yr))
    !          end if
    !          pt(j) = 0
    !        end if
    !     end do
    !  end do
    !
    !End Subroutine generate_P1

    Function get_modulation_function(x,x0,Amp,SpG,s,iq,lat,time_v) result(v)
      real(kind=cp), dimension(3),intent(in) :: x,x0
      real(kind=cp), dimension(6),intent(in) :: Amp    !Amplitudes of Modulation at position x0
      type(SuperSpaceGroup_type), intent(in) :: SpG
      integer,                    intent(in) :: s      !Pointer to the symmetry operator relating x and x0: x= g(s) x0 + t(s) + Lat
      integer,                    intent(in) :: iq     !Pointer to Q_coeff
      integer,       dimension(3),intent(in) :: Lat    !Additional lattice translation
      logical,                    intent(in) :: time_v !if .true. magnetic modulation
      real(kind=cp), dimension(3)            :: v      !Modulation at position x

      !---- Local variables ----!
      real(kind=cp)                      :: sig,xarg
      real(kind=cp), dimension(SpG%nk)   :: rI,rpI,tI,xI !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)  :: ts,inv_ts    ! ts=(t,tI) Total translation in superspace operator and its inverse
      integer                            :: s_inv, d, ds
      integer, dimension(3)              :: mM           ![m].M  3-components vector      !                         /  g    0   t  \
      integer, dimension(SpG%nk)         :: mE,inv_mE    ![m].Eg  nk-vector               !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(SpG%nk,3)       :: Hg,Ng,sigma  !Hg(nk,3),sigma(nk,3)            !                         \  0    0   1  /
      integer, dimension(3,3)            :: g, magm      !g, magm= delta * det(g) * g     !                         /  inv_g    0      it  \
      integer, dimension(3,3)            :: inv_g        !g^(-1)                          ! Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
      integer, dimension(SpG%nk,SpG%nk)  :: Eg,inv_Eg    !E (nk,nk)                       !                         \   0       0       1  /

      ! Transformation Equation for general vector modulation function
      !
      !   p(nu)[ Hg . rE(mu) + Eg rI(mu) + tI] = g_a p(mu)[rI(mu)]
      !   p(nu)[ rI(nu) ] = g_a p(mu)[inv_Og rI(mu)] = g_a p(mu) [Ng (rE(nu)-t) + inv_Eg (rI(nu)-tI) ]
      !
      ! Where g_a= g for polar displacement modulations and g_a=magm for magnetic moment modulations
      !
      !  rE(mu) = x0   rI(mu) = sigma x0   t=ts(1:3), tI=ts(4:d)
      !  rE(nu) = x    rI(nu) = sigma x
      !
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices
        g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
        if(time_v) magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                            !                         /  g    0   t  \
        ts(:)     = SpG%Op(s)%Mat(1:d,ds)   !   Superspace operator: |  Hg   Eg   tI  |    ts=(t,tI)
       !Eg(:,:)   = SpG%Op(s)%Mat(4:d,4:d)  !                         \  0    0   1  /
       !Hg(:,:)   = SpG%Op(s)%Mat(4:d,1:3)
       !tI(:)     = ts(4:d)

                                               !                           /  inv_g    0      it  \
      s_inv=SpG%inv(s)                         !   Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
      inv_Eg(:,:) = SpG%Op(s_inv)%Mat(4:d,4:d) !                           \    0      0       1  /
          Ng(:,:) = SpG%Op(s_inv)%Mat(4:d,1:3)

       sigma=transpose(SpG%kv)
       xI= matmul(sigma,x0)  !x0 or X
       rI= matmul(Ng,x0-(ts(1:3)+Lat))+matmul(inv_Eg,xI-ts(4:d))
       mE= matmul(SpG%q_coeff(:,iq),inv_Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
       sig=1.0
       if(equal_vector(mE,Spg%q_coeff(:,iq))) then
         sig=1.0
       else if(equal_vector(mE,-Spg%q_coeff(:,iq))) then
         sig=-1.0
       end if
       xarg=-2.0*pi*dot_product(mE,rI)
       v =Amp(1:3)*cos(xarg) + sig * Amp(4:6)*sin(xarg)
       if(time_v) then
         v=matmul(magm,v)
       else
         v=matmul(g,v)
       end if

    End Function get_modulation_function

    Subroutine Jmol_MAtoms_inBOX(A,SpG,TBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3),   intent(in)  :: TBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, n, ins, s,nt,d, ds, i1,i2,i3,n_mO, n_tr, n_orb, s_inv

      real(kind=cp)                                    :: xarg, sig
      real(kind=cp), dimension(3)                      :: xs,xsp,xst,rv
      real(kind=cp), dimension(SpG%d)                  :: rs,rsd,rsi
      real(kind=cp), dimension(3)                      :: u_mu,m_mu, u_nu,m_nu
      real(kind=cp), dimension(3)                      :: ms,msp,mst
      real(kind=cp), dimension(:,:),   allocatable     :: Lat          !lattice translations
      type(Point_Orbit)                                :: orb,orb3D    !Temporal orbits
      real(kind=cp), dimension(:),     allocatable     :: rI,rpI,tI,xI !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)     :: ts,inv_ts    ! ts=(t,tI) Total translation in superspace operator and its inverse

      integer, dimension(3)                 :: mM           ![m].M  3-components vector      !                         /  g    0   t  \
      integer, dimension(:),   allocatable  :: mE,inv_mE    ![m].Eg  nk-vector               !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(:,:), allocatable  :: Hg,Ng,sigma  !Hg(nk,3),sigma(nk,3)            !                         \  0    0   1  /
      integer, dimension(3,3)               :: g, magm      !g, magm= delta * det(g) * g     !                         /  inv_g    0      it  \
      integer, dimension(3,3)               :: inv_g        !g^(-1)                          ! Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
      integer, dimension(:,:), allocatable  :: Eg,inv_Eg    !E (nk,nk)                       !                         \   0       0       1  /

    !!
    !!  The text below is extracted and modified from the source code of Jmol
    !!
    !! The superspace operation matrix is (3+d+1)x(3+d+1) rotation/translation matrix
    !! that can be blocked as follows:
    !!
    !!              rotational part     | translational part                     In Jmol g=gamma_E, Hg=gamma_M, Eg=gamma_I
    !!
    !!                g         0        |   t + n              T=(t+n,tI)
    !! {Gs|ts} =
    !!               Hg         Eg       |  tI
    !!
    !!                0         0        |   1
    !!
    !! where    g is the "external" 3x3 R3 point group operation
    !!         Eg is the "intermal" dxd point group operation
    !!         Hg is the dx3 "mixing" component that adds an
    !!               external effect to tbe rotation of internal coordinates.
    !!       3x1 t and dx1 tI are the external and internal translations, respectively
    !!       n is the R3 part of the lattice translation that is part of this particular operation
    !!
    !! Note that all elements of Gs are 0, 1, or -1 -- "epsilons"
    !!
    !! Likewise, the 3+d coordinate vector that {Gs|ts} is being operated upon is a (3+d x 1) column vector
    !! that can be considered to be an external ("R3") component and an internal ("d-space") component:
    !!
    !!        rE (3 x 1)
    !!   r =
    !!        rI (d x 1)
    !!
    !! Thus, we have:
    !!
    !!   r' = Gs * r + T
    !!
    !! with components
    !!
    !!   r'E =   g  rE + t + n        (just as for standard structures)
    !!   r'I =  Hg  rE + Eg rI + tI
    !!
    !! These equations are not actually used here.
    !!
    !! The set of cell wave vectors form the sigma (d x 3) array, one vector per row.
    !! Multiplying sigma by the atom vector r'E and adding a zero-point offset
    !! in internal d-space, tuv, gives us r'I
    !!
    !!   r'I = sigma r'E + tuv
    !!
    !! However, this coordinate is not in the "space" that our modulation functions
    !! are defined for. In order to apply those functions, we must back-transform this
    !! point into the space of the asymmetric unit. We do that inverting our function
    !!
    !!   x'I = Hg xE + Eg xI + tI
    !!
    !! to give:
    !!
    !!   xI = (Eg^-1)(x'I - Hg xE - tI)
    !!
    !! The parameters to this Java function r0 and r00 provide values for r'E and rE,
    !! respectively. Substituting r'I for x'I and rE for xE, we get:
    !!
    !!   rI = (Eg^-1)(sigma  r'E + tuv - Hg rE - tI)
    !!
    !! In the code below, we precalculate all except the zero-point offset as "tau":
    !!
    !!   tau = gammaIinv.mul(sigma.mul(vR0).sub(gammaM.mul(vR00)).sub(sI));  <=Java code
    !!
    !!   tau = (Eg^-1)(sigma r0 - Hg r00 - tI)
    !!
    !! and then, in calculate(), we add in the tuv part and sum all the modulations:
    !!
    !!   rI = tau.add(gammaIinv.mul(tuv)).toArray();     rI = tau + (Eg^-1) tuv
    !!   for (int i = mods.size(); --i >= 0;)
    !!     mods.get(i).apply(this, rI);
    !!
    !! We can think of tau as an operator leading to a point in the "internal" d-space,
    !! as in a t-plot (van Smaalen, Fig. 2.6, p. 35) but for all internal coordinates together.
    !!
    !!
    !! Note that Hg is not necessarily all zeros. For example, in
    !! SSG 67.1.16.12  Acmm(1/2,0,g)0s0 we have an operator
    !!
    !!  (x1,x2,-x3,x1-x4+1/2)
    !!
    !! [http://stokes.byu.edu/iso/ssginfo.php?label=67.1.16.12&notation=x1x2x3x4]
    !!
    !! Prior to Jmol 14.[2/3].7 10/11/2014 this was not being considered.
    !!
    !! Noting that we have
    !!
    !!   Hg = sigma g - Eg sigma
    !!
    !! and
    !!
    !!   x'I = sigma x'E = sigma (g xE + t)   <- x'I = Hg xE + Eg xI + tI -> xI = (Eg^-1)(x'I - Hg xE - tI)
    !!
    !! we can, with some work, rewrite tau as:
    !!
    !!   tau = xI = sigma xE + (Eg^-1)(sigma t - tI)   !!!! tau = (Eg^-1)(sigma r0 - Hg r00 - tI)?
    !!
    !! This relationship is used in Jana2006 but not here, because it
    !! also necessitates adding in the final lattice shift, and that is not
    !! as easy as it sounds. It is easier just to use Gamma_M * X_E.
    !!     tau = xI = sigma xE + (Eg^-1)(sigma t - tI)

      ! Transformation Equation for general vector modulation function
      !
      !   p(nu)[ Hg . rE(mu) + Eg rI(mu) + tI] = g_a p(mu)[rI(mu)]
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
      !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]Eg) cos[2pi [m]Eg.rI(mu)] + Ps(mu)^([m]Eg) sin[2pi [m]Eg.rI(mu)] ]
      !
      !Calculation of the maximum number of additional lattice translations
      nt=(TBOX(1)+1)*(TBOX(2)+1)*(TBOX(3)+1)+1
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices
      !Calculate all the orbits in 3D before calculation of modulation functions

      !Allocation of main arrays
      Select Type(SpG)
        type is(SuperSpaceGroup_Type)
          allocate(mE(SpG%nk), Hg(SpG%nk,3), Eg(SpG%nk,SpG%nk), rI(SpG%nk))
          allocate(inv_Eg(SpG%nk,SpG%nk),Ng(SpG%nk,3),xI(SpG%nk),tI(SpG%nk))
          sigma=transpose(spg%kv) !Automatic allocation
      End Select
      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      !allocate(Lat(3,nt)) !Allocate lattice translations in superspace
      !nt=0
      !do i1=0,TBOX(1)
      !  do i2=0,TBOX(2)
      !     do i3=0,TBOX(3)
      !        !if(i1 == 0 .and. i2 == 0 .and. i3 == 0) cycle
      !        nt=nt+1
      !        Lat(:,nt) = real([i1,i2,i3],kind=cp)  !External space
      !     end do
      !  end do
      !end do
      !n_mO=2*Spg%multip*nt
      !n_tr=nt

      !Generate the orbits of all atoms

      do i=1,A%natoms
        xs=0.0; ms=0.0
        xsp=0.0; msp=0.0
        Select Type(at => A%Atom(i))

          class is (MAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
                n_orb= 0
                xs   = At%x
                ms   = At%Moment !Homogeneous moment

                Call Get_Orbita(xs,Spg,orb,ms,orb3D,Tbox=Tbox)

                nt=orb3D%mult
                allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt),Ol%orbit(i)%Latt(3,nt),Ol%orbit(i)%Lat(3,nt))
                allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%ChemSymb(nt),Ol%orbit(i)%pts(nt),Ol%orbit(i)%Ls(nt))
                Ol%orbit(i)%mult=nt
                Ol%orbit(i)%Ls=0

                do_m:do m=1,orb3D%mult
                  !s=orb3D%pts(orb%pts(m)) !Symmetry operator of the atom m
                  s=orb3D%pts(m)          !Symmetry operator of the atom m
                  s_inv=SpG%inv(s)        !s_inv Inverse operator to operate in internal space


                  g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
                  magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                                      !                         /  g    0   t  \
                  ts(:)     = SpG%Op(s)%Mat(1:d,ds)   !   Superspace operator: |  Hg   Eg   tI  |    ts=(t,tI)
                  Eg(:,:)   = SpG%Op(s)%Mat(4:d,4:d)  !                         \  0    0   1  /
                  Hg(:,:)   = SpG%Op(s)%Mat(4:d,1:3)
                  tI(:)     = ts(4:d)

                inv_g(:,:)  = SpG%Op(s_inv)%Mat(1:3,1:3) !                           /  inv_g    0      it  \
                inv_ts(:)   = SpG%Op(s_inv)%Mat(1:d,ds)  !   Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
                inv_Eg(:,:) = SpG%Op(s_inv)%Mat(4:d,4:d) !                           \    0      0       1  /
                    Ng(:,:) = SpG%Op(s_inv)%Mat(4:d,1:3)


                   xsp = orb3D%pos(:,m) !matmul(g,xs)+ts(1:3) + n  !enter in the calculation of rI
                   msp = orb3D%mom(:,m)

                   !Applying translations and calculating the modulation functions


                      u_mu= 0.0; m_nu=0.0
                      do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                         k=at%pdc_q(j)
                         !    Function get_modulation_function(x,x0,Amp,SpG,s,iq,lat,time_v) result(v)
                         u_mu=u_mu + Get_modulation_function(xsp,orb3D%pos(:,1),at%Dcs(:,j),SpG,s,k,orb3D%Lat(:,m),.false.)
                      end do
                      if(A%Atom(i)%Magnetic) then
                        do j=1,At%n_mc !Moment amplitudes
                           k=at%pmc_q(j)
                           m_nu=m_nu+Get_modulation_function(xsp,orb3D%pos(:,1),at%Mcs(:,j),SpG,s,k,orb3D%Lat(:,m),.true.)
                        end do
                      end if
                     !Homogeneous moment + modulation

                     Ol%orbit(i)%pos(:,m)= xsp + u_nu
                     Ol%orbit(i)%mom(:,m)= msp + m_nu
                    ! write(*,*) m, Ol%orbit(i)%pos(:,m)

                     Ol%orbit(i)%ChemSymb(m) = A%Atom(i)%ChemSymb
                     write(unit=Ol%orbit(i)%Lab(m),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",m
                     Ol%orbit(i)%Lab(m)=pack_string(Ol%orbit(i)%Lab(m))
                     Ol%orbit(i)%Latt(:,m)= orb%Lat(1:3,m)
                     Ol%orbit(i)%Lat(:,m) = orb3D%Lat(:,m)
                     Ol%orbit(i)%pts(m)=orb3D%pts(m)

                end do do_m

            End select !Spg_type

        End Select  !Atom_type

      end do !Atoms

    End Subroutine Jmol_MAtoms_inBOX

    Subroutine Get_MAtoms_inBOX(A,SpG,TBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3), intent(in)    :: TBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, n, ins, s,nt,d, ds, i1,i2,i3,n_mO, n_tr, n_orb

      real(kind=cp)                                    :: xarg, sig
      real(kind=cp), dimension(3)                      :: xs,xsp,xst,rv
      real(kind=cp), dimension(SpG%d)                  :: rs,rsd,rsi
      real(kind=cp), dimension(3)                      :: u_mu,m_mu, u_nu,m_nu
      real(kind=cp), dimension(3)                      :: ms,msp,mst
      real(kind=cp), dimension(:,:),   allocatable     :: Lat !lattice translations
      real(kind=cp), dimension(:,:) ,  allocatable     :: orb,morb !Temporal orbits
      real(kind=cp), dimension(:),     allocatable     :: rI,rpI !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)     :: ts,inv_ts    ! ts=(t,tI)  Total translation in superspace
      integer, dimension(3)                 :: mM           ![m].M  3-components vector      !                         /  g    0   t  \
      integer, dimension(:),   allocatable  :: mE,pt           ![m].Eg  nk-vector               !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(:,:), allocatable  :: Hg           !M (nk,3)                        !                         \   0    0   1 /
      integer, dimension(3,3)               :: g, magm      !g, magm= delta * det(g) * g
      integer, dimension(:,:), allocatable  :: Eg           !Eg (nk,nk)

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
      !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]Eg) cos[2pi [m]Eg.rI(mu)] + Ps(mu)^([m]Eg) sin[2pi [m]Eg.rI(mu)] ]
      !
      !Calculation of the maximum number of additional lattice translations
      nt=(TBOX(1)+1)*(TBOX(2)+1)*(TBOX(3)+1)
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices

      !Allocation of main arrays
      Select Type(SpG)
        type is(SuperSpaceGroup_Type)
          allocate(mE(SpG%nk),    Hg(SpG%nk,3),    Eg(SpG%nk,SpG%nk), rI(SpG%nk), rpI(SpG%nk))
      End Select
      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      allocate(Lat(3,nt)) !Allocate lattice translations in superspace
      nt=0
      do i1=0,TBOX(1)
        do i2=0,TBOX(2)
           do i3=0,TBOX(3)
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
        if(allocated(morb)) deallocate(morb)
        if(allocated(pt)) deallocate(pt)
        allocate(orb(3,n_mO),morb(3,n_mO),pt(n_mO))
        orb=0.0
        morb=0.0
        pt=1
        Select Type(at => A%Atom(i))

          class is (MAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
                n_orb= 0
                xs   = At%x
                ms=At%Moment
                do_s: do s=1,SpG%Multip
                    g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
                    magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                                     !                         /  g    0   t  \
                   ts(:) = SpG%Op(s)%Mat(1:d,d+1)    !   Superspace operator: |  Hg   Eg   tI  |    ts=(t,tI)
                   Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)  !                         \   0    0   1 /

                   xsp = matmul(g,xs)+ts(1:3)
                   msp = matmul(magm,ms)

                   !Applying translations
                   do_L:do L=1,n_tr
                      u_mu= 0.0; m_mu=0.0
                      xarg= 0.0
                      xst = xsp  + Lat(:,L)
                      do j=1,SpG%nk
                        rI(j)= dot_product(SpG%kv(:,j), xs + Lat(:,L))  !notice that here we put the original representant xs
                      end do
                      do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                         k=at%pdc_q(j)
                         mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                         sig=1.0
                         if(equal_vector(mE,Spg%q_coeff(:,k))) then
                           sig=1.0
                         else if(equal_vector(mE,-Spg%q_coeff(:,k))) then
                           sig=-1.0
                         end if
                         xarg=-2.0*pi*dot_product(mE,rI)
                         u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + sig * at%Dcs(4:6,j)*sin(xarg)
                      end do
                      u_nu = matmul(g,u_mu)
                      n_orb = n_orb+1
                      orb(:,n_orb) = xst + u_nu

                      if(A%Atom(i)%Magnetic) then
                        xarg=0.0; m_mu=0.0
                        do j=1,At%n_mc !Moment amplitudes
                           k=at%pmc_q(j)
                           mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                           if(equal_vector(mE,Spg%q_coeff(:,k))) then
                             sig=1.0
                           else if(equal_vector(mE,-Spg%q_coeff(:,k))) then
                             sig=-1.0
                           end if
                           xarg=-2.0*pi*dot_product(mE,rI)
                           m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + sig * at%Mcs(4:6,j)*sin(xarg)
                        end do
                        m_nu=matmul(magm,m_mu)
                        !Homogeneous moment + modulation
                        morb(:,n_orb) = msp + m_nu
                      end if
                   end do do_L
                end do do_s

                do j=1,n_orb
                  orb(:,j)=orb(:,j)/real(tbox)
                end do

                !Eliminate duplicates and re-count the effetively generated atoms
                !do j=1,n_orb
                !   if(pt(j) == 0) cycle
                !   do k=j+1,n_orb
                !      if(pt(k) == 0) cycle
                !      if (sum(abs(orb(:,j)-orb(:,k))) <= 0.0001)  then
                !        pt(k) = 0
                !      end if
                !   end do
                !end do
                do j=1,n_orb
                   write(*,"(2i6,3f12.5)") j,pt(j),orb(:,j)
                end do

                nt=sum(pt(1:n_orb))

                allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt))
                allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%ChemSymb(nt),Ol%orbit(i)%pts(nt))
                Ol%orbit(i)%ChemSymb(1:nt)=A%Atom(i)%ChemSymb
                Ol%orbit(i)%mult=nt
                j=0
                do k=1,n_orb
                  if(pt(k) == 0) cycle
                  j=j+1
                  Ol%orbit(i)%pos(:,j)=orb(:,k)
                  Ol%orbit(i)%mom(:,j)=morb(:,k)
                  write(unit=Ol%orbit(i)%lab(j),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",j
                  Ol%orbit(i)%lab(j)=pack_string(Ol%orbit(i)%lab(j))
                end do
            End select

        End Select

      end do !Atoms

    End Subroutine Get_MAtoms_inBOX

    Subroutine old_Get_MAtoms_inBOX(A,SpG,TBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3), intent(in)    :: TBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, n, ins, s,nt,d, ds, i1,i2,i3,n_mO, n_tr, n_orb
      real(kind=cp)                                    :: xarg
      real(kind=cp), dimension(3)                      :: xs,xsp,xst,rv
      real(kind=cp), dimension(SpG%d)                  :: rs,rsd,rsi
      real(kind=cp), dimension(3)                      :: u_mu,m_mu, u_nu,m_nu
      real(kind=cp), dimension(3)                      :: ms,msp,mst
      real(kind=cp), dimension(:,:),   allocatable     :: Lat !lattice translations
      real(kind=cp), dimension(:,:) ,  allocatable     :: orb,morb !Temporal orbits
      real(kind=cp), dimension(:),     allocatable     :: rI,rpI !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)     :: ts,inv_ts    ! ts=(t,tI)  Total translation in superspace
      integer, dimension(3)                 :: mM           ![m].M  3-components vector      !                         /  g    0   t  \
      integer, dimension(:),   allocatable  :: mE,inv_mE    ![m].Eg  nk-vector               !   Superspace operator: |   Hg   Eg  tI  |   ts=(t,tI)
      integer, dimension(:,:), allocatable  :: Hg,Ng        !M (nk,3)                        !                         \   0    0   1 /
      integer, dimension(3,3)               :: g, magm      !g, magm= delta * det(g) * g     !                         /  inv_g    0      it  \
      integer, dimension(3,3)               :: inv_g        !g, magm= delta * det(g) * g     ! Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
      integer, dimension(:,:), allocatable  :: Eg,inv_Eg    !E (nk,nk)                       !                         \     0      0      1  /

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
      !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]Eg) cos[2pi [m]Eg.rI(mu)] + Ps(mu)^([m]Eg) sin[2pi [m]Eg.rI(mu)] ]
      !
      !Calculation of the maximum number of additional lattice translations
      nt=(TBOX(1)+1)*(TBOX(2)+1)*(TBOX(3)+1)
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices

      !Allocation of main arrays
      Select Type(SpG)
        type is(SuperSpaceGroup_Type)
          allocate(mE(SpG%nk),    Hg(SpG%nk,3),    Eg(SpG%nk,SpG%nk), rI(SpG%nk), rpI(SpG%nk))
          !allocate(inv_mE(SpG%nk),Ng(SpG%nk,3),inv_Eg(SpG%nk,SpG%nk) )
      End Select
      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      allocate(Lat(3,nt)) !Allocate lattice translations in superspace
      nt=0
      do i1=0,TBOX(1)
        do i2=0,TBOX(2)
           do i3=0,TBOX(3)
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
        if(allocated(morb)) deallocate(morb)
        allocate(orb(3,n_mO),morb(3,n_mO))
        orb=0.0
        morb=0.0

        Select Type(at => A%Atom(i))

          class is (MAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
                n_orb= 0
                xs   = At%x
                ms=At%Moment
                do j=1,SpG%nk
                  rI(j)= dot_product(SpG%kv(:,j),xs)
                end do
                rs=[xs,ri,1.0] !Initial Superspace vector position
                do_s: do s=1,SpG%Multip
                    !ins=SpG%inv(s)
                    g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
                    magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                                     !                         /  g    0   t  \
                     !ts(:) = SpG%Op(s)%Mat(1:d,d+1) !   Superspace operator: |  Hg   Eg   tI  |    ts=(t,tI)
                   Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)  !                         \   0    0   1 /
                   !Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3)

                !inv_g(:,:) = SpG%Op(ins)%Mat(1:3,1:3) !                           /  inv_g    0      it  \
                ! inv_ts(:) = SpG%Op(ins)%Mat(1:d,d+1) !   Inverse operator:      |    Ng     inv_Eg  itI  |   inv_ts=(it,itI)
                !   Ng(:,:) = SpG%Op(ins)%Mat(4:d,4:d) !                           \    0      0       1  /
                !   Ng(:,:) = SpG%Op(ins)%Mat(4:d,1:3)
                ! magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                !rsi=matmul(SpG%Om(:,:,ins),rs)

                   rsd = matmul(SpG%Om(:,:,s),rs) !Transformed superspace vector position
                   xsp = rsd(1:3)                 !Transformed external space vector position
                   rpI = rsd(4:d)                 !Transformed internal space vector position
                   msp = matmul(magm,ms)          !Transformation of homogeneous moment
                   rv   = 0.0
                   !
                   !  Calculation of the modulation functions
                   !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]Eg) cos[2pi [m]Eg.rI(mu)] + Ps(mu)^([m]Eg) sin[2pi [m]Eg.rI(mu)] ]
                   !
                   !  SpG%q_coeff(:,k) = [n1 n2 .. nd](k)
                   n_orb=n_orb+1
                   xarg= 0.0; u_mu= 0.0
                   do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series
                      k=at%pdc_q(j)
                      mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                      xarg= 2.0*pi*dot_product(mE,rpI)
                      u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + at%Dcs(4:6,j)*sin(xarg)
                   end do
                   u_nu = matmul(g,u_mu)
                   orb(:,n_orb) = xsp + u_nu
                   if(A%Atom(i)%Magnetic) then
                     xarg=0.0; m_mu=0.0
                     !Homogeneous moment
                     do j=1,At%n_mc !Moment amplitudes
                        k=at%pmc_q(j)
                        mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                        xarg= 2.0*pi*dot_product(mE,rpI)
                        m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + at%Mcs(4:6,j)*sin(xarg)
                     end do
                     m_nu=matmul(magm,m_mu)
                     morb(:,n_orb) = msp+m_nu
                   end if

                   !Applying translations
                   do_L:do L=1,n_tr
                      u_mu= 0.0; m_mu=0.0
                      xarg= 0.0
                      xst = xs  + Lat(:,L)
                      do j=1,SpG%nk
                        rI(j)= dot_product(SpG%kv(:,j),xst)
                      end do
                      rs=[xs,rI,1.0] !translated Superspace vector position

                      do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                         k=at%pdc_q(j)
                         mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                         xarg= 2.0*pi*dot_product(mE,rI)
                         u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + at%Dcs(4:6,j)*sin(xarg)
                      end do
                      u_nu = matmul(g,u_mu)
                      n_orb = n_orb+1
                      orb(:,n_orb) = xst+u_nu

                      if(A%Atom(i)%Magnetic) then
                        xarg=0.0; m_mu=0.0
                        !Homogeneous moment
                        do j=1,At%n_mc !Moment amplitudes
                           k=at%pmc_q(j)
                           mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                           xarg= 2.0*pi*dot_product(mE,rI)
                           m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + at%Mcs(4:6,j)*sin(xarg)
                        end do
                        m_nu=matmul(magm,m_mu)
                        morb(:,n_orb) = msp+m_nu
                      end if
                   end do do_L
                end do do_s

                allocate(Ol%orbit(i)%pos(3,n_orb),Ol%orbit(i)%mom(3,n_orb))
                allocate(Ol%orbit(i)%Lab(n_orb),Ol%orbit(i)%ChemSymb(n_orb),Ol%orbit(i)%pts(nt))
                Ol%orbit(i)%mult=n_orb
                Ol%orbit(i)%ChemSymb(1:n_orb)=A%Atom(i)%ChemSymb
                do k=1,n_orb
                  Ol%orbit(i)%pos(:,k)=orb(:,k)
                  Ol%orbit(i)%mom(:,k)=morb(:,k)
                  write(unit=Ol%orbit(i)%lab(k),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",k
                  Ol%orbit(i)%lab(k)=pack_string(Ol%orbit(i)%lab(k))
                end do
            End select

        End Select

      end do !Atoms

    End Subroutine old_Get_MAtoms_inBOX

    Subroutine SupGet_MAtoms_inBOX(A,SpG,tBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3),   intent(in)  :: tBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, n,s,nt,d, ds, i1,i2,i3,n_Om, n_tr, n_orb, n_at
      real(kind=cp)                                    :: xarg, sig
      real(kind=cp), dimension(:,:),   allocatable     :: x_at
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(3)                      :: u_mu,m_mu,xwithin,rv
      real(kind=cp), dimension(Spg%d)                  :: ms,msp
      real(kind=cp), dimension(:,:),   allocatable     :: Lat !lattice translations
      real(kind=cp), dimension(:,:,:), allocatable     :: Om  !Extended set of operators
      real(kind=cp), dimension(:),     allocatable     :: det_tim,rI  !Product of determinant by time inversion, internal vector position
      real(kind=cp), dimension(:,:) ,  allocatable     :: orb,morb !Temporal orbits
      Integer,       dimension(:,:) ,  allocatable     :: Eg,Ls
      integer,       dimension(:),     allocatable     :: mE, pt
      logical :: within
      !Calculation of the maximum number of additional lattice translations
      nt=(TBOX(1)+2)*(TBOX(2)+2)*(TBOX(3)+2)
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices

      allocate(Lat(ds,nt)) !Allocate lattice translations in superspace

      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      nt=1
      Lat(:,nt)=0.0
      do i1=-1,TBOX(1)
        do i2=-1,TBOX(2)
           do i3=-1,TBOX(3)
              if(i1 == 0 .and. i2 == 0 .and. i3 == 0) cycle
              nt=nt+1
              Lat(1:3,nt) = real([i1,i2,i3],kind=cp)  !Just update the External space
           end do
        end do
      end do
      n_Om=Spg%multip*nt
      n_tr=nt
      allocate (Om(Spg%d,Spg%d,n_Om),det_tim(n_Om),x_at(3,n_Om))
      Select Type(SpG)
        type is (SuperSpaceGroup_Type)
           Om(:,:,1:Spg%Multip)=SpG%Om  !Operators of zero cell
           do i=1,Spg%Multip
             det_tim(i)=SpG%Op(i)%dt * SpG%Op(i)%time_inv
           end do
           allocate(Eg(SpG%nk,SpG%nk), rI(SpG%nk),mE(SpG%nk))
        class default
           do i=1,SpG%Multip
              Om(:,:,i)=SpG%Op(i)%Mat(:,:)
              det_tim(i)=SpG%Op(i)%dt * SpG%Op(i)%time_inv
           end do
      End Select

      !Generate the orbits of all atoms

      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      do i=1,A%natoms
        xs=0.0; ms=0.0; xs(ds)=1.0; ms(ds)=1.0
        xsp=0.0; msp=0.0; xsp(ds)=1.0; msp(ds)=1.0
        if(allocated(orb)) deallocate(orb)
        if(allocated(morb)) deallocate(morb)
        if(allocated(pt)) deallocate(pt)
        if(allocated(Ls)) deallocate(Ls)
        allocate(orb(ds,n_Om),morb(ds,n_Om),pt(n_Om),Ls(2,n_Om))
        orb=0.0; orb(ds,:)= 1.0; pt=1; Ls=0
        morb=0.0; morb(ds,:)= 1.0

        Select Type(at => A%Atom(i))

          class is (MAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
              n_orb=0
              x_at=0.0; n_at=0
              do_s: do s=1,SpG%Multip

                do_L:do L=1,n_tr
                  u_mu=0.0; m_mu=0.0
                  xarg=0.0
                  Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)
                  xs(1:3)=At%x
                  xsp=matmul(Om(:,:,s),xs)+Lat(:,L)
                  xwithin=xsp(1:3)/tbox
                  !xwithin=xsp(1:3)
                  rv=0.0  !Returning vector
                  do j=1,3
                    do
                       if(xwithin(j) < 0) then
                         xwithin(j)=xwithin(j)+1.0
                         rv(j)=rv(j)+1.0
                       else if(xwithin(j) >= 1.0) then
                         xwithin(j)=xwithin(j)-1.0
                         rv(j)=rv(j)-1.0
                       else
                         exit
                       end if
                    end do
                  end do
                  !rv=rv*tbox
                  xsp(1:3)=xsp(1:3)+rv
                  do n=n_at,1,-1
                    if(sum(abs(xsp(1:3)-x_at(:,n))) < 0.002) cycle do_s
                  end do
                  n_at=n_at+1
                  x_at(:,n_at) = xsp(1:3)
                  if(At%Magnetic) then
                     ms(1:3)=At%Moment
                     msp(1:d)=det_tim(s)*matmul(Om(1:d,1:d,s),ms(1:d))
                  end if
                  do j=1,SpG%nk
                    rI(j)= dot_product(SpG%kv(:,j), xs(1:3) + Lat(1:3,L))  !notice that here we put the original representant xs
                  end do
                  do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                     k=at%pdc_q(j)
                     mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                     sig=1.0
                     if(equal_vector(mE,Spg%q_coeff(:,k))) then
                       sig=1.0
                     else if(equal_vector(mE,-Spg%q_coeff(:,k))) then
                       sig=-1.0
                     end if
                     xarg=-2.0*pi*dot_product(mE,rI)
                     u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + sig * at%Dcs(4:6,j)*sin(xarg)
                  end do
                  u_mu(1:3)=matmul(Om(1:3,1:3,s),u_mu(1:3))

                  if(A%Atom(i)%Magnetic) then
                    xarg=0.0
                    do j=1,At%n_mc !Moment amplitudes
                       k=at%pmc_q(j)
                       mE=matmul(SpG%q_coeff(:,k),Eg) !Permutation and change of sign for some components of [n1 n2 .. nd](k)
                       sig=1.0
                       if(equal_vector(mE,Spg%q_coeff(:,k))) then
                         sig=1.0
                       else if(equal_vector(mE,-Spg%q_coeff(:,k))) then
                         sig=-1.0
                       end if
                       xarg=-2.0*pi*dot_product(mE,rI)
                       m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + sig * at%Mcs(4:6,j)*sin(xarg)
                    end do
                    m_mu(1:3)=det_tim(s)*matmul(Om(1:3,1:3,s),m_mu(1:3))
                  end if
                  xsp(1:3)=xsp(1:3)+u_mu  !Adding modulations in External space
                  if(At%Magnetic) then
                     msp(1:3)=msp(1:3)+m_mu
                  end if
                  do j=1,SpG%nk
                    xsp(3+j)=dot_product(xsp(1:3),SpG%kv(:,j))      !Extends the position and moment to superspace
                    msp(3+j)=dot_product(msp(1:3),SpG%kv(:,j))      !Modulations in external space already added
                  end do
                  n_orb=n_orb+1
                  !Generate the orbit of atom i

                  orb(:,n_orb)  = xsp
                  morb(:,n_orb) = msp
                  Ls(1,n_orb) = L
                  Ls(2,n_orb) = s
                end do do_L
              end do do_s
              do j=1,n_orb
                orb(1:3,j)=orb(1:3,j)/real(tbox)
              end do

              !Eliminate duplicates and re-count the effetively generated atoms
              !do j=1,n_orb
              !   if(pt(j) == 0) cycle
              !   do k=j+1,n_orb
              !      if(pt(k) == 0) cycle
              !      if (sum(abs(orb(1:3,j)-orb(1:3,k))) <= 0.0001 .and. Ls(2,k) > Ls(2,j) ) then ! .and. Ls(1,j) == Ls(1,k) )  then
              !        pt(k) = 0
              !      end if
              !   end do
              !end do

              nt=sum(pt(1:n_orb))

              allocate(Ol%orbit(i)%pos(ds,nt),Ol%orbit(i)%mom(ds,nt))
              allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%ChemSymb(nt))
              allocate(Ol%orbit(i)%Latt(3,nt),Ol%orbit(i)%Ls(nt),Ol%orbit(i)%pts(nt))
              Ol%orbit(i)%ChemSymb(1:nt)=A%Atom(i)%ChemSymb
              Ol%orbit(i)%mult=nt
              j=0
              do k=1,n_orb
                if(pt(k) == 0) cycle
                j=j+1
                Ol%orbit(i)%pos(:,j)=orb(:,k)
                Ol%orbit(i)%mom(:,j)=morb(:,k)
                Ol%orbit(i)%pts(j)=Ls(1,k)
                Ol%orbit(i)%Ls(j) =Ls(2,k)
                Ol%orbit(i)%Latt(:,j)=Lat(1:3,Ls(1,k))
                write(unit=Ol%orbit(i)%lab(j),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",j
                Ol%orbit(i)%lab(j)=pack_string(Ol%orbit(i)%lab(j))
              end do

            End select

        End Select

      end do !Atoms

    End Subroutine SupGet_MAtoms_inBOX

 End Module Atoms_in_BOX

 Program Test_SHX_CIF_CFL
    !---- Use Modules ----!
    use CFML_Globaldeps
    use CFML_Maths,        only: Set_EPS_Math
    use CFML_Strings,      only: File_type, u_case, Get_extension
    use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell, Change_Setting_Cell
    use CFML_gSpaceGroups, only: Spg_Type, SuperSpaceGroup_Type, Write_SpaceGroup_Info, &
                                 Get_moment_ctr, Get_TFourier_Ctr, Get_Orbit, point_orbit,Get_Inv_OP
    use CFML_Atoms,        only: AtList_Type, Write_Atom_List, MAtm_Std_Type
    use CFML_IOForm
    use Atoms_in_BOX

    !---- Local Variables ----!
    implicit none

    character(len=:), allocatable       :: ext,line
    character(len=512)                  :: fname,cmdline
    integer                             :: nlong,narg
    real(kind=cp)                       :: start, fin
    type(AtList_Type)                   :: Atm
    type(orbit_list)                    :: Ol
    integer, dimension(3)               :: TBOX=[2,1,2]
    integer, dimension(3)               :: mcell

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    character(len=256)                  :: setting,ctr_code,forma,formb
    character(len=256),dimension(26)    :: tctr_code
    type(Cell_G_Type)                   :: Cell,Celln
    class(Spg_Type), allocatable        :: Grp
    type(File_type)                     :: flist
    type(Point_Orbit)                   :: orb,orb3D
    integer :: i, j, L,k, d,Dd,nsg, ind, indexg, num_group, ier,codini,len_cmdline
    real(kind=cp), dimension(3)               :: codes=1.0
    real(kind=cp), dimension(:,:),allocatable :: codeT
    integer,       dimension(:),  allocatable :: ptr

    !> Init
    narg=COMMAND_ARGUMENT_COUNT()
    cmdline=" "; nlong=0
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
       mcell=0.0
       do i=1,flist%nlines
         line=adjustl(flist%line(i)%str)
         if(line(1:5) =="MCELL") then
           read(line(6:),*) mcell
           tbox(:)=mcell
           exit
         end if
       end do
       call Write_Crystal_Cell(Cell)
       if(len_trim(Grp%setting) /= 0) then
         write(*,"(/,a)") " => Transformed Cell"
         if(Grp%D > 4) then
           i=index(Grp%setting,"d")
           setting=Grp%setting(1:d-2)//";0,0,0"
         else
           setting=Grp%setting
         end if
         call Change_Setting_Cell(Cell,setting,Celln)
         call Write_Crystal_Cell(Celln)
       end if
       !Determine the inverse operators
       Grp%Inv= Get_Inv_OP(Grp%Op)

       call Write_SpaceGroup_Info(Grp)

       i=index(fname,".")
       call Write_Cif_Template(fname(1:i)//"cif", Cell, Grp, Atm, 2, "Testing WriteCIF")

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
          forma="(i5, f10.5,tr8, f10.5,i8,tr5,3i4)"
          write(forma(5:5),"(i1)") Grp%d-1
          write(forma(16:16),"(i1)") Grp%d-1
          write(*,"(//a)") "  Orbits of atoms after applying constraints on moments:"
          write(*,"(  a)") "  ======================================================"


          do i=1,Atm%natoms
            !codini=1; codes=1.0
            call Get_moment_ctr(Atm%Atom(i)%x,Atm%Atom(i)%moment,Grp,codini,codes,ctr_code=ctr_code)!,Ipr=6)
            write(*,"(a,3f10.5,a)") " => Moment of atom "//trim(Atm%Atom(i)%Lab)//": ",Atm%Atom(i)%moment,"    CtrCode: "//trim(ctr_code)

            call Get_Orbita(Atm%Atom(i)%x,Grp,orb,Atm%Atom(i)%moment,orb3D)
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

            do j=1,orb%Mult
                write(*,forma) j,orb%pos(:,j),orb%mom(:,j),orb%pts(j),orb%Lat(1:3,j)
            end do
            write(*,"(/,a)") " => Orbit of atom: "//trim(Atm%Atom(i)%Lab)//"  restricted to 3D"
            do j=1,orb3D%Mult
                write(*,"(i5,3f10.5,tr8,3f10.5,i8,tr5,3i4)") j,orb3D%pos(:,j),orb3D%mom(:,j),orb3D%pts(j),orb3D%Lat(:,j)
            end do

           Select Type(at => Atm%Atom(i))

             class is (MAtm_Std_Type)
               write(*,"(a)") " => Modulation amplitudes of atom: "//trim(Atm%Atom(i)%Lab)
               if(allocated(CodeT)) deallocate(CodeT)
               allocate(CodeT(6,at%n_mc))
               CodeT=1.0
               Select Type (Grp)
                 Type is (SuperSpaceGroup_Type)
                    call Get_TFourier_Ctr(At%x,At%Mcs(:,1:at%n_mc),codeT,Grp,codini,"M",ctr_code=tctr_code)
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
                      !Atm%Atom(i)%Dcs(:,j) = At%Dcs(:,j)
                    end do
               end select
           end select
          end do

       ! Testing atoms in BOX
          !call Get_MAtoms_inBOX(Atm,Grp,TBOX,Ol)
          call SupGet_MAtoms_inBOX(Atm,Grp,TBOX,Ol)
          !call Jmol_MAtoms_inBOX(Atm,Grp,TBOX,Ol)
          formb="(a15,tr5,a, 3f14.6,2i4,a,3i4,a)"
          !write(formb(12:13),"(i2)") Grp%D
          do i=1,Ol%num_orbs
            write(*,"(/,a,i4)") " Orbit of atom: ",i
            do j=1,Ol%orbit(i)%mult
               write(*,formb) Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb(j),Ol%orbit(i)%pos(1:3,j),Ol%orbit(i)%pts(j),Ol%orbit(i)%Ls(j),"  [",Ol%orbit(i)%Latt(:,j)," ]"
               write(*,"(tr22,3f14.6)") Ol%orbit(i)%mom(1:3,j)
            end do
          end do
          call Write_CIF_P1()
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
        write(unit=Ipr,fmt="(a)") "_atom_site_occupancy"
        do i=1,Ol%num_orbs
          do j=1,Ol%orbit(i)%mult
             write(Ipr,"(a15,tr5,a,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%ChemSymb(j),Ol%orbit(i)%pos(1:3,j) !/mcell
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
             write(Ipr,"(a15,tr5,10f14.6)") Ol%orbit(i)%Lab(j),Ol%orbit(i)%mom(1:3,j)
          end do
        end do
        close(unit=Ipr)
     End Subroutine Write_CIF_P1
End Program Test_SHX_CIF_CFL