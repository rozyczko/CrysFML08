!!----
SubModule (CFML_gSpaceGroups) SPG_Stabilizer_Constraints
    implicit none

    character (len=*), dimension(26),parameter   :: &
    cdd=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r', &
         's','t','u','v','w','x','y','z']
   Contains

   !!----
   !!---- GET_STABILIZER
   !!----    Subroutine to obtain the list of symmetry operators of a space group that leaves
   !!----    invariant an atomic position. This subroutine provides a pointer to the symmetry
   !!----    operators of the site point group and the additional translation with respect to
   !!----    the canonical representative.
   !!----
   !!---- 13/06/2019
   !!
   Module Subroutine Get_Stabilizer(X, Spg,Order,Ptr,Atr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),  intent (in)  :: x     ! real space position (fractional coordinates)
      class(Spg_Type),              intent (in)  :: Spg   ! Space group
      integer,                      intent(out)  :: order ! Number of sym.op. keeping invariant the position x
      integer, dimension(:),        intent(out)  :: ptr   ! Array pointing to the symmetry operators numbers
                                                          ! of the stabilizer of x
      real(kind=cp), dimension(:,:),intent(out)  :: atr   ! Associated additional translation to the symmetry operator

      !---- Local variables ----!
      real(kind=cp), dimension(3)    :: xx, tr
      integer                        :: j,n1,n2,n3

      !> Init
      order = 1              ! Identity belongs always to the stabilizer
      ptr   = 0; ptr(1)= 1
      atr   = 0.0_cp

       do n1=-1,1
          do n2=-1,1
             do n3=-1,1
               tr=real([n1, n2, n3])
               do j=2,Spg%NumOps
                  xx=Apply_OP(Spg%Op(j),x)  - x   + tr
                  if (sum(abs(xx)) > 2.0 * EPSS) cycle
                  order=order+1
                  ptr(order)=j
                  atr(:,order)=tr
               end do
             end do
          end do
       end do

   End Subroutine Get_Stabilizer

   !! Module Subroutine Get_Orbit(x,Spg,orbit,mom,orb3D,convl,Tbox)
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
   Module Subroutine Get_Orbit(x,Spg,orbit,mom,orb3D,convl,Tbox)
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
      real(kind=cp), dimension(Spg%d)                  :: momd

      conv=.true.
      if(present(convl)) conv=convl
      d=Spg%d-1
      xs(Spg%d)=1.0_cp
      ms=0.0
      ms(Spg%d)=1.0_cp
      orb=0.0_cp; morb=0.0_cp
      ptr=0
      momd=0.0; momd(Spg%d)=1.0_cp
      if(present(mom)) momd(1:3)=mom(1:3)

      Select Type(SpG)

        type is (SuperSpaceGroup_Type)
           Om=SpG%Om
           xs(1:3)=x   !Extend the position and moment to superspace
           ms(1:3) = momd(1:3)
           do i=1,SpG%nk
             xs(3+i)=dot_product(x,SpG%kv(:,i))
             ms(3+i)=dot_product(momd(1:3),SpG%kv(:,i))
           end do
        class default
           do i=1,Spg%Multip
             Om(:,:,i)=Spg%Op(i)%Mat
           end do
           xs(1:d)=x(1:d)
           ms(1:d) = momd(1:d)
      End Select

      allocate(ptr(Spg%multip))
      mult=1
      call Lat_Modulo(xs(1:d),v(1:d),Latt)
      orb(:,1)=v(1:d)
      Lat(:,1)=Latt
      ptr(mult) = 1
      morb(1:d,1)=ms(1:d)

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
         morb(1:d,mult)=msp(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(Tbox)) then  !Add supplemental cells
        n_tr=tbox(1)*tbox(2)*tbox(3); L=0
        allocate(add_Lat(d,n_tr))
        add_Lat=0
        do i1=0,tbox(1)-1
          do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
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
        allocate(orbit%mom(d,nt))
        orbit%pos(:,1:mult) = orb(:,1:mult)
        orbit%pts(1:mult)   = ptr(1:mult)
        orbit%Lat(:,1:mult) = Lat(:,1:mult)
        orbit%mom(:,1:mult) = morb(:,1:mult)
        nt=mult
        do L=1,n_tr
          do n=1,mult
            orbit%pos(:,nt+n)=orb(:,n)+ add_Lat(:,L)
            orbit%Lat(:,nt+n)=Lat(:,n)+ add_Lat(:,L)
            orbit%pts(nt+n)= ptr(n)
            orbit%mom(:,nt+n)=morb(:,n)
          end do
          nt=nt+mult
        end do
      else
         orbit%Mult=mult
         orbit%pos=orb(:,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(:,1:mult)
         orbit%mom=morb(:,1:mult)
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
        allocate(orb3D%mom(3,nt))
        do i=1,nt
          orb3D%pos(:,i)= orbE(:,i)
          orb3D%Lat(:,i)= LatE(:,i)
          orb3D%pts(i)  = orbit%pts(ptr(i)) !This points to the symmetry operator
          orb3D%mom(:,i) = orbit%mom(1:3,ptr(i))
        end do
      end if

   End Subroutine Get_Orbit

   !!
   !!----  Module Subroutine Get_moment_ctr(xnr,moment,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr      ! Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(3),            intent(in out) :: moment   ! Magnetic moment at position xnr
   !!----     class(SuperSpaceGroup_Type),            intent(in)     :: Spg      ! Super Space Group
   !!----     Integer,                                intent(in out) :: codini   ! Number of the Last attributed parameter
   !!----     real(kind=cp), dimension(3),            intent(in out) :: codes    ! codewords for magnetic moment
   !!----     integer,                       optional,intent(in)     :: ord      ! Order of stabilizer
   !!----     integer, dimension(:),         optional,intent(in)     :: ss       ! Pointer to operators
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att      ! Translations of stabilizer operators
   !!----     integer,                       optional,intent(in)     :: Ipr      ! Logical unit for writing
   !!----     character(len=*),              optional,intent(out)    :: ctr_code ! Symmetry code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: March 2020
   !!----
   !!
   Module Subroutine Get_moment_ctr(xnr,moment,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(3),            intent(in)     :: xnr
      real(kind=cp), dimension(:),            intent(in out) :: moment
      class(SpG_type),                        intent(in)     :: Spg
      Integer,                                intent(in out) :: codini
      real(kind=cp), dimension(:),            intent(in out) :: codes
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),              optional,intent(out)    :: ctr_code

      ! Local variables
      character(len=1),  dimension(3)   :: codd
      character(len=15), dimension(3)   :: St_Cod
      character(len=:), allocatable     :: mag
      integer                           :: i,j,order,n,ig,iss
      real(kind=cp)                     :: suma
      integer,           dimension(48)  :: ss_ptr
      real(kind=cp),     dimension(3,48):: atr
      real(kind=cp),     dimension(3)   :: cod,multi
      real(kind=cp),     dimension(3)   :: x
      real(kind=cp),     dimension(3,3) :: magm  !g, magm= delta * det(g) * g
      real(kind=dp),     dimension(3,3) :: sCtr
      real(kind=cp),     dimension(3)   :: momentL,TotMom


      !Test if all codes are given ... in such a case the user constraints
      !are prevalent

      suma=0.0_cp
      if(present(ctr_code)) ctr_code="(0,0,0)"
      n=3 !Real moments -> three components
      do j=1,3
         suma=suma+abs(codes(j))
      end do

      if(suma < epss ) return  !No refinement is required
      if(present(Ipr)) then
        write(Ipr,"(/,a)")         " => Calculation of symmetry constraints for magnetic moments "
      end if
      x=xnr
      !where(x < 0.0) x=x+1.0
      !where(x > 1.0) x=x-1.0

      if(present(ord) .and. present(ss) .and. present(att)) then
        order=ord
        ss_ptr(1:order) = ss(1:ord)
        atr(:,1:order)  = att(:,1:ord)
      else
        call get_stabilizer(x,SpG,order,ss_ptr,atr)
        if(present(ipr)) Write(unit=ipr,fmt="(a,i3)") " => Stabilizer without identity, order:",order
      end if

      momentL=moment
      sCtr=0.0_cp
      if(order > 1) then
        do ig=1,order
          j=ss_ptr(ig)
          magm(:,:) = real(Spg%Op(j)%Mat(1:3,1:3))*Spg%Op(j)%dt*Spg%Op(j)%time_inv
          mag=Set_Symb_From_Mat(magm,["u","v","w"])
          sCtr=sCtr+magm !Adding constraint matrices for each operator of stabilizer
          if(present(ipr)) then
            write(unit=ipr,fmt='(a,i2,a,t20,a,t55,a,t75,9f8.4)') '     Operator ',ig,": ",trim(Spg%Symb_Op(j)), &
             trim(mag), sCtr
          end if
        end do  !ig operators
        sCtr=sCtr/order
        suma=sum(abs(sCtr))
        !write(*,"(a,f10.4,a,i3)") " suma:",suma, "Mag_Type:", spg%mag_type
        if(suma < epss .or. spg%mag_type == 2) then !This corresponds to a grey point group
           moment=0.0_cp
           codes=0.0_cp
           if(present(Ipr)) then
             write(Ipr,"(a)")         " Grey point group or symmetry paramagnetic site: the attached moment is zero "
             write(Ipr,"(a,24f14.6)") " Final codes: ",codes(1:n)
             write(Ipr,"(a,24f14.6)") " Constrained moment: ",moment
           end if
           return
        end if
        TotMom=matmul(sCtr,momentL)
        call Get_Refinement_Codes(n,TotMom,sCtr,iss,multi,codd,momentL)
        cod=0.0
        do j=1,n
          if(codd(j) /= "0") then
            do i=1,iss
              if(codd(j) == cdd(i)) then
                cod(j)=codini+i
                exit
              end if
            end do
          end if
        end do
        moment=momentL
        codes=0.0
        do j=1,n
          if(abs(multi(j)) > epss)  codes(j) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
        end do
        codini=codini+iss
        if(present(Ipr)) then
          Write(unit=Ipr,fmt="(a,i4)")       " Number of free parameters: ",iss
          write(unit=Ipr,fmt="(a,3f14.6)")   " Multipliers: ",(multi(j), j=1,n)
          write(unit=Ipr,fmt="(28a)")        " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,3i6)")      " Resulting integer codes: ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,3f14.6)")   " Final codes: ",codes(1:n)
          write(unit=Ipr,fmt="(a,3f14.6)")   " Constrained Moment: ",moment
        end if

      else !No restrictions

        codd(1:n)=cdd(1:n)
        multi(1:n)=1.0_cp
        do j=1,n
          cod(j)=codini+j
          codes(j) = (abs(cod(j))*10.0_cp + abs(multi(j)))
        end do
        codini=codini+n
        if(present(Ipr)) then
          write(unit=Ipr,fmt="(a)")         " General position, no constraints in moment "
          write(unit=Ipr,fmt="(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,24f14.6)") " Final codes: ",codes(1:n)
          write(unit=Ipr,fmt="(a,24f14.6)") " Constrained moment: ",moment
        end if

      end if

      if(present(ctr_code)) then
          do j=1,3
            St_cod(j)=" "
            if(abs(multi(j)) < 0.00001) then
              St_cod(j) = "0"
            else
              if(multi(j) == 1.0_cp) then
                 St_cod(j)=codd(j)
              else if(multi(j) == -1.0_cp) then
                 St_cod(j)="-"//codd(j)
              else
                 write(unit=St_cod(j),fmt="(f10.5,a)")  multi(j),"*"//codd(j)
                 St_cod(j)=adjustl(St_cod(j))
              end if
            end if
          end do
         write(unit=ctr_code,fmt="(28a)") " ( ",(St_cod(j)//", ",j=1,n-1),St_cod(j)//" )"
         ctr_code=pack_string(ctr_code)
      end if
   End Subroutine Get_moment_ctr

   Subroutine Get_Refinement_Codes(n,vect_val,Ctr,iss,multi,codd,vect_out)
     integer,                       intent(in)    :: n !dimension of the vector and the matrix
     real(kind=cp), dimension(:),   intent(in)    :: vect_val
     real(kind=dp), dimension(:,:), intent(in out):: Ctr
     integer,                       intent(out)   :: iss
     real(kind=cp), dimension(:),   intent(out)   :: multi
     character(len=*), dimension(:),intent(out)   :: codd
     real(kind=cp), dimension(:),   intent(out)   :: vect_out
     !--- Local variables ---!
     real(kind=cp), dimension(n)   :: val
     integer,       dimension(n)   :: pti
     real(kind=dp), dimension(n,n) :: zv
     integer                       :: i,j,k,kval,ip !,ier
     real(kind=dp)                 :: zmi
     real(kind=dp), dimension(n)   :: Wr, Wi
     logical,       dimension(n)   :: done

     !Diagonalize the matrix and pickup the lambda=1 eigenvalues
     !The corresponding eigenvector contains the constraints of all moment components
     !Calling the general diagonalization subroutine from EisPack
     call Diagonalize_RGen(n,Ctr,wr,wi,.true.,zv)
     iss=0
     pti=0
     kval=0
     do i=1,n
       if(abs(wr(i)-1.0_dp) < epss .and. abs(wi(i)) < epss) then
         iss=iss+1   !Number of eigenvalues = 1 => number of free parameters
         pti(iss)=i !This points to the eigenvectors with eigenvalue equal to 1.
         zmi=1.0e6 !normalize the eigenvectors so that the minimum (non-zero value) is 1.
         j=1
         do k=1,n
           if(abs(zv(k,i)) < epss) cycle
           if(abs(zv(k,i)) < zmi) then
             zmi=abs(zv(k,i))
             kval=k  !This is the basis value
             j=nint(sign(1.0_dp,zv(k,i)))
           end if
         end do
         zv(1:n,i)=j*zv(1:n,i)/zmi  !This provides directly the multipliers for a single lambda=1 eigenvalue
         val(iss)=vect_val(kval) !This is the basis value to construct the new Moment
       end if
     end do
     codd="0"
     vect_out=0.0
     multi=0.0
     done=.false.
     where(abs(vect_val) < epss) done=.true.
     Select Case(iss)
       case(1)
         vect_out(1:n)=val(1)*zv(1:n,pti(1))
         where(abs(vect_out) > epss)  codd(:)=cdd(1)
         multi(1:n)=zv(1:n,pti(1))
       case(2:)
         ip=0
         do i=1,n
           if(.not. done(i)) then
             if(abs(vect_val(i)) > epss) then
               ip=ip+1
               codd(i)=cdd(ip)
               multi(i)=1.0
               vect_out(i)=vect_val(i)
               done(i)=.true.
               do j=i+1,n
                 if(.not. done(j)) then
                   if(abs(vect_val(i)-vect_val(j)) < epss) then
                     codd(j)=cdd(ip)
                     multi(j)=1.0
                     vect_out(j)=vect_val(i)
                     done(j)=.true.
                   else if(abs(vect_val(i)+vect_val(j)) < epss) then
                     codd(j)=cdd(ip)
                     multi(j)=-1.0
                     vect_out(j)=-vect_val(i)
                     done(j)=.true.
                   end if
                 end if
               end do
             end if
           end if
         end do
     End Select
   End Subroutine Get_Refinement_Codes

   !!
   !!----  Module Subroutine Get_TFourier_ctr(xnr,TFourier,codes,SpG,codini,mode,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr      ! Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(:,:),          intent(in out) :: TFourier ! Fourier coefficients at position xnr
   !!----     real(kind=cp), dimension(:,:),          intent(in out) :: codes    ! codewords for Fourier components
   !!----     class(SuperSpaceGroup_Type),            intent(in)     :: Spg      ! Super Space Group
   !!----     Integer,                                intent(in out) :: codini   ! Number of the Last attributed parameter
   !!----     character(len=*),                       intent(in)     :: Mode     ! "M" or "D" for modulation moments or displacements
   !!----     integer,                       optional,intent(in)     :: ord      ! Order of stabilizer
   !!----     integer, dimension(:),         optional,intent(in)     :: ss       ! Pointer to operators
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att      ! Translations of stabilizer operators
   !!----     integer,                       optional,intent(in)     :: Ipr      ! Logical unit for writing
   !!----     character(len=*),              optional,intent(out)    :: ctr_code ! Symmetry code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: March 2020
   !!----
   !!
   Module Subroutine Get_TFourier_ctr(xnr,TFourier,codes,SpG,codini,mode,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(3),            intent(in)     :: xnr
      real(kind=cp), dimension(:,:),          intent(in out) :: TFourier
      real(kind=cp), dimension(:,:),          intent(in out) :: codes
      class(SuperSpaceGroup_Type),            intent(in)     :: SpG
      Integer,                                intent(in out) :: codini
      character(len=*),                       intent(in)     :: mode
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),dimension(:), optional,intent(out)    :: ctr_code

      ! Local variables
      integer,          dimension(SpG%nk)     :: brack_m  ! hs=(H,[m])
      real(kind=cp),    dimension(spg%d-1)    :: ts  ! ts=(t,tI)
      integer,          dimension(SpG%nk)     :: mE  ![m].E
      integer,          dimension(SpG%nk,3)   :: Mx  !M
      integer,          dimension(3,3)        :: g, magm   !g, magm= delta * det(g) * g
      integer,      dimension(SpG%nk,SpG%nk)  :: ep  !E
      character(len=1),  dimension(26)        :: codd
      character(len=15), dimension(26)        :: st_cod
      integer                                 :: i,j,order,iq,ir,d,n,k,iqt,ig,ip,iss,nq
      real(kind=cp)                           :: suma,alpha
      integer,           dimension(48)        :: ss_ptr
      real(kind=cp),     dimension(3,48)      :: atr
      real(kind=cp),     dimension(6*SpG%nq)  :: cod,multi,val
      real(kind=cp),     dimension(3)         :: x
      real(kind=dp),     dimension(6,6)       :: subm
      real(kind=dp),  dimension(6*SpG%nq,6*SpG%nq) :: Ctr,sCtr
      real(kind=cp),     parameter                 :: epss=0.001_cp
      real(kind=cp), dimension(6*SpG%nq)           ::TFourL,sTF


      !Test if all codes are given ... in such a case the user constraints
      !are prevalent

      d=SpG%d-1
      suma=0.0
      !iq=0
      n=0
      nq=size(TFourier,dim=2)
      do k=1,nq
        do j=1,6
           n=n+1
           suma=suma+abs(codes(j,k))
        end do
      end do

      if(suma < epss ) return  !No refinement is required

      x=xnr
      where(x < 0.0) x=x+1.0
      where(x > 1.0) x=x-1.0

      if(present(ord) .and. present(ss) .and. present(att)) then
        order=ord
        ss_ptr(1:order) = ss(1:ord)
        atr(:,1:order)  = att(:,1:ord)
      else
        call get_stabilizer(x,SpG,order,ss_ptr,atr)
        if(present(ipr)) Write(unit=ipr,fmt="(a,i3)") " => Superspace stabilizer without identity, order:",order
      end if

      do ip=1,nq
        i=(ip-1)*6
        TFourL(i+1:i+6)=TFourier(:,ip)
      end do

      sCtr=0.0_cp
      sTF=0.0
      if(order > 1) then
        n=6*nq

        do ig=1,order
          ir=ss_ptr(ig)
              g(:,:) = SpG%Op(ir)%Mat(1:3,1:3)   !                          /  g    0   t  \
               ts(:) = SpG%Op(ir)%Mat(1:d,d+1)   !   Superspace operator:  |  Mx   ep   tI  |    !ts=(t,tI)
             Mx(:,:) = SpG%Op(ir)%Mat(4:d,1:3)   !                          \   0    0   1 /
             Ep(:,:) = SpG%Op(ir)%Mat(4:d,4:d)
             magm(:,:) = g(:,:)
             if(mode(1:1) == "M")  magm(:,:) = magm(:,:)*SpG%Op(ir)%time_inv*SpG%Op(ir)%dt
           !Identify the q_coefficients of mM to select the proper Tfourier
           !Construction of the Cr-matrix
           Ctr=0.0
           do ip=1,nq
             brack_m(:)=SpG%q_coeff(:,ip) !nk-components vector
             mE=matmul(brack_m,Ep)   ![m].Ep  nk-components vector
             do iqt=1,nq
               iq=iqt
               if(equal_vector(mE,SpG%q_coeff(:,iqt))) then
                 iss=1
                 exit
               end if
               if(equal_vector(mE,-SpG%q_coeff(:,iqt))) then
                 iss=-1  ! iss=-1
                 exit
               end if
             end do !iq
             alpha=tpi*dot_product(brack_m,matmul(Mx,x)+ts(4:))
             subm(1:3,1:3)=cos(alpha)*magm
             subm(4:6,1:3)=sin(alpha)*magm
             subm(1:3,4:6)=-iss*sin(alpha)*magm
             subm(4:6,4:6)= iss*cos(alpha)*magm
             !Block  ip,iq
             i=(ip-1)*6; j=(iq-1)*6
             Ctr(i+1:i+6,j+1:j+6)=subm
             Tfourl(i+1:i+6)=matmul(subm,TFourier(:,iq))
             sTF(i+1:i+6)=sTF(i+1:i+6)+TfourL(i+1:i+6)
           end do !ip
           sCtr=sCtr+Ctr !Adding constraint matrices for each operator of stabilizer
           if(present(ipr)) then
             do ip=1,nq
                i=(ip-1)*6
                write(unit=ipr,fmt='(a,i2,a,t20,a,t55,a,6f14.4,4i3)') '     Operator ',ir,": ",trim(Spg%Symb_Op(ir))," Matrix SubM x TFourier & [m] : ",Tfourl(i+1:i+6),brack_m(:)
             end do
           end if
        end do  !ig operators
        sCtr=sCtr/order
        sTF=sTF/order
        if(present(ipr)) then
          do ip=1,nq
             i=(ip-1)*6
             write(unit=ipr,fmt='(a,6f14.4)')     '     Sum of TFour: ',sTf(i+1:i+6)
          end do
        end if
        call Get_Refinement_Codes(n,sTF,sCtr,iss,multi,codd,TFourL)  !Get_Refinement_Codes(n,vect_val,Ctr,iss,multi,codd,vect_out)
        cod=0.0
        do j=1,n
          if(codd(j) /= "0") then
            do i=1,iss
              if(codd(j) == cdd(i)) then
                cod(j)=codini+i
                exit
              end if
            end do
          end if
        end do
        do ip=1,nq
          i=(ip-1)*6
          TFourier(:,ip)=TFourL(i+1:i+6)
        end do

        codes=0.0; j=0
        do ip=1,nq
          do i=1,6
            j=j+1
            if(abs(multi(j)) > epss)  codes(i,ip) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
          end do
        end do
        codini=codini+iss
        if(present(Ipr)) then
          Write(Ipr,"(a,i4)")      " Number of free parameters: ",iss
          Write(Ipr,"(a,24F14.6)") " Basic Values: ",TFourL(1:iss)
          write(Ipr,"(a,24f14.6)") " Multipliers: ",(multi(j), j=1,n)
          write(Ipr,"(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(Ipr,"(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          do ip=1,nq
            write(Ipr,"(a,i2, 6f14.6)") " Constrained Fourier Coefficients: ",ip,TFourier(:,ip)
            write(Ipr,"(a,tr2,6f14.6)") " Final codes: ",codes(:,ip)
          end do
        end if

      else !No restrictions

        codd(1:n)=cdd(1:n)
        multi(1:n)=1.0_cp
        j=0
        do ip=1,nq
          do i=1,6
            j=j+1
            cod(j)=codini+j
            codes(i,ip) = abs(cod(j))*10.0_cp + abs(multi(j))
          end do
        end do
        codini=codini+n
        if(present(Ipr)) then
          write(Ipr,"(a,24f10.6)") " General position, no constraints in Fourier Coefficients "
          write(Ipr,"(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(Ipr,"(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          do ip=1,nq
            write(Ipr,"(a,i2, 6f14.6)") " Constrained Fourier Coefficients: ",ip,TFourier(:,ip)
            write(Ipr,"(a,tr2,6f14.6)") " Final codes: ",codes(:,ip)
          end do
        end if

      end if

      if(present(ctr_code)) then
        St_cod="0"
        n=0
        do ip=1,nq
          do j=1,6
            n=n+1
            St_cod(n)=" "
            if(abs(multi(n)) < 0.00001) then
              St_cod(n) = " 0 "
            else
              if(multi(n) == 1.0_cp) then
                 St_cod(n)=codd(n)
              else if(multi(n) == -1.0_cp) then
                 St_cod(n)="-"//codd(n)
              else
                 write(unit=St_cod(n),fmt="(f10.5,a)")  multi(n),"*"//codd(n)
                 St_cod(n)=adjustl(St_cod(n))
              end if
            end if
          end do
        end do
        n=0
        do ip=1,nq
          write(unit=ctr_code(ip),fmt="(8a)") " ( ",(St_cod(n+j)//", ",j=1,5),St_cod(n+6)//" )"
          ctr_code(ip)=pack_string(ctr_code(ip))
          n=n+6
        end do
      end if

   End Subroutine Get_TFourier_ctr

End SubModule SPG_Stabilizer_Constraints