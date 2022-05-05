!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Generate
   implicit none
   Contains
   !!----
   !!---- Gener_Reflections
   !!----    Calculate unique reflections between two values of
   !!----    sin_theta/lambda.  The output is not ordered.
   !!----
   !!---- 21/06/2019
   !!
   Module Subroutine Gener_Reflections(Cell,Sintlmax,Mag,Reflex,SpG,kinfo,order,powder,mag_only,Friedel)
      !---- Arguments ----!
      class(Cell_G_Type),                          intent(in)     :: Cell
      real(kind=cp),                               intent(in)     :: Sintlmax
      logical,                                     intent(in)     :: Mag
      type(RefList_Type),                          intent(in out) :: Reflex
      class(Spg_Type) ,              optional,     intent(in)     :: SpG
      type(kvect_info_type),         optional,     intent(in)     :: Kinfo
      character(len=*),              optional,     intent(in)     :: Order
      logical,                       optional,     intent(in)     :: Powder
      logical,                       optional,     intent(in)     :: Mag_only
      logical,                       optional,     intent(in)     :: Friedel

      !---- Local variables ----!
      real(kind=cp)         :: epsr=0.00001, delt=0.0001
      real(kind=cp)         :: sval,max_s !,vmin,vmax
      integer               :: h,k,l,hmin,kmin,lmin,hmax,kmax,lmax, maxref,i,j,indp,indj, &
                               maxpos, mp, iprev,Dd, nf, ia, i0, nk, nharm,n,num_ref
      integer,      dimension(:),   allocatable :: hh,kk,nulo
      integer,      dimension(:,:), allocatable :: hkl,hklm
      integer,      dimension(:),   allocatable :: indx,indtyp,ind,ini,fin,itreat

      real(kind=cp),dimension(:),   allocatable :: sv,sm
      logical                                   :: kvect,ordering,magg,Frd

      !> Init
      Dd=3
      ordering=.false.
      kvect=.false.
      magg=.false.
      Frd=.true.
      if (present(Friedel)) Frd=Friedel
      if (present(mag_only)) magg=mag_only
      if (present(order) .or. present(powder)) ordering=.true.
      if (present(kinfo)) then
         nk=kinfo%nk
         nharm=kinfo%nq
         kvect=.true.
      end if
      if (kvect) Dd=3+nk ! total dimension of the reciprocal space

      hmax=nint(Cell%cell(1)*2.0*sintlmax+1.0)
      kmax=nint(Cell%cell(2)*2.0*sintlmax+1.0)
      lmax=nint(Cell%cell(3)*2.0*sintlmax+1.0)
      hmin=-hmax; kmin=-kmax; lmin= -lmax
      maxref= (2*hmax+1)*(2*kmax+1)*(2*lmax+1)
      if (kvect) then
         do k=1,nk
            maxref=maxref*2*nharm
         end do
         if (present(kinfo)) then
            max_s=maxval(kinfo%sintlim)
         else
            max_s=sintlmax
         end if
      end if

      allocate(hkl(Dd,maxref), indx(maxref), indtyp(maxref), ind(maxref), sv(maxref))
      allocate(hh(Dd), kk(Dd), nulo(Dd))
      nulo=0
      indtyp=0
      num_ref=0

      !> Generation of fundamental reflections
      i0=0
      ext_do: do h=hmin,hmax
         do k=kmin,kmax
            do l=lmin,lmax
               hh=0
               hh(1:3)=[h,k,l]
               sval=H_S(hh,Cell)
               if (sval > sintlmax) cycle

               num_ref=num_ref+1
               if (num_ref > maxref) then
                  num_ref=maxref
                  exit ext_do
               end if

               if (sval < epsr) i0=num_ref !localization of 0 0 0  reflection
               sv(num_ref)=sval
               hkl(:,num_ref)=hh
            end do
         end do
      end do ext_do

      !> Generation of satellites
      !> The generated satellites corresponds to those obtained from the list
      !> of +/-kinfo%q_qcoeff
      nf=num_ref
      if (kvect) then
         do_ex: do n=1,kinfo%nq
            do i=1,nf
               hh=hkl(:,i)
               do ia=-1,1,2
                  hh(4:3+nk)=ia*kinfo%q_coeff(1:nk,n)
                  sval=H_S(hh, Cell, nk, kinfo%kv)
                  if (sval > max_s) cycle

                  num_ref=num_ref+1
                  if (num_ref > maxref) then
                     num_ref=maxref
                     exit do_ex
                  end if

                  sv(num_ref)=sval
                  hkl(:,num_ref)=hh
               end do !ia
            end do  !i
         end do do_ex
      end if

      !> elimination of the reflection (0000..)
      do i=i0+1,num_ref
         sv(i-1)=sv(i)
         hkl(:,i-1)=hkl(:,i)
      end do
      num_ref=num_ref-1

      !> Determination of reflection character and extinctions (Lattice + others)
      n=0
      do i=1,num_ref
         hh=hkl(:,i)
         mp=0
         if (present(SpG)) then
            if (SpG%Num_Lat /= 0) then
               if (H_Latt_Absent(hh,SpG%Lat_tr,SpG%Num_Lat)) cycle
            end if
            if (H_Absent(hh,SpG)) then
               if (SpG%Mag_type /= 2 .and. Mag) then
                  if (mH_Absent(hh,SpG)) then
                     cycle
                  else
                     mp=1   !pure magnetic
                  end if
               else
                  cycle
               end if
            else
               if (SpG%Mag_type /= 2 .and. Mag) then
                  if (mH_Absent(hh,SpG)) then
                     mp=0  !pure nuclear
                  else
                     mp=2
                  end if
               end if
            end if
         end if

         n=n+1
         hkl(:,n)=hkl(:,i)
         sv(n)=sv(i)
         indtyp(n)=mp
      end do
      num_ref=n

      if (ordering) then
         indx=sort(sv,num_ref)
         allocate(hklm(Dd,num_ref),sm(num_ref))
         do i=1,num_ref
            j=indx(i)
            hklm(:,i)=hkl(:,j)
            sm(i)=sv(j)
            ind(i)=indtyp(j)
         end do

         !> contains now the type of reflection in the proper order
         indtyp(1:num_ref)=ind(1:num_ref)
         hkl(:,1:num_ref)=hklm(:,1:num_ref)
         sv(1:num_ref)=sm(1:num_ref)
         if (present(SpG) .and. present(powder)) then
            deallocate(hkl,sv,indx,ind)
            allocate(ini(num_ref),fin(num_ref),itreat(num_ref))
            itreat=0; ini=0; fin=0
            indp=0
            do i=1,num_ref       !Loop over all reflections
               if (itreat(i) == 0) then   !If not yet treated do the following
                  hh(:)=hklm(:,i)
                  indp=indp+1  !update the number of independent reflections
                  itreat(i)=i  !Make this reflection treated
                  ini(indp)=i  !put pointers for initial and final equivalent reflections
                  fin(indp)=i
                  do j=i+1,num_ref  !look for equivalent reflections to the current (i) in the list
                     if (abs(sm(i)-sm(j)) > delt) exit
                     kk=hklm(:,j)
                     if (h_equiv(hh,kk,SpG,Frd)) then ! if  hh eqv kk (Friedel law according to Frd)
                        itreat(j) = i                 ! add kk to the list equivalent to i
                        fin(indp) = j
                     end if
                  end do
               end if !itreat
            end do

            !> Selection of the most convenient independent reflections
            allocate(hkl(Dd,indp),sv(indp),ind(indp))
            do i=1,indp
               maxpos=0
               indj=ini(i)
               iprev=itreat(indj)
               do j=ini(i),fin(i)
                  if (iprev /= itreat(j)) cycle
                  hh=hklm(:,j)
                  mp=count(hh > 0)
                  if (mp > maxpos) then
                     indj=j
                     maxpos=mp
                  end if
               end do !j
               hkl(:,i)=hklm(:,indj)
               if (hkl(1,i) < 0) hkl(:,i)=-hkl(:,i)
               sv(i)=sm(indj)
               ind(i)=indtyp(indj)
            end do
            indtyp(1:indp)=ind(1:indp)
            num_ref=indp
         end if  !SpG and Powder

      end if !present "order"

      !> Final assignments
      if (kvect .or. magg) then
         call Initialize_RefList(Num_ref, reflex, 'MRefl', Dd)
      else
         call Initialize_RefList(Num_ref, reflex, 'SRefl', Dd)
      end if

      do i=1,num_ref
         reflex%ref(i)%h = hkl(:,i)
         if(dd > 3) then
           kk               = abs(hkl(4:3+nk,i))
           reflex%ref(i)%pcoeff = 0 !Fundamental reflections point to the Fourier coefficient [00...]
           do_n: do n=1,kinfo%nq
              do k=1,nk
                 if (equal_vector(kk(1:nk),abs(kinfo%q_coeff(1:nk,n))))  then
                    reflex%ref(i)%pcoeff=n
                    exit do_n
                 end if
              end do
           end do do_n
         end if
         reflex%ref(i)%s      = sv(i)
         if (present(SpG)) then
            reflex%ref(i)%mult = h_mult(reflex%ref(i)%h,SpG,Frd)
         else
           reflex%ref(i)%mult = 1
         end if
         reflex%ref(i)%imag = indtyp(i)
      end do
   End Subroutine Gener_Reflections

   !!----
   !!---- SUBROUTINE GENER_REFLECTIONS_SHUB
   !!----
   !!----    Calculate unique reflections below the maximum
   !!----    sin_theta/lambda provided.  The output is ordered.
   !!----
   !!---- Updated: April 2022
   !!
   Module Subroutine Gener_Reflections_Shub(Cell,SpG, Smax, Reflex,Friedel)
      !---- Arguments ----!
      type (Cell_G_Type),    intent(in)     :: Cell
      type (SpG_Type) ,      intent(in)     :: SpG
      real(kind=cp),         intent(in)     :: Smax        ! maximum SinTheta/Lambda
      type (RefList_Type),   intent(in out) :: Reflex
      logical, optional,     intent(in)     :: Friedel

      !---- Local variables ----!
      real(kind=cp)         :: sval
      integer               :: h,k,l,hmin,kmin,lmin,hmax,kmax,lmax, maxref,i,j,indp,indj, &
                               maxpos, mp, iprev,num_ref
      integer,       dimension(3) :: hh,kk,nulo
      integer,       dimension(:,:), allocatable :: hkl,hklm
      integer,       dimension(:),   allocatable :: indx,ini,fin,itreat
      real(kind=cp), dimension(:),   allocatable :: sv,sm
      logical :: Frd
      Frd=.true.
      if(present(Friedel)) Frd=Friedel
      !> Init
      nulo=0
      hmax=nint(Cell%cell(1)*2.0*smax+1.0)
      kmax=nint(Cell%cell(2)*2.0*smax+1.0)
      lmax=nint(Cell%cell(3)*2.0*smax+1.0)
      hmin=-hmax; kmin=-kmax; lmin = 0  !-lmax
      maxref= (2*hmax+1)*(2*kmax+1)*(lmax+1) !(2*lmax+1)
      allocate(hkl(3,maxref),indx(maxref),sv(maxref))


      num_ref=0
      ext_do: do h=hmin,hmax
         do k=kmin,kmax
            do l=lmin,lmax

               hh=[h,k,l]
               if (h_equal(hh,nulo)) cycle
               sval=h_s(hh,cell)
               if (sval > smax) cycle

               if (H_Latt_Absent(hh,Spg%Lat_tr,Spg%Num_Lat)) cycle

               num_ref=num_ref+1
               if (num_ref > maxref) then
                  num_ref=maxref
                  exit ext_do
               end if
               sv(num_ref)=sval
               hkl(:,num_ref)=hh
            end do
         end do
      end do ext_do

      indx = sort(sv,num_ref)

      allocate(hklm(3,num_ref),sm(num_ref),ini(num_ref),fin(num_ref),itreat(num_ref))
      do i=1,num_ref
         j=indx(i)
         hklm(:,i)=hkl(:,j)
         sm(i)=sv(j)
      end do

      deallocate(hkl,sv,indx)

      itreat=0; ini=0; fin=0
      indp=0
      do i=1,num_ref ! Loop over all reflections
         if (itreat(i) == 0) then   !If not yet treated do the following
            hh(:)=hklm(:,i)
            indp=indp+1  !update the number of independent reflections
            itreat(i)=i  !Make this reflection treated
            ini(indp)=i  !put pointers for initial and final equivalent reflections
            fin(indp)=i

            do j=i+1,num_ref  !look for equivalent reflections to the current (i) in the list
               if (abs(sm(i)-sm(j)) > 0.000001) exit
               kk=hklm(:,j)
               if (h_equiv(hh,kk,SpG,Frd)) then ! if  hh eqv kk
                  itreat(j) = i                 ! add kk to the list equivalent to i
                  fin(indp)=j
               end if
            end do
         end if !itreat
      end do

      !> Selection of the most convenient independent reflections
      allocate(hkl(3,indp),sv(indp),indx(indp))

      indx=2 !nuclear and magnetic contribution by default
      do i=1,indp
         maxpos=0
         indj=ini(i)
         iprev=itreat(indj)
         do j=ini(i),fin(i)
            if (iprev /= itreat(j)) cycle
            hh=hklm(:,j)
            mp=count(hh > 0)
            if (mp > maxpos) then
               indj=j
               maxpos=mp
            end if
         end do !j
         hkl(:,i)=hklm(:,indj)
         if (hkl(1,i) < 0) hkl(:,i)=-hkl(:,i)
         sv(i)=sm(indj)
      end do

      !> Now apply systematic absences other than lattice type
      num_ref=0
      do i=1,indp
         hh=hkl(:,i)
         if (H_Absent(hh,SpG)) then
            if (mH_Absent(hh,SpG)) then
               cycle
            else
               indx(i)=1   !pure magnetic
            end if
         else
            if (mH_Absent(hh,SpG)) indx(i)=0  !pure nuclear
         end if
         num_ref=num_ref+1
         hklm(:,num_ref)=hh
         sm(num_ref) = sv(i)
      end do

      !> Final assignments
      select type (r => Reflex%ref)
         type is (Srefl_type)
            call Initialize_RefList(Num_ref, reflex, 'SRefl', SpG%d-1)
         type is (MRefl_type)
            call Initialize_RefList(Num_ref, reflex, 'MRefl', SpG%d-1)
         class default
            call Initialize_RefList(Num_ref, reflex, 'Refl', SpG%d-1)
      end select
      do i=1,num_ref
         hh=hkl(:,i)
         reflex%ref(i)%h    = hh
         reflex%ref(i)%s    = sm(i)
         reflex%ref(i)%mult = h_mult(hh,SpG,.false.)
         reflex%ref(i)%imag = indx(i)
      end do
   End Subroutine Gener_Reflections_Shub

End SubModule Refl_Generate