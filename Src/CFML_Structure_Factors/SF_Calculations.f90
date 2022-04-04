Submodule (CFML_Structure_Factors) SF_Calculations
   !---- Variables ----!
   implicit none
   
 Contains
 
   !!----
   !!---- SUBROUTINE MODIFY_SF
   !!----
   !!----    Recalculation of Structure Factors because a list of Atoms
   !!----    parameters were modified. List variable
   !!----    contains the number of atoms to be changed.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Modify_SF(Reflex, Atm, Grp, List, Nlist, Partyp, Mode)
      !---- Arguments ----!
      type(RefList_Type),         intent(in out) :: Reflex
      type(AtList_type),          intent(in)     :: Atm
      type(SpG_type),             intent(in)     :: Grp
      integer,dimension(:),       intent(in)     :: List
      integer,                    intent(in)     :: NList
      character(len=*), optional, intent(in)     :: partyp
      character(len=*), optional, intent(in)     :: Mode

      !---- Local variables ----!
      character(len=2) :: typ
      integer          :: i,j,k,ii
      real(kind=cp)    :: arg,b,s

      typ="CO"
      if (present(partyp)) typ=adjustl(partyp)
      typ=U_Case(typ)

      select case (typ)
         case ("CO") ! by coordinates
            if (Grp%Centred == 2) then
               do j=1,Reflex%Nref
                  do ii=1,Nlist
                     i=list(ii)
                     Ajh(i,j)=0.0_cp
                     arg=0.0_cp
                     do k=1,grp%NumOps
                        arg=tpi*(dot_product(hr(k,j)%h,Atm%atom(i)%x)+ht(k,j))
                        Ajh(i,j)=Ajh(i,j)+cos(arg)
                     end do ! Symmetry
                  end do ! NList
               end do ! Reflections

            else
               do j=1,Reflex%Nref
                  do ii=1,Nlist
                     i=list(ii)
                     arg=0.0_cp
                     Ajh(i,j)=0.0_cp
                     Bjh(i,j)=0.0_cp
                     do k=1,grp%NumOps
                        arg=tpi*(dot_product(hr(k,j)%h,Atm%atom(i)%x)+ht(k,j))
                        Ajh(i,j)=Ajh(i,j)+cos(arg)
                        Bjh(i,j)=Bjh(i,j)+sin(arg)
                     end do ! Symmetry
                  end do ! NList
               end do ! Reflections
            end if

         case ("TH") ! by thermal parameter or occupation number
            do j=1,Reflex%Nref
               s=reflex%ref(j)%s
               do ii=1,Nlist
                  i=list(ii)
                  b=atm%atom(i)%u_iso
                  th(i,j)=atm%atom(i)%occ*exp(-b*s*s)
               end do ! NList
            end do ! Reflections

      end select

      !> Recalculation of SF
      if (present(mode)) then
         if (mode == "XRA" .or. mode == "ELE") then
            call Sum_AB(Reflex,Atm%Natoms,Grp%Centred)
        
         else if (mode == "NUC") then
            call Sum_AB_NeutNuc(Reflex,Atm%Natoms,Grp%Centred)
         end if
      else
         call Sum_AB(Reflex,Atm%Natoms,Grp%Centred)
      end if

   End Subroutine Modify_SF
   
   !!--++
   !!--++ SUBROUTINE SUM_AB
   !!--++    Calculate the Final Sum for Structure Factors calculations
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Sum_AB(Reflex, Natm, Icent)
      !---- Arguments ----!
      type(RefList_Type), intent(in out)  :: Reflex
      integer,            intent(in)      :: Natm
      integer,            intent(in)      :: icent

      !---- Local Variables ----!
      integer                                     :: i,j
      real(kind=cp)                               :: a,b, ph
      real(kind=cp), dimension(natm,reflex%nref)  :: aa,bb,cc,dd


      ! A(h)=SIG(i){(f0+Deltaf')*OCC*Tiso*Ag}    asfa=a-d
      ! C(h)=SIG(i){    Deltaf" *OCC*Tiso*Ag}    bsfa=b+c

      ! B(h)=SIG(i){(f0+Deltaf')*OCC*Tiso*Bg}
      ! D(h)=SIG(i){    Deltaf" *OCC*Tiso*Bg}

      !> Fj(h)*Aj(h)
      aa=af0*th*ajh

      if (icent == 2) then    
         !> Calculation for centrosymmetric structures
         do j=1,reflex%nref
            cc(:,j)= afpp(:)*th(:,j)*ajh(:,j)
         end do

         !> Final Sum
         select type (ref => reflex%ref)
            class is (SRefl_Type)
               do i=1,reflex%Nref
                  a=sum(aa(:,i))
                  b=sum(cc(:,i))
                  ref(i)%Fc=sqrt(a*a+b*b)
                  ph = atan2d(b,a)
                  if (ph < 0.0) ph=ph+360.0
                  ref(i)%Phase = ph
                  ref(i)%A=a
                  ref(i)%B=b
               end do
               
            class default
               err_CFML%Ierr=1
               err_CFML%Msg="You have to use SRef_Type class as minimum for this calculation!"
               return   
         end select      

      else       
         !> Calculation for non-centrosymmetric structures
         !> Fj(h)*Bj(h)
         bb=af0*th*bjh

         do j=1,reflex%nref
            cc(:,j)= afpp(:)*th(:,j)*ajh(:,j)
            dd(:,j)= afpp(:)*th(:,j)*bjh(:,j)
         end do

         !> Final Sum
         select type (ref => reflex%ref)
            class is (Srefl_Type)
               do i=1,reflex%Nref
                  a=sum(aa(:,i)-dd(:,i))
                  b=sum(bb(:,i)+cc(:,i))
                  ref(i)%Fc=sqrt(a*a+b*b)
                  ph = atan2d(b,a)
                  if (ph < 0.0) ph=ph+360.0
                  ref(i)%Phase = ph
                  ref(i)%A=a
                  ref(i)%B=b
               end do
               
            class default
               err_CFML%Ierr=1
               err_CFML%Msg="You have to use SRef_Type class as minimum for this calculation!"
               return   
         end select 
      end if

   End Subroutine Sum_AB
   
   !!--++
   !!--++ SUBROUTINE SUM_AB_NEUTNUC
   !!--++    Calculate the Final Sum for Structure Factors calculations
   !!--++    Adapted for Neutron Nuclear Scattering (real scattering lengths)
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Sum_AB_NeutNuc(Reflex, Natm, Icent)
      !---- Arguments ----!
      type(RefList_Type),   intent(in out) :: Reflex
      integer,              intent(in)     :: Natm
      integer,              intent(in)     :: icent

      !---- Local Variables ----!
      integer                                     :: i,j
      real(kind=cp)                               :: a,b, ph
      real(kind=cp), dimension(natm,reflex%nref)  :: aa,bb

      if (icent == 2) then    
         !> Calculation for centrosymmetric structures

         !> Fj(h)*Aj(h)
         do j=1,reflex%nref
            aa(:,j)= afp(:)*th(:,j)*ajh(:,j)
         end do

         !> Final Sum 
         select type (Ref => reflex%ref)
            class is (Srefl_Type)
               do i=1,reflex%Nref
                  a=sum(aa(:,i))
                  ref(i)%Fc=abs(a)
                  ref(i)%Phase = 90.0_cp - 90.0_cp * sign(1.0_cp,a)
                  ref(i)%A=a
                  ref(i)%B=0.0
               end do     
               
            class default
               err_CFML%Ierr=1
               err_CFML%Msg="You have to use SRef_Type class as minimum for this calculation!"
               return   
         end select     

      else       
         !> Calculation for non-centrosymmetric structures
         !> Fj(h)*Aj(h) / Fj(h)*Bj(h)
         do j=1,reflex%nref
            aa(:,j)= afp(:)*th(:,j)*ajh(:,j)
            bb(:,j)= afp(:)*th(:,j)*bjh(:,j)
         end do

         !> Final Sum
         select type (ref => reflex%ref)
            class is (Srefl_Type)
               do i=1,reflex%Nref
                  a=sum(aa(:,i))
                  b=sum(bb(:,i))
                  ref(i)%Fc=sqrt(a*a+b*b)
                  ph = atan2d(b,a)
                  if (ph < 0.0) ph=ph+360.0
                  ref(i)%Phase = ph
                  ref(i)%A=a
                  ref(i)%B=b
               end do
               
            class default
               err_CFML%Ierr=1
               err_CFML%Msg="You have to use SRef_Type class as minimum for this calculation!"
               return   
         end select    
      end if

   End Subroutine Sum_AB_NeutNuc
   
   !!----
   !!---- Subroutine Structure_Factors(Atm,Grp,Reflex,Mode,lambda)
   !!----    type(atom_list_type),               intent(in)     :: Atm    !List of atoms
   !!----    type(space_group_type),             intent(in)     :: Grp    !Space group
   !!----    type(reflection_list_type),         intent(in out) :: Reflex !It is completed on output
   !!----    character(len=*), optional,         intent(in)     :: Mode   !"NUC","ELE" for neutrons, electrons else: XRays
   !!----    real(kind=cp), optional,            intent(in)     :: lambda !Needed for Xrays
   !!----
   !!----    Calculate the Structure Factors from a list of Atoms
   !!----    and a set of reflections. A call to Init_Structure_Factors
   !!----    is a pre-requisite for using this subroutine. In any case
   !!----    the subroutine calls Init_Structure_Factors if SF_initialized=.false.
   !!----
   !!---- Update: February - 2005
   !!
   Module Subroutine Structure_Factors(Reflex, Atm, Grp, Mode, Lambda)
      !---- Arguments ----!
      type(RefList_Type),           intent(in out) :: Reflex
      type(AtList_type),            intent(in)     :: Atm
      type(SpG_type),               intent(in)     :: Grp
      character(len=*),   optional, intent(in)     :: Mode
      real(kind=cp),      optional, intent(in)     :: lambda

      if (present(Mode)) then
         if (present(lambda)) then
            if (.not. SF_Initialized) call Init_Structure_Factors(Reflex,Atm,Grp,Mode,lambda)
         else
            if (.not. SF_Initialized) call Init_Structure_Factors(Reflex,Atm,Grp,Mode)
         end if
      else
         if (present(lambda)) then
            if (.not. SF_Initialized) call Init_Structure_Factors(Reflex,Atm,Grp,Lambda=lambda)
         else          
            if (.not. SF_Initialized) call Init_Structure_Factors(Reflex,Atm,Grp)           
         end if
      end if
      if (err_CFML%Ierr /= 0) return

      !> Table TH
      call Calc_Table_TH(Reflex, Atm)

      !> Table AB
      call Calc_Table_AB(Reflex%Nref, Atm, Grp)

      !> Final Calculations
      if (present(mode)) then
         select case (u_case(mode(1:3)))
            case ('XRA','ELE')
               call Sum_AB(Reflex,Atm%Natoms,Grp%Centred)
            case ('NUC')   
               call Sum_AB_NeutNuc(Reflex,Atm%Natoms,Grp%Centred)
         end select      
      
      else
         call Sum_AB(Reflex,Atm%Natoms,Grp%Centred)
      end if
   End Subroutine Structure_Factors
   
   !!----
   !!---- SUBROUTINE CALC_GENERAL_STRFACTOR
   !!----
   !!----   Calculates nuclear, x-rays and electrostatic structure factors from
   !!----   the list of atoms Atm, space group Grp and scattering species Scf.
   !!----
   !!----  Update: April - 2022
   !!----
   Module Subroutine Calc_General_StrFactor(Hn, Sn, Atm, Grp, Scf, fn, fx, fe)
      !---- Arguments ----!
      real(kind=cp),dimension(3),    intent(in) :: Hn
      real(kind=cp),                 intent(in) :: Sn !(sinTheta/Lambda)**2
      type(AtList_type),             intent(in) :: Atm
      type(SpG_type),                intent(in) :: Grp
      type(Scattering_Species_Type), intent(in) :: Scf
      complex, optional,             intent(out):: fn,fx,fe

      !---- Local Variables ----!
      integer                               :: i,j,k 
      real(kind=cp)                         :: arg,anis,scosr,ssinr,b 
      real(kind=cp)                         :: a1,a3,b1,b3,av,bv,nffr,nffi        !fn
      real(kind=cp)                         :: xa1,xa3,xb1,xb3,xav,xbv,xffr,xffi  !fx
      real(kind=cp)                         :: ea1,ea3,eb1,eb3,effr               !fe
      real(kind=cp),dimension(3)            :: h,t
      real(kind=cp),dimension(6)            :: beta
      real(kind=cp),dimension(3,3)          :: Mat

      !> Init
      a1 =0.0_cp;  a3 =0.0_cp
      b1 =0.0_cp;  b3 =0.0_cp
      ea1=0.0_cp;  ea3=0.0_cp
      eb1=0.0_cp;  eb3=0.0_cp
      xa1=0.0_cp;  xa3=0.0_cp
      xb1=0.0_cp;  xb3=0.0_cp

      do i=1,Atm%natoms
         arg=0.0_cp
         scosr=0.0_cp
         ssinr=0.0_cp
         do k=1,grp%NumOps
            Mat=grp%op(k)%Mat(1:3,1:3)
            t=grp%op(k)%Mat(1:3,4)
            h=matmul(hn,Mat)
            arg=tpi*(dot_product(h,Atm%atom(i)%x)+ dot_product(hn,t))
            anis=1.0_cp
            if (Atm%atom(i)%thtype == "ani") then
               beta=Atm%atom(i)%u(1:6)
               anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                    +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
               anis=exp(-anis)
            end if
            scosr=scosr+COS(arg)*anis  ! Real part of geometrical structure factor for the current atom
            ssinr=ssinr+SIN(arg)*anis  ! Imaginary part of geometrical structure factor for the current atom
         end do ! symmetry

         b= atm%atom(i)%occ * exp(-atm%atom(i)%u_iso*sn)
         
         !> Calculation of scattering factors
         j=atm%atom(i)%ind_ff(1)  ! pointer to the form factor coefficients

         nffr = Scf%br(j)*b
         nffi = Scf%bi(j)*b

         xffr=Scf%Xcoef(j)%c
         do k=1,4
            xffr=xffr+Scf%Xcoef(j)%a(k)*exp(-Scf%Xcoef(j)%b(k)*sn)
         end do

         effr = (real(Scf%Xcoef(j)%Z)-xffr)*b  !<- Here delta_fp is not used ....
         xffr = (xffr+Scf%delta_fp(j))*b       ! (f0+Deltaf')*OCC*Tiso
         xffi = Scf%delta_fpp(j)*b             !     Deltaf" *OCC*Tiso

         a1 = a1 + nffr*scosr  ! F=A+iB: components of A  and B (ai,bi)
         b1 = b1 + nffi*scosr  ! a2,b2,a4,b4 are components for anisotropic form factors
         a3 = a3 + nffi*ssinr  ! they are not used here
         b3 = b3 + nffr*ssinr  ! For general case: av = a1-a2-a3-a4, bv = b1-b2+b3+b4

         xa1 = xa1 + xffr*scosr  ! F=A+iB: components of A  and B (ai,bi)
         xb1 = xb1 + xffi*scosr  ! a2,b2,a4,b4 are components for anisotropic form factors
         xa3 = xa3 + xffi*ssinr  ! they are not used here
         xb3 = xb3 + xffr*ssinr  ! For general case: av = a1-a2-a3-a4, bv = b1-b2+b3+b4

         ea1 = ea1 + effr*scosr  ! No anomalous imaginary component is used here
         eb3 = eb3 + effr*ssinr  ! there is no anomalous scattering neutron + electrons

      end do ! Atoms

      if (present(fn)) then
         av = a1-a3    ! real part of the Nuclear structure factor
         bv = b1+b3    ! imaginary part of the Nuclear structure factor
         fn=cmplx(av,bv) * Grp%Centred * Grp%Num_Lat
      end if   

      if (present(fx)) then
         xav = xa1-xa3    ! real part of the X-rays structure factor
         xbv = xb1+xb3    ! imaginary part of the X-rays structure factor
         fx=cmplx(xav,xbv) * Grp%Centred * Grp%Num_Lat
      end if
      
      if (present(fe)) then
         fe=cmplx(ea1,eb3) * Grp%Centred * Grp%Num_Lat
      end if
       
   End Subroutine Calc_General_StrFactor
   
   !!----
   !!---- SUBROUTINE CALC_HKL_STRFACTOR
   !!----
   !!----    Calculate Structure Factor for reflection "h=(hkl)" not related with
   !!----    previous lists and derivatives with respect to refined parameters.
   !!----    This subroutine calculates the form-factors internally without using
   !!----    global tables. The purpose of this procedure is to avoid the use of
   !!----    too much memory in tables.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Calc_hkl_StrFactor(Hn, Sn, Atm, Grp, Mode, Rad, Sf2, Deriv, fc)
      !---- Arguments ----!
      integer,dimension(3),                  intent(in) :: Hn
      real(kind=cp),                         intent(in) :: Sn    ! (sinTheta/Lambda)**2
      type(AtList_type),                     intent(in) :: Atm
      type(SpG_type),                        intent(in) :: Grp
      character(len=*),                      intent(in) :: Mode  ! S-XTAL (S) or Powder (P) 
      character(len=*),                      intent(in) :: Rad   ! Radiation: X-rays, Neutrons
      real(kind=cp),                         intent(out):: sf2
      real(kind=cp), dimension(:), optional, intent(out):: deriv
      complex,                     optional, intent(out):: fc

      !---- Local Variables ----!
      character(len=1)                      :: modi,crad
      integer                               :: i,j,k,m
      real(kind=cp)                         :: arg,anis,cosr,sinr,scosr,ssinr,fr,fi,der, hnt
      real(kind=cp)                         :: a1,a2,a3,a4,b1,b2,b3,b4,av,bv,f,occ,b, Tob
      real(kind=cp),dimension(3)            :: h,t
      real(kind=cp),dimension(6)            :: beta
      real(kind=cp),dimension(Atm%natoms)   :: frc,frs,otr,oti,afpxn,ff
      real(kind=cp),dimension(9,Atm%natoms) :: drs,drc
      real(kind=cp), dimension(3,3)         :: mat 


      !> Init
      modi=u_case(mode(1:1))
      crad=u_case(rad(1:1))
      a1=0.0_cp; a2=0.0_cp; a3=0.0_cp; a4=0.0_cp
      b1=0.0_cp; b2=0.0_cp; b3=0.0_cp; b4=0.0_cp
      av=0.0_cp; bv=0.0_cp
      frc=0.0_cp; frs=0.0_cp
      otr=0.0_cp; oti=0.0_cp
      
      !> Setting up the scattering form factors and multiply by group specific
      !> coefficients for calculating structure factors per conventional cell
      !> Modify the scattering factors to include the multipliers factors
      !> concerning centre of symmetry and centred translations
      fr=1.0_cp; fi=1.0_cp
      if (Grp%Centred == 2) fr=2.0_cp
      if (Grp%Num_Lat  > 1)  fi=Grp%Num_Lat
      select Case (crad)
         case("N")
             afpxn(:)=fr*fi*afp(:)
             
         case("X","E")
             do i=1, Nspecies
                ff(i)=FF_c(i)
                do j=1,4
                   ff(i)=ff(i)+FF_a(j,i)*exp(-sn*FF_b(j,i))
                end do
                if (crad == "E") ff(i)=0.023934*(FF_Z(i)-ff(i))/sn !Mott-Bethe formula fe=me^2/(8pi Eps0 h^2) (Z-fx(s))/s^2
             end do
             
             do i=1,Atm%natoms
                j=P_a(i)                ! pointer has been set up in Initialization subroutine
                afpxn(i)= fr*fi*ff(j)
             end do
      end select

      fr=1.0_cp
      fi=0.0_cp
      if (Grp%Centred == 2) then
         do i=1,Atm%natoms
            arg=0.0_cp
            scosr=0.0_cp
            ssinr=0.0_cp
            drs(:,i)=0.0_cp
            drc(:,i)=0.0_cp
            do k=1,grp%NumOps
               Mat=grp%op(k)%Mat(1:3,1:3)
               t=grp%op(k)%Mat(1:3,4)
               h=matmul(real(hn),Mat)
               hnt=dot_product(real(hn),t)  
               arg=TPI*(dot_product(h,Atm%atom(i)%x)+hnt)
                 
               anis=1.0_cp
               if (Atm%atom(i)%thtype == "ani") then
                  beta=Atm%atom(i)%u(1:6)
                  anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                        +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                  anis=exp(-anis)
               end if
               cosr=COS(arg)*anis*fr  ! fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               scosr=scosr+cosr       ! FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)

               if (present(deriv)) then
                  sinr=SIN(arg)*anis*fr   !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
                  drc(1:3,i)=drc(1:3,i)+h(1:3)*sinr      ! -
                  drc(4,i)=drc(4,i)+h(1)*h(1)*cosr
                  drc(5,i)=drc(5,i)+h(2)*h(2)*cosr
                  drc(6,i)=drc(6,i)+h(3)*h(3)*cosr
                  drc(7,i)=drc(7,i)+h(1)*h(2)*cosr
                  drc(8,i)=drc(8,i)+h(1)*h(3)*cosr
                  drc(9,i)=drc(9,i)+h(2)*h(3)*cosr
               end if
            end do ! symmetry
            
            occ= atm%atom(i)%occ
            b=atm%atom(i)%u_iso
            Tob= occ * exp(-b*sn)
            frc(i) = scosr
            otr(i) = afpxn(i)* Tob
            oti(i) =  afpp(i)* Tob
            a1= a1 + otr(i)*frc(i)
            b1= b1 + oti(i)*frc(i)
         end do ! Atoms

         av = a1-a2-a3-a4    !real part of the structure factor
         bv = b1-b2+b3+b4    !imaginary part of the structure factor

      else
         do i=1,Atm%natoms
            arg=0.0_cp
            scosr=0.0_cp
            ssinr=0.0_cp
            drs(:,i)=0.0_cp
            drc(:,i)=0.0_cp
            do k=1,grp%NumOps
               Mat=grp%op(k)%Mat(1:3,1:3)
               t=grp%op(k)%Mat(1:3,4)
               h=matmul(real(hn),Mat) 
               hnt=dot_product(real(hn),t)
               arg=TPI*(dot_product(h,Atm%atom(i)%x)+hnt)
               
               anis=1.0_cp
               if (Atm%atom(i)%thtype == "ani") then
                   beta=Atm%atom(i)%u(1:6)
                   anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                        +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                   anis=exp(-anis)
               end if
               cosr=COS(arg)*anis*fr     !fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               sinr=SIN(arg)*anis*fr     !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               scosr=scosr+cosr          !FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)
               ssinr=ssinr+sinr          !FRS= SIG fr(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)

               if (present(deriv)) then
                  drc(1:3,i)=drc(1:3,i)+h(1:3)*sinr      ! -
                  drs(1:3,i)=drs(1:3,i)+h(1:3)*cosr      ! +

                  drc(4,i)=drc(4,i)+h(1)*h(1)*cosr
                  drc(5,i)=drc(5,i)+h(2)*h(2)*cosr
                  drc(6,i)=drc(6,i)+h(3)*h(3)*cosr
                  drc(7,i)=drc(7,i)+h(1)*h(2)*cosr
                  drc(8,i)=drc(8,i)+h(1)*h(3)*cosr
                  drc(9,i)=drc(9,i)+h(2)*h(3)*cosr

                  drs(4,i)=drs(4,i)+h(1)*h(1)*sinr
                  drs(5,i)=drs(5,i)+h(2)*h(2)*sinr
                  drs(6,i)=drs(6,i)+h(3)*h(3)*sinr
                  drs(7,i)=drs(7,i)+h(1)*h(2)*sinr
                  drs(8,i)=drs(8,i)+h(1)*h(3)*sinr
                  drs(9,i)=drs(9,i)+h(2)*h(3)*sinr
               end if
            end do ! symmetry

            occ= atm%atom(i)%occ
            b=atm%atom(i)%u_iso
            Tob= occ * exp(-b*sn)
            frc(i) = scosr
            frs(i) = ssinr
            otr(i) = afpxn(i)* Tob
            oti(i) =  afpp(i)* Tob
            a1= a1 + otr(i)*frc(i)
            b1= b1 + oti(i)*frc(i)
            a3 = a3 + oti(i)*frs(i)
            b3 = b3 + otr(i)*frs(i)

         end do ! Atoms

         av = a1-a2-a3-a4    !real part of the structure factor
         bv = b1-b2+b3+b4    !imaginary part of the structure factor

      end if

      if (modi == "P") then
         sf2 = a1*a1 + a2*a2 + a3*a3 + a4*a4 + b1*b1 + b2*b2 + b3*b3 + b4*b4
         sf2 = sf2 + 2.0_cp*(b1*b4 -  a1*a4 + a2*a3 - b2*b3)
      else
         sf2= av*av+bv*bv
      end if

      if (present(fc)) fc=cmplx(av,bv)

      if (present(deriv)) then
         select type (At => Atm%atom)
            type is (Atm_Ref_Type) 
               if (modi == "P") then
                  do i=1,Atm%natoms
                     !> derivatives with respect to coordinates  POWDER
                     do m=1,3
                        k= At(i)%l_x(m)
                        if (k /= 0) then
                           f=at(i)%m_x(m)
                           der= otr(i)*(-a1*drc(m,i)+b3*drs(m,i))+oti(i)*(-b1*drc(m,i)+a3*drs(m,i))
                           der=2.0_cp*der*TPI
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do

                     k=At(i)%l_u_iso  ! Derivatives w.r.t. Biso  POWDER
                     if (k /= 0) then
                        f=At(i)%m_u_iso
                        der= otr(i)*(a1*frc(i) +b3*frs(i))+oti(i)*(b1*frc(i) +a3*frs(i))
                        der=-2.0_cp*der*sn
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if

                     k=At(i)%l_occ    ! Derivatives w.r.t. occupation factor   POWDER
                     if (k /= 0) then
                        f=At(i)%m_occ
                        der= otr(i)*(a1*frc(i)+b3*frs(i))+oti(i)*(b1*frc(i)+a3*frs(i))
                        der=2.0_cp*der/atm%atom(i)%occ
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if

                     do m=4,9      ! Derivatives w.r.t. anisotropic temperature factors   POWDER
                        j=m-3
                        k=At(i)%l_u(j)
                        if (k /= 0) then
                           f=At(i)%m_u(j)
                           der=  otr(i)*(a1*drc(i,j)+b3*drs(m,i))+oti(i)*(b1*drc(m,i)+a3*drs(m,i))
                           der=-2.0_cp*der
                           if (j > 3) der=2.0*der
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do
                  end do ! Natoms

               else
                  do i=1,Atm%natoms
                     !> derivatives with respect to coordinates  S-XTAL
                     do m=1,3
                        k= At(i)%l_x(m)
                        if (k /= 0) then
                           f=at(i)%m_x(m)
                           der=   -av*(otr(i)*drc(m,i) + oti(i)*drs(m,i))
                           der=der-bv*(oti(i)*drc(m,i) - otr(i)*drs(m,i))
                           der=2.0_cp*der*tpi
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do

                     k=At(i)%l_u_iso  !Derivatives w.r.t. Biso  S-XTAL
                     if (k /= 0) then
                        f=At(i)%m_u_iso
                        der=   -av*( otr(i)*frc(i) - oti(i)*frs(i) )
                        der=der-bv*( oti(i)*frc(i) + otr(i)*frs(i) )
                        der=2.0_cp*der*sn
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if

                     k=At(i)%l_occ    !Derivatives w.r.t. occupation factor  S-XTAL
                     if (k /= 0) then
                        f=At(i)%m_occ
                        der=    av*( otr(i)*frc(i) - oti(i)*frs(i) )
                        der=der+bv*( oti(i)*frc(i) + otr(i)*frs(i) )
                        der=2.0_cp*der/atm%atom(i)%occ
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if

                     do m=4,9        !Derivatives w.r.t. anisotropic temperature factors S-XTAL
                        j=m-3
                        k=At(i)%l_u(j)
                        if (k /= 0) then
                           f=At(i)%m_u(j)
                           der=   -av*(otr(i)*drc(m,i) - oti(i)*drs(m,i))
                           der=der-bv*(oti(i)*drc(m,i) + otr(i)*drs(m,i))
                           der=2.0_cp*der
                           if (j > 3) der=2.0_cp*der
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do

                  end do ! Natoms
               end if ! modi
         end select      
      end if  ! derivatives

   End Subroutine Calc_hkl_StrFactor
   
   !!----
   !!---- SUBROUTINE CALC_STRFACTOR
   !!----
   !!----    Calculate Structure Factor for reflection "nn" in the list
   !!----    and derivatives with respect to refined parameters
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Calc_StrFactor(Nn, Sn, Atm, Grp, Mode, Rad, Sf2, Deriv, fc)
      !---- Arguments ----!
      integer,                            intent(in) :: nn
      real(kind=cp),                      intent(in) :: sn    !(sinTheta/Lambda)**2
      type(AtList_type),                  intent(in) :: Atm
      type(SpG_type),                     intent(in) :: Grp
      character(len=*),                   intent(in) :: mode  ! S-XTAL (S) or Powder (P) 
      character(len=*),                   intent(in) :: rad   ! Xray or Neutrons
      real(kind=cp),                      intent(out):: sf2
      real(kind=cp),dimension(:),optional,intent(out):: deriv
      complex, optional,                  intent(out):: fc

      !---- Local Variables ----!
      character(len=1)                      :: modi,crad
      integer                               :: i,j,k,m
      real(kind=cp)                         :: arg,anis,cosr,sinr,scosr,ssinr,fr,der !,fi
      real(kind=cp)                         :: a1,a2,a3,a4,b1,b2,b3,b4,av,bv,f
      real(kind=cp),dimension(3)            :: h
      real(kind=cp),dimension(6)            :: beta
      real(kind=cp),dimension(Atm%natoms)   :: frc,frs,otr,oti,afpxn
      real(kind=cp),dimension(9,Atm%natoms) :: drs,drc

      !> Init
      modi=u_case(mode(1:1))
      crad=u_case(rad(1:1))
      a1=0.0_cp; a2=0.0_cp; a3=0.0_cp; a4=0.0_cp
      b1=0.0_cp; b2=0.0_cp; b3=0.0_cp; b4=0.0_cp
      av=0.0_cp; bv=0.0_cp
      fr=1.0_cp
      !fi=0.0
      frc=0.0_cp; frs=0.0_cp
      otr=0.0_cp; oti=0.0_cp

      if (crad == "N") then
         afpxn(:)=afp(:)
      else
         afpxn(:)=af0(:,nn)
      end if

      if (Grp%Centred == 2) then
         do i=1,Atm%natoms
            arg=0.0_cp
            scosr=0.0_cp
            ssinr=0.0_cp
            drs(:,i)=0.0_cp
            drc(:,i)=0.0_cp
            do k=1,grp%NumOps
               h=hr(k,nn)%h
               arg=TPI*(dot_product(h,Atm%atom(i)%x)+ht(k,nn))
               anis=1.0_cp
               if (Atm%atom(i)%thtype == "ani") then
                  beta=Atm%atom(i)%u(1:6)
                  anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                       +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                  anis=exp(-anis)
               end if
               cosr=COS(arg)*anis*fr     !fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               scosr=scosr+cosr          !FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)

               if (present(deriv)) then
                  sinr=SIN(arg)*anis*fr   !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
                  drc(1:3,i)=drc(1:3,i)+h(1:3)*sinr      ! -
                  drc(4,i)=drc(4,i)+h(1)*h(1)*cosr
                  drc(5,i)=drc(5,i)+h(2)*h(2)*cosr
                  drc(6,i)=drc(6,i)+h(3)*h(3)*cosr
                  drc(7,i)=drc(7,i)+h(1)*h(2)*cosr
                  drc(8,i)=drc(8,i)+h(1)*h(3)*cosr
                  drc(9,i)=drc(9,i)+h(2)*h(3)*cosr
               end if
            end do ! symmetry

            frc(i) = scosr
            otr(i) = afpxn(i)*th(i,nn)
            oti(i) =  afpp(i)*th(i,nn)
            a1= a1 + otr(i)*frc(i)
            b1= b1 + oti(i)*frc(i)
         end do ! Atoms

         av = a1-a2-a3-a4    !real part of the structure factor
         bv = b1-b2+b3+b4    !imaginary part of the structure factor

      else
         do i=1,Atm%natoms
            arg=0.0_cp
            scosr=0.0_cp
            ssinr=0.0_cp
            drs(:,i)=0.0_cp
            drc(:,i)=0.0_cp
            do k=1,grp%NumOps
               h=hr(k,nn)%h
               arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,nn))
               anis=1.0_cp
               if (Atm%atom(i)%thtype == "ani") then
                  beta=Atm%atom(i)%u(1:6)
                  anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                       +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                  anis=exp(-anis)
               end if
               cosr=COS(arg)*anis*fr     !fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               sinr=SIN(arg)*anis*fr     !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
               scosr=scosr+cosr          !FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)
               ssinr=ssinr+sinr          !FRS= SIG fr(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)

               if (present(deriv)) then
                  drc(1:3,i)=drc(1:3,i)+h(1:3)*sinr      ! -
                  drs(1:3,i)=drs(1:3,i)+h(1:3)*cosr      ! +

                  drc(4,i)=drc(4,i)+h(1)*h(1)*cosr
                  drc(5,i)=drc(5,i)+h(2)*h(2)*cosr
                  drc(6,i)=drc(6,i)+h(3)*h(3)*cosr
                  drc(7,i)=drc(7,i)+h(1)*h(2)*cosr
                  drc(8,i)=drc(8,i)+h(1)*h(3)*cosr
                  drc(9,i)=drc(9,i)+h(2)*h(3)*cosr

                  drs(4,i)=drs(4,i)+h(1)*h(1)*sinr
                  drs(5,i)=drs(5,i)+h(2)*h(2)*sinr
                  drs(6,i)=drs(6,i)+h(3)*h(3)*sinr
                  drs(7,i)=drs(7,i)+h(1)*h(2)*sinr
                  drs(8,i)=drs(8,i)+h(1)*h(3)*sinr
                  drs(9,i)=drs(9,i)+h(2)*h(3)*sinr
               end if
            end do ! symmetry

            frc(i) = scosr
            frs(i) = ssinr
            otr(i) = afpxn(i)*th(i,nn)
            oti(i) =  afpp(i)*th(i,nn)
            a1= a1 + otr(i)*frc(i)
            b1= b1 + oti(i)*frc(i)
            a3 = a3 + oti(i)*frs(i)
            b3 = b3 + otr(i)*frs(i)
         end do ! Atoms

         av = a1-a2-a3-a4    !real part of the structure factor
         bv = b1-b2+b3+b4    !imaginary part of the structure factor
      end if

      if (modi == "P") then
         sf2 = a1*a1 + a2*a2 + a3*a3 + a4*a4 + b1*b1 + b2*b2 + b3*b3 + b4*b4
         sf2 = sf2 + 2.0*(b1*b4 -  a1*a4 + a2*a3 - b2*b3)
      else
         sf2= av*av+bv*bv
      end if

      if (present(fc)) fc=cmplx(av,bv)

      if (present(deriv)) then
         select type (At => Atm%atom)
            type is (Atm_Ref_Type)
     
               if (modi == "P") then
                  do i=1,Atm%natoms
                     !> derivatives with respect to coordinates  POWDER
                     do m=1,3
                        k= At(i)%l_x(m)
                        if (k /= 0) then
                           f=at(i)%m_x(m)
                           der= otr(i)*(-a1*drc(m,i)+b3*drs(m,i))+oti(i)*(-b1*drc(m,i)+a3*drs(m,i))
                           der=2.0_cp*der*tpi
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do

                     k=At(i)%l_u_iso  !Derivatives w.r.t. Biso  POWDER
                     if (k /= 0) then
                        f=At(i)%m_u_iso
                        der= otr(i)*(a1*frc(i) +b3*frs(i))+oti(i)*(b1*frc(i) +a3*frs(i))
                        der=-2.0_cp*der*sn
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if
                     
                     k=At(i)%l_occ    !Derivatives w.r.t. occupation factor   POWDER
                     if (k /= 0) then
                        f=At(i)%m_occ
                        der= otr(i)*(a1*frc(i)+b3*frs(i))+oti(i)*(b1*frc(i)+a3*frs(i))
                        der=2.0_cp*der/atm%atom(i)%occ
                        deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                     end if

                     do m=4,9      !Derivatives w.r.t. anisotropic temperature factors   POWDER
                        j=m-3
                        k=At(i)%l_u(j)
                        if (k /= 0) then
                           f=At(i)%m_u(j)
                           der=  otr(i)*(a1*drc(i,j)+b3*drs(m,i))+oti(i)*(b1*drc(m,i)+a3*drs(m,i))
                           der=-2.0_cp*der
                           if (j > 3) der=2.0_cp*der
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                     end do

                  end do ! Natoms

               else
                  do i=1,Atm%natoms
                     !>derivatives with respect to coordinates  S-XTAL
                     do m=1,3
                        k= At(i)%l_x(m)
                        if (k /= 0) then
                           f=at(i)%m_x(m)
                           der=   -av*(otr(i)*drc(m,i) + oti(i)*drs(m,i))
                           der=der-bv*(oti(i)*drc(m,i) - otr(i)*drs(m,i))
                           der=2.0_cp*der*tpi
                           deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                        end if
                      end do

                      k=At(i)%l_u_iso  !Derivatives w.r.t. Biso  S-XTAL
                      if (k /= 0) then
                         f=At(i)%m_u_iso
                         der=   -av*( otr(i)*frc(i) - oti(i)*frs(i) )
                         der=der-bv*( oti(i)*frc(i) + otr(i)*frs(i) )
                         der=2.0_cp*der*sn
                         deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                      end if
                      
                      k=At(i)%l_occ    !Derivatives w.r.t. occupation factor  S-XTAL
                      if (k /= 0) then
                         f=At(i)%m_occ
                         der=    av*( otr(i)*frc(i) - oti(i)*frs(i) )
                         der=der+bv*( oti(i)*frc(i) + otr(i)*frs(i) )
                         der=2.0_cp*der/atm%atom(i)%occ
                         deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                      end if

                      do m=4,9        !Derivatives w.r.t. anisotropic temperature factors S-XTAL
                         j=m-3
                         k=At(i)%l_u(j)
                         if (k /= 0) then
                            f=At(i)%m_u(j)
                            der=   -av*(otr(i)*drc(m,i) - oti(i)*drs(m,i))
                            der=der-bv*(oti(i)*drc(m,i) + otr(i)*drs(m,i))
                            der=2.0_cp*der
                            if (j > 3) der=2.0_cp*der
                            deriv(k) = sign(1.0_cp,f)*der+deriv(k)
                         end if
                      end do

                  end do ! Natoms
               end if !modi
         end select      
      end if  !derivatives

   End Subroutine Calc_StrFactor
 
   
End SubModule SF_Calculations   