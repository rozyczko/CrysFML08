!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Property
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_PROPERTY_X
   !!----
   !!---- Returns the ...
   !!----
   !!---- Date: 16/03/2017
   !!
   Module Function Get_Property_X(P, T, Eos, Xtype) Result(Val)
      !---- Arguments ----!
      real(kind=cp),     intent(in) :: P       ! Pressure
      real(kind=cp),     intent(in) :: T       ! Temperature
      type(Eos_Type),    intent(in) :: EoS     ! Eos Parameter
      integer, optional, intent(in) :: xtype   ! =0 when X=V, =1 for X=K (isothermal)
      real(kind=cp)                 :: Val

      !---- Local Variables ----!
      integer       :: itype
      real(kind=cp) :: vol, agt

      !> Init
      itype=0               ! default
      if (present(xtype)) itype=xtype

      select case(itype)
         case(0)                 ! Volume
            val=Get_Volume(P,T,Eos)

         case(1)                 ! Isothermal modulus
            vol=Get_Volume(P,T,Eos)
            val=K_Cal(Vol,T,Eos,p=p)

         case(2)                 ! Adiabatic modulus
            vol=Get_Volume(P,T,Eos)
            agt=Alpha_Cal(P,T,Eos)*get_grun_th(P,T,eos)*T        ! Get_Grun knows about linear
            if (eos%linear) agt=3.0_cp*agt
            val=(1.0_cp+agt)*K_Cal(Vol,T,Eos,p=p)

         case default
            val=0.0
      end select

   End Function Get_Property_X

   !!----
   !!---- FUNCTION GET_PROPS_GENERAL
   !!----
   !!---- Returns elastic properties input P,T for a general direction axis in cell_eos
   !!---- Use Eos_Cal when the eos for an axis is known
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Props_General(P, T, Cell_Eos, Axis) Result(Parvals)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp), dimension(6)     :: parvals

      !---- Local Variables ----!

      !>init
      parvals=0.0_cp

      parvals(1)=get_Volume_general(P,T,cell_eos,axis)      ! length
      parvals(2)=get_mod_general(P,T,cell_eos,axis)         ! M
      parvals(3)=get_modp_general(P,T,cell_eos,axis)        ! Kp
      parvals(5)=get_dMdT_general(P,T,cell_eos,axis)        ! dK/dT
      parvals(6)=get_alpha_general(P,T,cell_eos,axis)       ! alpha

   End Function Get_Props_General

   !!----
   !!---- FUNCTION GET_PROPS_THIRD
   !!----
   !!---- Returns elastic properties input P,T for principal axis ieos in cell_eos
   !!---- when it can calculated from other eos
   !!---- Use Eos_Cal when the eos for an axis is known
   !!----
   !!---- Date: 09/09/2020
   !!----
   Module Function Get_Props_Third(P, T, Cell_Eos, Ieos) Result(Parvals)
      !---- Arguments ----!
      real(kind=cp),             intent(in)  :: p,T
      type(eos_cell_type),       intent(in)  :: cell_eos
      integer,                   intent(in)  :: ieos     ! the axis (1,2,3) or V (0) to be calculated
      real(kind=cp), dimension(6)            :: parvals

      !---- Local Variables ----!

      !>init
      parvals=0.0_cp

      parvals(1)=get_volume_third(P,T,cell_eos,ieos)       ! V
      parvals(2)=get_mod_third(P,T,cell_eos,ieos)          ! K
      parvals(3)=get_modp_third(P,T,cell_eos,ieos)         ! Kp
      parvals(5)=get_dMdT_third(P,T,cell_eos,ieos)         ! dK/dT
      parvals(6)=get_alpha_third(P,T,cell_eos,ieos)        ! alpha

   End Function Get_Props_Third

   !!--++
   !!--++ FUNCTION GET_PROPS_PTVTABLE
   !!--++
   !!--++ Returns the requested property at two of P,T,V from an eos loaded as a table of values
   !!--++ The volume values in the table are expected to be scaled to V=1.0 at Pref, Tref
   !!--++ and the true V0 is in eospar%params(1)
   !!--++
   !!--++ Date: 14/10/16
   !!
   Module Function Get_Props_PTVTable(P, T, V, Eos, Res) Result(Val)
      !---- Arguments ----!
      real(kind=cp),    intent(in) :: P       ! Pressure
      real(kind=cp),    intent(in) :: T       ! Temperature
      real(kind=cp),    intent(in) :: V       ! Volume
      type(Eos_Type),   intent(in) :: EoS     ! Eos Parameter
      character(len=*), intent(in) :: Res     ! Parameter requested for calculation (P,T, or V)
      real(kind=cp)                :: Val

      !---- Local Variables ----!
      integer                   :: i,j
      real(kind=cp)             :: VV0,tt,vm,vp,km,kp,va,vb,dvdp
      type(Eos_Type)            :: eosm,eosp,eosv     ! Eos for local Murn at Tminus and Tplus
      character(len=10)         :: Var
      !character(len=60)         :: text

      !>Init
      Val=0.0_cp

      !> Check valid request
      if (EoS%imodel/= -1) then
         call set_error(1,' ')
         write(err_cfml%msg,'(''Request to get_props_pvttable with invalid imodel #'',i5)')EoS%imodel
         return
      end if

      VV0=V/EoS%params(1)        ! table values are all V/V0

      !> Determine what is the request: P,T,V, K, KP, AL
      Var=Res
      Var=U_Case(adjustl(Var))

      !> Find lower-corner coords for P,T  : works because P and T always ascend
      if (Var(1:1) /= 'P')then                         ! Only test if P supplied in argument, not if P requested from T and V
         do i=1,EoS%table%np
            if (p < EoS%table%ptv(i,1,1)) exit
         end do

         if (i < 2)then
            call set_error(-1,' ')
            write(err_cfml%msg, &
                 '(''PTV table request for P = '',f6.2,'' smaller than Pmin = '',f6.2,'' at T ='',f6.1,'': Linear guess made'')') &
                 p,EoS%table%pmin,t
            i=2
         end if

         if (i > EoS%table%np-1) then
            call set_error(-1,' ')
            write(err_cfml%msg,&
                 '(''PTV table request for P = '',f6.2,'' bigger than Pmax = '',f6.2,'' at T ='',f6.1,'': Linear guess made'')') &
                 p,EoS%table%pmax,t
            i=EoS%table%np-1
         end if
      end if

      !> Check the T limits
      do j=1,EoS%table%nt
         if (t < EoS%table%ptv(1,j,2) ) exit
      end do

      if (j < 2 ) then
         call set_error(-1,' ')
         write(err_cfml%msg, &
              '(''PTV table request for T = '',f6.1,'' smaller than Tmin = '',f6.1,'' at P ='',f6.2,'': Linear guess made'')') &
              t,EoS%table%tmin,p
         j=2
      end if

      if (j > EoS%table%nt-1) then
         call set_error(-1,' ')
         write(err_cfml%msg, &
              '(''PTV table request for T = '',f6.1,'' bigger than Tmax = '',f6.1,'' at P ='',f6.1,'': Linear guess made'')') &
              t,EoS%table%tmax,p
         j = EoS%table%nt-1
      end if

      !> Get the Murngahan EoS from Pminus at Tminus and Tplus, but set as if a T=Tref
      !> Must then call Eos functions with Tref as argument
      if (Var(1:1) /= 'P')then       ! cannot do this if P being requested
         EoSM= Murn_Ptv_table(i-1,j-1,EoS)
         EoSp= Murn_Ptv_table(i-1,j,EoS)
      end if

      Select case(Var(1:3))
         case('V  ')
            !> Murnaghan interpolation on P axis, followed by linear in T
            Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)
            Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)
            val=Vm+(Vp-Vm)*(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))  ! linear T
            val=val*EoS%params(1)

         case('P  ')
            tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
            do i=1,EoS%table%np
               vb=va
               va=tt*(EoS%table%ptv(i,j,3)-EoS%table%ptv(i,j-1,3)) +EoS%table%ptv(i,j-1,3)      ! volume at each P row for target T
               if (va < vv0) exit
            end do

            if (i == 1) then
               call set_error(-1,' ')
               write(err_cfml%msg, &
                    '(''PTV table request for V = '',f6.1,''at T = '',f6.1,'' is below Pmin: Linear guess made'')') v,t

               !> linear guess beyond table bottom
               dvdp=(tt*(EoS%table%ptv(2,j,3)-EoS%table%ptv(2,j-1,3)) +EoS%table%ptv(2,j-1,3)-va)/(EoS%table%ptv(2,j,1)- &
                    EoS%table%ptv(1,j,1))
               val=EoS%table%ptv(1,j,1)- (va-vv0)/dvdp
               return
            end if

            if (i >= EoS%table%np) then
               call set_error(-1,' ')
               write(err_cfml%msg, &
                    '(''PTV table request for V = '',f6.1,''at T = '',f6.1,'' is above Pmax: Linear guess made'')') v,t
               !> linear guess beyond table bottom
               dvdp=(va-vb)/(EoS%table%ptv(EoS%table%np,j,1)-EoS%table%ptv(EoS%table%np-1,j,1))
               val=EoS%table%ptv(EoS%table%np,j,1)+ (vv0-va)/dvdp
               return
            end if

            !> Set up Murn in Eosm
            call Init_EoS_Type(Eosv)
            eosv%imodel=1
            call set_eos_names(eosv)      ! sets names for params
            eosv%title='Murnaghan for interpolation'
            eosv%params(1)=vb
            EoSM= Murn_Ptv_table(i-1,j-1,EoS)
            EoSP= Murn_Ptv_table(i-1,j,EoS)
            eosv%params(2)=eosm%params(2)+(eosp%params(2)-eosm%params(2))*tt
            eosv%params(3)=eosm%params(3)+(eosp%params(3)-eosm%params(3))*tt
            val=get_pressure(vv0,eosv%tref,eosv)+EoS%table%ptv(i-1,j,1)

         case('K  ') !input must be p and t
            km=get_k(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)      ! K at Tminus
            kp=get_k(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)          ! K at Tplus
            tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
            val=(kp-km)*tt + km

         case('KP ')
            tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
            val=eosm%params(3)+(eosp%params(3)-eosm%params(3))*tt

         case('AL ')
            !> Murnaghan interpolation on P axis, followed by linear in T
            Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)
            Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)
            val=2.0_cp*(Vp-Vm)/(EoS%table%ptv(i-1,j,2)-EoS%table%ptv(i-1,j-1,2))/(Vm+Vp)

         case default
            call set_error(1,' ')
            write(err_cfml%msg,'(''Request for '',a1,'' to get_props_pvttable not valid'')')Var(1:1)

      end select

   End Function Get_Props_PTVTable

End SubModule EoS_Property