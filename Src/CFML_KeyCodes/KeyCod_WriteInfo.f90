!!
Submodule (CFML_KeyCodes) KeyCod_WriteInfo
   implicit none

   Contains

   !!----
   !!---- Subroutine WriteInfo_RefParams
   !!----
   !!----    Write the information about Refinement parameters in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_RefParams(Iunit)
      !---- Arguments ----!
      integer, optional,   intent(in) :: Iunit

      !---- Local variables ----!
      integer :: i,lun

      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Ref <= 0) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i5)") " Number of Refinable Parameters: ",NP_Ref
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,a)")" N.Par             Name                 Value     Sigma    ",&
                                  "L.Bound    U.Bound   Step    BCon   Par "
      do i=1,NP_Ref
         write(unit=lun,fmt="(i4,tr12,a20,5f10.5, i5, i6)") i, Vec_NamePar( i), Vec_RefPar(i), Vec_RefParSTD(i), &
                                                               Vec_LimPar(:,i), Vec_BCond(i),  Vec_PointPar(i)
      end do

   End Subroutine WriteInfo_RefParams

   !!----
   !!---- Subroutine WriteInfo_Restraints
   !!----
   !!----    Write the current values of the "observed" and calculated
   !!----    restraints.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Restraints(RDis, RAng, RTor, IPhase, AtList, Calc,Iunit)
      !---- Arguments ----!
      type(RestList_Type), intent(in) :: RDis
      type(RestList_Type), intent(in) :: RAng
      type(RestList_Type), intent(in) :: RTor
      integer,             intent(in) :: IPhase
      type(AtList_Type),   intent(in) :: AtList
      logical, optional,   intent(in) :: Calc
      integer, optional,   intent(in) :: iunit

      !---- Local variables ----!
      character(len=14) :: car1,car2,car3,car4
      integer           :: i,i1,i2,i3,i4,lun
      real              :: disto,distc,ango,angc,sigm, cost,w, delta
      logical           :: done

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      done=.false.
      if (present(calc)) done=calc

      !> -------------------
      !> Distance Restraints
      !> -------------------
      if (RDis%NRel > 0 .and. any(RDis%RT(:)%IPh(1) ==Iphase)) then

         write(unit=lun,fmt="(/,a)") " ============================================================"
         write(unit=lun,fmt="(a)")   "   Distance Restraints: Atoms, Dobs, Dcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================"
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2              Dobs        Dcalc       Sigma   (Do-Dc)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RDis%NRel
            i1=RDis%RT(i)%p(1)
            i2=RDis%RT(i)%p(2)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//'_'//trim(RDis%RT(i)%code(1))
            disto=RDis%RT(i)%obs
            distc=RDis%RT(i)%cal
            delta=disto-distc
            sigm=RDis%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,2a,4f12.5)") i,car1,car2,disto,distc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Distance Restraints Cost = Sum{[(Dobs-Dcalc)/Sigma]^2} = ",cost
      end if

      !> ----------------
      !> Angle Restraints
      !> ----------------
      if (RAng%NRel > 0 .and. any(RAng%RT(:)%IPh(1) ==Iphase)) then
         write(unit=lun,fmt="(/,a)") " ============================================================="
         write(unit=lun,fmt="(a)")   "   Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================="
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2          Atom3            Ang_obs    Ang_calc      Sigma   (Ao-Ac)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RAng%NRel
            i1=RAng%RT(i)%p(1)
            i2=RAng%RT(i)%p(2)
            i3=RAng%RT(i)%p(3)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//'_'//trim(RAng%RT(i)%code(1) )
            car3=trim(Atlist%Atom(i3)%lab)//'_'//trim(RAng%RT(i)%code(2) )
            ango=RAng%RT(i)%obs
            angc=RAng%RT(i)%cal
            delta=ango-angc
            sigm=RAng%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,3a,4f12.5)") i,car1,car2,car3,ango,angc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

      !> -------------------------
      !> Torional Angle Restraints
      !> -------------------------
      if (RTor%NRel > 0 .and. any(RTor%RT(:)%IPh(1) ==Iphase)) then
         write(unit=lun,fmt="(/,a)") " ====================================================================="
         write(unit=lun,fmt="(a)")   "   Torsion Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ====================================================================="
         write(unit=lun,fmt="(a)") " Rest#    Atom1         Atom2          Atom3          Atom4            "//&
              "Ang_obs    Ang_calc      Sigma   (Ang_obs-Ang_calc)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RTor%NRel
            i1=RTor%RT(i)%p(1)
            i2=RTor%RT(i)%p(2)
            i3=RTor%RT(i)%p(3)
            i4=RTor%RT(i)%p(4)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//'_'//trim(RTor%RT(i)%code(1))
            car3=trim(Atlist%Atom(i3)%lab)//'_'//trim(RTor%RT(i)%code(2))
            car4=trim(Atlist%Atom(i4)%lab)//'_'//trim(RTor%RT(i)%code(3))
            ango=RTor%RT(i)%obs
            angc=RTor%RT(i)%cal
            delta=ango-angc
            sigm=RTor%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,4a,4f12.5)") i,car1,car2,car3,car4,ango,angc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Torsion Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

   End Subroutine WriteInfo_Restraints

   !!----
   !!---- Subroutine WriteInfo_Constraints
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Constraints(AtList, Iunit)
      !---- Arguments ----!
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: Iunit

      !---- Local Variables ----!
      integer           :: i,j,k,m,n,np,lun,nmax
      character(len=20) :: car
      character(len=60) :: fmt1

      !> Format Zone
      fmt1="(t10,i3,t21,a,t31,a,t39,f8.4)"

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Constr <=0) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i5)") " => Constraints relations: ",NP_Constr
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a)") "       N.Constr     Name      Father    Factor"
      write(unit=lun, fmt="(a)") "    ============================================="

      np=0
      !associate(A => AtList%Atom)
      !   select type (A)
      !      type is (Atm_Ref_Type)
      !         do i=1,NP_Ref
      !            n=0
      !            do j=1,AtList%natoms
      !               n=n+count(A(j)%L_X == i)
      !               n=n+count(A(j)%L_u ==i)
      !               n=n+count(A(j)%L_Moment ==i)
      !               if (A(j)%L_occ==i)    n=n+1
      !               if (A(j)%L_U_iso==i)  n=n+1
      !            end do
      !
      !            if (n > 1) then
      !               do j=1,AtList%natoms
      !                  !> X's
      !                  do k=1,3
      !                     if (A(j)%l_x(k) == i) then
      !                        car=trim(key_atm(k))//'_'//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        car='XYZ'//'_'//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        np=np+1
      !                        write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)), A(j)%m_x(k)
      !                     end if
      !                  end do
      !
      !                  !> Occ
      !                  if (A(j)%l_occ == i) then
      !                     car=trim(key_atm(5))//'_'//trim(A(j)%lab)
      !                     if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                     np=np+1
      !                     write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)), A(j)%m_occ
      !                  end if
      !
      !                  !> U_iso
      !                  if (A(j)%l_U_iso == i) then
      !                     car=trim(key_atm(6))//'_'//trim(A(j)%lab)
      !                     if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                     np=np+1
      !                     write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_U_iso
      !                  end if
      !
      !                  !> U's
      !                  do k=1,6
      !                     if (A(j)%l_u(k) == i) then
      !                        car=trim(key_atm(7+k))//'_'//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        car='U'//'_'//trim(A(j)%Lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        np=np+1
      !                        write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_u(k)
      !                     end if
      !                  end do
      !
      !                  !> Moment
      !                  do k=1,3
      !                     if (A(j)%l_moment(k) == i) then
      !                        car='MOM'//'_'//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        np=np+1
      !                        write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Moment(k)
      !                     end if
      !                  end do
      !
      !
      !               end do ! Natoms
      !            end if
      !         end do
      !
      !      Type is (ModAtm_Ref_Type)
      !         do i=1,NP_Ref
      !            n=0
      !            do j=1,AtList%natoms
      !               n=n+count(A(j)%L_X == i)
      !               n=n+count(A(j)%L_u ==i)
      !               if (A(j)%L_occ==i)   n=n+1
      !               if (A(j)%L_U_iso==i) n=n+1
      !
      !               n=n+count(A(j)%L_Ocs ==i)
      !               n=n+count(A(j)%L_Bcs ==i)
      !               n=n+count(A(j)%L_Mcs ==i)
      !               n=n+count(A(j)%L_Dcs ==i)
      !               n=n+count(A(j)%L_Ucs ==i)
      !            end do
      !
      !            if (n > 1) then
      !               do j=1,AtList%natoms
      !                  !> X's
      !                  do k=1,3
      !                     if (A(j)%l_x(k) == i) then
      !                        car=trim(key_atm(k))//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        np=np+1
      !                        write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)), A(j)%m_x(k)
      !                     end if
      !                  end do
      !
      !                  !> Occ
      !                  if (A(j)%l_occ == i) then
      !                     car=trim(key_atm(5))//trim(A(j)%lab)
      !                     if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                     np=np+1
      !                     write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)), A(j)%m_occ
      !                  end if
      !
      !                  !> U_iso
      !                  if (A(j)%l_U_iso == i) then
      !                     car=trim(key_atm(6))//trim(A(j)%lab)
      !                     if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                     np=np+1
      !                     write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_U_iso
      !                  end if
      !
      !                  !> U's
      !                  do k=1,6
      !                     if (A(j)%l_u(k) == i) then
      !                        car=trim(key_atm(7+k))//trim(A(j)%lab)
      !                        if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                        np=np+1
      !                        write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_u(k)
      !                     end if
      !                  end do
      !
      !                  !> Ocs
      !                  nmax=2
      !                  do k=1,MAX_MOD
      !                     do m=1,nmax
      !                        if (A(j)%l_Ocs(m,k) == i) then
      !                           car='XXX'//'_'//trim(A(j)%lab)
      !                           if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                           np=np+1
      !                           write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Ocs(m,k)
      !                        end if
      !                     end do
      !                  end do
      !
      !                  !> Bcs
      !                  nmax=2
      !                  do k=1,MAX_MOD
      !                     do m=1,nmax
      !                        if (A(j)%l_Bcs(m,k) == i) then
      !                           car='XXX'//'_'//trim(A(j)%lab)
      !                           if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                           np=np+1
      !                           write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Bcs(m,k)
      !                        end if
      !                     end do
      !                  end do
      !
      !                  !> Mcs
      !                  nmax=6
      !                  do k=1,MAX_MOD
      !                     do m=1,nmax
      !                        if (A(j)%l_Mcs(m,k) == i) then
      !                           car='XXX'//'_'//trim(A(j)%lab)
      !                           if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                           np=np+1
      !                           write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Mcs(m,k)
      !                        end if
      !                     end do
      !                  end do
      !
      !                  !> Dcs
      !                  nmax=6
      !                  do k=1,MAX_MOD
      !                     do m=1,nmax
      !                        if (A(j)%l_Dcs(m,k) == i) then
      !                           car='XXX'//'_'//trim(A(j)%lab)
      !                           if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                           np=np+1
      !                           write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Dcs(m,k)
      !                        end if
      !                     end do
      !                  end do
      !
      !                  !> Ucs
      !                  nmax=12
      !                  do k=1,MAX_MOD
      !                     do m=1,nmax
      !                        if (A(j)%l_Ucs(m,k) == i) then
      !                           car='XXX'//'_'//trim(A(j)%lab)
      !                           if (trim(car)==trim(Vec_NamePar(i))) cycle
      !
      !                           np=np+1
      !                           write(unit=lun,fmt=fmt1)  np, trim(car), trim(Vec_NamePar(i)),A(j)%m_Ucs(m,k)
      !                        end if
      !                     end do
      !                  end do
      !
      !               end do ! Natoms
      !            end if
      !         end do
      !
      !   end select
      !end associate

   End Subroutine WriteInfo_Constraints

End SubModule KeyCod_WriteInfo

