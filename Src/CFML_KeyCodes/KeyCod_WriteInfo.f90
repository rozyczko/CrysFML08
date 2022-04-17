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
      write(unit=lun, fmt="(a,a)")" N.Par             Name             Value         Sigma     ",&
                                  "   L.Bound       U.Bound       Step       BCon  Par "
      do i=1,NP_Ref
         write(unit=lun,fmt="(i6,tr5,a20,5f14.5, i4, i6)") i, Vec_NamePar(i), Vec_RefPar(i), Vec_RefParSTD, &
                                                              Vec_LimPar(:,i), Vec_BCon(i), Vec_PointPar(i)
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
   Module Subroutine WriteInfo_Restraints(AtList, Iunit)
      !---- Arguments ----!
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: iunit

      !---- Local variables ----!
      character(len=14) :: car1,car2,car3,car4
      integer           :: i,i1,i2,i3,i4,lun
      real              :: disto,distc,ango,angc,sigm, cost,w, delta

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Rest_Dis > 0) then
         write(unit=lun,fmt="(/,a)") " ============================================================"
         write(unit=lun,fmt="(a)")   "   Distance Restraints: Atoms, Dobs, Dcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================"
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2              Dobs        Dcalc       Sigma   (Do-Dc)/Sigma"
         
         cost=0.0_cp
         do i=1,NP_Rest_Dis
            i1=Dis_rest(i)%p(1)
            i2=Dis_rest(i)%p(2)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//dis_rest(i)%code
            disto=Dis_rest(i)%obs
            distc=Dis_rest(i)%cal
            delta=disto-distc
            sigm=Dis_rest(i)%sig
            w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,2a,4f12.5)") i,car1,car2,disto,distc,sigm,delta/sigm
         end do
         write(unit=lun,fmt="(/,a,f12.5)") "   Distance Restraints Cost = Sum{[(Dobs-Dcalc)/Sigma]^2} = ",cost
      end if

      if (NP_Rest_Ang > 0) then
         write(unit=lun,fmt="(/,a)") " ============================================================="
         write(unit=lun,fmt="(a)")   "   Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================="
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2          Atom3            Ang_obs    Ang_calc      Sigma   (Ao-Ac)/Sigma"

         cost=0.0_cp
         do i=1,NP_Rest_Ang
            i1=Ang_rest(i)%p(1)
            i2=Ang_rest(i)%p(2)
            i3=Ang_rest(i)%p(3)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//ang_rest(i)%code(1)
            car3=trim(Atlist%Atom(i3)%lab)//ang_rest(i)%code(2)
            ango=Ang_rest(i)%obs
            angc=Ang_rest(i)%cal
            delta=ango-angc
            sigm=Ang_rest(i)%sig
            w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,3a,4f12.5)") i,car1,car2,car3,ango,angc,sigm,delta/sigm
         end do
         write(unit=lun,fmt="(/,a,f12.5)") "   Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

      if (NP_Rest_Tor > 0) then
         write(unit=lun,fmt="(/,a)") " ====================================================================="
         write(unit=lun,fmt="(a)")   "   Torsion Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ====================================================================="
         write(unit=lun,fmt="(a)") " Rest#    Atom1         Atom2          Atom3          Atom4            "//&
              "Ang_obs    Ang_calc      Sigma   (Ang_obs-Ang_calc)/Sigma"

         cost=0.0_cp
         do i=1,NP_Rest_tor
            i1=Tor_rest(i)%p(1)
            i2=Tor_rest(i)%p(2)
            i3=Tor_rest(i)%p(3)
            i4=Tor_rest(i)%p(4)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)//tor_rest(i)%code(1)
            car3=trim(Atlist%Atom(i3)%lab)//tor_rest(i)%code(2)
            car4=trim(Atlist%Atom(i4)%lab)//tor_rest(i)%code(3)
            ango=Tor_rest(i)%obs
            angc=Tor_rest(i)%cal
            delta=ango-angc
            sigm=Tor_rest(i)%sig
            w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,4a,4f12.5)") i,car1,car2,car3,car4,ango,angc,sigm,delta/sigm
         end do
         write(unit=lun,fmt="(/,a,f12.5)") "   Torsion Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

   End Subroutine WriteInfo_Restraints

End SubModule KeyCod_WriteInfo
