!!
SubModule (CFML_Atoms) Atm_PointList

   implicit none

   Contains

   !!----
   !!---- Function Index_AtLab_on_AtList
   !!----
   !!---- Date: April - 2022
   !!
   Pure Module Function Index_AtLab_on_AtList(AtLab, IPhase, AtList) Result(Indx)
      !---- Arguments ----!
      character(len=*),  intent(in) :: AtLab
      integer,           intent(in) :: IPhase
      type(AtList_Type), intent(in) :: AtList
      integer                       :: Indx

      !---- Local Variables ----!
      integer :: i

      !> Init
      Indx=0

      do i=1,AtList%natoms
         if (IPhase > 0 ) then
            if (AtList%Iph(i) /= IPhase) cycle
         end if
         if (trim(u_case(Atlab)) == trim(u_case(AtList%Atom(i)%Lab))) then
            Indx=i
            exit
         end if
      end do

   End Function Index_AtLab_on_AtList

End SubModule Atm_PointList