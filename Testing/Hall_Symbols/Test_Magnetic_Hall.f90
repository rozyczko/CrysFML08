!!----
!!---- Program: Test_Magnetic_Hall_Symbols
!!----
!!---- JGP May2019
!!
Program Test_Magnetic_Hall_Symbols
   !---- Use Modules ----!
   Use CFML_Globaldeps, only: cp, err_CFML,clear_error
   Use CFML_gSpaceGroups

   !---- Variables ----!
   implicit none

   character(len=80)                            :: input_Hall, str_Hall
   character(len=60), dimension(:), allocatable :: gen
   character(len=:), allocatable                :: generators, setting
   integer                                      :: i,ngen
   type(spg_type)                               :: SpG
   real(kind=cp) :: start, fin


   call system("cls")
   print*,'----------------------'
   print*,' Testing Hall symbols '
   print*,'----------------------'

   !> Main
   do
      write(*,"(a)",advance="no") " => Enter the magnetic Hall symbol: "
      read(*,"(a)") input_Hall
      if(len_trim(input_Hall) == 0) exit
      call cpu_time(start)
      !> Magnetic Hall symbol
      i=index(input_Hall,":")
      if(i /= 0) then
        str_Hall=input_Hall(1:i-1)
        setting=adjustl(input_Hall(i+1:))
      else
        str_Hall=input_Hall
        setting=" "
      end if
      !str_Hall=trim(Mag_symb(ik)%HallM)
      ngen=0
      gen=" "
      call Get_Generators(str_Hall, Gen, ngen)

      generators=" "
      do i=1,ngen
        generators=trim(generators)//trim(gen(i))//";"
      end do
      generators=generators(1:len_trim(generators)-1)
      write(*,"(a)") " => Obtained generators: "//trim(generators)
      if(len_trim(setting) /= 0) then
        write(*,"(a)") " => Followed by a change of basis: "//trim(setting)
      end if


      !!> Constructor
      call Init_SpaceGroup(SpG)
      call Group_Constructor(gen,SpG)
      if (Err_CFML%Ierr /= 0) then
         print*,'    --->'//trim(Err_CFML%Msg)
         cycle
      end if
      !Check if a change of basis is provided
      if(len_trim(setting) /= 0) then
        call Change_Setting_SpaceG(setting, SpG)
      end if

      !
      !!> Identify group
      call Identify_Group(Spg)
      if(Err_CFML%Ierr == 1) then
         write(unit=*,fmt="(a)") "  WARNING: "//Err_CFML%Msg
         call clear_error()
      end if
      call Write_SpaceGroup_Info(SpG)
      call cpu_time(fin)
      write(*,"(a,f12.3,a)") "Total CPU_TIME for this calculation: ",fin-start," seconds"

   end do
End Program Test_Magnetic_Hall_Symbols
