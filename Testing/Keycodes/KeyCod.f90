!!----
!!---- KeyCodes Program (For Testing purposes)
!!----
!!----
Program KeyCodes
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String 
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_IOForm,       only: Read_Xtal_Structure
   use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_Ref_Type, Write_Atom_List
   
   !---- Variables ----!
   Implicit None

   type (File_type)                :: ffile
   class (SPG_Type), allocatable   :: SpGr
   type (Cell_G_Type)              :: Cell
   type (AtList_Type)              :: At
   
   character(len=256)              :: filcod 
   character(len=2)                :: ans
   integer                         :: i, narg, lun
   logical                         :: esta, arg_given=.false.
   real(kind=cp)                   :: tini,tfin, total_time
   

   !> Arguments on the command line
   narg=Command_Argument_Count()
   if (narg > 0) then
      call Get_Command_Argument(1,filcod)
      i=index(filcod,'.cfl')
      if (i /= 0) filcod=filcod(1:i-1)
      arg_given=.true.
   end if
   
   Total_time=0.0_cp

   do   

      write(unit=*,fmt="(/,/,8(a,/))")                                                        &
              "                      =============================="                        , &
              "                      ====== PROGRAM KEYCODES ======"                        , &
              "                      =============================="                        , &
              "    ***********************************************************************" , &
              "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
              "    *                            Reading only .cfl                        *" , &
              "    ***********************************************************************" , &
              "                           (version: April 2022)"
      write(unit=*,fmt=*) " "
      
      if (.not. arg_given) then
         write(unit=*,fmt="(a)") " => Code of the file xx.cfl (give xx): "
         read(unit=*,fmt="(a)") filcod
         if (len_trim(filcod) == 0) then
            write(unit=*,fmt="(a)",advance="no") " => Please, press <cr> to finish the program"
            read(unit=*,fmt="(a)") ans
            stop 
         end if 
      end if
      
      inquire(file=trim(filcod)//'.cfl',exist=esta)
      if (.not. esta) then
         write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl does'nt exist!"
         stop
      end if 
      call cpu_time(tini)
      
      !> Read file 
      call Read_Xtal_Structure(trim(filcod)//'.cfl', Cell, SpGr, At, FType=ffile)
      if (Err_CFML%Ierr /=0) then
         write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
         write(unit=*,fmt="(/,a)") " => PROGRAM KEYCODES finished in error!"
         stop
      end if   
      
      !> Write Information
      open(newunit=lun,file=trim(filcod)//".log", status="replace",action="write")
      
      write(unit=lun,fmt="(a,/)") " KEYCODE TESTING"
      
      call Write_Crystal_Cell(Cell,lun)
      write(unit=lun,fmt="(a,/)") " "
      
      call Write_SpaceGroup_Info(SpGr,lun)
      write(unit=lun,fmt="(a,/)") " "
      
      call Write_Atom_List(At,lun)
      
      !> Testing Zone     
      
      
      !> End Testing Zone
      call cpu_time(tfin)
      tfin=tfin-tini
      total_time=total_time+tfin
      tini=tfin/60.0
      tfin=int(tini)
      tini=(tini-tfin)*60.0
      write(unit=lun,fmt="(/,a)")        " => Normal End of: PROGRAM KEYCODES "
      write(unit=*,fmt="(a,i3,a,f8.4,a)")     " => CPU-time: ",nint(tfin)," minutes",tini," seconds"
      write(unit=lun,fmt="(a,i3,a,f8.4,a)")   " => CPU-time: ",nint(tfin)," minutes",tini," seconds"
      close(unit=lun)
      
      arg_given=.false.
   end do
   tini=total_time/60.0
   tfin=int(tini)
   tini=(tini-tfin)*60.0
   write(unit=*,fmt="(a,i3,a,f8.4,a)")     " => TOTAL CPU-time: ",nint(tfin)," minutes",tini," seconds"

End Program KeyCodes

