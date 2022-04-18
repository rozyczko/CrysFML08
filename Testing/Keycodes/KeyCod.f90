!!----
!!---- KeyCodes Program (For Testing purposes)
!!----
!!----
Program KeyCodes
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error, set_error
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String, Get_Words, &
                                Get_Num
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_IOForm,       only: Read_Xtal_Structure
   use CFML_Metrics,      only: Cell_G_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_Ref_Type, Write_Atom_List, &
                                Index_AtLab_on_AtList

   use CFML_KeyCodes,     only: KEY_ATM, split_genrefcod_atm, Split_LocRefCod_ATM, &
                                Fill_RefCodes_Atm, WriteInfo_RefParams, Allocate_VecRef,&
                                NP_Ref, NP_Ref_Max

   !---- Variables ----!
   Implicit None

   type (File_type)                :: ffile
   class (SPG_Type), allocatable   :: SpGr
   type (Cell_G_Type)              :: Cell
   type (AtList_Type)              :: At

   character(len=256)              :: filcod
   character(len=150)              :: line
   character(len=2)                :: ans
   character(len=40), dimension(10):: dire
   integer                         :: i, j,narg, lun
   integer                         :: nc_i,nc_f
   integer, dimension(10)          :: ivet
   logical                         :: esta, arg_given=.false.
   real(kind=cp)                   :: tini,tfin, total_time
   real(kind=cp), dimension(10)    :: vet

   integer, parameter              ::NKEY_ATM=14


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

      !> -------------
      !> Testing Zone
      !> -------------
      nc_i=0; nc_f=0
      do i=1,ffile%nlines
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         if (nc_i == 0) then
            j=index(u_case(line),'COMMA')
            if (j > 0) then
               nc_i=i+1
               cycle
            end if
         end if

         if (nc_i > 0 .and. i > nc_i) then
            j=index(u_case(line),'END')
            if (j > 0) then
               nc_f=i-1
               exit
            end if
         end if
      end do

      if (nc_i ==0 .or. nc_f ==0) then
         write(unit=*,fmt="(/,a)") " => Don't found the zone for COMMANDS!"
         stop
      end if

      call Read_RefCodes_ATM(ffile, nc_i, nc_f, Spgr, At)

      !> ----------------
      !> End Testing Zone
      !> ----------------

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

   Contains

   !!--++
   !!--++ SUBROUTINE READ_REFCODES_ATM
   !!--++
   !!
   Subroutine Read_RefCodes_ATM(ffile, n_ini, n_end, Spg, Atlist)
      !---- Arguments ----!
      Type(file_type),    intent(in)     :: ffile
      integer,            intent(in)     :: n_ini
      integer,            intent(in)     :: n_end
      class (SpG_type),   intent(in)     :: Spg
      type(AtList_Type),  intent(in out) :: AtList

      !---- Local variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: ndir, nloc, nc,nt
      integer                               :: i,j,k,nlong, na
      integer, dimension(NMAX_GEN)          :: Idir, Idir2,Iph
      real, dimension(3)                    :: Bounds

      !> Init
      call clear_error()
      bounds = [0.0, 1.0, 0.1]
      nt=n_end-n_ini +1
      if (nt <=0) return

      !> Allocating vector for Refinement parameters
      call Allocate_VecRef(AtList%natoms * 15)
      print*,'Numero Maximo de Parametros:',NP_Ref_Max,NP_Ref

      do i=n_ini,n_end
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX .....
               call cut_string(line,nlong)

               !> general directives
               call split_genrefcod_atm(line,ndir,Idir)

               !> Locals  directives
               call Split_LocRefCod_ATM(line, nloc, dir_loc, Idir2, dir_lab, IPh)

               if (ndir > 0 .and. nloc > 0) then
                  call set_error(1, "Bad format for FIX directive!")
                  return
               end if

               if (ndir > 0) then
                  call get_words(line,dire,nc)
                  do j=1,ndir
                     do k=ndir+1,nc
                        na=Index_AtLab_on_AtList(dire(k),At)
                        if (na /= 0) then
                           call Fill_RefCodes_Atm('FIX', Idir(j), Bounds, 1, Na, Spg, Atlist)
                        end if
                     end do
                  end do
               end if

               if (nloc > 0) then
                  do j=1,nloc
                     na=Index_AtLab_on_AtList(dir_lab(j),At)
                     if (na /= 0) then
                        call Fill_RefCodes_Atm('FIX', Idir2(j), Bounds, 1, Na, Spg, Atlist)
                     end if
                  end do
               end if

            case ("VARY")    ! VARY .....
               call cut_string(line,nlong)

               !> general directives
               call split_genrefcod_atm(line,ndir,Idir,dir_gen)

               !> Locals  directives
               call Split_LocRefCod_ATM(line, nloc, dir_loc, Idir2, dir_lab, IPh)

               if (ndir > 0 .and. nloc > 0) then
                  call set_error(1, "Bad format for VARY directive!")
                  return
               end if

               !> Testing
               !>--------

            case ("AFIX") ! AFIX ang sigma    (Angles restraints)
               call cut_string(line,nlong)

            case ("DFIX") ! DFIX d sigma      (Distance restraints)
               call cut_string(line,nlong)

            case ("TFIX") ! TFIX ang sigma    (Torsion angle restraints)
               call cut_string(line,nlong)

         end select
      end do

   End Subroutine Read_RefCodes_ATM





End Program KeyCodes

