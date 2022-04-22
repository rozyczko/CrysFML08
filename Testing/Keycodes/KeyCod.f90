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
   use CFML_Atoms,        only: AtList_Type, Atm_type, Atm_Std_Type, Atm_Ref_Type, &
                                Matm_Std_Type, Matm_Ref_type, Write_Atom_List, &
                                Index_AtLab_on_AtList, Change_AtomList_Type

   use CFML_KeyCodes,     only: KEY_ATM, split_genrefcod_atm, Split_LocRefCod_ATM, &
                                Fill_RefCodes_Atm, WriteInfo_RefParams, Allocate_VecRef,&
                                NP_Ref, NP_Ref_Max, Allocate_Restraints_Vec, Get_Afix_Line, &
                                Get_DFix_Line, Get_TFix_Line, NP_Rest_Ang, NP_Rest_Dis, NP_Rest_Tor, &
                                Ang_Rest, Dis_Rest, Tor_Rest, WriteInfo_Restraints

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

      !> Print info
      call WriteInfo_RefParams()
      call WriteInfo_RefParams(lun)

      call WriteInfo_Restraints(At)
      call WriteInfo_Restraints(At, Iunit=lun)

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
      integer :: i, k, nt, nlong
      integer :: n_dfix,n_afix,n_tfix


      !> Init
      call clear_error()

      nt=n_end-n_ini +1
      if (nt <=0) return

      !> Allocating vector for Refinement parameters
      call Allocate_VecRef(AtList%natoms * 15)

      !> Check the Atom type in the list
      select type (A => Atlist%atom)
         type is (Atm_Type)
            call Change_AtomList_Type(AtList, 'Atm_Ref_Type', 0)

         type is (Atm_Std_Type)
            call Change_AtomList_Type(AtList, 'Atm_Ref_Type', 0)

         type is (Atm_Ref_Type)
            ! Change no necessary

         type is (Matm_Std_Type)
            call Change_AtomList_Type(AtList, 'MAtm_Ref_Type', 0)

         type is (Matm_Ref_Type)
            ! Change no necessary
      end select
      if (err_CFML%Ierr /= 0) then
         print*, trim(err_CFML%Msg)
         return
      end if

      !> Restrains Information?
      call Allocate_Restraints_Vec(Ffile, n_ini, n_end, n_dfix, n_afix, n_tfix)

      do i=n_ini,n_end
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_ATM(line, AtList, Spg)
               if (err_CFML%IErr /=0) then
                  print*,err_CFML%Msg
               end if

            case ("VARY")    ! VARY
               call ReadCode_VARY_ATM(line, AtList, Spg)
               if (err_CFML%IErr /=0) then
                  print*,err_CFML%Msg
               end if

            case ("EQUA") ! Equal (Constraints)
               call cut_string(line,nlong)

            case ("AFIX") ! AFIX ang sigma    (Angles restraints)
               call cut_string(line,nlong)
               call Get_AFIX_Line(line, AtList)

            case ("DFIX") ! DFIX d sigma      (Distance restraints)
               call cut_string(line,nlong)
               call Get_DFIX_Line(line, AtList)

            case ("TFIX") ! TFIX ang sigma    (Torsion angle restraints)
               call cut_string(line,nlong)
               call Get_TFIX_Line(line, AtList)

         end select
      end do

   End Subroutine Read_RefCodes_ATM

   !!----
   !!---- ReadCode_FIX_ATM
   !!----
   !!---- Update: April - 2022
   !!
   Subroutine ReadCode_FIX_ATM(String, AtList, Spg)
      !---- Arguments ----!
      character(len=*),   intent(in)     :: String
      type(AtList_Type),  intent(in out) :: AtList
      class (SpG_type),   intent(in)     :: Spg

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=3)                      :: car
      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: npos, nlong, ndir, nloc, nc
      integer                               :: ii,j,k,na
      integer, dimension(NMAX_GEN)          :: Idir, Idir2, IPh
      real, dimension(3)                    :: Bounds
      logical                               :: done

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'FIX') then
         call set_error(1,'Wrong Directive for FIX instruction: '//trim(line))
         return
      end if

      !> Cut FIX word
      call cut_string(line,nlong)

      !> general directives
      call split_genrefcod_atm(line,ndir,Idir)

      !> Locals  directives
      call Split_LocRefCod_ATM(line, nloc, dir_loc, Idir2, dir_lab, IPh)

      if (ndir > 0 .and. nloc > 0) then
         call set_error(1,'Wrong form for FIX: '//trim(line))
         return
      end if

      bounds = [0.0, 1.0, 0.1]

      if (ndir > 0) then
         call get_words(line,dire,nc)
         do j=1,ndir
            do k=ndir+1,nc
               na=Index_AtLab_on_AtList(dire(k),Atlist)
               if (na > 0) then
                  call Fill_RefCodes_Atm('FIX', Idir(j), Bounds, 1, Na, Spg, Atlist)
               else
                  !> Species
                  done=.false.
                  do ii=1,AtList%Natoms
                     if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle
                     call Fill_RefCodes_Atm('FIX', Idir(j), Bounds, 1, ii, Spg, Atlist)
                     done=.true.
                  end do

                  if (.not. done) then
                     call set_error(1,'Not found the Atom label: '//trim(dire(k)))
                     return
                  end if
               end if
            end do ! Objects
         end do ! ndir
      end if

      if (nloc > 0) then
         do j=1,nloc
            na=Index_AtLab_on_AtList(dir_lab(j),AtList)
            if (na==0) then
               call set_error(1,'Not found the Atom given in the list! -> '//trim(dir_lab(j)))
               return
            end if
            call Fill_RefCodes_Atm('FIX', Idir2(j), Bounds, 1, Na, Spg, Atlist)
         end do
      end if

   End Subroutine ReadCode_FIX_ATM

   !!----
   !!---- ReadCode_VARY_ATM
   !!----
   !!---- Update: April - 2022
   !!
   Subroutine ReadCode_VARY_ATM(String, AtList, Spg)
      !---- Arguments ----!
      character(len=*),   intent(in)     :: String
      type(AtList_Type),  intent(in out) :: AtList
      class (SpG_type),   intent(in)     :: Spg

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=3)                      :: car
      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: npos, nlong, ndir, nloc, nc
      integer                               :: ii,j,k,na
      integer, dimension(NMAX_GEN)          :: Idir, Idir2, IPh
      real, dimension(3)                    :: Bounds
      logical                               :: done

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'VAR') then
         call set_error(1,'Wrong Directive for VARY instruction: '//trim(line))
         return
      end if

      !> Cut FIX word
      call cut_string(line,nlong)

      !> general directives
      call split_genrefcod_atm(line,ndir,Idir,dir_gen)

      !> Locals  directives
      call Split_LocRefCod_ATM(line, nloc, dir_loc, Idir2, dir_lab, IPh)

      if (ndir > 0 .and. nloc > 0) then
         call set_error(1,'Wrong form for VARY: '//trim(line))
         return
      end if

      bounds = [0.0, 1.0, 0.1]
      if (ndir > 0) then
         call get_words(line,dire,nc)
         do j=1,ndir
            do k=ndir+1,nc
               na=Index_AtLab_on_AtList(dire(k),Atlist)
               if (na > 0) then
                  call Fill_RefCodes_Atm('VARY', Idir(j), Bounds, 1, Na, Spg, Atlist)
               else
                  !> Species
                  done=.false.
                  do ii=1,AtList%Natoms
                     if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle
                     call Fill_RefCodes_Atm('VARY', Idir(j), Bounds, 1, ii, Spg, Atlist)
                     done=.true.
                  end do
                  if (.not. done) then
                     call set_error(1,'Not found the Atom label: '//trim(dire(k)))
                     return
                  end if
               end if
            end do !k
         end do ! ndir
      end if

      if (nloc > 0) then
         do j=1,nloc
            na=Index_AtLab_on_AtList(dir_lab(j),AtList)
            if (na==0) then
               call set_error(1,'Not found the Atom given in the list! -> '//trim(dir_lab(j)))
               return
            end if
            call Fill_RefCodes_Atm('VARY', Idir2(j), Bounds, 1, Na, Spg, Atlist)
         end do
      end if

   End Subroutine ReadCode_VARY_ATM





End Program KeyCodes

