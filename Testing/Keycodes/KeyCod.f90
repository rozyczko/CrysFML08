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

   use CFML_KeyCodes

   !---- Variables ----!
   Implicit None

   type (File_type)                :: ffile
   class (SPG_Type), allocatable   :: SpGr
   type (Cell_G_Type)              :: Cell
   type (AtList_Type)              :: At

   character(len=256)              :: filcod
   character(len=150)              :: line
   character(len=2)                :: ans
   character(len=3)                :: ktype
   character(len=40), dimension(10):: dire
   integer                         :: i, j,narg, lun
   integer                         :: nc_i,nc_f
   integer, dimension(10)          :: ivet
   logical                         :: esta, arg_given=.false.
   real(kind=cp)                   :: tini,tfin, total_time
   real(kind=cp), dimension(10)    :: vet

   integer                         :: NB_Patt, NB_Phas, NB_Mol, NB_RGB
   integer, dimension(2,30)        :: IB_Patt, IB_PHas, IB_MOL, IB_RGB


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

      call Write_Crystal_Cell(Cell,lun)
      write(unit=lun,fmt="(a,/)") " "

      call Write_SpaceGroup_Info(SpGr,lun)
      write(unit=lun,fmt="(a,/)") " "

      call Write_Atom_List(At,Iunit=lun)

      !> -------------
      !> Testing Zone
      !> -------------
      call get_zonecommands(ffile,nc_i,nc_f)
      if (nc_i > 0 .and. nc_i < nc_f) then

         !> ==== Blocks ====
         call Get_Block_Key('Pattern', ffile, nc_i, nc_f, NB_Patt, IB_Patt)
         call Get_Block_Key('Phases',  ffile, nc_i, nc_f, NB_Phas, IB_Phas)
         call Get_Block_Key('Molec',   ffile, nc_i, nc_f, NB_Mol, IB_Mol)
         call Get_Block_Key('RGB',     ffile, nc_i, nc_f, NB_RGB, IB_RGB)

         !> ==== Check Keycodes for Atoms ====
         !call Read_RefCodes_ATM(ffile, nc_i, nc_f, Spgr, At)

         !> Print info
         call WriteInfo_RefParams()
         call WriteInfo_RefParams(lun)

         call WriteInfo_Restraints(At)
         call WriteInfo_Restraints(At, Iunit=lun)
      end if

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
   !!----
   !!----
   !!----
   !!----
   !!
   Function Get_Keycode_Type(String) Result(KType)
      !---- Arguments ----!
      character(len=*), intent(in) :: String
      character(len=3)             :: KType

      !---- Local Arguments ----!
      logical            :: debug=.true.
      character(len=132) :: line
      character(len=3)   :: car
      integer            :: i,j

      !> Init
      KType=" "

      line=trim(u_case(string))
      call cut_string(line)

      !> PAT
      car=" "
      do i=1,7 !NKEY_PATT
         j=index(line,trim(adjustl(KEY_PATT(i))))
         if (j > 0) then
            car='PAT'
            exit
         end if
      end do
      kType=car
      if (debug) print*, 'PAT? '//ktype

      !> PHA
      car=" "
      do i=1,7 !NKEY_PHAS
         j=index(line,trim(adjustl(KEY_PHAS(i))))
         if (j > 0) then
            car='PHA'
            exit
         end if
      end do
      if (len_trim(ktype) > 0 .and. len_trim(car) > 0) then
         ktype=" "
         call set_error(1," Incompatible set of directives in the comand line: "//trim(string))
         if (debug) then
            print*, " Incompatible set of directives in the comand line: "//trim(string)
            stop
         end if
         return
      end if
      kType=car
      if (debug) print*, 'PHAS? '//ktype


      !> RGB
      car=" "
      do i=1,5 !NKEY_RGB
         j=index(line,trim(adjustl(KEY_RGB(i))))
         if (j > 0) then
            car='RGB'
            exit
         end if
      end do
      if (len_trim(ktype) > 0 .and. len_trim(car) > 0) then
         ktype=" "
         call set_error(1," Incompatible set of directives in the comand line: "//trim(string))
         if (debug) then
            print*, " Incompatible set of directives in the comand line: "//trim(string)
            stop
         end if
         return
      end if
      kType=car
      if (debug) print*, 'RGB? '//ktype

      !> MOL
      car=" "
      do i=1,8 !NKEY_MOL
         j=index(line,trim(adjustl(KEY_MOL(i))))
         if (j > 0) then
            car='MOL'
            exit
         end if
      end do
      if (len_trim(ktype) > 0 .and. len_trim(car) > 0) then
         ktype=" "
         call set_error(1," Incompatible set of directives in the comand line: "//trim(string))
         if (debug) then
            print*, " Incompatible set of directives in the comand line: "//trim(string)
            stop
         end if
         return
      end if
      kType=car
      if (debug) print*, 'MOL? '//ktype

      !> MATM
      car=" "
      do i=1,25 !NKEY_MATM
         j=index(line,trim(adjustl(KEY_MATM(i))))
         if (j > 0) then
            car='MAT'
            exit
         end if
      end do
      if (len_trim(ktype) > 0 .and. len_trim(car) > 0) then
         ktype=" "
         call set_error(1," Incompatible set of directives in the comand line: "//trim(string))
         if (debug) then
            print*, " Incompatible set of directives in the comand line: "//trim(string)
            stop
         end if
         return
      end if
      kType=car
      if (debug) print*, 'MATM? '//ktype

      !> ATM
      car=" "
      do i=1,14 !NKEY_ATM
         j=index(line,trim(adjustl(KEY_ATM(i))))
         if (j > 0) then
            car='ATM'
            exit
         end if
      end do
      if (len_trim(ktype) > 0 .and. len_trim(car) > 0) then
         ktype=" "
         call set_error(1," Incompatible set of directives in the comand line: "//trim(string))
         if (debug) then
            print*, " Incompatible set of directives in the comand line: "//trim(string)
            stop
         end if
         return
      end if
      kType=car
      if (debug) print*, 'ATM? '//ktype

   End Function Get_Keycode_Type

   !!----
   !!---- SUBROUTINE GET_ZONECOMMANDS
   !!----
   !!---- Date: 11/05/2022
   !!
   Subroutine Get_ZoneCommands(ffile, N_Ini, N_End)
      !---- Arguments ----!
      Type(file_type),    intent(in)  :: ffile
      integer,            intent(out) :: n_ini
      integer,            intent(out) :: n_end

      !---- Local Variables ----!
      logical            :: Debug =.false.
      character(len=180) :: line
      integer            :: i,j

      !> Init
      n_Ini=0; n_End=0

      !> Determine the zone of commands in the file
      do i=1,ffile%nlines
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)

         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         if (n_Ini == 0) then
            j=index(u_case(line),'COMMA')
            if (j > 0) then
               n_ini=i+1
               cycle
            end if
         end if

         if (n_ini > 0 .and. i >= n_ini) then
            j=index(u_case(line),'END CO')
            if (j > 0) then
               n_End=i-1
               exit
            end if
         end if
      end do

      !> Debug Info
      if (debug) then
         if (n_ini ==0 .or. n_end ==0) then
            write(unit=*,fmt="(/,a)") " => Don't found the zone for COMMANDS!"
         end if
      end if

   End Subroutine Get_ZoneCommands

  !!----
  !!---- SUBROUTINE GET_BLOCK_KEY
  !!----
  !!----
  !!---- Update: 12/05/2022
  !!
  Subroutine Get_Block_KEY(Key,ffile, N_Ini, N_End, Nkey, IndLines)
     !---- Arguments ----!
     character(len=*),        intent(in)  :: Key
     Type(file_type),         intent(in)  :: ffile
     integer,                 intent(in)  :: n_ini
     integer,                 intent(in)  :: n_end
     integer,                 intent(out) :: Nkey
     integer, dimension(:,:), intent(out) :: IndLines   ! dim(2,Npatt)

     !---- Local Arguments ----!
     logical                          :: Debug=.true.
     integer                          :: i,j,k,n,nc,iv,kmax
     character(len=3)                 :: car
     character(len=132)               :: line
     character(len=60), dimension(5)  :: dire
     real, dimension(5)               :: vet
     integer, dimension(5)            :: ivet

     !> Init
     car=u_case(key)

     NKey=0
     IndLines=0
     kmax=0

     i=N_ini
     do while(i <=N_end)
        line=adjustl(ffile%line(i)%str)

        if (line(1:1) =='!') then
           i=i+1
           cycle
        end if
        if (line(1:1) ==' ') then
           i=i+1
           cycle
        end if
        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)

        j=index(u_case(line),'%'//car)
        if (j <=0) then
           i=i+1
           cycle
        end if

        call cut_string(line)  ! Cut %PAT..

        k=0
        if (len_trim(line) ==0) then
           k=1
        else
           call get_words(line, dire, nc)
           call get_num(dire(1), vet, ivet, iv)
           if (iv < 1) then
              call set_error(-1, " You have to give the number indentification in Block definition")
              return
           end if
           k=ivet(1)
        end if
        kmax=max(kmax,k)

        do n=i+1,n_end
           line=adjustl(ffile%line(n)%str)
           if (line(1:1) =='!') cycle
           if (line(1:1) ==' ') cycle

           j=index(u_case(line),'%END'//car)
           if (j <= 0) cycle

           IndLines(1,k)=i+1
           IndLines(2,k)=n-1

           i=n
           exit
        end do
        i=i+1
     end do

     do i=1, kmax
        if (IndLines(1,i) == 0) cycle

        if (IndLines(1,i) > 0 .and. Indlines(2,i) ==0) then
           call set_error(1,"Error in Block definition!")
           NKey=0
           return

        else if (IndLines(1,i) > IndLines(2,i) ) then
           !> Empty block
           IndLines(1,i) =0
           IndLines(2,i) =0

        else
           NKey=NKey+1
        end if
     end do

     !> Debug
     if (debug .and. nkey > 0) then
        print*,'Number of Blocks readed: ',NKey
        do i=1,kmax
           if (IndLines(1,i)==0) cycle
           print*,'Key: '//car, i, 'Ini: ',IndLines(1,i), 'End: ',IndLines(2,i)
        end do
        print*,' '
     end if

  End Subroutine Get_Block_Key



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
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               print*,' ==> FIX Directive: '//trim(line)
               call ReadCode_FIX_ATM(line, AtList, Spg)
               if (err_CFML%IErr /=0) then
                  print*,err_CFML%Msg
               end if

            case ("VARY")    ! VARY
               print*,' ==> VARY Directive: '//trim(line)
               call ReadCode_VARY_ATM(line, AtList, Spg)
               if (err_CFML%IErr /=0) then
                  print*,err_CFML%Msg
               end if

            case ("EQUA") ! Equal (Constraints)
               print*,' ==> EQUA Directive: '//trim(line)
               call ReadCode_EQUAL_ATM(line, AtList, Spg)
               if (err_CFML%IErr /=0) then
                  print*,err_CFML%Msg
               end if

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
   !!---- ReadCode_EQUAL_ATM
   !!----
   !!---- Update: April - 2022
   !!
   Subroutine ReadCode_EQUAL_ATM(String, AtList, Spg)
      !---- Arguments ----!
      character(len=*),   intent(in)     :: String
      type(AtList_Type),  intent(in out) :: AtList
      class (SpG_type),   intent(in)     :: Spg

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=3)                      :: car
      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: npos, nlong, n_dir, n_loc, nc
      integer                               :: ii,j,jj,k,iv,na, nb
      integer, dimension(NMAX_GEN)          :: Ind_dir, Ind_dir2, IPh_dir, Iph_loc
      real                                  :: fac
      real, dimension(3)                    :: Bounds
      logical                               :: done, with_val

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'EQU') then
         call set_error(1,'Wrong Directive for EQUAL instruction: '//trim(line))
         return
      end if

      !> Cut EQUAL word
      call cut_string(line,nlong)

      !> general directives
      call split_genrefcod_atm(line, n_dir, Ind_dir, IPh_dir, dir_gen)

      !> Locals  directives
      call Split_LocRefCod_ATM(line, n_loc, dir_loc, Ind_dir2, Iph_loc, dir_lab)

      if (n_dir > 0 .and. n_loc > 0) then
         call set_error(1,'Wrong form for EQUAL: '//trim(line))
         return
      end if

      bounds = [0.0, 1.0, 0.1]
      if (n_dir > 0) then
         call get_words(line,dire,nc)

         do j=1,n_dir
            done=.false.

            !> First Atom
            k=n_dir+1
            na=Index_AtLab_on_AtList(dire(k),iph_dir(j),Atlist)

            if (na > 0) then
               !> First reference is an atom label

               do while( k < nc)
                  !> Second Atom
                  k=k+1
                  nb=Index_AtLab_on_AtList(dire(k),iph_dir(j),Atlist)

                  !> The next is an multiplier?
                  with_val=.false.
                  fac=1.0_cp
                  if (k+1 <= nc) then
                     call get_num(dire(k+1),vet,ivet,iv)
                     if (iv == 1) then
                        fac=vet(1)
                        with_val=.true.
                     end if
                  end if

                  done=.true.
                  if (nb > 0) then
                     !> Second reference is an atom label
                     select case (ind_dir(j))
                        case (1:3) ! X,Y,Z
                           if (with_val) then
                              call equal_xyz_atm(Atlist, na,nb,ind_dir(j),fac)
                           else
                              call equal_xyz_atm(Atlist, na,nb,ind_dir(j))
                           end if

                        case (4)   ! XYZ
                           if (with_val) then
                              call equal_xyz_atm(Atlist, na,nb,0,fac)
                           else
                              call equal_xyz_atm(Atlist, na,nb,0)
                           end if

                        case (5)   ! OCC
                           if (with_val) then
                              call equal_occ_atm(Atlist, na,nb,fac)
                           else
                              call equal_occ_atm(Atlist, na,nb)
                           end if

                        case (6)   ! Uiso
                           if (with_val) then
                              call equal_u_atm(Atlist, na,nb,0,Spg,fac)
                           else
                              call equal_u_atm(Atlist, na,nb,0,Spg)
                           end if
                        case (7)   ! All U's
                           if (with_val) then
                              call equal_u_atm(Atlist, na,nb,-1,Spg,fac)
                           else
                              call equal_u_atm(Atlist, na,nb,-1,Spg)
                           end if

                        case (8:13)! Uij
                           if (with_val) then
                              call equal_u_atm(Atlist, na,nb,ind_dir(j)-7,Spg,fac)
                           else
                              call equal_u_atm(Atlist, na,nb,ind_dir(j)-7,Spg)
                           end if

                        case (14)  ! ALL
                           if (with_val) then
                              call equal_xyz_atm(Atlist, na,nb,0,fac)
                              call equal_occ_atm(Atlist, na,nb,fac)
                              call equal_u_atm(Atlist, na,nb,-1,Spg,fac)
                           else
                              call equal_xyz_atm(Atlist, na,nb,0)
                              call equal_occ_atm(Atlist, na,nb)
                              call equal_u_atm(Atlist, na,nb,-1,Spg)
                           end if

                        case default
                           done=.false.
                     end select

                  else
                     done=.false.

                     !> Chemical Specie in second reference
                     do ii=1,AtList%Natoms
                        if (iph_dir(j) > 0) then
                           if (atList%iph(ii) /= iph_dir(j)) cycle
                        end if
                        if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle

                        !>Don't give the same chemical species !!
                        if (trim(u_case(Atlist%atom(na)%ChemSymb)) == trim(u_case(AtList%atom(ii)%ChemSymb))) then
                           call set_error(1, 'Both references have the same chemical symbols: '//trim(dire(n_dir+1)) &
                                             //'  '//trim(dire(k)) )
                           return
                        end if

                        done=.true.
                        select case (ind_dir(j))
                           case (1:3) ! X,Y,Z
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, na,ii,ind_dir(j),fac)
                              else
                                 call equal_xyz_atm(Atlist, na,ii,ind_dir(j))
                              end if

                           case (4)   ! XYZ
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, na,ii,0,fac)
                              else
                                 call equal_xyz_atm(Atlist, na,ii,0)
                              end if

                           case (5)   ! OCC
                              if (with_val) then
                                 call equal_occ_atm(Atlist, na,ii,fac)
                              else
                                 call equal_occ_atm(Atlist, na,ii)
                              end if

                           case (6)   ! Uiso
                              if (with_val) then
                                 call equal_u_atm(Atlist, na,ii,0,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, na,ii,0,Spg)
                              end if
                           case (7)   ! All U's
                              if (with_val) then
                                 call equal_u_atm(Atlist, na,ii,-1,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, na,ii,-1,Spg)
                              end if

                           case (8:13)! Uij
                              if (with_val) then
                                 call equal_u_atm(Atlist, na,ii,ind_dir(j)-7,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, na,ii,ind_dir(j)-7,Spg)
                              end if

                           case (14)  ! ALL
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, na,ii,0,fac)
                                 call equal_occ_atm(Atlist, na,ii,fac)
                                 call equal_u_atm(Atlist, na,ii,-1,Spg,fac)
                              else
                                 call equal_xyz_atm(Atlist, na,ii,0)
                                 call equal_occ_atm(Atlist, na,ii)
                                 call equal_u_atm(Atlist, na,ii,-1,Spg)
                              end if
                           case default
                              done=.false.
                        end select

                     end do
                  end if ! nb

                  if (.not. done) then
                     call set_error(1,'Not found the Atom Reference: '//trim(dire(n_dir+1))//'  '//trim(dire(k)) )
                     return
                  end if

                  if (with_val) k=k+1

               end do

            else
               !> First reference is a Chemical symbol
               done=.false.
               do ii=1,AtList%Natoms
                  if (iph_dir(j) > 0) then
                     if (atList%iph(ii) /= iph_dir(j)) cycle
                  end if
                  if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle

                  do while( k < nc)
                     !> Second Atom
                     k=k+1
                     nb=Index_AtLab_on_AtList(dire(k),iph_dir(j),Atlist)

                     !> The next is an multiplier?
                     with_val=.false.
                     fac=1.0_cp
                     if (k+1 <= nc) then
                        call get_num(dire(k+1),vet,ivet,iv)
                        if (iv == 1) then
                           fac=vet(1)
                           with_val=.true.
                        end if
                     end if

                     if (nb > 0) then
                        done=.true.
                        !> Second reference is an atom label
                        select case (ind_dir(j))
                           case (1:3) ! X,Y,Z
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, ii,nb,ind_dir(j),fac)
                              else
                                 call equal_xyz_atm(Atlist, ii,nb,ind_dir(j))
                              end if

                           case (4)   ! XYZ
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, ii,nb,0,fac)
                              else
                                 call equal_xyz_atm(Atlist, ii,nb,0)
                              end if

                           case (5)   ! OCC
                              if (with_val) then
                                 call equal_occ_atm(Atlist, ii,nb,fac)
                              else
                                 call equal_occ_atm(Atlist, ii,nb)
                              end if

                           case (6)   ! Uiso
                              if (with_val) then
                                 call equal_u_atm(Atlist, ii,nb,0,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, ii,nb,0,Spg)
                              end if

                           case (7)   ! All U's
                              if (with_val) then
                                 call equal_u_atm(Atlist, ii,nb,-1,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, ii,nb,-1,Spg)
                              end if

                           case (8:13)! Uij
                              if (with_val) then
                                 call equal_u_atm(Atlist, ii,nb,ind_dir(j)-7,Spg,fac)
                              else
                                 call equal_u_atm(Atlist, ii,nb,ind_dir(j)-7,Spg)
                              end if

                           case (14)  ! ALL
                              if (with_val) then
                                 call equal_xyz_atm(Atlist, ii,nb,0,fac)
                                 call equal_occ_atm(Atlist, ii,nb,fac)
                                 call equal_u_atm(Atlist, ii,nb,-1,Spg,fac)
                              else
                                 call equal_xyz_atm(Atlist, ii,nb,0)
                                 call equal_occ_atm(Atlist, ii,nb)
                                 call equal_u_atm(Atlist, ii,nb,-1,Spg)
                              end if

                           case default
                              done=.false.
                        end select

                     else
                        !> Second reference is a chemical symbol
                        done=.false.
                        do jj=1,AtList%Natoms
                           if (iph_dir(j) > 0) then
                              if (atList%iph(jj) /= iph_dir(j)) cycle
                           end if
                           if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(jj)%ChemSymb))) cycle

                           !> Don't give the same chemical species
                           if (trim(u_case(Atlist%atom(ii)%ChemSymb)) == trim(u_case(AtList%atom(jj)%ChemSymb))) then
                              call set_error(1, 'Both references have the same chemical symbols: '//trim(line) )
                              return
                           end if

                           done=.true.
                           select case (ind_dir(j))
                              case (1:3) ! X,Y,Z
                                 if (with_val) then
                                    call equal_xyz_atm(Atlist, ii,jj,ind_dir(j),fac)
                                 else
                                    call equal_xyz_atm(Atlist, ii,jj,ind_dir(j))
                                 end if

                              case (4)   ! XYZ
                                 if (with_val) then
                                    call equal_xyz_atm(Atlist, ii,jj,0,fac)
                                 else
                                    call equal_xyz_atm(Atlist, ii,jj,0)
                                 end if

                              case (5)   ! OCC
                                 if (with_val) then
                                    call equal_occ_atm(Atlist, ii,jj,fac)
                                 else
                                    call equal_occ_atm(Atlist, ii,jj)
                                 end if

                              case (6)   ! Uiso
                                 if (with_val) then
                                    call equal_u_atm(Atlist, ii,jj,0,Spg,fac)
                                 else
                                    call equal_u_atm(Atlist, ii,jj,0,Spg)
                                 end if
                              case (7)   ! All U's
                                 if (with_val) then
                                    call equal_u_atm(Atlist, ii,jj,-1,Spg,fac)
                                 else
                                    call equal_u_atm(Atlist, ii,jj,-1,Spg)
                                 end if

                              case (8:13)! Uij
                                 if (with_val) then
                                    call equal_u_atm(Atlist, ii,jj,ind_dir(j)-7,Spg,fac)
                                 else
                                    call equal_u_atm(Atlist, ii,jj,ind_dir(j)-7,Spg)
                                 end if

                              case (14)  ! ALL
                                 if (with_val) then
                                    call equal_xyz_atm(Atlist, ii,jj,0,fac)
                                    call equal_occ_atm(Atlist, ii,jj,fac)
                                    call equal_u_atm(Atlist, ii,jj,-1,Spg,fac)
                                 else
                                    call equal_xyz_atm(Atlist, ii,jj,0)
                                    call equal_occ_atm(Atlist, ii,jj)
                                    call equal_u_atm(Atlist, ii,jj,-1,Spg)
                                 end if

                              case default
                                 done=.false.
                           end select

                        end do
                     end if

                     if (.not. done) then
                        call set_error(1,'Not found the Atom Reference: '//trim(line) )
                        return
                     end if

                     if (with_val) k =k+1
                  end do
               end do

               if (.not. done) then
                  call set_error(1,'Not found the Atom label: '//trim(line) )
                  return
               end if
            end if

         end do ! ndir
      end if


      if (n_loc > 0) then
         j=1
         do while(j < n_loc)
            na=Index_AtLab_on_AtList(dir_lab(j), iph_loc(j), AtList)
            nb=Index_AtLab_on_AtList(dir_lab(j+1), iph_loc(j+1), AtList)

            !>Multiplier
            with_val=.false.
            if (j+2 <= n_loc) then
               call get_num(dir_lab(j+2), vet,ivet,iv)
               if (iv ==1) then
                  fac=vet(1)
                  with_val=.true.
               end if
            end if

            if (na > 0) then
               !> First reference is an atom label

               if (nb > 0) then
                  !> Second reference is an atom label

               else
                  !> Second reference is a chemical species

               end if

            else
               !> First reference is a chemical symbol

               if (nb > 0) then
                  !> Second reference is an atom label

               else
                  !> Second reference is a chemical species

               end if

            end if


            if (with_val) then
               j=j+3
            else
               j=j+2
            end if
         end do
      end if

   End Subroutine ReadCode_EQUAL_ATM

   !!--++
   !!--++ Subroutine Equal_XYZ_Atm
   !!--++
   !!--++    Equal Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Subroutine EQUAL_XYZ_Atm(Atlist, NAtm1, Natm2, Ind, Fac)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm1
      integer,                     intent(in)     :: NAtm2
      integer,                     intent(in)     :: Ind    ! 1:X, 2:Y, 3:Z, 0:XYZ
      real, optional,              intent(in)     :: Fac


      !---- Local Variables ----!
      integer :: i,nc
      integer :: n1,n2
      real    :: val0

      associate (A => AtList%atom(NAtm1), B=> AtList%atom(NAtm2) )
         select type (A)
            type is (Atm_Ref_Type)
               select type (B)
                  type is (Atm_Ref_type)
                     select case (Ind)
                        case (1:3)
                           val0=A%m_x(Ind)
                           n1=A%l_x(Ind)

                           n2=B%l_x(Ind)
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_x(ind)=fac
                           else
                              B%m_x(ind)=val0
                           end if
                           B%l_x(ind)=n1

                           NP_Constr=NP_Constr+1

                        case (0)
                           do i=1,3
                              val0=A%m_x(i)
                              n1=A%l_x(i)

                              n2=B%l_x(i)
                              call Del_RefCode_Atm(AtList, n2)
                              if (present(Fac)) then
                                 B%m_x(i)=fac
                              else
                                 B%m_x(i)=val0
                              end if
                              B%l_x(i)=n1

                              NP_Constr=NP_Constr+1
                           end do

                     end select  ! Ind
               end select

            type is (Matm_Ref_Type)
               select type (B)
                  type is (Matm_Ref_Type)
                     select case (Ind)
                        case (1:3)
                           val0=A%m_x(Ind)
                           n1=A%l_x(Ind)

                           n2=B%l_x(Ind)
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_x(ind)=fac
                           else
                              B%m_x(ind)=val0
                           end if
                           B%l_x(ind)=n1

                           NP_Constr=NP_Constr+1

                        case (0)
                           do i=1,3
                              val0=A%m_x(i)
                              n1=A%l_x(i)

                              n2=B%l_x(i)
                              call Del_RefCode_Atm(AtList, n2)
                              if (present(Fac)) then
                                 B%m_x(i)=fac
                              else
                                 B%m_x(i)=val0
                              end if
                              B%l_x(i)=n1

                              NP_Constr=NP_Constr+1
                           end do

                     end select
               end select
         end select ! A
      end associate

   End Subroutine EQUAL_XYZ_Atm

   !!--++
   !!--++ Subroutine Equal_OCC_Atm
   !!--++
   !!--++    Equal Occupancy Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Subroutine EQUAL_OCC_Atm(Atlist, NAtm1, Natm2, Fac)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm1
      integer,                     intent(in)     :: NAtm2
      real, optional,              intent(in)     :: Fac


      !---- Local Variables ----!
      integer :: i,nc
      integer :: n1,n2
      real    :: val0

      associate (A => AtList%atom(NAtm1), B=> AtList%atom(NAtm2) )
         select type (A)
            type is (Atm_Ref_Type)
               select type (B)
                  type is (Atm_ref_Type)
                     val0=A%m_occ
                     n1=A%l_occ

                     n2=B%l_occ
                     call Del_RefCode_Atm(AtList, n2)
                     if (present(Fac)) then
                        B%m_occ=fac
                     else
                        B%m_occ=val0
                     end if
                     B%l_occ=n1
               end select

               NP_Constr=NP_Constr+1

            type is (Matm_Ref_Type)
               select type (B)
                  type is (Matm_ref_Type)
                     val0=A%m_occ
                     n1=A%l_occ

                     n2=B%l_occ
                     call Del_RefCode_Atm(AtList, n2)
                     if (present(Fac)) then
                        B%m_occ=fac
                     else
                        B%m_occ=val0
                     end if
                     B%l_occ=n1
               end select

               NP_Constr=NP_Constr+1
         end select ! A

      end associate

   End Subroutine EQUAL_OCC_Atm

   !!--++
   !!--++ Subroutine Equal_U_Atm
   !!--++
   !!--++    Equal Thermal Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Subroutine EQUAL_U_Atm(Atlist, NAtm1, Natm2, Ind, Spg, Fac)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm1
      integer,                     intent(in)     :: NAtm2
      integer,                     intent(in)     :: Ind    ! 0:Usio, 1-6:Uij, -1: All
      class(SpG_Type),             intent(in)     :: Spg
      real, optional,              intent(in)     :: Fac


      !---- Local Variables ----!
      integer :: i,nc
      integer :: n1,n2
      real    :: val0

      associate (A => AtList%atom(NAtm1), B=> AtList%atom(NAtm2) )
         select type (A)
            type is (Atm_Ref_Type)
               select type (b)
                  type is (Atm_Ref_Type)
                     select case (Ind)
                        case (0) ! Uiso
                           val0=A%m_U_iso
                           n1=A%l_U_iso

                           n2=B%l_U_iso
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_U_iso=fac
                           else
                              B%m_U_iso=val0
                           end if
                           B%l_U_iso=n1

                           NP_Constr=NP_Constr+1

                        case (1:6) ! Uij
                           val0=A%m_U(Ind)
                           n1=A%l_U(Ind)

                           n2=B%l_U(Ind)
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_U(ind)=fac
                           else
                              B%m_U(ind)=val0
                           end if
                           B%l_U(ind)=n1
                           !call Get_AtomBet_CTR(B%x,B%u,Spg, NP_Ref, A%L_u, A%m_u)

                           NP_Constr=NP_Constr+1

                        case (-1) ! All U's
                           do i=1,6
                              val0=A%m_U(i)
                              n1=A%l_U(i)

                              n2=B%l_U(i)
                              call Del_RefCode_Atm(AtList, n2)
                              if (present(Fac)) then
                                 B%m_U(i)=fac
                              else
                                 B%m_U(i)=val0
                              end if
                              B%l_U(i)=n1
                              !call Get_AtomBet_CTR(B%x,B%u,Spg, NP_Ref, A%L_u, A%m_u)

                              NP_Constr=NP_Constr+1
                           end do
                     end select  ! Ind
               end select

            type is (Matm_Ref_Type)
               select type (B)
                  type is (Matm_Ref_Type)
                     select case (Ind)
                        case (0) ! Uiso
                           val0=A%m_U_iso
                           n1=A%l_U_iso

                           n2=B%l_U_iso
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_U_iso=fac
                           else
                              B%m_U_iso=val0
                           end if
                           B%l_U_iso=n1

                           NP_Constr=NP_Constr+1

                        case (1:6) ! Uij
                           val0=A%m_U(Ind)
                           n1=A%l_U(Ind)

                           n2=B%l_U(Ind)
                           call Del_RefCode_Atm(AtList, n2)
                           if (present(Fac)) then
                              B%m_U(ind)=fac
                           else
                              B%m_U(ind)=val0
                           end if
                           B%l_U(ind)=n1
                           !call Get_AtomBet_CTR(B%x,B%u,Spg, NP_Ref, A%L_u, A%m_u)

                           NP_Constr=NP_Constr+1

                        case (-1) ! All
                           do i=1,6
                              val0=A%m_U(i)
                              n1=A%l_U(i)

                              n2=B%l_U(i)
                              call Del_RefCode_Atm(AtList, n2)
                              if (present(Fac)) then
                                 B%m_U(i)=fac
                              else
                                 B%m_U(i)=val0
                              end if
                              B%l_U(i)=n1
                              !call Get_AtomBet_CTR(B%x,B%u,Spg, NP_Ref, A%L_u, A%m_u)

                              NP_Constr=NP_Constr+1
                           end do
                     end select  ! Ind
               end select
         end select ! A
      end associate

   End Subroutine EQUAL_U_Atm

End Program KeyCodes

