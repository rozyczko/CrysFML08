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

   class(SPG_Type), allocatable   :: SpGr
   type(File_type)                :: ffile
   type(Cell_G_Type)              :: Cell    ! Only 1 Phase
   type(AtList_Type)              :: At
   type(RelationList_Type)        :: Pat     ! Only 1 Pattern
   type(RelationList_Type)        :: Phas    ! Only 1 Phase


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

   integer, parameter              :: NB_Max=10

   integer                         :: NB_Atm, NB_Patt, NB_Phas, NB_Mol, NB_RGB
   integer, dimension(2,NB_Max)    :: IB_Atm, IB_Patt, IB_PHas, IB_MOL, IB_RGB

   integer                         :: Npar,ip,icyc


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

      !> Allocating Relation List for Non atomic parameters
      call Allocate_RelationList(50,Pat)  ! 18 Parameters for Pattern
      call Allocate_RelationList(50,Phas)  ! 6  Parameters for Phase

      !> Doing space for Refinement vectors
      !> add parameters according to the number of atoms in each phase
      Npar=18+ 6+ 11*At%natoms   ! At the moment only 1 Phase
      call Allocate_VecRef(Npar)

      !> Set Parameters Model
      !call set_model_Pattern(Pat) ! Define Name parameters and NPar
      !call set_model_Phase(Phas)

      !> Determine the command zone
      call get_zonecommands(ffile,nc_i,nc_f)

      if (nc_i > 0 .and. nc_i < nc_f) then

         !> ==== Blocks ====
         !> NB... is the number of blocks in the command zone
         !> IB... star/end for each Phase/Pattern/....
         call Get_Block_Key('Pattern', ffile, nc_i, nc_f, NB_Patt, IB_Patt)
         call Get_Block_Key('Phase',   ffile, nc_i, nc_f, NB_Phas, IB_Phas)
         call Get_Block_Key('Atoms',   ffile, nc_i, nc_f, NB_Atm,  IB_Atm)
         call Get_Block_Key('Molec',   ffile, nc_i, nc_f, NB_Mol, IB_Mol)
         call Get_Block_Key('RGB',     ffile, nc_i, nc_f, NB_RGB, IB_RGB)

         !> Patterns
         if (NB_Patt > 0) then
            icyc=0
            do ip=1,size(IB_Patt,dim=2)
               if (IB_Patt(1,ip) ==0) cycle

               icyc=icyc+1
               call Read_RefCodes_PATT(ffile, IB_Patt(1,ip),IB_Patt(2,ip), Ip, Pat)
               if (icyc == NB_Patt) exit
            end do
         end if

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

   !!============================================================================================================!!
   !!============================================================================================================!!
   !!============================================================================================================!!
   !!============================================================================================================!!
   !!============================================================================================================!!
   !!============================================================================================================!!

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

