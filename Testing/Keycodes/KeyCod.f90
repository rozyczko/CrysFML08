!!----
!!---- KeyCodes Program (For Testing purposes)
!!----
!!----
Program KeyCodes
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error, set_error
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String, Get_Words, &
                                Get_Num, Reading_File
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_IOForm,       only: Read_Xtal_Structure, Read_CFL_Cell
   use CFML_Metrics,      only: Cell_GLS_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_type, Atm_Std_Type, Atm_Ref_Type, &
                                Matm_Std_Type, Matm_Ref_type, Write_Atom_List, &
                                Index_AtLab_on_AtList, Change_AtomList_Type
   use CFML_Molecules,    only: Molecule_type

   use CFML_KeyCodes

   !---- Variables ----!
   Implicit None

   class(SPG_Type), allocatable     :: SpGr
   type(File_type)                  :: ffile
   type(Cell_GLS_Type),dimension(2) :: Cell    ! 2 Phase
   type(AtList_Type)                :: At
   type(molecule_type)              :: Mol

   type(RelationList_Type)          :: RPat     ! Only 1 Pattern
   type(RelationList_Type)          :: RPhas    ! Only 1 Phase
   type(RelationList_Type)          :: RMol     ! Only 1 Molec


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

   integer                         :: NB_Atm, NB_Patt, NB_Phas, NB_Mol
   integer, dimension(2,NB_Max)    :: IB_Atm, IB_Patt, IB_PHas, IB_MOL

   integer                         :: Npar,ip,im,icyc


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
      ffile=reading_file(trim(filcod)//'.cfl')

      !call Read_Xtal_Structure(trim(filcod)//'.cfl', Cell, SpGr, At, FType=ffile)

      if (Err_CFML%Ierr /=0) then
         write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
         write(unit=*,fmt="(/,a)") " => PROGRAM KEYCODES finished in error!"
         stop
      end if

      !> Write Information
      open(newunit=lun,file=trim(filcod)//".log", status="replace",action="write")

      !call Write_SpaceGroup_Info(SpGr,lun)
      !write(unit=lun,fmt="(a,/)") " "

      !call Write_Atom_List(At,Iunit=lun)

      !> -------------
      !> Testing Zone
      !> -------------

      !> Allocating Relation List for Non atomic parameters
      call Allocate_RelationList(50,RPat)   ! 18 Parameters for Pattern
      call Allocate_RelationList(50,RPhas)  ! 6  Parameters for Phase
      call Allocate_RelationList(50,RMol)   ! 6  Parameters for Phase

      !> Doing space for Refinement vectors
      !> add parameters according to the number of atoms in each phase
      Npar=18+ 6+ 11*20   ! At the moment only 1 Phase
      call Allocate_VecRef(Npar)

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

         !> Patterns
         if (NB_Patt > 0) then
            icyc=0
            do ip=1,size(IB_Patt,dim=2)
               if (IB_Patt(1,ip) ==0) cycle

               icyc=icyc+1
               call Read_RefCodes_PATT(ffile, IB_Patt(1,ip),IB_Patt(2,ip), Ip, RPat)
               if (icyc == NB_Patt) exit
            end do
         end if

         !> Phases
         if (NB_Phas > 0) then
            icyc=0
            do ip=1,size(IB_Phas,dim=2)
               if (IB_Phas(1,ip) ==0) cycle

               icyc=icyc+1
               call read_cfl_cell(ffile, cell(icyc),i_ini=IB_Phas(1,ip),i_end=IB_Phas(2,ip))
               call Write_Crystal_Cell(Cell(icyc),lun)
               write(unit=lun,fmt="(a,/)") " "

               call Read_RefCodes_PHAS(ffile, IB_Phas(1,ip),IB_Phas(2,ip), Ip, RPhas)
               call RList_to_Cell(RPhas, ip, Cell(icyc))

               if (icyc == NB_Phas) exit
            end do
         end if

         !> Molec
         if (NB_Mol > 0) then
            icyc=0
            do im=1,size(IB_Mol,dim=2)
               if (IB_Mol(1,im) ==0) cycle

               icyc=icyc+1
               call Read_RefCodes_MOL(ffile, IB_Mol(1,im),IB_Mol(2,im), Im, RMol)
               call RList_to_Molec(RMol, im, Mol)

               if (icyc == NB_Mol) exit
            end do
         end if

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



End Program KeyCodes

