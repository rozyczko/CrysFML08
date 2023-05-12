!!----
!!---- KeyCodes Program (For Testing purposes)
!!----
!!----
Program KeyCodes
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error, set_error
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String, Get_Words, &
                                Get_Num, Reading_File, l_case
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_IOForm,       only: Read_Xtal_Structure, Read_CFL_Cell
   use CFML_Metrics,      only: Cell_GLS_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_type, Atm_Std_Type, Atm_Ref_Type, &
                                ModAtm_Std_Type, ModAtm_Ref_type, Write_Atom_List, &
                                Index_AtLab_on_AtList, Change_AtomList_Type
   use CFML_Molecules,    only: Molecule_type

   use CFML_KeyCodes

   !---- Variables from CFML ----!
   implicit none

   logical                          :: Debug=.true.

   logical                          :: ZoneCommand=.false.
   integer, dimension(2)            :: ZComm =0

   integer, parameter               :: NMAX_PHAS =10
   integer, parameter               :: NMAX_PATT =10
   integer, parameter               :: NMAX_MOLE =10
   integer, parameter               :: NMAX_ATLIS=10

   type(File_type)                  :: Ffile

    type :: Phase_Type
       character(len=:),      allocatable :: phas_name
       logical, dimension(:), allocatable :: patt_contrib ! Contribution to patterns
       type(Cell_GLS_Type)                :: Cell         ! Cell object
       class(SpG_Type),       allocatable :: SpG          ! Space Group object
       type(Atlist_Type)                  :: Atm          ! Atom List object
    end type Phase_Type

    type, extends(Phase_Type) :: PowderPhase_Type
       real(kind=cp) :: iso_size, Gauss_iso_size_frac
       real(kind=cp) :: miso_size, mGauss_iso_size_frac
       integer       :: Liso_size, LGauss_iso_size_frac
       real(kind=cp) :: iso_strain, Lorent_iso_strain_frac
       real(kind=cp) :: miso_strain, mLorent_iso_strain_frac
       integer       :: Liso_strain, LLorent_iso_strain_frac
       integer       :: aniso_size_model
       integer       :: aniso_strain_model
       real(kind=cp), dimension(:),  allocatable :: aniso_size, aniso_strain
       real(kind=cp), dimension(:),  allocatable :: maniso_size, maniso_strain
       integer,       dimension(:),  allocatable :: Laniso_size, Laniso_strain
    end type PowderPhase_Type

    Type, extends(Phase_Type) :: MolPhase_Type
       integer                                              :: N_Mol=0          ! Number of Molecules
       integer                                              :: N_Species=0      ! Number of species
       type(Molecule_type),     allocatable, dimension(  :) :: Mol              ! Molecules
    End type MolPhase_Type

    Type, extends(PowderPhase_Type) :: PowderMolPhase_Type
       integer                                              :: N_Mol=0          ! Number of Molecules
       integer                                              :: N_Species=0      ! Number of species
       type(Molecule_type),     allocatable, dimension(  :) :: Mol              ! Molecules
    End type PowderMolPhase_Type

    type(Phase_Type), dimension(:), allocatable :: Ph

   !class(SPG_Type),     dimension(:), allocatable  :: SpGr
   !type(Cell_GLS_Type), dimension(NMAX_PHAS)  :: Cell
   !type(AtList_Type),   dimension(NMAX_ATLIS) :: At
   !type(molecule_type), dimension(NMAX_MOLE)  :: Mol

   type(GenParList_Type)          :: RelG

   type(GenParList_Type)          :: RPat
   type(GenParList_Type)          :: RPhas
   type(GenParList_Type)          :: RMol


   !---- Variables ----!
   logical                         :: arg_given=.false.
   logical                         :: existe=.false.

   character(len=256)              :: filcod
   character(len=2)                :: ans
   character(len=40)               :: str

   integer                         :: i, j, k, narg, lun, nphases, npatterns
   integer                         :: n_ini, n_end

   real(kind=cp)                   :: T_ini,T_fin, T_Time

   character(len=150)              :: line
   character(len=3)                :: ktype
   character(len=40), dimension(10):: dire
   integer, dimension(10)          :: ivet
   real(kind=cp), dimension(10)    :: vet



   integer                         :: Npar,ip,im,icyc

   !---- Types ----!
   Type :: Bl_Type
      character(len=40)     :: Name=" "
      character(len=10)     :: Type=" "  ! Phase, Pattern, Molecule, ....
      integer               :: IType=-1  !    0      1        2
      integer, dimension(2) :: Line=0    ! start, end
   End Type Bl_Type

   Type :: Block_Info_Type
      type(Bl_type)                            :: Block
      integer                                  :: N_SBlocks
      type(Bl_type), dimension(:), allocatable :: SBlock
   End Type Block_Info_Type

   integer, parameter                     :: NB_MAX=50
   integer                                :: NB_Phas, NB_Patt, NB_Mol, NB_Atm, NB_Tot
   integer, dimension(2,NB_MAX)           :: Ind
   integer, dimension(2,NMAX_PATT)        :: ib_Patt
   integer, dimension(2,NMAX_MOLE)        :: Ib_Mol
   integer, dimension(2,NMAX_ATLIS)       :: Ib_Atm
   integer, dimension(2,NMAX_PHAS)        :: Ib_Phas
   character(len=40), dimension(4,NB_MAX) :: ID_Str

   type(Block_Info_Type), dimension(NB_MAX) :: Block_Phases
   type(Block_Info_Type), dimension(NB_MAX) :: Block_Patterns
   type(Block_Info_Type), dimension(NB_MAX) :: Block_Molec
   type(Block_Info_Type), dimension(NB_MAX) :: Block_Atm


   !> Init
   T_Time=0.0

   !> Arguments on the command line
   narg=Command_Argument_Count()
   if (narg > 0) then
      call Get_Command_Argument(1, filcod)
      i=index(filcod,'.cfl')
      if (i /= 0) filcod=filcod(1:i-1)
      arg_given=.true.
   end if

   write(unit=*,fmt="(/,/,8(a,/))")                                                           &
              "                      =============================="                        , &
              "                      ====== PROGRAM KEYCODES ======"                        , &
              "                      =============================="                        , &
              "    ***********************************************************************" , &
              "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
              "    *                            Reading only .cfl                        *" , &
              "    ***********************************************************************" , &
              "                     (version: May 2023 JGP+NAK+JRC)"
   write(unit=*,fmt=*) " "

   if (.not. arg_given) then
      write(unit=*,fmt="(a)") " => Code of the file xxx.cfl (give xxx): "
      read(unit=*,fmt="(a)") filcod
      if (len_trim(filcod) == 0) then
         write(unit=*,fmt="(a)",advance="no") " => Please, press <cr> to finish the program"
         read(unit=*,fmt="(a)") ans
         stop
      end if

      i=index(filcod,".cfl")
      if (i /= 0) filcod=filcod(1:i-1)
   end if

   inquire(file=trim(filcod)//'.cfl',exist=existe)
   if (.not. existe) then
      write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl doesn't exist!"
      stop
   end if

   !> Initialize the clock
   call cpu_time(T_Ini)

   !> Read file CFL
   ffile=reading_file(trim(filcod)//'.cfl')
   if (Err_CFML%Ierr /=0) then
      write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
      write(unit=*,fmt="(/,a)") " => PROGRAM KEYCODES finished in error!"
      stop
   end if

   !> Determine the number of Phases
   nphases=0
   do i = 1 , ffile%nlines
      line = adjustl(ffile%line(i)%str)
      if (l_case(line(1:6)) == "phase_")  nphases = nphases + 1
   end do
   if (nphases == 0) then
      write(*,"(a)") ' No phase_ command found in the CFL file'
      write(*,"(a)") ' Assuming a single phase'
      nphases=1
   else
      write(*,'(a,i4)') 'Number of phases: ', nphases
   end if

   !> Determine the number of Patterns
   npatterns=0
   do i = 1 , ffile%nlines
      line = adjustl(ffile%line(i)%str)
      if (l_case(line(1:8)) == "pattern_")  npatterns = npatterns + 1
   end do
   if (npatterns == 0) then
      write(*,"(a)") ' No pattern_ command found in the CFL file'
   else
      write(*,'(a,i4)') 'Number of patterns: ', npatterns
   end if

   !> Determine a Command Zone
   call Get_ZoneCommands(ffile, N_Ini, N_End)
   if (n_ini==0 .or. n_end==0) then
      write(*,"(a)") ' No command zone was found in the CFL file'
   else
      write(*,'(a)') 'There is a Command zone'
   end if

   !   !> Create a Log File
   !   if (debug) then
   !      open(newunit=lun,file=trim(filcod)//".log", status="replace",action="write")
   !      write(unit=lun,fmt="(/,/,8(a,/))")                                                        &
   !           "                      =============================="                        , &
   !           "                      ====== PROGRAM KEYCODES ======"                        , &
   !           "                      =============================="                        , &
   !           "    ***********************************************************************" , &
   !           "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
   !           "    *                            Reading only .cfl                        *" , &
   !           "    ***********************************************************************" , &
   !           "                     (version: December 2022 JGP+NAK+JRC)"
   !   end if
   !
   !  ! Reading phases
   !  write(unit=lun,fmt='(a,1x,i2,1x,a)') ' => Reading',nphases,'phases'
   !  allocate(ph(nphases))
   !  !Setting the phases' name
   !  j=0
   !   do i = 1 , ffile%nlines
   !       line = adjustl(ffile%line(i)%str)
   !       if (l_case(line(1:6)) == "phase_")  then
   !           j = j + 1
   !           k=index(line," ")
   !           if(k >= 7) then
   !             ph(j)%Phas_name=line(7:k)
   !           else
   !             write(*,"(a,i3)")  ' => A name is compulsory for phase #',j
   !            stop
   !           end if
   !       end if
   !   end do
   !
   !  do i = 1 , nphases
   !      call Read_XTal_Structure(trim(filcod)//'.cfl',ph(i)%Cell,ph(i)%Spg,ph(i)%Atm,IPhase=i)
   !      if (Err_CFML%IErr == 1) then
   !          write(*,"(a,i3)")  ' => Error reading phase #',i
   !          stop
   !      end if
   !      write(unit=lun,fmt='(4x,a,1x,i2,1x,a)') 'Phase',i,'read'
   !  end do
   !
   !   !> Write Information about the read phases
   !  do i = 1 , nphases
   !      write(unit=lun,fmt='(/,a)')  '  ==========================='
   !      write(unit=lun,fmt='(a,i4)') '  Information about Phase',i
   !      write(unit=lun,fmt='(a,/)')  '  ==========================='
   !      call Write_SpaceGroup_Info(Ph(i)%SpG,lun)
   !      call Write_Atom_List(Ph(i)%atm,Iunit=lun)
   !  end do
   !
   !
   !   !> ----------------------------------
   !   !> ---- Testing Zone for commands----
   !   !> ----------------------------------
   !
   !   !> Command zone?
   !   zonecommand=.false.
   !   Zcomm=0
   !   call get_zonecommands(ffile, zcomm(1), zcomm(2))  ! Start/End zone
   !   if (any(zcomm > 0)) zonecommand=.true.
   !
   !   if (Debug) then
   !      write(unit=lun, fmt='(//,a)') ' ---- Command Zone ----'
   !      if (zonecommand) then
   !         write(unit=lun, fmt='(a, i5)') "   Start line: ", zcomm(1)
   !         write(unit=lun, fmt='(a, i5)') "   End   line: ", zcomm(2)
   !      else
   !         write(unit=lun, fmt='(a)') "   Command Zone doesnt'exist ! "
   !      end if
   !      write(unit=lun, fmt='(a)') ' ---- End Command Zone ----'
   !      write(unit=lun, fmt='(a)') ' '
   !   end if
   !
   !   !> Blocks zone
   !   NB_Tot=0
   !   NB_Patt=0; NB_Phas=0; NB_Atm=0; NB_Mol=0
   !
   !   if (zonecommand) then
   !      call Get_Block_Key('Phase',   ffile, zcomm(1), zcomm(2), NB_Phas, Ind, ID_str(1,:))
   !      do i=1,NB_Phas
   !         call Set_Block_Phase(Block_Phases, ID_str(1,i), ind(:,i),ffile)
   !      end do
   !      NB_Phas=0
   !      do i=1,size(Block_Phases)
   !         if (len_trim(Block_Phases(i)%Block%Name) >0) NB_Phas=NB_Phas+1
   !      end do
   !
   !
   !      call Get_Block_Key('Pattern', ffile, zcomm(1), zcomm(2), NB_Patt, ib_Patt, ID_str(2,:))
   !      call Get_Block_Key('Molec',   ffile, zcomm(1), zcomm(2), NB_Mol,  Ib_Mol,  ID_str(3,:))
   !      call Get_Block_Key('Atoms',   ffile, zcomm(1), zcomm(2), NB_Atm,  Ib_Atm,  ID_str(4,:))
   !
   !      NB_Tot=NB_Patt+NB_Phas+NB_Mol+NB_Atm
   !
   !      write(unit=lun, fmt='(a,i4)') 'Num. Total Blocks: ',NB_Tot
   !      write(unit=lun, fmt='(a,i4)') '     Phase Blocks: ',NB_Phas
   !      write(unit=lun, fmt='(a,i4)') '  Patterns Blocks: ',NB_Patt
   !      write(unit=lun, fmt='(a,i4)') ' Molecules Blocks: ',NB_Mol
   !      write(unit=lun, fmt='(a,i4)') '      AtomsBlocks: ',NB_Atm
   !      write(unit=lun, fmt='(a)') ' '
   !
   !      do i=1,NB_Phas
   !         call Set_Block_Phase(Block_Phases, ID_str(1,i), ind(:,i),ffile)
   !      end do
   !
   !
   !      NB_Tot=NB_Patt+NB_Phas+NB_Mol+NB_Atm
   !
   !      if (Debug) then
   !         write(unit=lun, fmt='(a)') ' --------------------'
   !         write(unit=lun, fmt='(a)') ' ---- Block Zone ----'
   !         write(unit=lun, fmt='(a)') ' --------------------'
   !         write(unit=lun, fmt='(a)') ' '
   !
   !         if (NB_Tot ==0 ) then
   !             write(unit=lun, fmt='(a)') "   Blocks dont'exist ! "
   !             write(unit=lun, fmt='(a)') ' '
   !         else
   !            if (NB_Phas > 0) then
   !               write(unit=lun, fmt='(a)') ' >>>> Phase Block Zone'
   !               write(unit=lun, fmt='(a)') ' '
   !               do i=1,NB_Phas
   !                  call WriteInfo_Block(Block_Phases(i), lun)
   !               end do
   !            end if
   !
   !            if (NB_Patt > 0) then
   !               write(unit=lun, fmt='(a)') ' ---- Pattern Block Zone ----'
   !               do i=1,NB_Patt
   !                  write(unit=lun, fmt='(a, i3)') "   Pattern: ", i
   !                  write(unit=lun, fmt='(a, i5)') "       Ini: ", ib_Patt(1,i)
   !                  write(unit=lun, fmt='(a, i5)') "       End: ", ib_Patt(2,i)
   !                  write(unit=lun, fmt='(a)') ' '
   !               end do
   !            end if
   !
   !
   !            if (NB_Mol > 0) then
   !               write(unit=lun, fmt='(a)') ' ---- Molecule Block Zone ----'
   !               do i=1,NB_Mol
   !                  write(unit=lun, fmt='(a, i3)') "  Molecule: ", i
   !                  write(unit=lun, fmt='(a, i5)') "       Ini: ", ib_Mol(1,i)
   !                  write(unit=lun, fmt='(a, i5)') "       End: ", ib_Mol(2,i)
   !                  write(unit=lun, fmt='(a)') ' '
   !               end do
   !            end if
   !
   !            if (NB_Atm > 0) then
   !               write(unit=lun, fmt='(a)') ' ---- Atom Block Zone ----'
   !               do i=1,NB_Atm
   !                  write(unit=lun, fmt='(a, i3)') "      Atom: ", i
   !                  write(unit=lun, fmt='(a, i5)') "       Ini: ", ib_Atm(1,i)
   !                  write(unit=lun, fmt='(a, i5)') "       End: ", ib_Atm(2,i)
   !                  write(unit=lun, fmt='(a)') ' '
   !               end do
   !            end if
   !
   !         end if
   !
   !         write(unit=lun, fmt='(a)') ' ---- End Block Zone ----'
   !      end if
   !
   !      !> Allocating Relations
   !      call Allocate_GenParList(100,RelG)
   !
   !      !> No Blocks => 1 Phase
   !
   !   end if  ! ZoneCommnads
   !
   !
   !
   !
   !   arg_given=.false.
   !
   !
   !   !> Allocating Relation List for Non atomic parameters
   !    call Allocate_GenParList(50,RPat)   ! 18 Parameters for Pattern
   !    call Allocate_GenParList(50,RPhas)  ! 6  Parameters for Phase
   !    call Allocate_GenParList(50,RMol)   ! 6  Parameters for Phase
   !
   !   !> Doing space for Refinement vectors
   !   !> add parameters according to the number of atoms in each phase
   !   Npar=18+ 6+ 11*20   ! At the moment only 1 Phase
   !   call Allocate_VecRef(Npar)
   !
   !
   !   !if (nc_i > 0 .and. nc_i < nc_f) then
   !
   !      !> ==== Blocks ====
   !      !> NB... is the number of blocks in the command zone
   !      !> IB... star/end for each Phase/Pattern/....
   !
   !      !> Patterns
   !      if (NB_Patt > 0) then
   !         icyc=0
   !         do ip=1,size(IB_Patt,dim=2)
   !            if (IB_Patt(1,ip) == 0) cycle
   !            icyc=icyc+1
   !            call Read_RefCodes_PATT(ffile, IB_Patt(1,ip),IB_Patt(2,ip), Ip, RPat)
   !            if (icyc == NB_Patt) exit
   !         end do
   !      end if
   !
   !      !> Phases
   !      if (NB_Phas > 0) then
   !         icyc=0
   !         do ip=1,size(IB_Phas,dim=2)
   !            if (IB_Phas(1,ip) == 0) cycle
   !
   !            icyc=icyc+1
   !            !call read_cfl_cell(ffile, Ph(icyc)%cell,i_ini=IB_Phas(1,ip),i_end=IB_Phas(2,ip))
   !            !call Write_Crystal_Cell(Cell(icyc),lun)
   !            write(unit=lun,fmt="(a,/)") " "
   !
   !            call Read_RefCodes_PHAS(ffile, IB_Phas(1,ip),IB_Phas(2,ip), Ip, RPhas)
   !            call GPList_to_Cell(RPhas, ip, Ph(icyc)%Cell)
   !
   !            if (icyc == NB_Phas) exit
   !         end do
   !      end if
   !
   !      !> Molec
   !      !if (NB_Mol > 0) then
   !      !   icyc=0
   !      !   do im=1,size(IB_Mol,dim=2)
   !      !      if (IB_Mol(1,im) ==0) cycle
   !      !
   !      !      icyc=icyc+1
   !      !      call Read_RefCodes_MOL(ffile, IB_Mol(1,im),IB_Mol(2,im), Im, RMol)
   !      !      call RList_to_Molec(RMol, im, Mol)
   !      !
   !      !      if (icyc == NB_Mol) exit
   !      !   end do
   !      !end if
   !
   !      !> Print info
   !      call WriteInfo_RefParams()
   !      call WriteInfo_RefParams(lun)
   !
   !      !call WriteInfo_Restraints(At)
   !      !call WriteInfo_Restraints(At, Iunit=lun)
   !   !end if
   !
   !   !> --------------------------
   !   !> ---- End Testing Zone ----
   !   !> --------------------------
   !   call cpu_time(T_fin)
   !   T_fin=T_fin-T_ini
   !   T_time=T_time + T_fin
   !
   !   T_ini=T_fin/60.0
   !   T_fin=int(T_ini)
   !   T_ini=(T_ini-T_fin)*60.0
   !
   !   write(unit=*,fmt='(a,i3,a,f8.4,a)')    " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
   !   T_ini=T_time/60.0
   !   T_fin=int(T_ini)
   !   T_ini=(T_ini-T_fin)*60.0
   !   write(unit=*,fmt='(a,i3,a,f8.4,a)')       " => TOTAL CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
   !   if (Debug) then
   !     write(unit=lun,fmt='(/,a)')            " => Normal End of: PROGRAM KEYCODES "
   !     write(unit=lun,fmt='(a,i3,a,f8.4,a)')  " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
   !     close(unit=lun)
   !   end if
   !!end do
   !
   !Contains
   !
   !!!============================================================================================================!!
   !!!============================================================================================================!!
   !!!============================================================================================================!!
   !!!============================================================================================================!!
   !!!============================================================================================================!!
   !!!============================================================================================================!!
   !Subroutine Set_Block_Phase(B, StrName, Ind, ffile)
   !   !---- Arguments ----!
   !   type(Block_Info_Type), dimension(:), intent(in out) :: B
   !   character(len=*),                    intent(in)     :: StrName
   !   integer, dimension(2),               intent(in)     :: ind
   !   type(File_type) ,                    intent(in)     :: ffile
   !
   !   !---- Local Variables ----!
   !   integer                  :: i,j,k,ii,ndim,nb
   !   integer, dimension(2,10) :: indc
   !   character(len=40)                :: Str1, Str2
   !   character(len=3)                 :: car
   !   character(len=40), dimension(10) :: IDStr
   !
   !   !> Init
   !   Str1 = u_case(adjustl(StrName))
   !
   !   ndim=size(B)
   !   do i=1,ndim
   !      str2=u_case(adjustl(B(i)%Block%Name))
   !      if (len_trim(str2) > 0) then
   !         if (trim(str1) == trim(str2)) return
   !      end if
   !   end do
   !
   !   do i=1,ndim
   !      str2=u_case(adjustl(B(i)%Block%Name))
   !      if (len_trim(str2) /= 0) cycle
   !
   !      B(i)%Block%name = adjustl(StrName)
   !      B(i)%Block%Type='Phase'
   !      B(i)%Block%IType=1
   !      B(i)%Block%line=Ind
   !
   !      exit
   !   end do
   !
   !   !> SubBlocks
   !   nb=0
   !   do j=B(i)%Block%line(1),B(i)%Block%line(2)
   !      car=adjustl(ffile%line(j)%str)
   !      if (car(1:1) =='!') cycle
   !      if (car(1:1) ==' ') cycle
   !
   !      if (index(car,'%') > 0) nb=nb+1
   !   end do
   !   if (mod(nb,2) /= 0) then
   !      print*,'Error in Number of Blocks!!!!'
   !      return
   !   end if
   !
   !   B(i)%N_SBlocks=nb/2
   !   if (allocated(B(i)%SBlock)) deallocate(B(i)%SBlock)
   !   if (B(i)%N_SBlocks > 0) allocate(B(i)%SBlock(nb))
   !
   !   !> Subblock Pattern
   !   call Get_Block_Key('Pattern', ffile, B(i)%Block%line(1), B(i)%Block%line(2), nb, Indc, IDstr)
   !   k=0
   !   if (nb > 0) then
   !      loop1: do j=1,nb
   !         if (k > 0) then
   !            do ii=1,k
   !               if (u_case(B(i)%SBlock(ii)%Name)==u_case(IDstr(j))) cycle loop1
   !            end do
   !         end if
   !
   !         k=k+1
   !         B(i)%SBlock(k)%Name=adjustl(IDStr(j))
   !         B(i)%SBlock(k)%Type='Pattern'
   !         B(i)%SBlock(k)%IType=2
   !         B(i)%SBlock(k)%line=Indc(:,j)
   !      end do loop1
   !   end if
   !
   !   !> Subblock Molec
   !   call Get_Block_Key('Molec', ffile, B(i)%Block%line(1), B(i)%Block%line(2), nb, Indc, IDstr)
   !   k=0
   !   if (nb > 0) then
   !      loop2: do j=1,nb
   !         if (k > 0) then
   !            do ii=1,k
   !               if (u_case(B(i)%SBlock(ii)%Name)==u_case(IDstr(j))) cycle loop2
   !            end do
   !         end if
   !
   !         k=k+1
   !         B(i)%SBlock(k)%Name=adjustl(IDStr(j))
   !         B(i)%SBlock(k)%Type='Molec'
   !         B(i)%SBlock(k)%IType=3
   !         B(i)%SBlock(k)%line=Indc(:,j)
   !      end do loop2
   !   end if
   !
   !   !> Subblock Atoms
   !   call Get_Block_Key('Atoms', ffile, B(i)%Block%line(1), B(i)%Block%line(2), nb, Indc, IDstr)
   !   k=0
   !   if (nb > 0) then
   !      loop3: do j=1,nb
   !         if (k > 0) then
   !            do ii=1,k
   !               if (u_case(B(i)%SBlock(ii)%Name)==u_case(IDstr(j))) cycle loop3
   !            end do
   !         end if
   !
   !         k=k+1
   !         B(i)%SBlock(k)%Name=adjustl(IDStr(j))
   !         B(i)%SBlock(k)%Type='Atoms'
   !         B(i)%SBlock(k)%IType=4
   !         B(i)%SBlock(k)%line=Indc(:,j)
   !      end do loop3
   !   end if
   !
   !End Subroutine
   !
   !
   !Subroutine WriteInfo_Block(B, lun)
   !   !---- Arguments ----!
   !   type(Block_Info_Type), intent(in) :: B
   !   integer, optional,     intent(in) :: lun
   !
   !   !---- Local Variables ----!
   !   integer :: io,k
   !
   !   io=6
   !   if (present(lun)) io=lun
   !
   !   write(unit=io, fmt='(a)') '  Block Name: '//trim(B%Block%Name)
   !   write(unit=io, fmt='(a)') '        Type: '//trim(B%Block%Type)
   !   write(unit=io, fmt='(a,i4)') '       Ini: ', B%Block%line(1)
   !   write(unit=io, fmt='(a,i4)') '       End: ', B%Block%line(2)
   !   write(unit=io, fmt='(a)') ' '
   !   write(unit=io, fmt='(a,i4)') '   Num. SB: ', B%N_SBlocks
   !   write(unit=io, fmt='(a)') ' '
   !   do k=1, B%N_SBlocks
   !      write(unit=io, fmt='(a)') '    SB_Name: '//trim(B%SBlock(k)%Name)
   !      write(unit=io, fmt='(a)') '    SB_Type: '//trim(B%SBlock(k)%Type)
   !      write(unit=io, fmt='(a,i4)') '   SB_Ini: ', B%SBlock(k)%line(1)
   !      write(unit=io, fmt='(a,i4)') '   SB_End: ', B%SBlock(k)%line(2)
   !      write(unit=io, fmt='(a)') ' '
   !   end do
   !   if (B%N_SBlocks > 0) write(unit=io, fmt='(a)') ' '
   !
   !End Subroutine WriteInfo_Block

End Program KeyCodes

