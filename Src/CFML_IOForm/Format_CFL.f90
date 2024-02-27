!!----
!!----
!!----
SubModule (CFML_IOForm) Format_CFL
   !---- Variables ----!
   implicit none

 Contains
   !!----
   !!---- READ_CFL_ATOM
   !!----    Subroutine to read atoms parameters
   !!----
   !!----         ATOM   Label  ChemSymb   x y z B Occ Us or Moment
   !!----
   !!----     For charge obtained from Label: Label[+/-][number]
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_Atoms(cfl, AtmList, Type_Atm, d, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),      intent(in)     :: cfl     ! Containing information
      Type(AtList_Type),    intent(out)    :: AtmList
      character(len=*),     intent(in)     :: Type_Atm
      integer,              intent(in)     :: d
      integer, optional,    intent(in)     :: i_ini, i_end

      !---- Local variables -----!
      character(len=80) :: mom_comp
      character(len=80) :: direc
      integer           :: i, j, na, npos, n_oc, n_mc,n_dc,n_uc
      integer           :: j_ini, j_end

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Atoms@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      if (AtmList%natoms > 0) call Allocate_Atom_List(0, Atmlist, Type_Atm, d)

      !> Calculate number of Atoms
      na=0
      mom_comp=" "
      do i=j_ini,j_end
          line=adjustl(cfl%line(i)%str)
          if (len_trim(line) == 0) cycle
          if (line(1:1) == "!" .or. line(1:1) == "#") cycle
          if (line(1:1) == ' ') cycle

          if (index(u_case(line),"ATM_MOM_COMP") /= 0) then
             j=index(line,"!")
             if ( j /= 0) then
                mom_comp=adjustl(line(13:j-1))
             else
                mom_comp=adjustl(line(13:))
             end if
          end if
          npos=index(u_case(line),'ATOM')
          if (npos > 0) na=na+1
      end do
      if (na == 0) return             ! No atoms in the lines

      !> Allocate List
      call Allocate_Atom_List(na, Atmlist, Type_Atm, d)
      if (len_trim(mom_comp) > 2) Atmlist%mcomp=mom_comp

      na=0
      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) ==' ') cycle

         !> Truncate line from symbols: # and !
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         !> Eliminate Tabs
         do
            npos=index(line,TAB)
            if (npos == 0) exit
            line(npos:npos)=' '
         end do

         !> ATOM Directive
         if(len_trim(line) < 4) then
           cycle
         else
           direc=adjustl(u_case(line(1:4)))
         end if
         if (trim(direc) /= "ATOM") cycle

         na=na+1
         call read_atom(line, Atmlist%atom(na))  ! Utype is read now in the line
         Atmlist%atom(na)%ThType="iso"
         if (len_trim(Atmlist%atom(na)%SfacSymb) == 0) Atmlist%atom(na)%SfacSymb=Atmlist%atom(na)%chemSymb

         !Debugging
         !associate (Atm => Atmlist%atom)
         !  write(*,"(a)") " => "//trim(line)
         !  write(*,fmt=fmt1)  trim(Atm(na)%Lab), trim(Atm(na)%SfacSymb), &
         !     Atm(na)%mult,  Atm(na)%X, Atm(na)%U_iso, Atm(na)%Occ,"   "//Atm(na)%UType
         !end associate

         !> Trial to read anisotropic thermal and
         !> magnetic moment parameters
         j=i
         do
            j=j+1
            if ( j < j_end ) then
               line=adjustl(cfl%line(j)%str)

               if (len_trim(line) == 0) cycle
               if (line(1:1) == "!" .or. line(1:1) == "#") cycle
               if (u_case(line(1:4)) == "ATOM") exit

               npos=index(line," ")
               if (npos <= 1) cycle

               select case (u_case(line(1:npos-1)))
                  case ("MOMENT")
                     call read_Moment(line,Atmlist%atom(na))

                  case ("U_IJ")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType= "U"
                     Atmlist%atom(na)%ThType= "ANI"

                  case ("B_IJ")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType="B"
                     Atmlist%atom(na)%ThType="ANI"

                  case ("BETA")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType= "BETA"
                     Atmlist%atom(na)%ThType="ANI"
               end select
               if (Err_CFML%Ierr /= 0) return

            else
               exit
            end if
         end do

         select type(at => Atmlist%atom)
            class is(ModAtm_Std_Type)
               n_oc=0; n_mc=0; n_dc=0; n_uc=0
               j=i
               do
                  j=j+1
                  if ( j < j_end ) then
                     line=adjustl(cfl%line(j)%str)

                     if (len_trim(line) == 0) cycle
                     if (line(1:1) == "!" .or. line(1:1) == "#") cycle
                     if (line(1:1) == ' ') cycle
                     if (u_case(line(1:4)) == "ATOM") exit

                     npos=index(line," ")
                     if (npos <= 1) then
                        err_CFML%Ierr=1
                        err_CFML%Msg="Read_CFL_Atoms: Error in line "//trim(line)
                        return
                     end if

                     select case (u_case(line(1:npos-1)))
                        case ("O_CS")
                           n_oc=n_oc+1
                           call read_modulation_amplitudes(line,At(na),"O_CS",n_oc)

                        case ("M_CS")
                           n_mc=n_mc+1
                           call read_modulation_amplitudes(line,At(na),"M_CS",n_mc)

                        case ("D_CS")
                           n_dc=n_dc+1
                           call read_modulation_amplitudes(line,At(na),"D_CS",n_dc)

                        case ("U_CS")
                           n_uc=n_uc+1
                           call read_modulation_amplitudes(line,At(na),"U_CS",n_uc)
                     end select
                     if (Err_CFML%Ierr /= 0) return
                  else
                     exit
                  end if
               end do
               At(na)%n_oc=n_oc
               At(na)%n_mc=n_mc
               At(na)%n_dc=n_dc
               At(na)%n_uc=n_uc
         end select
      end do
      AtmList%natoms=na

   End Subroutine Read_CFL_Atoms

   !!----
   !!---- READ_CFL_CELL
   !!----
   !!----    Obtaining Cell Parameter from CFL Format
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_Cell(cfl, Cell, CFrame, i_ini, i_end, cmd)
      !---- Arguments ----!
      type(File_Type),                intent(in)     :: cfl     ! Containing information
      class(Cell_G_Type),allocatable, intent(out)    :: Cell    ! Cell object
      character(len=*),     optional, intent(in)     :: CFrame
      integer,              optional, intent(in)     :: i_ini, i_end     ! Lines to explore
      logical,              optional, intent(in)     :: cmd
      !---- Local variables -----!
      integer                              :: i, iv, n_ini, n_end
      integer                              :: j_ini,j_end
      real(kind=cp), dimension (6)         :: vcell, std
      character(len=132), dimension(1)     :: linec

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      !> Search: CELL
      do i=j_ini,j_end
         linec(1)=adjustl(u_case(cfl%line(i)%str))
         if (linec(1)(1:4) == "CELL") exit
         linec(1)=" "
      end do

      if (len_trim(linec(1)) == 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: Instruction 'CELL' not provided "
         return
      end if

      !> Eliminate Tabs
      do
         iv=index(linec(1),TAB)
         if (iv == 0) exit
         linec(1)(iv:iv)=' '
      end do

      n_ini=1; n_end=1
      vcell=0.0
      std=0.0
      call Read_Key_ValueSTD(linec, n_ini, n_end,"CELL", vcell, std, iv)
      if (iv /= 6) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: Problems reading cell parameters!"
         return
      end if
      if(present(cmd)) then
        allocate(Cell_GLS_Type :: Cell)
      else
        allocate(Cell_G_Type :: Cell)
      end if
      if (present(CFrame)) then
         call Set_Crystal_Cell(vcell(1:3),vcell(4:6), Cell, CarType=CFrame, Vscell=std(1:3), Vsang=std(4:6))
      else
         call Set_Crystal_Cell(vcell(1:3),vcell(4:6), Cell, Vscell=std(1:3), Vsang=std(4:6))
      end if

   End Subroutine Read_CFL_Cell

   !!----
   !!---- READ_CFL_KVECTORS
   !!----
   !!---- Read K-vectors information
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_KVectors(cfl, Kvec, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),         intent(in)     :: cfl
      type(kvect_info_Type),   intent(out)    :: Kvec
      integer,       optional, intent(in)     :: i_ini, i_end

      !---- Local Variables ----!
      integer                      :: i,j,ier,nk,nq,iv
      integer                      :: j_ini, j_end
      character(len=:),allocatable :: uline

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Kvectors: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      nk=0; nq=0
      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) == " ") cycle

         !> Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         j=index(line,"!")
         if (j /= 0) line=line(:j-1)

         j=index(line," ")
         if ( j == 0) then
            uline=u_case(line)
         else
            uline=u_case(line(:j-1))
         end if
         line=adjustl(line(j+1:))

         select case(trim(uline))
            case("NQVECT","NKVECT","NKVEC","NQVEC")
               read(unit=line,fmt=*,iostat=ier) Kvec%nk, Kvec%nq
               if (ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%Msg="Error reading the number of k-vectors and/or number of Q-coefficients"
                  return
               end if
               allocate(Kvec%kv(3,Kvec%nk),Kvec%q_coeff(Kvec%nk,Kvec%nq))
               allocate(Kvec%nharm(Kvec%nk),Kvec%sintlim(Kvec%nk))
               Kvec%kv=0.0_cp; Kvec%q_coeff=1; Kvec%nharm=1; Kvec%sintlim=1.0

            case("QVECT","KVECT","KVEC","QVEC")
               if (Kvec%nk > 0) then
                  nk=nk+1
                  read(unit=line,fmt=*,iostat=ier) Kvec%kv(:,nk)
                  if (ier /= 0) then
                     Err_CFML%Ierr=1
                     write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the k-vector #",nk
                     return
                  end if
               end if

           case("NHARM")
              if (Kvec%nk > 0) then
                 read(unit=line,fmt=*,iostat=ier) Kvec%nharm(1:Kvec%nk)
                 if (ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg = "Error reading the nk harmonics !"
                    return
                 end if
              end if

           case("SINTL")
              if (Kvec%nk > 0) then
                 read(unit=line,fmt=*,iostat=ier) Kvec%sintlim(1:Kvec%nk)
                 if (ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg = "Error reading the maximum sinTheta/Lambda for harmonics!"
                    return
                 end if
              end if

           case("Q_COEFF")
              nq=nq+1
              read(unit=line,fmt=*,iostat=ier) Kvec%q_coeff(1:Kvec%nk,nq)
              if (ier /= 0) then
                 Err_CFML%Ierr=1
                 write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the Q-coefficent # ",nq
                 return
              end if
         end select
      end do

      if (Kvec%nk /= nk) then
         Err_CFML%Ierr=1
         write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of k-vectors,",Kvec%nk, ", does not correspond with the prescribed number: ",nk
         return
      end if

      if (Kvec%nq /= nq) then
         Err_CFML%Ierr=1
         write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of expected Q-coefficients,",Kvec%nq, ", does not correspond with number of read Q-coefficients ",nq
         return
      end if

   End Subroutine Read_CFL_KVectors

   !!----
   !!---- READ_CFL_SPG
   !!----
   !!---- Read Space group information ina CFL file
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_SpG(cfl, SpG, xyz_type, i_ini, i_end)
      !---- Arguments ----!
      Type(File_Type),                 intent(in)     :: cfl
      class(SpG_Type), allocatable,    intent(out)    :: SpG
      character(len=*), optional,      intent(in)     :: xyz_type
      integer,          optional,      intent(in)     :: i_ini, i_end

      !--- Local Variables ---!
      integer                           :: i,j,ngen,nk,nq,iv,ier,d,dr,Mult
      integer                           :: j_ini, j_end
      character(len=:),     allocatable :: uline,setting,strcode
      character(len=40), dimension(192) :: gen
      logical                           :: change_setting

      !> Init
      call clear_error()

      if (cfl%nlines <= 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Spg: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      !> Look for the appropriate keywords to construct the space group:
      !> Crystallographic, Shubnikov, or superspace
      ngen=0
      setting=" "
      change_setting=.false.

      strcode="xyz"
      if (present(xyz_type)) strcode=trim(xyz_type)

      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) == " ") cycle

         !> Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         j=index(line,"!")
         if (j /= 0) line=line(:j-1)

         j=index(line,"::")
         if (j /= 0) then
            setting=trim(adjustl(line(j+2:)))
            if (len_trim(setting) /= 0) change_setting=.true.
            line=line(:j-1)
         end if

         j=index(line," ")
         uline=u_case(line(:j-1))

         line=adjustl(line(j+1:))
         select case(trim(uline))
            case("HALL","SPGR","SPACEG")
               allocate(SpG_Type :: Spg)
               call Set_SpaceGroup(line, SpG)
               exit

            case("SHUB")
               allocate(SpG_Type :: Spg)
               call Set_SpaceGroup(line,"SHUBN",SpG)
               exit

            case("SSG","SUPER","SSPG")
               allocate(SuperSpaceGroup_Type :: Spg)
               call Set_SpaceGroup(line,"SUPER",SpG, strcode)
               d=SpG%d
               exit

            case("GENLIST","GENERATORS","LIST")
               j=index(line,";")
               if(j == 0) then
                 d=Get_Dimension_SymmOp(trim(line))
               else
                 d=Get_Dimension_SymmOp(trim(line(1:j-1)))
               end if
               if(d > 4) then
                 allocate(SuperSpaceGroup_Type :: Spg)
               else
                 allocate(SpG_Type :: Spg)
               end if
               call Set_SpaceGroup(line,SpG)
               exit

            case("GEN","GENR","SYMM")
               ngen=ngen+1
               gen(ngen)=line

         end select
      end do

      if (ngen > 0) then
        d=Get_Dimension_SymmOp(trim(gen(1)))
        if(d > 4) then
          allocate(SuperSpaceGroup_Type :: Spg)
        else
          allocate(SpG_Type :: Spg)
        end if
        call Set_SpaceGroup("  ",SpG,ngen,gen)
      end if

      if (Err_CFML%Ierr == 1) return

      if (change_setting) then
         if (strcode == "xyz")  then
            call Change_Setting_SpaceG(setting, SpG)
         else
            call Change_Setting_SpaceG(setting, SpG, strcode)
         end if
      end if
      if (Err_CFML%Ierr == 1) return

      !> Now read q-vectors and other items if the class of SpG is SuperSpaceGroup_Type
      Select Type (SpG)
         Class is (SuperSpaceGroup_Type)
            dr=d-1
            nk=dr-3
            Mult=SpG%Multip
            if (allocated(SpG%kv))      deallocate (SpG%kv)
            if (allocated(SpG%nharm))   deallocate (SpG%nharm)
            if (allocated(SpG%sintlim)) deallocate (SpG%sintlim)
            if (allocated(SpG%q_coeff)) deallocate (SpG%q_coeff)
            if (allocated(SpG%Rot)) deallocate (SpG%Rot)
            if (allocated(SpG%t)) deallocate (SpG%t)
            if (allocated(SpG%tI)) deallocate (SpG%tI)
            if (allocated(SpG%M)) deallocate (SpG%M)
            if (allocated(SpG%Ep)) deallocate (SpG%Ep)


            allocate(SpG%Rot(3,3,Mult),SpG%t(3,Mult),SpG%tI(nk,Mult),SpG%M(nk,3,Mult),SpG%Ep(nk,nk,Mult))

            do i=1,Mult
                SpG%Rot(:,:,i)=SpG%Op(i)%Mat(1:3,1:3)
                SpG%t    (:,i)=SpG%Op(i)%Mat(1:3,d)
                SpG%tI   (:,i)=SpG%Op(i)%Mat(4:dr,d)
                SpG%M  (:,:,i)=SpG%Op(i)%Mat(4:dr,1:3)
                SpG%Ep (:,:,i)=SpG%Op(i)%Mat(4:dr,4:dr)
            end do

            nk=0; nq=0
            do i=j_ini,j_end
               line=adjustl(cfl%line(i)%str)
               if (len_trim(line) == 0) cycle
               if (line(1:1) == "!" .or. line(1:1) == "#") cycle

               j=index(line,"!")
               if (j /= 0) line=line(:j-1)

               j=index(line," ")
               uline=u_case(line(:j-1))

               line=adjustl(line(j+1:))
               select case(trim(uline))
                  case("NQVECT","NKVECT")
                     read(unit=line,fmt=*,iostat=ier) Spg%nk, Spg%nq
                     if (ier /= 0) then
                        Err_CFML%Ierr=1
                        Err_CFML%Msg="Error reading the number of k-vectors and/or number of Q-coefficients"
                        return
                     end if
                     allocate(Spg%kv(3,Spg%nk),Spg%q_coeff(Spg%nk,Spg%nq))
                     allocate(Spg%nharm(Spg%nk),Spg%sintlim(Spg%nk))
                     SpG%kv=0.0_cp; SpG%q_coeff=1; Spg%nharm=1; Spg%sintlim=1.0

                  case("QVECT","KVECT")
                     if (Spg%nk > 0) then
                        nk=nk+1
                        read(unit=line,fmt=*,iostat=ier) Spg%kv(:,nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the k-vector #",nk
                           return
                        end if
                     end if

                  case("NHARM")
                     if (Spg%nk > 0) then
                        read(unit=line,fmt=*,iostat=ier) Spg%nharm(1:Spg%nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg = "Error reading the nk harmonics !"
                           return
                        end if
                     end if

                  case("SINTL")
                     if (Spg%nk > 0) then
                        read(unit=line,fmt=*,iostat=ier) Spg%sintlim(1:Spg%nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg = "Error reading the maximum sinTheta/Lambda for harmonics!"
                           return
                        end if
                     end if

                  case("Q_COEFF")
                     nq=nq+1
                     read(unit=line,fmt=*,iostat=ier) Spg%q_coeff(1:Spg%nk,nq)
                     if (ier /= 0) then
                        Err_CFML%Ierr=1
                        write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the Q-coefficent # ",nq
                        return
                     end if

               end select
            end do

            if (Spg%nk /= (Spg%D-4)) then
               Err_CFML%Ierr=1
               write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of k-vectors,",Spg%nk, ", does not correspond with the additional dimensions of the group ",Spg%D-4
               return
            end if

            if (Spg%nq /= nq) then
               Err_CFML%Ierr=1
               write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of expected Q-coefficients,",Spg%nq, ", does not correspond with number of read Q-coefficients ",nq
               return
            end if

      End Select

   End Subroutine Read_CFL_SpG

   !!----
   !!---- WRITE_CFL_ATOMS
   !!----
   !!----    Write the atoms in the asymmetric unit for a CFL file
   !!----
   !!---- 08/05/2020
   !!
   Module Subroutine Write_CFL_Atoms(AtmList, Lun, Cell)
      !---- Arguments ----!
      Type(AtList_Type),            intent(in) :: AtmList
      integer,            optional, intent(in) :: Lun
      class(Cell_G_Type), optional, intent(in) :: Cell

      !---- Local Variables ----!
      character(len=36)              :: forma,fom
      character(len=30),dimension(6) :: text
      real(kind=cp), dimension(6)    :: u,bet,sb
      integer                        :: i, j, iunit, leng !, maxl

      !> Unit
      iunit=6
      if (present(lun)) iunit=lun

      if (AtmList%natoms == 0) then
         write (unit=iunit,fmt="(a)") " There are no atoms defined!"
         return
      end if

      !> Determine the maximum length of the atom labels
      !maxl=0
      !do i=1,AtmList%natoms
      !   leng=len_trim(atmList%atom(i)%lab)
      !   if (leng > maxl) maxl=leng
      !end do
      !maxl=max(maxl,4)+1

      !> define format forma
      forma="(a,a8,tr2,a4,tr3,5a14,2f9.2,tr3,a)"
      !select case(maxl)
      !   case(:9)
      !      write(unit=forma(5:5),fmt="(i1)") maxl
      !   case(10:)
      !      write(unit=forma(5:6),fmt="(i2)") maxl
      !end select

      write (unit=iunit,fmt="(a)")  &
      "!    Atom      Type     x/a           y/b           z/c           Biso          Occ             Spin    Charge    Info"

      select type (at => AtmList%atom)
         class is (Atm_Std_Type)
            do i=1,AtmList%natoms

               do j=1,3
                  text(j)=string_NumStd(at(i)%x(j), at(i)%x_std(j))
               end do
               text(4)=string_NumStd(at(i)%U_iso, at(i)%U_iso_std)
               text(5)=string_NumStd(at(i)%Occ, at(i)%Occ_std)

               if(at(i)%magnetic) then
                   write (unit=iunit,fmt=forma) "Atom ",at(i)%lab,at(i)%SfacSymb, &
                     (text(j),j=1,5), at(i)%mom, real(at(i)%charge), " # "//trim(at(i)%AtmInfo)
                   write (unit=iunit,fmt="(a,3f14.5)") "Moment          ",at(i)%moment
               else
                   write (unit=iunit,fmt=forma) "Atom ",at(i)%lab,at(i)%ChemSymb, &
                     (text(j),j=1,5), at(i)%mom, real(at(i)%charge), " # "//trim(at(i)%AtmInfo)
               end if

               select case (l_case(at(i)%ThType))
                  case ('ani')
                     select case (l_case(at(i)%UType))
                        case ('beta')
                           u=at(i)%u(1:6)
                           if (present(cell)) then
                              bet=Get_U_from_Betas(u,cell)
                              sb=Get_U_from_Betas(at(i)%u_std,cell)
                           else
                              bet=u
                               sb=at(i)%u_std(1:6)
                           end if
                           do j=1,6
                              text(j)=string_NumStd(bet(j), sb(j))
                           end do
                           if (present(cell)) then
                              write(unit=iunit,fmt="(a,6a14)") "!U_ij  ", text
                           else
                              write (unit=iunit,fmt="(a,tr1,6a14)") "Beta  ", text
                           end if

                        case ('u_ij')
                           u=at(i)%u(1:6)
                           if (present(cell)) then
                              bet=Get_Betas_from_U(u,cell)
                              sb=Get_Betas_from_U(at(i)%u_std,cell)
                           else
                              bet=u
                               sb=at(i)%u_std(1:6)
                           end if
                           do j=1,6
                              text(j)=string_NumStd(bet(j), sb(j))
                           end do
                           if (present(cell)) then
                              write(unit=iunit,fmt="(a,6a14)") "!Beta  ", text
                           else
                              write (unit=iunit,fmt="(a,tr1,6a14)") "U_ij  ", text
                           end if

                     end select
               end select
            end do
      end select

   End Subroutine Write_CFL_Atoms

   !!----
   !!---- Write_CFL_File
   !!----
   !!----    Write a CFL file
   !!----
   !!---- 08/05/2020
   !!
   Module Subroutine Write_CFL_File(Lun,Cell, SpG, Atm, Title,info_lines)
      !---- Arguments ----!
      integer,                               intent(in)    :: lun
      class(Cell_G_Type),                    intent(in)    :: Cell
      class(SpG_Type),                       intent(in)    :: SpG
      Type(AtList_Type), optional,           intent(in)    :: Atm
      character(len=*),  optional,           intent(in)    :: Title
      character(len=*),dimension(:),optional,intent(in)    :: info_lines
      !----- Local variables -----!
      integer                         :: i,j
      real(kind=cp), dimension(6)     :: a,sa
      character(len=30), dimension(6) :: text

      !> Title
      if (present(title)) write(unit=lun,fmt="(a)") "TITLE "//trim(title)

      write(unit=lun,fmt='(a)')" "
      write(unit=lun,fmt='(a)') "!  Automatically generated CFL file (Write_CFL_file)"
      write(unit=lun,fmt='(a)')" "

      !> Cell
      a(1:3)=Cell%Cell
      a(4:6)=Cell%ang
      sa(1:3)=Cell%scell
      sa(4:6)=Cell%sang
      do j=1,6
         text(j)=string_NumStd(a(j), sa(j))
      end do
      write(unit=lun,fmt="(a)") "!         a               b               c            alpha           beta            gamma"
      write(unit=lun,fmt="(a,6a16)") "Cell ",text
      write(unit=lun,fmt='(a)')" "

      !> Space group
      if(SpG%magnetic) then
        write(unit=lun,fmt="(a)")"! Magnetic Space Group, BNS number: "//SpG%BNS_num
        write(unit=lun,fmt="(a)")"!                       BNS symbol: "//SpG%BNS_symb
        if(len_trim(SpG%UNI) /= 0) write(unit=lun,fmt="(a)")"!                       UNI symbol: "//SpG%UNI
        if(SpG%standard_setting) then
          write(unit=lun,fmt="(a)") "SHUB "//SpG%BNS_symb
        else
          do i=1,SpG%Multip
             write(unit=lun,fmt="(a)") "SYMM "//trim(Spg%Symb_Op(i))
          end do
        end if
        write(unit=lun,fmt="(a)")"  "
      else
        write(unit=lun,fmt="(a,i3)")"!     Space Group # ",SpG%NumSpg
        write(unit=lun,fmt="(a,a)") "Spgr  ",SpG%spg_symb
        write(unit=lun,fmt='(a)')" "
      end if

      !> Atoms
      if (present(Atm)) then
         call Write_CFL_Atoms(Atm,Lun,cell)
         write(unit=lun,fmt='(a)')" "
      end if
      if(present(info_lines)) then
        j=0
        write(unit=lun,fmt="(a)") "!"
        do
         j=j+1
         write(unit=lun,fmt="(a)") trim(info_lines(j))
         if(u_case(info_lines(j)(1:14)) == "END_INFO_LINES" .or. j > 100) exit
        end do
      end if

   End Subroutine Write_CFL_File

   !!--++
   !!--++ READ_XTAL_CFL
   !!--++
   !!--++ Read Crystal Information in a CFL File
   !!--++
   !!--++ 10/05/2020
   !!
   Module Subroutine Read_XTal_CFL(cfl, Cell, SpG, AtmList, Atm_Typ, Nphase, CFrame, Job_Info)
      !---- Arguments ----!
      type(File_Type),               intent(in)  :: cfl
      class(Cell_G_Type),allocatable,intent(out) :: Cell
      class(SpG_Type),   allocatable,intent(out) :: SpG
      Type(AtList_Type),             intent(out) :: Atmlist
      character(len=*),    optional, intent(in)  :: Atm_Typ
      Integer,             optional, intent(in)  :: Nphase   ! Select the Phase to read
      character(len=*),    optional, intent(in)  :: CFrame
      Type(Job_Info_type), optional, intent(out) :: Job_Info

      !---- Local variables ----!
      logical                          :: set_moment, set_ModAtm_std
      integer, dimension(MAX_PHASES)   :: ip
      integer                          :: i, j,nt_phases, iph, n_ini, n_end
      integer                          :: k

      real(kind=cp),dimension(:),allocatable:: xvet

      type(kvect_info_Type)            :: Kvec
      logical                          :: commands

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_XTal_CFL: No lines in the file!"
         return
      end if

      commands=.false.

      !> Calculating number of Phases
      nt_phases=0; ip=cfl%nlines; ip(1)=1
      do i=1,cfl%nlines
         line=u_case(adjustl(cfl%line(i)%str))
         if (len_trim(line) <= 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         if(index(line,"COMMANDS") /= 0 .or. index(line,"VARY") /= 0 .or. index(line,"FIX") /= 0) commands=.true.
         if (line(1:6) == 'PHASE_') then
            nt_phases=nt_phases+1
            ip(nt_phases)=i
         end if
      end do
      if (nt_phases ==0) nt_phases=1

      !> Read the Phase information
      iph=1
      if (present(nphase)) then
         iph=min(nphase, nt_phases)
         iph=max(1,iph)
      end if

      n_ini=ip(iph)
      n_end=ip(iph+1)

      if (present(Job_Info)) then
         call Get_Job_Info(cfl,Job_info, n_ini, n_end)
      end if

      !> Reading Cell Parameters
      if (present(CFrame)) then
         if(commands) then
            call read_cfl_cell(cfl, Cell, CFrame,n_ini,n_end,commands)
         else
            call read_cfl_cell(cfl, Cell, CFrame,n_ini,n_end)
         end if
      else
         if(commands) then
            call read_cfl_cell(cfl, Cell, i_ini=n_ini,i_end=n_end,cmd=commands)
         else
            call read_cfl_cell(cfl, Cell, i_ini=n_ini,i_end=n_end)
         end if
      end if
      if (Err_CFML%IErr == 1) return

      !> Reading Space groups
      call read_CFL_SpG(cfl,SpG, i_ini=n_ini, i_end=n_end)
      if (Err_CFML%IErr == 1 .or. .not. allocated(SpG)) return

      !> Read Atoms information
      set_moment=.false.
      set_ModAtm_std=.false.
      do i=n_ini,n_end
         line=adjustl(cfl%line(i)%str)

         if (len_trim(line) <=0) cycle
         if (line(1:1) == '!') cycle
         if (line(1:1) == ' ') cycle

         if (u_case(line(1:4)) /= 'ATOM') cycle

         do j=i+1,n_end
            line=adjustl(cfl%line(j)%str)
            if (len_trim(line) <=0) cycle
            if (line(1:1) == '!') cycle

            if (l_case(line(1:4)) == 'atom') exit
            if (l_case(line(1:6)) == 'moment') set_moment=.true.
            if (l_case(line(2:4)) == '_cs')    set_ModAtm_std=.true.
         end do
      end do

      if(SpG%d == 4) set_ModAtm_std=.false.

      if(present(Atm_Typ)) then
        call read_cfl_Atoms(cfl,AtmList,Atm_Typ,0,n_ini,n_end)
      else
         if ((.not. set_moment) .and. (.not. set_ModAtm_std)) then
            !> Type of Atoms: Atm_std
            call read_cfl_Atoms(cfl,AtmList,'Atm_std_type',0,n_ini,n_end)

         else if (set_moment .and. (.not. set_ModAtm_std)) then
            !> Type of Atoms: Atm_std
            call read_cfl_Atoms(cfl,AtmList,'Atm_std_type',0,n_ini,n_end)

         else if (set_moment .and. set_ModAtm_std) then
            !> Type of Atoms: ModAtm_std
            call read_cfl_kvectors(cfl,kvec,n_ini,n_end)
            if (err_CFML%Ierr ==1) return
            call read_cfl_Atoms(cfl,AtmList,'ModAtm_std_type',Kvec%nk,n_ini,n_end)

         else
            !> Type of atoms not defined
            err_CFML%Ierr=1
            err_CFML%Msg="Read_XTal_CFL: Impossible to define the type of Atoms. Please, check it!"
            return
         end if
      end if
      if (allocated(xvet)) deallocate(xvet)
      Select Type (SpG)
         type is (SuperSpaceGroup_Type)
             allocate(xvet(SpG%D-1))
             do i=1,Atmlist%natoms
                xvet(1:3)=Atmlist%atom(i)%x
                do k=1,Spg%nk
                   xvet(3+k)=dot_product(xvet(1:3),SpG%kv(:,k))
                end do
                Atmlist%atom(i)%Mult=Get_Multip_Pos(xvet,SpG)
                if (Atmlist%atom(i)%occ < EPSV) Atmlist%atom(i)%occ=real(Atmlist%atom(i)%Mult)/real(SpG%Multip)
                select type (at => Atmlist%atom(i))
                   class is (ModAtm_Std_Type)
                      At%Xs=xvet
                end select
             end do

         class Default
             allocate(xvet(3))
             do i=1,Atmlist%natoms
                xvet=Atmlist%atom(i)%x
                Atmlist%atom(i)%Mult=Get_Multip_Pos(xvet,SpG)
                if (Atmlist%atom(i)%occ < EPSV) Atmlist%atom(i)%occ=real(Atmlist%atom(i)%Mult)/real(SpG%Multip)
             end do
      End Select

      !> Convert Us to Betas and Uiso to Biso
      do i=1,AtmList%natoms
         select case (AtmList%atom(i)%thtype)
            case ("iso")

               if(l_case(Atmlist%atom(i)%utype) == "u_ij") then  !Only multiply by 8 pi^2 when Utype is explicitly provided
                 Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u_iso*78.95683521
               end if

            case ("ani")

               Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u(1)*78.95683521 !by default

               select type (cell)
                  class is (Cell_G_Type)
                     Atmlist%atom(i)%u_iso=U_Equiv(cell,Atmlist%atom(i)%u(1:6))  ! Uequi
                     Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u_iso*78.95683521

                     select case (l_case(Atmlist%atom(i)%Utype))
                        case ("u_ij")
                           Atmlist%atom(i)%u(1:6) =  Get_Betas_from_U(Atmlist%atom(i)%u(1:6),Cell)

                        case ("b_ij")
                           Atmlist%atom(i)%u(1:6) = Get_Betas_from_B(Atmlist%atom(i)%u(1:6),Cell)
                     end select
               end select
               Atmlist%atom(i)%Utype="beta"

            case default

               !Atmlist%atom(i)%u_iso = Atmlist%atom(i)%u_iso*78.95683521
               Atmlist%atom(i)%thtype = "iso"
               Atmlist%atom(i)%Utype="b_ij"

         end select
      end do

   End Subroutine Read_XTal_CFL

   !!----
   !!---- GET_JOB_INFO
   !!----
   !!----    Constructor of the object Job_info.
   !!----
   !!---- 10/05/2020
   !!
   Module Subroutine Get_Job_Info(cfl,Job_info, i_ini,i_end)
      !---- Arguments ----!
      type(File_Type),      intent(in)  :: cfl              ! Containing information
      type(job_info_type),  intent(out) :: Job_info         ! Object to be constructed
      integer,              intent(in)  :: i_ini, i_end     ! Lines to explore

      !---- Local Variables ----!
      integer                           :: i,nphas, ncmd,n_pat,ier, j
      integer, dimension(i_end-i_ini+1) :: ip,ic,ipt
      real(kind=sp)                     :: a1,a2,a3,a4,a5
      character(len=120)                :: fmtfields, fmtformat

      !> Init
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Get_Job_Info: 0 lines "
         return
      end if

      !> Initialize FindFMT
      call Init_FindFMT(i_ini)

      nphas=0
      ncmd=0
      n_pat=0
      ip=i_end
      ic=0
      ipt=0

      Job_info%title=" General Job: CrysFML"
      Job_info%Num_Patterns=1

      do i=i_ini,i_end
         line=u_case(adjustl(cfl%line(i)%str))

         if (line(1:5) == "TITLE") Job_info%title=line(7:)

         if (line(1:5) == "NPATT") then
            read(unit=line(7:), fmt=*,iostat=ier) Job_info%Num_Patterns
            if (ier /= 0) Job_info%Num_Patterns=1
         end if

         if (line(1:6) == "PHASE_") then
            nphas=nphas+1
            ip(nphas)=i
         end if

         if (line(1:4) == "CMDL") then
            ncmd=ncmd+1
            ic(ncmd)=i
         end if

         if (line(1:5) == "PATT_") then
            n_pat=n_pat+1
            ipt(n_pat)=i
         end if
      end do

      if (nphas == 0) then
         nphas=1
         ip(nphas)=0
      end if
      if (n_pat == 0) then
         n_pat=1
         ipt(n_pat) = 0
      end if

      if (Job_info%Num_Patterns /= n_pat) Job_info%Num_Patterns = n_pat
      Job_info%Num_Phases=nphas
      Job_info%Num_Cmd=ncmd

      if (allocated(Job_Info%Patt_typ))     deallocate(Job_Info%Patt_typ)
      if (allocated(Job_Info%Phas_nam))     deallocate(Job_Info%Phas_nam)
      if (allocated(Job_Info%range_stl))    deallocate(Job_Info%range_stl)
      if (allocated(Job_Info%range_q))      deallocate(Job_Info%range_q)
      if (allocated(Job_Info%range_d))      deallocate(Job_Info%range_d)
      if (allocated(Job_Info%range_2theta)) deallocate(Job_Info%range_2theta)
      if (allocated(Job_Info%range_energy)) deallocate(Job_Info%range_energy)
      if (allocated(Job_Info%range_tof))    deallocate(Job_Info%range_tof)
      if (allocated(Job_Info%lambda))       deallocate(Job_Info%lambda)
      if (allocated(Job_Info%ratio))        deallocate(Job_Info%ratio)
      if (allocated(Job_Info%dtt1))         deallocate(Job_Info%dtt1)
      if (allocated(Job_Info%dtt2))         deallocate(Job_Info%dtt2)

      allocate(Job_Info%Patt_typ(n_pat))
      allocate(Job_Info%Phas_nam(nphas))
      allocate(Job_Info%range_stl(n_pat))
      allocate(Job_Info%range_q(n_pat))
      allocate(Job_Info%range_d(n_pat))
      allocate(Job_Info%range_2theta(n_pat))
      allocate(Job_Info%range_energy(n_pat))
      allocate(Job_Info%range_tof(n_pat))
      allocate(Job_Info%lambda(n_pat))
      allocate(Job_Info%ratio(n_pat))
      allocate(Job_Info%dtt1(n_pat))
      allocate(Job_Info%dtt2(n_pat))

      !> Initialize all variables
      Job_Info%Patt_typ    =" "
      Job_Info%Phas_nam    =" "
      Job_Info%range_stl%mina=0.0
      Job_Info%range_stl%maxb=0.0
      Job_Info%range_q%mina=0.0
      Job_Info%range_q%maxb=0.0
      Job_Info%range_d%mina=0.0
      Job_Info%range_d%maxb=0.0
      Job_Info%range_2theta%mina=0.0
      Job_Info%range_2theta%maxb=0.0
      Job_Info%range_Energy%mina=0.0
      Job_Info%range_Energy%maxb=0.0
      Job_Info%range_tof%mina=0.0
      Job_Info%range_tof%maxb=0.0
      Job_Info%Lambda%mina=0.0
      Job_Info%Lambda%maxb=0.0
      Job_Info%ratio = 0.0
      Job_Info%dtt1 = 0.0
      Job_Info%dtt2 = 0.0

      if (ncmd > 0) then
         if (allocated(Job_Info%cmd)) deallocate(Job_Info%cmd)
         allocate(Job_Info%cmd(ncmd))
         Job_Info%cmd=" "
      end if

      !> Fill the different fields of Job_Info
      !> Start with patterns
      fmtfields = "9fffff"

      !> First asks if there is a PATT_ card, if not a standard is taken
      if (ipt(1) /= 0) then
         do n_pat=1, Job_info%Num_Patterns
            i=ipt(n_pat)

            line=u_case(adjustl(cfl%line(i)%str))
            line=line(8:)
            call findfmt(0,line,fmtfields,fmtformat)
            read(unit=line,fmt=fmtformat) Job_Info%Patt_typ(n_pat), a1,a2,a3,a4,a5
            if (Err_CFML%Ierr /= 0) return

            line=u_case(Job_Info%Patt_typ(n_pat))

            select case(line(1:9))
               case("XRAY_2THE","NEUT_2THE","XRAY_SXTA","NEUT_SXTA")
                  if ( a1 <= 0.000001) a1=1.5405
                  if ( a2 <= 0.000001) then
                     a2=a1
                     a3=0.0
                  end if
                  if (a5 <= a4) a5=120.0
                  Job_Info%Lambda(n_pat)%mina=a1
                  Job_Info%Lambda(n_pat)%maxb=a2
                  Job_Info%ratio(n_pat)=a3
                  Job_Info%range_2theta(n_pat)%mina=a4
                  Job_Info%range_2theta(n_pat)%maxb=a5
                  a4=sind(0.5*a4)/a1
                  a5=sind(0.5*a5)/a2
                  Job_Info%range_stl(n_pat)%mina=a4
                  Job_Info%range_stl(n_pat)%maxb=a5
                  Job_Info%range_q(n_pat)%mina=a4*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=a5*4.0*pi
                  Job_Info%range_d(n_pat)%mina=0.5/a5
                  Job_Info%range_d(n_pat)%maxb=0.5/a4

               case("NEUT_TOF ")
                  if (a1 <= 0.000001) a1=1000.0
                  if (a4 <= a3) a4=2.0*abs(a3)
                  Job_Info%dtt1(n_pat)=a1
                  Job_Info%dtt2(n_pat)=a2
                  Job_Info%range_tof(n_pat)%mina=a3
                  Job_Info%range_tof(n_pat)%maxb=a4
                  Job_Info%range_d(n_pat)%mina=0.5*(-1.0+sqrt(1.0+4.0*a2*a3/a1/a1))
                  Job_Info%range_d(n_pat)%maxb=0.5*(-1.0+sqrt(1.0+4.0*a2*a4/a1/a1))
                  Job_Info%range_stl(n_pat)%mina=0.5/Job_Info%range_d(n_pat)%maxb
                  Job_Info%range_stl(n_pat)%maxb=0.5/Job_Info%range_d(n_pat)%mina
                  Job_Info%range_q(n_pat)%mina=Job_Info%range_stl(n_pat)%mina*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=Job_Info%range_stl(n_pat)%maxb*4.0*pi

               case("XRAY_ENER")
                  if (a1 <= 0.000001) a1=12.4 !(=hc(keV.Angstr.)
                  Job_Info%dtt1(n_pat)=a1
                  Job_Info%dtt2(n_pat)=0.0
                  Job_Info%range_energy(n_pat)%mina=a3
                  Job_Info%range_energy(n_pat)%maxb=a4
                  if (a3 <= 0.00001) a3=0.01
                  if (a4 <= 0.00001) a4=2.00
                  Job_Info%range_d(n_pat)%mina=a1/a4
                  Job_Info%range_d(n_pat)%maxb=a1/a3
                  Job_Info%range_stl(n_pat)%mina=0.5/Job_Info%range_d(n_pat)%maxb
                  Job_Info%range_stl(n_pat)%maxb=0.5/Job_Info%range_d(n_pat)%mina
                  Job_Info%range_q(n_pat)%mina=Job_Info%range_stl(n_pat)%mina*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=Job_Info%range_stl(n_pat)%maxb*4.0*pi

            end select
         end do

      else
         n_pat=1
         a1=1.5405
         a2=a1
         a3=0.0
         a4=0.0
         a5=120.0
         Job_Info%Patt_typ(n_pat)="XRAY_2THE"
         Job_Info%Lambda(n_pat)%mina=a1
         Job_Info%Lambda(n_pat)%maxb=a2
         Job_Info%ratio(n_pat)=a3
         Job_Info%range_2theta(n_pat)%mina=a4
         Job_Info%range_2theta(n_pat)%maxb=a5
         a4=sind(0.5*a4)/a1
         a5=sind(0.5*a5)/a2
         Job_Info%range_stl(n_pat)%mina=a4
         Job_Info%range_stl(n_pat)%maxb=a5
         Job_Info%range_q(n_pat)%mina=a4*4.0*pi
         Job_Info%range_q(n_pat)%maxb=a5*4.0*pi
         Job_Info%range_d(n_pat)%mina=0.5/a5
         Job_Info%range_d(n_pat)%maxb=0.5/a4
      end if

      !> Phase names
      if (ip(1) /= 0) then
         do i=1,nphas
            j=ip(i)
            line=adjustl(cfl%line(j)%str)
            Job_Info%Phas_nam(i)=line(8:)
         end do
      else
         Job_Info%Phas_nam(1)= Job_info%title
      end if

      !> Command Lines, stored but not analysed here
      do i=1,ncmd
         j=ic(i)
         line=adjustl(cfl%line(j)%str)
         Job_Info%cmd(i)=line(8:)
      end do

   End Subroutine Get_Job_Info

   !!----
   !!---- SUBROUTINE READINFO_MOLECULE
   !!----
   !!----    Subroutine to read a molecule from a CFL ffile.
   !!----    The format is:
   !!----
   !!----        MOLEX_MoleculeName Coordinates_Type
   !!----        ....
   !!----        END_MOLEX_MoleculeName
   !!----
   !!----    where:
   !!----        Coordinates_Type    C: Cartesian coordinates
   !!----                            F: Fractional coordinates
   !!----                            S: Spherical coordinates
   !!----                            Z: Z-Matrix coordinates
   !!----
   !!----    Into the MOLEX Block:
   !!----       First line:Molecule_Centre(3), Molecule_Orient(3), Rotational_Angle Type(1), Thermal_Factor Type(1)
   !!----
   !!----       where:
   !!----        Molecule_Centre     Coordinate of Center of Molecule
   !!----        Molecule_Orient     Angles orientation
   !!----        Rotational Angle    E: Conventional Euler angles (alpha, beta, gamma)
   !!----                            P: Polar Euler angles (Phi, theta, Chi) (default)
   !!----        Thermal Factor    ISO: No collective motion
   !!----                          TLS: Traslational + Librational + Correlation
   !!----                           TL: Traslational + Librational
   !!----                            T: Traslational
   !!----
   !!----        According to Thermal Factors, next lines will be read
   !!----                          [T]: 6 Thermal Factors (Line1)
   !!----
   !!----                         [TL]: 6 Thermal Factors (Line1)
   !!----                               6 Thermal Factors (Line3)
   !!----
   !!----                        [TLS]: 6 Thermal Factors (Line1)
   !!----                               6 Thermal Factors (Line3)
   !!----                               9 Thermal Factors (Line5)
   !!----
   !!----    Internal Coordinates for Atoms (N_Atoms Lines)
   !!----        Atom_Name(6)  Atom_Specie(4)  Coordinates(3)  [N1  N2  N3]  Biso  Occ
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Read_CFL_Molecule(cfl, N_ini, N_end, Mol)
      !---- Arguments ----!
      Type(file_type),      intent(in)   :: cfl
      integer, optional,    intent(in)   :: N_Ini
      integer, optional,    intent(in)   :: N_End
      type (Molecule_type), intent(out)  :: Mol

      !---- Local variables -----!
      character(len=40)               :: Molname
      character(len=6)                :: cmol, Atname
      character(len=4)                :: AtSymb,var
      character(len=1)                :: ct
      integer                         :: i,k,n, ic,iv,ini,npos,na
      real(kind=cp),dimension(3,3)    :: Eu

      logical :: err_flag

      !> Init
      call clear_error()

      !> First line: MOLEX_MoleculeName Coordinates_Type  Number
      i=n_ini
      line=adjustl(cfl%line(i)%str)

      k=index(line,'_')
      if (k ==0) then
         call set_error(1, 'Bad format for MOLEX block definition!')
         return
      end if
      line=line(k+1:)

      call get_words(line, dire, ic)
      if (ic < 2) then
         call set_error(1, 'Bad format for MOLEX block definition!')
         return
      end if

      !> Name of the Molecule
      Molname=trim(dire(1))

      !> Format of Coordinates
      ct='-'
      ct=adjustl(dire(2))
      ct=u_case(ct)
      select case (ct)
         case ('F','C','S','Z')
         case default
            call set_error(1," The type of the coordinates is unknown: "//ct )
            return
      end select

      !> Number of atoms
      na=0
      do i=n_ini+1, n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         if (line(1:1) ==" ") cycle
         na=na+1
      end do
      na=na-1    ! Delete the line for Center definition
      if (na <= 0) then
         call set_error(1, "The number of atoms in the Molecule was zero!" )
         return
      end if

      !> Initialize the Molecule_Type
      call Init_Molecule(Mol, na)

      Mol%Name_Mol=trim(Molname)
      Mol%coor_type=ct

      !> Centre / Orientation
      i=n_ini+1
      do i=n_ini+1, n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         if (line(1:1) ==" ") cycle

         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         call get_words(line, dire, ic)
         if (ic /= 8) then
            call set_error(1, "Molecule_Centre(3R), Molecule_Orient(3R), Rotational_Angle Type(1C), Thermal_Factor Type(1C)" )
            return
         end if

         !> Centre of Molecule
         line=trim(dire(1))//'  '//trim(dire(2))//'  '//trim(dire(3))
         call get_num(line,vet,ivet,iv)
         if (iv /=3) then
            call set_error(1, "Wrong number of parameters describing the Centre position of the molecule!")
            return
         end if
         Mol%xcentre=vet(1:3)

         !> Orientation
         line=trim(dire(4))//'  '//trim(dire(5))//'  '//trim(dire(6))
         call get_num(line,vet,ivet,iv)
         if (iv /=3) then
            call set_error(1, "Wrong number of parameters describing the Orientation of the molecule!")
            return
         end if
         Mol%orient=vet(1:3)

         !> Rotation type
         ct=adjustl(u_case(trim(dire(7))))
         select case (ct)
            case ('E','P')  ! Euler, Polar
            case default
               call set_error(1, "Wrong description for angle rotation type: "//ct)
               return
         end select
         Mol%rot_type=ct

         !> Thermal type
         var=adjustl(u_case(trim(dire(8))))
         select case (trim(var))
            case ('ISO')
            case ('TLS')
               Na=Na-3
            case ('TL')
               Na=Na-2
            case ('T')
               Na=Na-1
            case default
               call set_error(1, "Wrong description for Thermal type: "//trim(var))
               return
         end select
         Mol%therm_type=trim(var)
         Mol%natoms=Na

         exit
      end do
      Eu=Set_Euler_Matrix(Mol%rot_type, Mol%orient(1), Mol%orient(2), Mol%orient(3))
      Mol%Euler=Eu
      Mol%is_EulerMat=.true.

      !> Read the internal coordinates of the atoms in the Mol
      !> Read the Z-matrix/Cartesian/spherical/Fractional coordinates of the Mol
      k=0
      n=i+1

      do i=n, n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         if (line(1:1) ==" ") cycle

         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         !> Atom Name
         call Cut_string(line, ic, Atname)

         !> Chemical symbol
         call Cut_string(line, ic, AtSymb)

         !> Numbers
         call get_num(line,vet,ivet,iv)

         select case (Mol%coor_type)
            case ('F','C','S')
               k=k+1
               if (iv >= 3 .and. iv <=5) then
                  select case (iv)
                     case (3)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                     case (4)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =1.0_cp
                     case (5)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =vet(5)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if

            case ('Z')
               if (iv >= 6 .and. iv <=8) then
                  k=k+1
                  select case (iv)
                     case (6)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (7)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (8)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =vet(8)
                        Mol%conn(1:3,k)  =ivet(4:6)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if
         end select

         mol%Atname(k)=trim(atname)
         mol%AtSymb(k)=trim(atsymb)

         if (k == na) exit
      end do
      if (k /= na) then
         call set_error(1," The number of Atoms readen in the file is different from asked! ")
         return
      end if

      !> TLS Info
      n=i+1
      if (var(1:1) == 'T') then
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 6) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%T_TLS=vet(1:6)
            exit
         end do
      end if

      if (var(2:2) == 'L') then
         n=i+1
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 6) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%L_TLS=vet(1:6)
            exit
         end do
      end if

      if (var(3:3) == 'S') then
         n=i+1
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 9) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%S_TLS(1,:)=vet(1:3)
            Mol%S_TLS(2,:)=vet(4:6)
            Mol%S_TLS(3,:)=vet(7:9)
            exit
         end do
      end if

      !> Check connectivity if ZMatrix coordinates
      Mol%is_connect=.false.
      err_flag=.false.
      if (Mol%coor_type == "Z") then
         do i=2,Na
            if (i==2 .and. all(Mol%conn(:,2) ==0) ) Mol%conn(1,i)=1
            if (any(Mol%conn(:,i) >= i)) then
               err_flag=.true.
               exit
            end if
            if (i == 3 .and. (Mol%conn(1,i) == 0 .or. Mol%conn(2,i) == 0)) then
               err_flag=.true.
               exit
            end if
            if (i > 3) then
               if (any(Mol%conn(:,i) == 0)) then
                  err_flag=.true.
                  exit
               end if
            end if
         end do
         if (err_flag) then
            call set_error(1,"The Z-matrix connectivity is wrong!" )
            return
         end if
      end if

   End Subroutine Read_CFL_Molecule



End SubModule Format_CFL