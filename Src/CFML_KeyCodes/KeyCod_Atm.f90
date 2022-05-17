Submodule (CFML_KeyCodes) KeyCod_Atm
   implicit none

   Contains
   !!--++
   !!--++ SUBROUTINE READ_REFCODES_ATM
   !!--++
   !!
   Module Subroutine Read_RefCodes_ATM(ffile, n_ini, n_end, Spg, Atlist)
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
      if (err_CFML%Flag) then
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
               if (err_CFML%Flag) then
                  print*,err_CFML%Msg
               end if

            case ("VARY")    ! VARY
               print*,' ==> VARY Directive: '//trim(line)
               call ReadCode_VARY_ATM(line, AtList, Spg)
               if (err_CFML%Flag) then
                  print*,err_CFML%Msg
               end if

            case ("EQUA") ! Equal (Constraints)
               print*,' ==> EQUA Directive: '//trim(line)
               !call ReadCode_EQUAL_ATM(line, AtList, Spg)
               if (err_CFML%Flag) then
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
   !!---- ReadCode_FIX_ATM
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine ReadCode_FIX_ATM(String, AtList, Spg)
      !---- Arguments ----!
      character(len=*),   intent(in)     :: String
      type(AtList_Type),  intent(in out) :: AtList
      class (SpG_type),   intent(in)     :: Spg

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=3)                      :: car
      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: npos, nlong, n_dir, n_loc, nc
      integer                               :: ii,j,k,na,iv, iphas
      integer, dimension(NMAX_GEN)          :: Ind_dir, Ind_dir2, IPh_dir, Iph_loc
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
      call split_genrefcod_atm(line,n_dir, ind_dir, Iph_dir)

      !> Locals  directives
      call Split_LocRefCod_ATM(line, n_loc, dir_loc, Ind_dir2, Iph_loc, dir_lab)

      if (n_dir > 0 .and. n_loc > 0) then
         call set_error(1,'Wrong form for FIX: '//trim(line))
         return
      end if

      bounds = [0.0, 1.0, 0.1]

      if (n_dir > 0) then
         call get_words(line, dire, nc)
         do j=1,n_dir
            do k=n_dir+1,nc

               !> Atom label
               na=Index_AtLab_on_AtList(dire(k), Iph_dir(j), Atlist)
               if (na > 0) then
                  call Fill_RefCodes_Atm('FIX', Ind_dir(j), Bounds, 1, Na, Spg, Atlist)
               else
                  !> Species
                  done=.false.
                  do ii=1,AtList%Natoms
                     if (iph_dir(j) > 0) then
                        if (atList%iph(ii) /= iph_dir(j)) cycle
                     end if
                     if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle
                     call Fill_RefCodes_Atm('FIX', Ind_dir(j), Bounds, 1, ii, Spg, Atlist)
                     done=.true.
                  end do

                  if (.not. done) then
                     call set_error(1,'Not found the Atom label: '//trim(dire(k)))
                     return
                  end if
               end if

            end do ! Objects
         end do ! n_dir
      end if

      if (n_loc > 0) then
         do j=1,n_loc
            na=Index_AtLab_on_AtList(dir_lab(j), iph_loc(j), AtList)
            if (na==0) then
               call set_error(1,'Not found the Atom given in the list! -> '//trim(dir_lab(j)))
               return
            end if
            call Fill_RefCodes_Atm('FIX', Ind_dir2(j), Bounds, 1, Na, Spg, Atlist)
         end do
      end if

   End Subroutine ReadCode_FIX_ATM

   !!----
   !!---- ReadCode_VARY_ATM
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine ReadCode_VARY_ATM(String, AtList, Spg)
      !---- Arguments ----!
      character(len=*),   intent(in)     :: String
      type(AtList_Type),  intent(in out) :: AtList
      class (SpG_type),   intent(in)     :: Spg

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=3)                      :: car
      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: npos, nlong, n_dir, n_loc, nc
      integer                               :: ii,j,k,na
      integer, dimension(NMAX_GEN)          :: Ind_dir, Ind_dir2, IPh_dir, Iph_loc
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

      !> Cut VARY word
      call cut_string(line,nlong)

      !> general directives
      call split_genrefcod_atm(line, n_dir, Ind_dir, IPh_dir, dir_gen)

      !> Locals  directives
      call Split_LocRefCod_ATM(line, n_loc, dir_loc, Ind_dir2, Iph_loc, dir_lab)

      if (n_dir > 0 .and. n_loc > 0) then
         call set_error(1,'Wrong form for VARY: '//trim(line))
         return
      end if

      bounds = [0.0, 1.0, 0.1]
      if (n_dir > 0) then
         call get_words(line,dire,nc)

         do j=1,n_dir
            do k=n_dir+1,nc
               na=Index_AtLab_on_AtList(dire(k),iph_dir(j),Atlist)
               if (na > 0) then
                  call Fill_RefCodes_Atm('VARY', Ind_dir(j), Bounds, 1, Na, Spg, Atlist)

               else
                  !> Species
                  done=.false.
                  do ii=1,AtList%Natoms
                     if (iph_dir(j) > 0) then
                        if (atList%iph(ii) /= iph_dir(j)) cycle
                     end if
                     if (trim(u_case(dire(k))) /= trim(u_case(AtList%atom(ii)%ChemSymb))) cycle
                     call Fill_RefCodes_Atm('VARY', Ind_dir(j), Bounds, 1, ii, Spg, Atlist)
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

      if (n_loc > 0) then
         do j=1,n_loc

            na=Index_AtLab_on_AtList(dir_lab(j), iph_loc(j), AtList)
            if (na==0) then
               call set_error(1,'Not found the Atom given in the list! -> '//trim(dir_lab(j)))
               return
            end if
            call Fill_RefCodes_Atm('VARY', Ind_dir2(j), Bounds, 1, Na, Spg, Atlist)
         end do
      end if

   End Subroutine ReadCode_VARY_ATM

   !!--++
   !!--++ Subroutine Fix_XYZ_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_XYZ_Atm(Atlist, NAtm, Ind)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm
      integer,           intent(in)     :: Ind ! 1:X, 2:Y, 3:Z, 0:XYZ


      !---- Local Variables ----!
      integer :: i,nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) /=0) then
                         nc=A%l_x(Ind)
                         call Del_RefCode_Atm(Atlist,nc)
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) /=0) then
                           nc=A%l_x(i)
                           call Del_RefCode_Atm(Atlist,nc)
                        end if
                     end do
               end select

            type is (MAtm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) /=0) then
                         nc=A%l_x(Ind)
                         call Del_RefCode_Atm(Atlist,nc)
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) /=0) then
                           nc=A%l_x(i)
                           call Del_RefCode_Atm(Atlist,nc)
                        end if
                     end do
               end select
         end select
      end associate

   End Subroutine Fix_XYZ_Atm

   !!--++
   !!--++ Subroutine Fix_OCC_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_OCC_Atm(Atlist, NAtm)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm

      !---- Local Variables ----!
      integer:: nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               if (A%l_occ /=0) then
                  nc=A%l_occ
                  call Del_RefCode_Atm(Atlist, nc)
               end if

            type is (MAtm_Ref_Type)
               if (A%l_occ /=0) then
                  nc=A%l_occ
                  call Del_RefCode_Atm(Atlist, nc)
               end if
         end select
      end associate

   End Subroutine Fix_OCC_Atm

   !!--++
   !!--++ Subroutine Fix_U_Atm
   !!--++
   !!--++    Fix Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fix_U_Atm(Atlist, NAtm, Ind)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NAtm
      integer,           intent(in)     :: Ind  ! 0:U_iso, 1:U11, 2:U22, 3:U33, 4:U12
                                                ! 5: U13  6: U23, -1: All U's


      !---- Local Variables ----!
      integer :: i,nc
      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_u_iso /=0) then
                         nc=A%l_u_iso
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (1:6)
                     if (A%l_u(Ind) /=0) then
                         nc=A%l_u(Ind)
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) /=0) then
                           nc=A%l_u(i)
                           call Del_RefCode_Atm(Atlist, nc)
                        end if
                     end do
               end select

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_u_iso /=0) then
                         nc=A%l_u_iso
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (1:6)
                     if (A%l_u(Ind) /=0) then
                         nc=A%l_u(Ind)
                         call Del_RefCode_Atm(Atlist, nc)
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) /=0) then
                           nc=A%l_u(i)
                           call Del_RefCode_Atm(Atlist, nc)
                        end if
                     end do
               end select
         end select
      end associate

   End Subroutine Fix_U_Atm

   !!--++
   !!--++ Subroutine Vary_XYZ_Atm
   !!--++
   !!--++    Vary Coordinates Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_XYZ_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      integer,                     intent(in)     :: Ind    ! 1:X, 2:Y, 3:Z, 0:XYZ
      class(SpG_Type),              intent(in)    :: Spg
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic


      !---- Local Variables ----!
      integer :: i,nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) ==0) then
                        A%m_x(Ind)=1.0_cp
                        call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                        if (A%l_x(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%X(Ind)
                           Vec_NamePar(NP_Ref)=trim(key_atm(Ind))//'_'//trim(A%Lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                           !Vec_RefParSTD=0.0_cp
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) ==0) then
                           A%m_x(i)=1.0_cp
                           call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                           if (A%l_x(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%X(i)
                              Vec_NamePar(NP_Ref)=trim(key_atm(i))//'_'//trim(A%Lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                              !Vec_RefParSTD=0.0_cp
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (1:3)
                     if (A%l_x(Ind) ==0) then
                        A%m_x(Ind)=1.0_cp
                        call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                        if (A%l_x(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%X(Ind)
                           Vec_NamePar(NP_Ref)=trim(key_atm(Ind))//'_'//trim(A%Lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                           !Vec_RefParSTD=0.0_cp
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (0)
                     do i=1,3
                        if (A%l_x(i) ==0) then
                           A%m_x(i)=1.0_cp
                           call Get_AtomPos_CTR(A%X, Spg, NP_Ref, A%l_x, A%m_x)
                           if (A%l_x(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%X(i)
                              Vec_NamePar(NP_Ref)=trim(key_atm(i))//'_'//trim(A%Lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                              !Vec_RefParSTD=0.0_cp
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind
         end select ! A
      end associate

   End Subroutine Vary_XYZ_Atm

   !!--++
   !!--++ Subroutine Vary_OCC_Atm
   !!--++
   !!--++    Vary Occupancy factor Code
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic

      !---- Local Variables ----!
      integer:: nc

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               if (A%l_occ ==0) then
                  NP_Ref=NP_Ref+1

                  Vec_RefPar(NP_Ref)=A%occ
                  Vec_NamePar(NP_Ref)="Occ_"//trim(A%lab)
                  A%m_occ=1.0_cp
                  A%l_occ=NP_Ref
                  Vec_LimPar(:,NP_Ref)=Bounds
                  Vec_BCond(NP_Ref)=Ic
                  Vec_PointPar(NP_Ref)=Natm
               end if

            type is (Matm_Ref_Type)
               if (A%l_occ ==0) then
                  NP_Ref=NP_Ref+1

                  Vec_RefPar(NP_Ref)=A%occ
                  Vec_NamePar(NP_Ref)="Occ_"//trim(A%lab)
                  A%m_occ=1.0_cp
                  A%l_occ=NP_Ref
                  Vec_LimPar(:,NP_Ref)=Bounds
                  Vec_BCond(NP_Ref)=Ic
                  Vec_PointPar(NP_Ref)=Natm
               end if
         end select
      end associate

   End Subroutine Vary_OCC_Atm

   !!--++
   !!--++ Subroutine Vary_U_Atm
   !!--++
   !!--++    Vary Thermal factors  Codes
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Vary_U_Atm(Atlist, NAtm, Ind, Spg, Bounds, Ic)
      !---- Arguments ----!
      type(AtList_Type),           intent(in out) :: AtList
      integer,                     intent(in)     :: NAtm
      integer,                     intent(in)     :: Ind    ! 0:Uiso, 1-6:Uij, -1:All thermal
      class(SpG_Type),              intent(in)     :: Spg
      real(kind=cp), dimension(3), intent(in)     :: Bounds ! Lower, Upper and Step limits
      integer,                     intent(in)     :: Ic


      !---- Local Variables ----!
      integer :: i

      associate (A => AtList%atom(NAtm))
         select type (A)
            type is (Atm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_U_iso ==0) then
                        NP_Ref=NP_Ref+1

                        Vec_RefPar(NP_Ref)=A%U_iso
                        Vec_NamePar(NP_Ref)="U_iso_"//trim(A%lab)
                        A%m_U_iso=1.0_cp
                        A%l_U_iso=NP_Ref
                        Vec_LimPar(:,NP_Ref)=Bounds
                        Vec_BCond(NP_Ref)=Ic
                        Vec_PointPar(NP_Ref)=Natm
                     end if

                  case (1:6)
                     if (A%l_u(Ind) ==0) then
                        A%m_u(Ind)=1.0_cp
                        call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                        if (A%l_u(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%U(Ind)
                           Vec_NamePar(NP_Ref)=trim(Key_atm(7+ind))//'_'//trim(A%lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) ==0) then
                           A%m_u(i)=1.0_cp
                           call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                           if (A%l_u(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%U(i)
                              Vec_NamePar(NP_Ref)=trim(Key_atm(7+i))//'_'//trim(A%lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

            type is (Matm_Ref_Type)
               select case (Ind)
                  case (0)
                     if (A%l_U_iso ==0) then
                        NP_Ref=NP_Ref+1

                        Vec_RefPar(NP_Ref)=A%U_iso
                        Vec_NamePar(NP_Ref)="U_iso_"//trim(A%lab)
                        A%m_U_iso=1.0_cp
                        A%l_U_iso=NP_Ref
                        Vec_LimPar(:,NP_Ref)=Bounds
                        Vec_BCond(NP_Ref)=Ic
                        Vec_PointPar(NP_Ref)=Natm
                     end if

                  case (1:6)
                     if (A%l_u(Ind) ==0) then
                        A%m_u(Ind)=1.0_cp
                        call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                        if (A%l_u(Ind) == NP_Ref) then
                           Vec_RefPar(NP_Ref)=A%U(Ind)
                           Vec_NamePar(NP_Ref)=trim(Key_atm(7+ind))//'_'//trim(A%lab)
                           Vec_LimPar(:,NP_Ref)=Bounds
                           Vec_BCond(NP_Ref)=Ic
                           Vec_PointPar(NP_Ref)=Natm
                        else
                           NP_Ref=NP_Ref-1
                        end if
                     end if

                  case (-1)
                     do i=1,6
                        if (A%l_u(i) ==0) then
                           A%m_u(i)=1.0_cp
                           call Get_AtomBet_CTR(A%x,A%u,Spg, NP_Ref, A%L_u, A%m_u)
                           if (A%l_u(i) == NP_Ref) then
                              Vec_RefPar(NP_Ref)=A%U(i)
                              Vec_NamePar(NP_Ref)=trim(Key_atm(7+i))//'_'//trim(A%lab)
                              Vec_LimPar(:,NP_Ref)=Bounds
                              Vec_BCond(NP_Ref)=Ic
                              Vec_PointPar(NP_Ref)=Natm
                           else
                              NP_Ref=NP_Ref-1
                           end if
                        end if
                     end do

               end select  ! Ind

         end select ! A
      end associate

   End Subroutine Vary_U_Atm

   !!--++
   !!--++ SUBROUTINE SPLIT_GENREFCOD_ATM
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Split_GenRefCod_ATM(String, Nc, Ikeys, IPhas, Keys)
      !---- Arguments ----!
      character(len=*),                         intent(in)  :: String
      integer,                                  intent(out) :: Nc
      integer, dimension(:),                    intent(out) :: IKeys
      integer, dimension(:),                    intent(out) :: IPhas
      character(len=*), dimension(:), optional, intent(out) :: Keys

      !---- Local Variables ----!
      integer :: i,j,n,iv,npos

      !> Init
      Nc=0; Ikeys=0; IPhas=0
      if (present(keys)) Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)
      loop1: do i=1,n

         !> Phases
         npos=index(u_case(dire(i)),'_PH')
         if (npos > 0) then
            call get_num(dire(i)(npos+3:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include Phase information!')
               return
            end if
            Iphas(i)=ivet(1)    ! Positive values for Phases references
            dire(i)=dire(i)(:npos-1)
         end if

         !> Molecule
         npos=index(u_case(dire(i)),'_MOL')
         if (npos > 0) then
            call get_num(dire(i)(npos+4:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include Molecule information!')
               return
            end if
            Iphas(i)=-ivet(1)   ! Negative values for molecules references
            dire(i)=dire(i)(:npos-1)
         end if

         do j=1,NKEY_ATM
            if (trim(KEY_ATM(j)) == trim(u_case(dire(i)))) then
               nc=nc+1
               ikeys(nc)=j
               if (present(keys)) keys(nc)=trim(key_atm(j))
               cycle loop1
            end if
         end do ! Key_atm

      end do loop1 ! General

   End Subroutine Split_GenRefCod_ATM

   !!--++
   !!--++ SUBROUTINE SPLIT_LOCREFCOD_ATM
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Split_LocRefCod_ATM(String, Nc, Keys, Ikeys, IPhas, AtLab)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String
      integer,                        intent(out) :: Nc
      character(len=*), dimension(:), intent(out) :: Keys
      integer,          dimension(:), intent(out) :: Ikeys
      integer,          dimension(:), intent(out) :: IPhas
      character(len=*), dimension(:), intent(out) :: AtLab

      !---- Local Variables ----!
      integer           :: i,j,n,iv
      character(len=40) :: str

      !> Init
      Nc=0
      Keys=" "
      Ikeys=0
      AtLab=" "
      IPhas=0

      if (len_trim(string) == 0) return

      call get_words(string, dire, n)

      do i=1,n
         str=adjustl(dire(i))
         j=index(str,'_')
         if (j == 0) cycle

         nc=nc+1
         keys(nc)=trim(str(:j-1))

         !> Look for phase /mol
         str=str(j+1:)
         j=index(str,'_')
         if ( j > 0) then
            if (str(j+1:j+2)=='PH') then
               call get_num(str(j+3:),vet,ivet,iv)
               if (iv ==1) iphas(nc)=ivet(1) ! Positive values for phases

            else if (str(j+1:j+3) == 'MOL') then
               call get_num(str(j+4:),vet,ivet,iv)
               if (iv ==1) iphas(nc)=-ivet(1) ! Negative values for Molecules

            end if
            atlab(nc)=trim(str(:j-1))
         else
            atlab(nc)=trim(str)
         end if
      end do

      do i=1,nc
         do j=1,NKEY_ATM
            if (trim(KEY_ATM(j)) == trim(u_case(keys(i)))) then
               ikeys(i)=j
               exit
            end if
         end do
      end do

   End Subroutine Split_LocRefCod_ATM

   !!--++
   !!--++ Subroutine Del_RefCode_Atm
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the Atom list
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Del_RefCode_Atm(AtList, NPar)
      !---- Arguments ----!
      type(AtList_Type), intent(in out) :: AtList
      integer,           intent(in)     :: NPar

      !---- Local Variables ----!
      logical :: deleted
      integer :: i,j

      deleted=.false.

      select type (A => AtList%Atom)
         type is (Atm_Ref_Type)
            !> Delete the NPar Parameter
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) == NPar) then
                     A(i)%l_x(j)=0
                     A(i)%m_x(j)=0.0_cp
                     deleted=.true.
                  end if
               end do

               if (A(i)%l_U_iso == NPar) then
                  A(i)%l_U_iso=0
                  A(i)%m_U_iso=0.0_cp
                  deleted=.true.
               end if

               if (A(i)%l_occ == NPar) then
                  A(i)%l_occ=0
                  A(i)%m_occ=0.0_cp
                  deleted=.true.
               end if

               do j=1,6
                  if (A(i)%l_u(j) == NPar) then
                     A(i)%l_u(j)=0
                     A(i)%m_u(j)=0.0_cp
                     deleted=.true.
                  end if
               end do
            end do

            !> Updating Variables
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) > NPar) then
                     A(i)%l_x(j)=A(i)%l_x(j)-1
                  end if
               end do

               if (A(i)%l_U_iso > NPar) then
                  A(i)%l_U_iso=A(i)%l_U_iso-1
               end if

               if (A(i)%l_occ > NPar) then
                  A(i)%l_occ=A(i)%l_occ-1
               end if

               do j=1,6
                  if (A(i)%l_u(j) > NPar) then
                     A(i)%l_u(j)=A(i)%l_u(j)-1
                  end if
               end do
            end do

         type is (MAtm_Ref_Type)
            !! Faltan todavia partes magnéticas...
            !> Delete the NPar Parameter
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) == NPar) then
                     A(i)%l_x(j)=0
                     A(i)%m_x(j)=0.0_cp
                     deleted=.true.
                  end if
               end do

               if (A(i)%l_U_iso == NPar) then
                  A(i)%l_U_iso=0
                  A(i)%m_U_iso=0.0_cp
                  deleted=.true.
               end if

               if (A(i)%l_occ == NPar) then
                  A(i)%l_occ=0
                  A(i)%m_occ=0.0_cp
                  deleted=.true.
               end if

               do j=1,6
                  if (A(i)%l_u(j) == NPar) then
                     A(i)%l_u(j)=0
                     A(i)%m_u(j)=0.0_cp
                     deleted=.true.
                  end if
               end do
            end do

            !> Updating Variables
            do i=1,AtList%natoms
               do j=1,3
                  if (A(i)%l_x(j) > NPar) then
                     A(i)%l_x(j)=A(i)%l_x(j)-1
                  end if
               end do

               if (A(i)%l_U_iso > NPar) then
                  A(i)%l_U_iso=A(i)%l_U_iso-1
               end if

               if (A(i)%l_occ > NPar) then
                  A(i)%l_occ=A(i)%l_occ-1
               end if

               do j=1,6
                  if (A(i)%l_u(j) > NPar) then
                     A(i)%l_u(j)=A(i)%l_u(j)-1
                  end if
               end do
            end do

      end select

      !> Updating Vec_Vectors
      if (deleted) call Del_Element_in_VRef(NPar)

   End Subroutine Del_RefCode_Atm

   !!--++
   !!--++ SUBROUTINE FILL_REFCODES_ATM
   !!--++
   !!--++   Write on Vectors the Information for Free Atoms
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Fill_RefCodes_Atm(Keyword, Npar, Bounds, Ic, Natm, Spg, AtList)
      !---- Arguments ----!
      character(len=*),              intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                       intent(in)     :: NPar        ! Specific parameter X_,Y_,Occ_,...
      real(kind=cp), dimension(3),   intent(in)     :: Bounds      ! Lower, Upper and Step limits
      integer,                       intent(in)     :: Ic          ! 0/1 boundary conditions (0:fixed or 1:periodic)
      integer,                       intent(in)     :: Natm        ! Number of specific atom on the list
      type(Spg_Type),                intent(in)     :: Spg
      type(AtList_Type),             intent(in out) :: AtList

      !---- Local variables ----!
      integer          :: j,nc,np_ini
      character(len=4) :: cdire

      !> Init
      call clear_error()

      !> Check
      if (Natm <=0) then
         call set_error(1," The directive have to be apply on a specific atom!")
         return
      end if

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for specific Atom!")
                  return

               case ( 1) ! X
                  call Fix_XYZ_Atm(Atlist, Natm, 1)

               case ( 2) ! Y
                  call Fix_XYZ_Atm(Atlist, Natm, 2)

               case ( 3) ! Z
                  call Fix_XYZ_Atm(Atlist, Natm, 3)

               case ( 4) ! XYZ
                  call Fix_XYZ_Atm(Atlist, Natm, 0)

               case ( 5) ! OCC
                  call Fix_Occ_Atm(Atlist, Natm)

               case ( 6) ! U_ISO
                  call Fix_U_Atm(Atlist, Natm,0)

               case ( 7) ! U
                  call Fix_U_Atm(Atlist, Natm,-1)

               case ( 8) ! U11
                  call Fix_U_Atm(Atlist, Natm,1)

               case ( 9) ! U22
                  call Fix_U_Atm(Atlist, Natm,2)

               case (10) ! U33
                  call Fix_U_Atm(Atlist, Natm,3)

               case (11) ! U12
                  call Fix_U_Atm(Atlist, Natm,4)

               case (12) ! U13
                  call Fix_U_Atm(Atlist, Natm,5)

               case (13) ! U23
                  call Fix_U_Atm(Atlist, Natm,6)

               case (14) ! ALL
                  call Fix_XYZ_Atm(Atlist, Natm, 0)
                  call Fix_Occ_Atm(Atlist, Natm)
                  call Fix_U_Atm(Atlist,  Natm,0)
                  call Fix_U_Atm(Atlist,  Natm,-1)

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for specific Atom!")
                  return

               case ( 1:3) ! X
                  call Vary_XYZ_Atm(Atlist, NAtm, NPar, Spg, Bounds, Ic)

               case ( 4) ! XYZ
                  call Vary_XYZ_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)

               case ( 5) ! OCC
                  call Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)

               case ( 6) ! U_ISO
                  call Vary_U_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)

               case ( 7) ! U
                  call Vary_U_Atm(Atlist, NAtm, -1, Spg, Bounds, Ic)

               case ( 8:13) ! U's'
                  call Vary_U_Atm(Atlist, NAtm, NPar-7, Spg, Bounds, Ic)

               case (14) ! ALL
                  call Vary_XYZ_Atm(Atlist, NAtm, 0, Spg, Bounds, Ic)
                  call Vary_OCC_Atm(Atlist, NAtm, Bounds, Ic)
                  call Vary_U_Atm(Atlist,  NAtm, 0, Spg, Bounds, Ic)
                  call Vary_U_Atm(Atlist,  NAtm, -1, Spg, Bounds, Ic)

            end select ! Npar

      end select ! Directives

   End Subroutine Fill_RefCodes_Atm

End SubModule KeyCod_Atm
