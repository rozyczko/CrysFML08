!!
Submodule (CFML_KeyCodes) KeyCod_ReadCodes
   implicit none

   Contains


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

End SubModule KeyCod_ReadCodes
