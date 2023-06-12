!!
Submodule (CFML_KeyCodes) KeyCod_Molec
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE READ_REFCODES_MOL
   !!----
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Read_RefCodes_MOL(ffile, n_ini, n_end, Im, M)
      !---- Arguments ----!
      Type(file_type),         intent(in)   :: ffile
      integer,                 intent(in)   :: n_ini
      integer,                 intent(in)   :: n_end
      integer,                 intent(in)   :: Im
      type(GenParList_Type), intent(in out) :: M

      !---- Local Variables ----!
      integer                 :: i, k

      !> Init
      call clear_error()

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
               call ReadCode_FIX_MOL(line, Im, M)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_MOL(line, Im, M)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               !call ReadCode_EQUAL_PHAS(line, AtList, Spg)
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_MOL

   !!----
   !!---- SUBROUTINE READCODE_FIX_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_MOL(String, Im, M)
      !---- Arguments ----!
      character(len=*),        intent(in)   :: String
      integer,                 intent(in)   :: Im
      type(GenParList_Type), intent(in out) :: M

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IM_dir
      integer                               :: npos, nlong, n_dir,  nc
      integer                               :: j, k, iv

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
      call split_refcod_MOL(line, n_dir, ind_dir, IM_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line, dire, nc)
         do j=1,n_dir
            iv=im
            if (IM_dir(j) /=0) iv=iM_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_MOL('FIX', ind_dir(j), iv, M)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'MOL')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+3:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_MOL('FIX', ind_dir(j), ivet(1), M)
                  end if
               end do !k
            end if

            do k=n_dir+1,nc
               call Set_RefCodes_MOL('FIX', ind_dir(j), iv, M)
            end do ! Objects
         end do ! n_dir
      end if

   End Subroutine ReadCode_FIX_MOL

   !!----
   !!---- SUBROUTINE READCODE_VARY_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_MOL(String, Im, M)
      !---- Arguments ----!
      character(len=*),        intent(in)    :: String
      integer,                 intent(in)    :: Im
      type(GenParList_Type), intent(inout) :: M

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IM_dir
      integer                               :: npos, nlong, n_dir,  nc
      integer                               :: j, k, iv

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
      call split_refcod_MOL(line, n_dir, ind_dir, IM_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line,dire,nc)
         do j=1,n_dir
            iv=im
            if (im_dir(j) /=0) iv=im_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_MOL('VARY', ind_dir(j), Iv, M)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'MOL')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+3:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_MOL('VARY', ind_dir(j), ivet(1), M)
                  end if
               end do !k
            end if
         end do ! ndir
      end if

   End Subroutine ReadCode_VARY_MOL

   !!--++
   !!--++ SUBROUTINE SPLIT_REFCOD_MOL
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Split_RefCod_MOL(String, Nc, Ikeys, IMol, Keys)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String
      integer,                        intent(out) :: Nc
      integer, dimension(:),          intent(out) :: IKeys
      integer, dimension(:),          intent(out) :: IMol
      character(len=*), dimension(:), intent(out) :: Keys

      !---- Local Variables ----!
      integer  :: i,n,iv,npos

      !> Init
      Nc=0; Ikeys=0; IMol=0; Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)

      loop1: do i=1,n
         !> Molecules
         npos=index(u_case(dire(i)),'_MOL')
         if (npos > 0) then
            call get_num(dire(i)(npos+4:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include the Molecules!')
               return
            end if
            IMol(i)=ivet(1)    ! Positive values for Molecules references
            dire(i)=dire(i)(:npos-1)
         end if

         !do j=1, NKEY_MOL
         !   if (trim(KEY_MOL(j)) == trim(u_case(dire(i)))) then
         !      nc=nc+1
         !      ikeys(nc)=j
         !      keys(nc)=trim(dire(i))
         !      cycle loop1
         !   end if
         !end do ! Key_Mol

      end do loop1 ! General

   End Subroutine Split_RefCod_MOL

   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_MOL
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_MOL(Keyword, Npar,  Im, M)
      !---- Arguments ----!
      character(len=*),              intent(in)   :: Keyword     ! VARY/FIX/....
      integer,                       intent(in)   :: NPar        ! Specific parameter A,B,C,...
      integer,                       intent(in)   :: Im
      type(GenParList_Type),       intent(in out) :: M

      !---- Local variables ----!
      character(len=4) :: cdire, car

      !> Init
      call clear_error()

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (1) ! XC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'XC_MOL'//trim(car))

               case (2) ! YC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'YC_MOL'//trim(car))

               case (3) ! ZC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'ZC_MOL'//trim(car))

               case (4) ! CENTRE
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'XC_MOL'//trim(car))
                  call Fix_GPList_Par(M,'YC_MOL'//trim(car))
                  call Fix_GPList_Par(M,'ZC_MOL'//trim(car))

               case (5) ! THE
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'THE_MOL'//trim(car))

               case (6) ! PHI
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'PHI_MOL'//trim(car))

               case (7) ! CHI
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'CHI_MOL'//trim(car))

               case (8) ! ORIENT
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'THE_MOL'//trim(car))
                  call Fix_GPList_Par(M,'PHI_MOL'//trim(car))
                  call Fix_GPList_Par(M,'CHI_MOL'//trim(car))

               case (9) ! T
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'T_MOL'//trim(car))

               case (10) ! L
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'L_MOL'//trim(car))

               case (11) ! S
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'S_MOL'//trim(car))

               case (12) ! TL
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'T_MOL'//trim(car))
                  call Fix_GPList_Par(M,'L_MOL'//trim(car))

               case (13) ! LS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'L_MOL'//trim(car))
                  call Fix_GPList_Par(M,'S_MOL'//trim(car))

               case (14) ! TS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'T_MOL'//trim(car))
                  call Fix_GPList_Par(M,'S_MOL'//trim(car))

               case (15) ! TLS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Fix_GPList_Par(M,'T_MOL'//trim(car))
                  call Fix_GPList_Par(M,'L_MOL'//trim(car))
                  call Fix_GPList_Par(M,'S_MOL'//trim(car))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (1) ! XC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'XC_MOL'//trim(car))

               case (2) ! YC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'YC_MOL'//trim(car))

               case (3) ! ZC
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'ZC_MOL'//trim(car))

               case (4) ! CENT
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'XC_MOL'//trim(car))
                  call Vary_GPList_Par(M,'YC_MOL'//trim(car))
                  call Vary_GPList_Par(M,'ZC_MOL'//trim(car))

               case (5) ! THE
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'THE_MOL'//trim(car))

               case (6) ! PHI
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'PHI_MOL'//trim(car))

               case (7) ! CHI
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'CHI_MOL'//trim(car))

               case (8) ! ORIENT
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'THE_MOL'//trim(car))
                  call Vary_GPList_Par(M,'PHI_MOL'//trim(car))
                  call Vary_GPList_Par(M,'CHI_MOL'//trim(car))

               case (9) ! T
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'T_MOL'//trim(car))

               case (10) ! L
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'L_MOL'//trim(car))

               case (11) ! S
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'S_MOL'//trim(car))

               case (12) ! TL
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'T_MOL'//trim(car))
                  call Vary_GPList_Par(M,'L_MOL'//trim(car))

               case (13) ! LS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'L_MOL'//trim(car))
                  call Vary_GPList_Par(M,'S_MOL'//trim(car))

               case (14) ! TS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'T_MOL'//trim(car))
                  call Vary_GPList_Par(M,'S_MOL'//trim(car))

               case (15) ! TLS
                  write(car,fmt='(i3)') im
                  car=adjustl(car)
                  call Vary_GPList_Par(M,'T_MOL'//trim(car))
                  call Vary_GPList_Par(M,'L_MOL'//trim(car))
                  call Vary_GPList_Par(M,'S_MOL'//trim(car))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_MOL

End Submodule KeyCod_Molec