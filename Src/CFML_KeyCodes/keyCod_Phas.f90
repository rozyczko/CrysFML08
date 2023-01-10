!!
Submodule (CFML_KeyCodes) KeyCod_Phas
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE READ_REFCODES_PHAS
   !!----
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Read_RefCodes_PHAS(ffile, n_ini, n_end, Ip, Ph)
      !---- Arguments ----!
      Type(file_type),         intent(in)     :: ffile
      integer,                 intent(in)     :: n_ini
      integer,                 intent(in)     :: n_end
      integer,                 intent(in)     :: Ip
      type(RelationList_Type), intent(inout)  :: Ph

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
               call ReadCode_FIX_PHAS(line, Ip, Ph)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_PHAS(line, Ip, Ph)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               !call ReadCode_EQUAL_PHAS(line, AtList, Spg)
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_PHAS

   !!----
   !!---- SUBROUTINE READCODE_FIX_PHAS
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_PHAS(String, Ip, Ph)
      !---- Arguments ----!
      character(len=*),        intent(in)    :: String
      integer,                 intent(in)    :: Ip
      type(RelationList_Type), intent(inout) :: Ph

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IPHas_dir
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
      call split_refcod_PHAS(line, n_dir, ind_dir, IPhas_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line, dire, nc)
         do j=1,n_dir
            iv=ip
            if (IPhas_dir(j) /=0) iv=iPhas_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_PHAS('FIX', ind_dir(j), iv, Ph)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'PHAS')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+4:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_Phas('FIX', ind_dir(j), ivet(1), Ph)
                  end if
               end do !k
            end if

            do k=n_dir+1,nc
               call Set_RefCodes_Phas('FIX', ind_dir(j), iv, Ph)
            end do ! Objects
         end do ! n_dir
      end if

   End Subroutine ReadCode_FIX_PHAS

   !!----
   !!---- SUBROUTINE READCODE_VARY_PHAS
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_PHAS(String, Ip, Ph)
      !---- Arguments ----!
      character(len=*),        intent(in)    :: String
      integer,                 intent(in)    :: Ip
      type(RelationList_Type), intent(inout) :: Ph

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IPhas_dir
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
      call split_refcod_Phas(line, n_dir, ind_dir, Iphas_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line,dire,nc)
         do j=1,n_dir
            iv=ip
            if (iphas_dir(j) /=0) iv=iphas_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_PHAS('VARY', ind_dir(j), Iv, Ph)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'PHAS')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+4:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_Phas('VARY', ind_dir(j), ivet(1), Ph)
                  end if
               end do !k
            end if
         end do ! ndir
      end if

   End Subroutine ReadCode_VARY_PHAS

   !!--++
   !!--++ SUBROUTINE SPLIT_REFCOD_PHAS
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Split_RefCod_PHAS(String, Nc, Ikeys, IPhas, Keys)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String
      integer,                        intent(out) :: Nc
      integer, dimension(:),          intent(out) :: IKeys
      integer, dimension(:),          intent(out) :: IPhas
      character(len=*), dimension(:), intent(out) :: Keys

      !---- Local Variables ----!
      integer          :: i,j,n,iv,npos

      !> Init
      Nc=0; Ikeys=0; IPhas=0; Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)

      loop1: do i=1,n
         !> Phases
         npos=index(u_case(dire(i)),'_PHAS')
         if (npos > 0) then
            call get_num(dire(i)(npos+5:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include the Phases!')
               return
            end if
            IPhas(i)=ivet(1)    ! Positive values for Phases references
            dire(i)=dire(i)(:npos-1)
         end if

         do j=1, NKEY_PHAS
            if (trim(KEY_PHAS(j)) == trim(u_case(dire(i)))) then
               nc=nc+1
               ikeys(nc)=j
               keys(nc)=trim(dire(i))
               cycle loop1
            end if
         end do ! Key_Phas

      end do loop1 ! General

   End Subroutine Split_RefCod_PHAS

   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_PHAS
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_PHAS(Keyword, Npar,  IP, Ph)
      !---- Arguments ----!
      character(len=*),              intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                       intent(in)     :: NPar        ! Specific parameter A,B,C,...
      integer,                       intent(in)     :: IP
      type(RelationList_Type),       intent(in out) :: Ph

      !---- Local variables ----!
      integer          :: i
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
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (1) ! A
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'A_PHAS'//trim(car))

               case (2) ! B
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'B_PHAS'//trim(car))

               case (3) ! C
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'C_PHAS'//trim(car))

               case (4) ! ALP
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'ALP_PHAS'//trim(car))

               case (5) ! BET
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'BET_PHAS'//trim(car))

               case (6) ! GAM
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_RelationList_Par(Ph,'GAM_PHAS'//trim(car))

               case (7) ! CELL
                write(car,fmt='(i3)') ip
                car=adjustl(car)
                call Fix_RelationList_Par(Ph,'A_PHAS'//trim(car))
                call Fix_RelationList_Par(Ph,'B_PHAS'//trim(car))
                call Fix_RelationList_Par(Ph,'C_PHAS'//trim(car))
                call Fix_RelationList_Par(Ph,'ALP_PHAS'//trim(car))
                call Fix_RelationList_Par(Ph,'BET_PHAS'//trim(car))
                call Fix_RelationList_Par(Ph,'GAM_PHAS'//trim(car))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (1) ! A
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'A_PHAS'//trim(car))

               case (2) ! B
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'B_PHAS'//trim(car))

               case ( 3) ! C
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'C_PHAS'//trim(car))

               case (4)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'ALP_PHAS'//trim(car))

               case (5)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'BET_PHAS'//trim(car))

               case (6)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'GAM_PHAS'//trim(car))

               case (7)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_RelationList_Par(Ph,'A_PHAS'//trim(car))
                  call Vary_RelationList_Par(Ph,'B_PHAS'//trim(car))
                  call Vary_RelationList_Par(Ph,'C_PHAS'//trim(car))
                  call Vary_RelationList_Par(Ph,'ALP_PHAS'//trim(car))
                  call Vary_RelationList_Par(Ph,'BET_PHAS'//trim(car))
                  call Vary_RelationList_Par(Ph,'GAM_PHAS'//trim(car))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_PHAS

End SubModule KeyCod_Phas