!!
Submodule (CFML_KeyCodes) KeyCod_Patt
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE READ_EXCLUDEREG_PATT
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Background_PATT(ffile, n_ini, n_end, Ip)
      !---- Arguments ----!
      Type(file_type),         intent(in)    :: ffile
      integer,                 intent(in)    :: n_ini
      integer,                 intent(in)    :: n_end
      integer,                 intent(in)    :: Ip

      !---- Local Variables ----!
      integer, dimension(2) :: Ind
      integer               :: j, k, iv, ic, nt, kk

      !> Init
      call clear_error()

      call Get_SubBlock_KEY('BACKGD', ffile, n_ini, n_end, Ind)
      if (all(Ind ==0)) return

      j=ind(1)
      do while(j <= ind(2)-2)
         j=j+1
         line = adjustl(ffile%line(j)%str)
         if (line(1:1) ==' ') cycle
         if (line(1:1) =='!') cycle
         if (len_trim(line) == 0) cycle

         k=index(line,'!')
         if (k > 0) line=line(:k-1)
         k=index(line,'#')
         if (k > 0) line=line(:k-1)

         !> Read the model of Background
         call Get_words(line, dire, iv)
         if (iv < 1 .or. iv > 2) then
            call set_error(1,'Wrong format for Background model into BACKGD zone')
            return
         end if

         !> Models for Background considerations
         select case (trim(u_case(dire(1))))
            case ('LINEAR_INTERPOLATION')
               select case (iv)
                  case (1)
                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic < 2 .or. ic > 4) then
                            call set_error(1,'Wrong format for linear interpolation points in '//trim(line))
                            return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='LINEAR'
                        Vec_Backgd(NP_backgd)%V(1:ic)=vet(1:ic)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic < 2 .or. ic > 4) then
                           call set_error(1,'Wrong format for linear interpolation points in '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='LINEAR'
                        Vec_Backgd(NP_backgd)%V(1:ic)=vet(1:ic)

                        kk=kk+1
                        if (kk ==nt) exit
                     end do

               end select

            case ('SPLINE_INTERPOLATION')
               select case (iv)
                  case (1)
                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic < 2 .or. ic > 4) then
                           call set_error(1,'Wrong format for spline interpolation points in '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='SPLINE'
                        Vec_Backgd(NP_backgd)%V(1:ic)=vet(1:ic)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic < 2 .or. ic > 4) then
                           call set_error(1,'Wrong format for spline interpolation points in '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='SPLINE'
                        Vec_Backgd(NP_backgd)%V(1:ic)=vet(1:ic)
                        kk=kk+1
                        if (kk ==nt) exit
                     end do

               end select

            case ('POLYNOMIAL')
               select case (iv)
                  case (1)
                     call set_error(1,'Wrong format for Polynomial model into BACKGD zone')
                     return

                  case (2)
                     call get_num(dire(2), vet, ivet, ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic == 0 ) then
                           call set_error(1,'Problem reading coefficients for polynomial background: '//trim(line))
                           return
                        end if
                        do k=1,ic
                           kk=kk+1
                           NP_backgd=NP_backgd+1
                           Vec_backgd(NP_backgd)%Ic=Ip
                           Vec_backgd(NP_backgd)%Str='POLYNOM'
                           Vec_Backgd(NP_backgd)%V(1)=vet(kk)
                        end do

                        if (kk ==nt) exit
                     end do
                     if (kk /= nt) then
                        call set_error(1,'The number of coefficients read is not correct!')
                        return
                     end if

               end select

            case ('CHEBYCHEV')
               select case (iv)
                  case (1)
                     call set_error(1,'Wrong format for Chebychev polynomial model into BACKGD zone')
                     return

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic ==0 ) then
                           call set_error(1,'Problem reading coefficients for chebychev background: '//trim(line))
                           return
                        end if
                        do k=1,ic
                           kk=kk+1
                           NP_backgd=NP_backgd+1
                           Vec_backgd(NP_backgd)%Ic=Ip
                           Vec_backgd(NP_backgd)%Str='CHEBYCHEV'
                           Vec_Backgd(NP_backgd)%V(1)=vet(kk)
                        end do

                        if (kk ==nt) exit
                     end do
                     if (kk /= nt) then
                        call set_error(1,'The number of coefficients read is not correct!')
                        return
                     end if

               end select

            case ('PEAKS_PVOIGT')
               select case (iv)
                  case (1)
                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)
                        call get_num(line, vet, ivet, ic)
                        if (ic /=3) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='PKS_PVOIGT'
                        Vec_Backgd(NP_backgd)%V(1:3)=vet(1:3)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)
                        call get_num(line, vet, ivet, ic)
                        if (ic /=3) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='PKS_PVOIGT'
                        Vec_Backgd(NP_backgd)%V(1:3)=vet(1:3)
                        kk=kk+1
                        if (kk ==nt) exit
                     end do
                     if (kk /= nt) then
                        call set_error(1,'The number of peaks pvoights read are not correct!')
                        return
                     end if

               end select

            case ('PEAKS_SPLIT_PVOIGT')
               select case (iv)
                  case (1)
                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)
                        call get_num(line, vet, ivet, ic)
                        if (ic /=4) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='PKS_SPLITPVOIGT'
                        Vec_Backgd(NP_backgd)%V=vet(1:4)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (line(1:1) ==' ') cycle
                        if (line(1:1) =='!') cycle
                        if (len_trim(line) == 0) cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)
                        call get_num(line, vet, ivet, ic)
                        if (ic /=4) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%Ic=Ip
                        Vec_backgd(NP_backgd)%Str='PKS_SPLITPVOIGT'
                        Vec_Backgd(NP_backgd)%V=vet(1:4)
                        kk=kk+1
                        if (kk ==nt) exit
                     end do
                     if (kk /= nt) then
                        call set_error(1,'The number of peaks split pvoights read are not correct!')
                        return
                     end if

               end select

            case default
               call set_error(1,'The method for Background determination is not implemented!')
               return
         end select

      end do

   End Subroutine Read_Background_PATT

   !!----
   !!---- SUBROUTINE READ_EXCLUDEREG_PATT
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_ExcludeReg_PATT(ffile, n_ini, n_end, Ip)
      !---- Arguments ----!
      Type(file_type),         intent(in)    :: ffile
      integer,                 intent(in)    :: n_ini
      integer,                 intent(in)    :: n_end
      integer,                 intent(in)    :: Ip

      !---- Local Variables ----!
      integer               :: j, iv
      integer, dimension(2) :: Ind

      !> Init
      call clear_error()

      call Get_SubBlock_KEY('EXCLUDED_REGIONS', ffile, n_ini, n_end, Ind)
      if (all(Ind ==0)) return

      do j=Ind(1)+1, Ind(2)-1
         line=adjustl(ffile%line(j)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         if (len_trim(line) == 0) cycle

         call Get_Num(line, vet, ivet, iv)
         if (iv /=2) then
            call set_error(1,'Wrong format for Exclude regions interval: '//trim(line))
            return
         end if

         NP_exreg=NP_exreg+1
         Vec_ExReg(NP_exreg)%ic=Ip
         Vec_ExReg(NP_exreg)%V(1:2)=vet(1:2)
      end do

   End Subroutine Read_ExcludeReg_PATT

   !!----
   !!---- SUBROUTINE READ_REFCODES_PATT
   !!----
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Read_RefCodes_PATT(ffile, n_ini, n_end, Ip, Pat)
      !---- Arguments ----!
      Type(file_type),         intent(in)   :: ffile
      integer,                 intent(in)   :: n_ini
      integer,                 intent(in)   :: n_end
      integer,                 intent(in)   :: Ip
      type(GenParList_Type), intent(in out) :: Pat

      !---- Local Variables ----!
      integer           :: i,k

      !> Init
      call clear_error()

      do i=n_ini,n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         if (len_trim(line) == 0) cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_PATT(line, Ip, Pat)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_PATT(line, Ip, Pat)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               !call ReadCode_EQUAL_PATT(line, AtList, Spg)
               if (err_CFML%Flag) then
                  print*,err_CFML%Msg
                  stop
               end if

         end select
      end do

   End Subroutine Read_RefCodes_PATT

   !!--++
   !!--++ SUBROUTINE SPLIT_REFCOD_PATT
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Split_RefCod_Patt(String, Nc, Ikeys, IPatt, Keys)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String
      integer,                        intent(out) :: Nc
      integer, dimension(:),          intent(out) :: IKeys
      integer, dimension(:),          intent(out) :: IPatt
      character(len=*), dimension(:), intent(out) :: Keys

      !---- Local Variables ----!
      integer          :: i,j,n,iv,npos
      character(len=2) :: car

      !> Init
      Nc=0; Ikeys=0; IPatt=0; Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)

      loop1: do i=1,n
         !> Patterns
         npos=index(u_case(dire(i)),'_PAT')
         if (npos > 0) then
            call get_num(dire(i)(npos+4:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include the Pattern!')
               return
            end if
            IPatt(i)=ivet(1)    ! Positive values for Patterns references
            dire(i)=dire(i)(:npos-1)
         end if

         do j=1, NKEY_PATT
            if (trim(KEY_PATT(j)) == trim(u_case(dire(i)))) then
               nc=nc+1
               ikeys(nc)=j
               keys(nc)=trim(dire(i))
               cycle loop1
            end if
         end do ! Key_Patt

      end do loop1 ! General

   End Subroutine Split_RefCod_Patt

   !!----
   !!---- SUBROUTINE READCODE_FIX_PATT
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_PATT(String, Ip, Pat)
      !---- Arguments ----!
      character(len=*),        intent(in)   :: String
      integer,                 intent(in)   :: Ip
      type(GenParList_Type), intent(in out) :: Pat

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IPat_dir
      integer                               :: npos, nlong, n_dir,  nc
      integer                               :: ii,j,k,na,iv

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
      call split_refcod_Patt(line, n_dir, ind_dir, Ipat_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line, dire, nc)
         do j=1,n_dir
            iv=ip
            if (Ipat_dir(j) /=0) iv=ipat_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_Patt('FIX', ind_dir(j), iv, Pat)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'PAT')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+3:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_Patt('FIX', ind_dir(j), ivet(1), Pat)
                  end if
               end do !k
            end if

            do k=n_dir+1,nc
               call Set_RefCodes_Patt('FIX', ind_dir(j), iv, Pat)
            end do ! Objects
         end do ! n_dir
      end if

   End Subroutine ReadCode_FIX_PATT

   !!----
   !!---- SUBROUTINE READCODE_VARY_PATT
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_PATT(String, Ip, Pat)
      !---- Arguments ----!
      character(len=*),        intent(in)     :: String
      integer,                 intent(in)     :: Ip
      type(GenParList_Type), intent(in out) :: Pat

      !---- Local Variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen
      character(len=3)                      :: car

      integer, dimension(NMAX_GEN)          :: Ind_dir, IPat_dir
      integer                               :: npos, nlong, n_dir,  nc
      integer                               :: ii,j,k,na,iv


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
      call split_refcod_Patt(line, n_dir, ind_dir, Ipat_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line,dire,nc)
         do j=1,n_dir
            iv=ip
            if (ipat_dir(j) /=0) iv=ipat_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_Patt('VARY', ind_dir(j), Iv, Pat)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'PAT')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+3:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_Patt('VARY', ind_dir(j), ivet(1), Pat)
                  end if
               end do !k
            end if
         end do ! ndir
      end if

   End Subroutine ReadCode_VARY_PATT

   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_PATT
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_PATT(Keyword, Npar,  IP, Pat)
      !---- Arguments ----!
      character(len=*),              intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                       intent(in)     :: NPar        ! Specific parameter U,V,W,...
      integer,                       intent(in)     :: IP
      type(GenParList_Type),       intent(in out) :: Pat

      !---- Local variables ----!
      integer          :: i
      character(len=4) :: cdire, car, car_n

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

               case (1) ! U
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_GenParList_Par(Pat,'U_PAT'//trim(car))

               case (2) ! V
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_GenParList_Par(Pat,'V_PAT'//trim(car))

               case (3) ! W
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_GenParList_Par(Pat,'W_PAT'//trim(car))

               case ( 4) ! UVW
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Fix_GenParList_Par(Pat,'U_PAT'//trim(car))
                  call Fix_GenParList_Par(Pat,'V_PAT'//trim(car))
                  call Fix_GenParList_Par(Pat,'W_PAT'//trim(car))

               case ( 5:16) ! BKG
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-4
                  car_n=adjustl(car_n)
                  call Fix_GenParList_Par(Pat,'BKG'//trim(car_n)//'_PAT'//trim(car))

               case (17) ! All BKG
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,12
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GenParList_Par(Pat,'BKG'//trim(car_n)//'_PAT'//trim(car))
                  end do

               case (18:20) ! SC
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-17
                  car_n=adjustl(car_n)
                  call Fix_GenParList_Par(Pat,'SC'//trim(car_n)//'_PAT'//trim(car))

               case (21)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,3
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GenParList_Par(Pat,'SC'//trim(car_n)//'_PAT'//trim(car))
                  end do

               case (22:24) ! EXTI
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-21
                  car_n=adjustl(car_n)
                  call Fix_GenParList_Par(Pat,'EXTI'//trim(car_n)//'_PAT'//trim(car))

               case (25)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,3
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GenParList_Par(Pat,'EXTI'//trim(car_n)//'_PAT'//trim(car))
                  end do

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case ( 1) ! U
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_GenParList_Par(Pat,'U_PAT'//trim(car))

               case ( 2) ! V
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_GenParList_Par(Pat,'V_PAT'//trim(car))

               case ( 3) ! U
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_GenParList_Par(Pat,'W_PAT'//trim(car))

               case ( 4) ! UVW
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  call Vary_GenParList_Par(Pat,'U_PAT'//trim(car))
                  call Vary_GenParList_Par(Pat,'V_PAT'//trim(car))
                  call Vary_GenParList_Par(Pat,'W_PAT'//trim(car))

               case (5:16) ! BKG
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-4
                  car_n=adjustl(car_n)
                  call Vary_GenParList_Par(Pat,'BKG'//trim(car_n)//'_PAT'//trim(car))

               case (17)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,12
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GenParList_Par(Pat,'BKG'//trim(car_n)//'_PAT'//trim(car))
                  end do

               case (18:20) ! SC
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-17
                  car_n=adjustl(car_n)
                  call Vary_GenParList_Par(Pat,'SC'//trim(car_n)//'_PAT'//trim(car))

               case (21)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,3
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GenParList_Par(Pat,'SC'//trim(car_n)//'_PAT'//trim(car))
                  end do

               case (22:24) ! EXTI
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  write(car_n,fmt='(i3)') Npar-21
                  car_n=adjustl(car_n)
                  call Vary_GenParList_Par(Pat,'EXTI'//trim(car_n)//'_PAT'//trim(car))

               case (25)
                  write(car,fmt='(i3)') ip
                  car=adjustl(car)
                  do i=1,3
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GenParList_Par(Pat,'EXTI'//trim(car_n)//'_PAT'//trim(car))
                  end do

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_Patt

End Submodule KeyCod_Patt