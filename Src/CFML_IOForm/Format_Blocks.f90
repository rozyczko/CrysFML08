!!
Submodule (CFML_IOForm) Format_Blocks
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE GET_BLOCK_KEY
   !!----
   !!----      %Pattern ID_String
   !!----      .....
   !!----      %EndPattern
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Get_Block_KEY(Key, ffile, N_Ini, N_End, Ind, StrName, N_Id)
      !---- Arguments ----!
      character(len=*),              intent(in)  :: Key         ! 'Pattern','Phase',....
      Type(file_type),               intent(in)  :: ffile
      integer,                       intent(in)  :: n_ini
      integer,                       intent(in)  :: n_end
      integer, dimension(2),         intent(out) :: Ind         ! Start; End
      character(len=*),              intent(out) :: StrName     ! String identification
      integer,                       intent(out) :: N_Id        ! Number of ID

     !---- Local Arguments ----!
     logical                          :: Debug=.true.

     integer                          :: i,j,k,n,nc,iv
     character(len=:), allocatable    :: car, str

     !> Init
     Ind=0
     StrName=' '
     N_Id=0

     car=u_case(trim(key))

     i=N_ini
     do while(i <= N_end)
        line=adjustl(ffile%line(i)%str)

        !> No blank lines
        if (len_trim(line) ==0 ) then
           i=i+1
           cycle
        end if

        !> No comments
        if (line(1:1) =='!') then
           i=i+1
           cycle
        end if

        !> Purge comments
        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)

        !> Block type
        j=index(u_case(line), trim(car)//'_')
        if (j <=0) then
           i=i+1
           cycle
        end if

        !> Identification String
        j=index(u_case(line), '_')
        call get_words(line(j+1:), dire, nc)
        select case (nc)
           case (1)
              strname=adjustl(dire(1))

           case (2)
              strname=adjustl(dire(1))
              call get_num(dire(2), vet, ivet, iv)
              if (iv ==1) N_Id=ivet(1)

           case default
              call set_error(-1, " Error in the format of the Block definition")
              return
        end select

        do n=i+1,n_end
           line=adjustl(ffile%line(n)%str)
           if (line(1:1) =='!') cycle
           if (line(1:1) ==' ') cycle

           j=index(u_case(line),'END_'//trim(car)//'_'//trim(u_case(strname)))
           if (j <= 0) cycle

           !> Max dimension for Ind and ID_Str. Pay attention
           Ind(1)=i
           Ind(2)=n

           exit
        end do
        exit
     end do

     !> Debug information
     if (Debug .and. any(ind /=0)) then
        write(unit=*,fmt='(a)') ' '
        write(unit=*,fmt='(a)') ' '//trim(car)//': '//trim(strname)
        write(unit=*,fmt='(a, i5)') '    Num. ID: ',N_Id
        write(unit=*,fmt='(a, i5)') ' Start line: ',ind(1)
        write(unit=*,fmt='(a, i5)') '   End line: ',ind(2)
        write(unit=*,fmt='(a)') ' '
     end if

  End Subroutine Get_Block_Key

   !!----
   !!---- SUBROUTINE GET_ZONECOMMANDS
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_ZoneCommands(ffile, N_Ini, N_End)
      !---- Arguments ----!
      Type(file_type),    intent(in)  :: ffile
      integer,            intent(out) :: n_ini
      integer,            intent(out) :: n_end

      !---- Local Variables ----!
      logical            :: Debug =.false.
      integer            :: i,j

      !> Init
      n_Ini=0; n_End=0

      !> Determine the zone of commands in the file
      do i=1,ffile%nlines
         line=adjustl(ffile%line(i)%str)
         if(len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)

         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (n_Ini == 0) then
            j=index(u_case(line),'COMMANDS')
            if (j > 0) then
               n_ini=i
               cycle
            end if
         end if

         !> N_end point the previous line from end Command zone
         if (n_ini > 0 .and. i >= n_ini) then
            j=index(u_case(line),'END_COMMANDS')
            if (j > 0) then
               n_End=i
               exit
            end if
         end if
      end do

      !> Debug Info
      if (debug) then
         if (n_ini ==0 .or. n_end ==0) then
            write(unit=*,fmt="(/,a)") " GET_ZONECOMMANDS: Cannot identify the COMMANDS Zone!"
         end if
      end if

   End Subroutine Get_ZoneCommands

   !!----
   !!---- SUBROUTINE GET_SUBBLOCK
   !!----
   !!---- Date: 15/05/2023
   !!
   Module Subroutine Get_SubBlock_KEY(Key, ffile, n_ini, n_end, Ind)
      !---- Arguments ----!
      character(len=*),      intent(in)  :: key
      Type(file_type),       intent(in)  :: ffile
      integer,               intent(in)  :: n_ini
      integer,               intent(in)  :: n_end
      integer, dimension(2), intent(out) :: Ind

      !---- Local Variables ----!
      character(len=:), allocatable    :: car
      integer                          :: i,j
      logical                          :: Debug =.false.


      !> Init
      Ind=0
      car=u_case(trim(key))

      !> Determine the zone of Background in the file
      do i=n_ini, n_end
         line=adjustl(ffile%line(i)%str)
         if(len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)

         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (Ind(1) == 0) then
            j=index(u_case(line),trim(car))
            if (j > 0) then
               Ind(1)=i
               cycle
            end if
         end if

         !> N_end point the previous line from end Command zone
         if (Ind(1) > 0 .and. i >= Ind(1)) then
            j=index(u_case(line),'END_'//trim(car))
            if (j > 0) then
               ind(2)=i
               exit
            end if
         end if
      end do

   End Subroutine Get_SubBlock_KEY

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
   !!---- Subroutine WriteInfo_Backgd_Block
   !!----
   !!----    Write the information about Background Blocks in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine WriteInfo_Backgd_Block(Ip, Iunit)
      !---- Arguments ----!
      integer,             intent(in) :: Ip
      integer, optional,   intent(in) :: Iunit

      !---- Local variables ----!
      integer :: lun
      integer :: i,j
      logical :: info
      character(len=3) :: car

      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Backgd <= 0) return
      if (.not. any(Vec_Backgd%Ic == Ip)) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i4)") " Background Definition for Pattern: ",Ip
      write(unit=lun, fmt="(a)") " "

      !> Linear Interpolation
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='LINEAR') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") " Background points using linear interpolation"
         write(unit=lun, fmt="(a)") " --------------------------------------------"
         write(unit=lun, fmt="(a)") " "
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='LINEAR') then
               write(unit=lun, fmt="(2f12.3)") vec_backgd(i)%V(1:2)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Spline Interpolation
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='SPLINE') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") " Background points using Spline interpolation"
         write(unit=lun, fmt="(a)") " --------------------------------------------"
         write(unit=lun, fmt="(a)") " "
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='SPLINE') then
               write(unit=lun, fmt="(2f12.3)") vec_backgd(i)%V(1:2)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Polynomial
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='POLYNOM') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") " Background polynomial coefficients"
         write(unit=lun, fmt="(a)") " ----------------------------------"
         write(unit=lun, fmt="(a)") " "

         line=' Coeff: '
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='POLYNOM') then
                line = trim(line)//'  '//String_Real(vec_backgd(i)%V(1),10)
            end if
         end do
         write(unit=lun, fmt="(a)") " "//trim(line)
         write(unit=lun, fmt="(a)") " "
      end if

      !> Chebychev
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='CHEBYCHEV') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") " Background Chebychev coefficients"
         write(unit=lun, fmt="(a)") " ----------------------------------"
         write(unit=lun, fmt="(a)") " "

         line=' Coeff: '
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='CHEBYCHEV') then
                line = trim(line)//'  '//String_Real(vec_backgd(i)%V(1),10)
            end if
         end do
         write(unit=lun, fmt="(a)") " "//trim(line)
         write(unit=lun, fmt="(a)") " "
      end if

      !> Peaks_pVoigt
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='PKS_PVOIGT') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") "          Peaks pVoigt parameters"
         write(unit=lun, fmt="(a)") " ----------------------------------------------"
         write(unit=lun, fmt="(a)") "   !        Position    Intensity      FWHM"

         j=0
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='PKS_PVOIGT') then
               j=j+1
               write(car,fmt='(i3)') j
               car=adjustl(car)
               write(unit=lun, fmt="(a,5x,f8.3, f12.3, 4x,f8.3)") " peak"//trim(car),vec_backgd(i)%V(1:3)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Peaks_Split_pVoigt
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='PKS_SPLITPVOIGT') then
            info=.true.
            exit
         end if
      end do

      if (info) then
         write(unit=lun, fmt="(a)") " "
         write(unit=lun, fmt="(a)") "        Peaks Split pVoigt parameters"
         write(unit=lun, fmt="(a)") " ----------------------------------------------"
         write(unit=lun, fmt="(a)") "    !       Position         Intensity    Left-FWHM   Right-FWHM"

         j=0
         do i=1, NP_backgd
            if (Vec_backgd(i)%Ic ==Ip .and. vec_backgd(i)%Str=='PKS_SPLITPVOIGT') then
               j=j+1
               write(car,fmt='(i3)') j
               car=adjustl(car)
               write(unit=lun, fmt="(a,5x,f8.3,7x,f12.3, f10.2, 2x, f10.2)") " peak"//trim(car),vec_backgd(i)%V
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

   End Subroutine WriteInfo_Backgd_Block

   !!----
   !!---- Subroutine WriteInfo_ExcludedRegions
   !!----
   !!----    Write the information about Excluded Regions in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine WriteInfo_ExcludedRegions(Ip, Iunit)
      !---- Arguments ----!
      integer,             intent(in) :: Ip
      integer, optional,   intent(in) :: Iunit

      !---- Local variables ----!
      integer :: i,j,lun

      lun=6
      if (present(iunit)) lun=iunit

      if (NP_ExReg <= 0) return
      if (.not. any(Vec_ExReg%Ic == Ip)) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i4)") " Excluded Regions for Pattern: ",Ip
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,a)")"  Region             Start          End"

      j=0
      do i=1,NP_ExReg
         if (Vec_ExReg(i)%Ic /= Ip) cycle
         j=j+1
         write(unit=lun,fmt="(i6,5x,2f15.3)") j, Vec_ExReg(i)%V(1:2)
      end do

   End Subroutine WriteInfo_ExcludedRegions

End SubModule Format_Blocks