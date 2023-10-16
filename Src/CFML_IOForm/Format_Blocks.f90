!!
Submodule (CFML_IOForm) Format_Blocks
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE GET_BLOCK_KEY
   !!----
   !!---- BLOCK definition:
   !!----     KEY_StringName Number
   !!----     ....
   !!----     ....
   !!----     END_KEY_StringName
   !!----
   !!---- Update: 17/05/2023
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
     integer           :: i, j, n, nc, iv
     character(len=40) :: car
     logical           :: Debug=.false.

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
        if (line(1:1) =='!' .or. line(1:1) ==' ') then
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
           if (len_trim(line) <= 0) cycle
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
   Module Subroutine Get_Block_Commands(ffile, N_Ini, N_End)
      !---- Arguments ----!
      Type(file_type),    intent(in)  :: ffile
      integer,            intent(out) :: n_ini
      integer,            intent(out) :: n_end

      !---- Local Variables ----!
      integer            :: i, j

      !> Init
      n_Ini=0; n_End=0

      !> Determine the zone of commands in the file
      do i=1,ffile%nlines
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) <= 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (n_Ini == 0) then
            j=index(u_case(line),'COMMANDS')
            if (j > 0) n_ini=i
            cycle
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

      !> Check error
      if (n_ini > 0 .and. n_end ==0) then
         call set_error(1, "Error in COMMANDS/END_COMMANDS Block definition!")
      end if

   End Subroutine Get_Block_Commands

   !!----
   !!---- SUBROUTINE GET_SUBBLOCK
   !!----
   !!---- SUBBLOCK definition:
   !!---- It is a block into a Block
   !!----     KEY_StringName
   !!----     ....
   !!----     ....
   !!----     END_KEY_StringName
   !!----
   !!---- Date: 15/05/2023
   !!
   Module Subroutine Get_SubBlock_KEY(Key, ffile, n_ini, n_end, Ind, StrName)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: key
      Type(file_type),            intent(in)  :: ffile
      integer,                    intent(in)  :: n_ini
      integer,                    intent(in)  :: n_end
      integer, dimension(2),      intent(out) :: Ind
      character(len=*), optional, intent(out) :: StrName

      !---- Local Variables ----!
      character(len=:), allocatable    :: car
      integer                          :: i,j

      !> Init
      Ind=0
      StrName=' '
      car=u_case(trim(key))

      !> Determine the zone of Background in the file
      do i=n_ini, n_end
         line=adjustl(ffile%line(i)%str)
         if(len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

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

      if (present(StrName) .and. ind(1) > 0) then
         line=adjustl(ffile%line(ind(1))%str)
         j=index(line,'_')
         StrName=' '
         if (j > 0) StrName=line(j+1:)
      end if

   End Subroutine Get_SubBlock_KEY

   !!----
   !!---- SUBROUTINE READBLOCK_EXCLUDEREG
   !!----
   !!----     Exclude regions into a Pattern zone
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_ExcludeReg(ffile, n_ini, n_end, IPatt)
      !---- Arguments ----!
      Type(file_type),         intent(in)    :: ffile
      integer,                 intent(in)    :: n_ini
      integer,                 intent(in)    :: n_end
      integer,                 intent(in)    :: IPatt

      !---- Local Variables ----!
      integer               :: i, j, iv
      integer, dimension(2) :: Ind

      !> Init
      call clear_error()

      call Get_SubBlock_KEY('EXCLUDED_REGIONS', ffile, n_ini, n_end, Ind)
      if (all(Ind ==0)) return

      do i=Ind(1)+1, Ind(2)-1
         line=adjustl(ffile%line(i)%str)

         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         if (line(1:1) ==" ") cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         call Get_Num(line, vet, ivet, iv)
         if (iv /=2) then
            call set_error(1,'Wrong format for Exclude regions interval: '//trim(line))
            return
         end if

         NP_exreg=NP_exreg+1
         Vec_ExReg(NP_exreg)%IV(1)=IPatt             ! Pattern identification
         Vec_ExReg(NP_exreg)%NPar=2                  ! Number of parameters
         Vec_ExReg(NP_exreg)%RV(1:2)=vet(1:2)        ! Min-Max limits
      end do

   End Subroutine Read_Block_ExcludeReg

   !!----
   !!---- SUBROUTINE READBLOCK_BACKGD
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_Backgd(ffile, n_ini, n_end, IPatt)
      !---- Arguments ----!
      Type(file_type),         intent(in)    :: ffile
      integer,                 intent(in)    :: n_ini
      integer,                 intent(in)    :: n_end
      integer,                 intent(in)    :: IPatt

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
         if (len_trim(line) == 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

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
                        Vec_backgd(NP_backgd)%Str='LINEAR'
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%NPar=ic
                        Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

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
                        Vec_backgd(NP_backgd)%Str='LINEAR'
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%NPar=ic
                        Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

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
                        Vec_backgd(NP_backgd)%Str='SPLINE'
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%NPar=ic
                        Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

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
                        Vec_backgd(NP_backgd)%Str='SPLINE'
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%NPar=ic
                        Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic == 0 ) then
                           call set_error(1,'Problem reading coefficients for polynomial background: '//trim(line))
                           return
                        end if

                        if (ic <= size(Vec_backgd(NP_backgd)%RV) ) then
                            NP_backgd=NP_backgd+1
                            Vec_backgd(NP_backgd)%Str='POLYNOM'
                            Vec_backgd(NP_backgd)%IV(1)=IPatt
                            Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)
                            Vec_Backgd(NP_backgd)%NPar=ic
                            kk=kk+ic

                        else
                           call set_error(1,'The number of coefficients read in this line exceeds of the limit!')
                           return
                        end if

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call get_num(line, vet, ivet, ic)
                        if (ic ==0 ) then
                           call set_error(1,'Problem reading coefficients for chebychev background: '//trim(line))
                           return
                        end if

                        if (ic <= size(Vec_backgd(NP_backgd)%RV) ) then
                            NP_backgd=NP_backgd+1
                            Vec_backgd(NP_backgd)%Str='CHEBYCHEV'
                            Vec_backgd(NP_backgd)%IV(1)=IPatt
                            Vec_Backgd(NP_backgd)%RV(1:ic)=vet(1:ic)
                            Vec_Backgd(NP_backgd)%NPar=ic
                            kk=kk+ic

                        else
                           call set_error(1,'The number of coefficients read in this line exceeds of the limit!')
                           return
                        end if

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)   ! Label peak

                        call get_num(line, vet, ivet, ic)
                        if (ic /=3) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%Str='PKS_PVOIGT'
                        Vec_Backgd(NP_backgd)%Npar=3
                        Vec_Backgd(NP_backgd)%RV(1:3)=vet(1:3)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)   ! Label peak
                        call get_num(line, vet, ivet, ic)
                        if (ic /=3) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%Str='PKS_PVOIGT'
                        Vec_Backgd(NP_backgd)%Npar=3
                        Vec_Backgd(NP_backgd)%RV(1:3)=vet(1:3)

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
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)  ! label peak
                        call get_num(line, vet, ivet, ic)
                        if (ic /=4) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%Str='PKS_SPLITPVOIGT'
                        Vec_backgd(NP_backgd)%NPar=4
                        Vec_Backgd(NP_backgd)%RV(1:4)=vet(1:4)
                     end do

                  case (2)
                     call get_num(dire(2),vet,ivet,ic)
                     nt=ivet(1)
                     kk=0

                     do while(j <= ind(2)-2)
                        j=j+1
                        line = adjustl(ffile%line(j)%str)
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) =='!') cycle
                        if (line(1:1) ==' ') cycle

                        k=index(line,'!')
                        if (k > 0) line=line(:k-1)
                        k=index(line,'#')
                        if (k > 0) line=line(:k-1)

                        call cut_string(line)  ! Label peaks
                        call get_num(line, vet, ivet, ic)
                        if (ic /=4) then
                           call set_error(1,'Wrong number of values for Peaks_pvoigt definitions: '//trim(line))
                           return
                        end if

                        NP_backgd=NP_backgd+1
                        Vec_backgd(NP_backgd)%IV(1)=IPatt
                        Vec_backgd(NP_backgd)%Str='PKS_SPLITPVOIGT'
                        Vec_backgd(NP_backgd)%Npar=4
                        Vec_Backgd(NP_backgd)%RV(1:4)=vet(1:4)

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

   End Subroutine Read_Block_Backgd

   !!----
   !!---- SUBROUTINE READBLOCK_INSTRUCTIONS
   !!----
   !!----
   !!---- Update: 12/05/2023
   !!
   Module Subroutine Read_Block_Instructions(ffile, N_ini, N_end, LSymm)
      !---- Arguments ----!
      type(File_type),   intent(in) :: Ffile
      integer,           intent(in) :: N_ini
      integer,           intent(in) :: N_end
      logical, optional, intent(in) :: LSymm

      !---- Local Variables ----!
      logical               :: exc_reg, exc_bck, exc_symm
      integer               :: i, j, k, kk, ic, iv
      integer, dimension(2) :: Ind1, Ind2
      character(len=10)     :: str
      character(len=132)    :: linec


      !> Init
      Ind1=0; Ind2=0
      exc_symm=.false.
      if (present(lsymm)) exc_symm=lsymm

      call clear_error()

      !> Exclude regions
      exc_reg=.false.
      call Get_SubBlock_KEY('EXCLUDED_REGIONS', ffile, n_ini, n_end, Ind1)
      if (all(ind1 >0)) exc_reg=.true.

      !> BackGD
      exc_bck=.false.
      call Get_SubBlock_KEY('BACKGD', ffile, n_ini, n_end, Ind2)
      if (all(ind2 >0)) exc_bck=.true.

      !> Main bucle
      i=n_ini+1
      do while (i <= n_end-1)
         !> Exclusion zones
         if (exc_reg) then
            if (i >= ind1(1) .and. i <= ind1(2)) then
               i=ind1(2)+1
               cycle
            end if
         end if

         if (exc_bck) then
            if (i >= ind2(1) .and. i <= ind2(2)) then
               i=ind2(2)+1
               cycle
            end if
         end if

         line=adjustl(ffile%line(i)%str)

         if (len_trim(line) == 0) then
            i=i+1
            cycle
         end if

         if (line(1:1) =="!" .or. line(1:1)==' ') then
            i=i+1
            cycle
         end if

         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         if (exc_symm) then
            !> Special cases on symmetry / space groups
            linec = line
            call cut_string(linec, ic, str)
            str=adjustl(u_case(str))
            j=string_count(linec,';')

            select case (trim(str))
               case ('SPG','SSPG','MSSPG','SPGR','SHUB')
                  select case (j)
                     case(0)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case(1)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case default
                        call set_error(1,'Format not defined! ->'//trim(str)//' '//trim(linec))
                        return
                  end select

                  i=i+1
                  cycle

               case ('GEN','SYMM','GENERATORS')
                  select case (j)
                     case(0)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        Vec_Instr(NP_Instr)%NPar=1
                        Vec_Instr(NP_Instr)%CV(1)=trim(linec)

                     case(1:)
                        NP_Instr=NP_Instr+1
                        Vec_Instr(NP_Instr)%Str=trim(str)
                        kk=0
                        do while(len_trim(linec) > 0)
                           k=index(linec,';')
                           if (k > 0) then
                              kk=kk+1
                              Vec_Instr(NP_Instr)%CV(kk)=linec(:k-1)
                              linec=linec(k+1:)

                           else
                              kk=kk+1
                              Vec_Instr(NP_Instr)%CV(kk)=linec
                              linec=''
                           end if
                        end do

                     case default
                        call set_error(1,'Format not defined! ->'//trim(str)//' '//trim(line))
                        return
                  end select

                  i=i+1
                  cycle
            end select
         end if

         do while(len_trim(line) > 0)
            j=index(line,';')
            if (j > 0) then
               linec=line(:j-1)
               line=line(j+1:)
            else
               linec=trim(line)
               line=''
            end if

            call get_words(linec, dire, ic)
            if (ic < 1 .or. ic > 15) then
               call set_error(1,'The directive has a wrong number of parameters! '//trim(linec))
               return
            end if

            NP_Instr=NP_Instr+1
            Vec_Instr(NP_Instr)%Str=trim(u_case(dire(1)))
            Vec_Instr(NP_Instr)%NPar=ic-1
            do j=2,ic
               call get_num(dire(j),vet,ivet,iv)
               if (iv <=0) then
                  Vec_Instr(NP_Instr)%CV(j-1)=trim(dire(j))
               else
                  Vec_Instr(NP_Instr)%IV(j-1)=ivet(1)
                  Vec_Instr(NP_Instr)%RV(j-1)=vet(1)
               end if
            end do

         end do

         i=i+1
      end do

   End Subroutine Read_Block_Instructions

   !!----
   !!---- SUBROUTINE GET_ZONEPATTERNS
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_Block_Patterns(ffile, N_Ini, N_End, NPatt, Patt, Ex_Ind)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NPatt
      type(BlockInfo_Type), dimension(:), intent(in out) :: Patt
      integer, dimension(2), optional,    intent(in)     :: Ex_ind


      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, n_fin, N_id
      integer, dimension(2) :: Ind, Indx
      logical               :: exclusion=.false.

      !> Init
      NPatt=0
      Indx=0
      if (present(Ex_Ind)) then
         exclusion=.true.
         Indx=Ex_ind
      end if

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         !> Exclude zone
         if (exclusion) then
            if (i < Indx(1)-1) n_fin=Indx(1)-1
            if (i >=Indx(1) .and. i <=Indx(2)) then
               n_fin=ffile%nlines
               i=Indx(2)+1
               cycle
            end if
         end if

         call Get_Block_KEY('PATTERN', ffile, i, n_fin, Ind, StrName, N_Id)
         if (Err_CFML%IErr /= 0) return

         if (all(ind == 0)) then
            i=i+1
            cycle
         end if

         select case (N_Id)
            case (0)
               if (NPatt ==0) then
                  NPatt=1

                  Patt(1)%StrName=trim(StrName)
                  Patt(1)%BlName='PATTERN'
                  Patt(1)%IBl=2
                  Patt(1)%Nl=Ind
               else
                  call set_error(1,'There is a previous Pattern block defined as 1!')
                  return
               end if

            case (1:MAX_PATTERNS)
               if (Patt(N_id)%IBl == 1) then
                  call set_error(1,'There is a previous Pattern Block defined with the same identificator!')
                  return
               end if

               Patt(N_id)%StrName=trim(StrName)
               Patt(N_id)%BlName='PATTERN'
               Patt(N_id)%IBl=2
               Patt(N_id)%Nl=Ind

               NPatt=NPatt+1
         end select

         i=ind(2)+1
      end do

   End Subroutine Get_Block_Patterns

   !!----
   !!---- SUBROUTINE GET_ZONEPHASES
   !!----
   !!---- Date: 11/05/2022
   !!
   Module Subroutine Get_Block_Phases(ffile, N_Ini, N_End, NPhas, Phas, Ex_Ind)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NPhas
      type(BlockInfo_Type), dimension(:), intent(in out) :: Phas
      integer, dimension(2), optional,    intent(in)     :: Ex_ind


      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, n_fin, N_Id
      integer, dimension(2) :: Ind, Indx
      logical               :: exclusion=.false.

      !> Init
      NPhas=0
      Indx=0
      if (present(Ex_Ind)) then
         exclusion=.true.
         Indx=Ex_ind
      end if

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         !> Exclude zone
         if (exclusion) then
            if (i < Indx(1)-1) n_fin=Indx(1)-1
            if (i >=Indx(1) .and. i <=Indx(2)) then
               n_fin=ffile%nlines
               i=Indx(2)+1
               cycle
            end if
         end if

         call Get_Block_KEY('PHASE', ffile, i, n_fin, Ind, StrName, N_Id)

         if (Err_CFML%IErr /= 0) return
         if (all(ind == 0)) then
            i=i+1
            cycle
         end if

         select case (N_Id)
            case (0)
               if (NPhas ==0) then
                  NPhas=1

                  Phas(1)%StrName=trim(StrName)
                  Phas(1)%BlName='PHASE'
                  Phas(1)%IBl=1
                  Phas(1)%Nl=Ind
               else
                  call set_error(1,'There is a previous Phase block defined as 1!')
                  return
               end if

            case (1:MAX_PHASES)
               if (Phas(N_id)%IBl == 1) then
                  call set_error(1,'There is a previous Phase Block defined with the same identificator!')
                  return
               end if

               Phas(N_id)%StrName=trim(StrName)
               Phas(N_id)%BlName='PHASE'
               Phas(N_id)%IBl=1
               Phas(N_id)%Nl=Ind

               NPhas=NPhas+1
         end select

         i=ind(2)+1
      end do

   End Subroutine Get_Block_Phases

   !!----
   !!---- SUBROUTINE GET_BLOCK_MOLEX
   !!----
   !!---- Date: June- 2023
   !!
   Module Subroutine Get_SubBlock_MolPhases(ffile, N_Ini, N_End, NMol, Mol)
      !---- Arguments ----!
      Type(file_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: n_ini
      integer,                            intent(in)     :: n_end
      integer,                            intent(out)    :: NMol
      type(BlockInfo_Type), dimension(:), intent(in out) :: Mol

      !---- Local Variables ----!
      character(len=60)     :: StrName
      integer               :: i, nc, n_fin, iv, n
      integer, dimension(2) :: Ind

      !> Init
      NMol=0

      i=N_ini
      n_fin=N_end

      do while (i < N_end)
         call Get_SubBlock_KEY('MOLEX', ffile, i, n_fin, Ind, StrName)
         if (Err_CFML%IErr /= 0) return
         if (all(ind == 0)) then
            i=i+1
            cycle
         end if

         call get_words(strname,dire,nc)
         n=0
         if (nc ==3) then
            call get_num(dire(3),vet, ivet, iv)
            if (iv == 1) n=ivet(1)
         end if

         NMol=NMol+1

         Mol(NMol)%StrName=trim(dire(1))
         Mol(NMol)%BlName='MOLEX'
         Mol(NMol)%IBl=3
         Mol(NMol)%Nl=Ind
         Mol(NMol)%Iex=n

         i=ind(2)+1
      end do

   End Subroutine Get_SubBlock_MolPhases

   !!----
   !!---- Subroutine Write_InfoBlock_Backgd
   !!----
   !!----    Write the information about Background Blocks in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine Write_InfoBlock_Backgd(IPatt, Iunit)
      !---- Arguments ----!
      integer,             intent(in) :: IPatt
      integer, optional,   intent(in) :: Iunit

      !---- Local variables ----!
      integer :: lun
      integer :: i,j,k
      logical :: info
      character(len=3) :: car
      character(len=30):: fmtt

      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Backgd <= 0) return
      if (.not. any(Vec_Backgd%IV(1) == IPatt)) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i4)") " Background Definition for Pattern: ",IPatt
      write(unit=lun, fmt="(a)") " "

      !> Linear Interpolation
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='LINEAR') then
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
            k=Vec_backgd(i)%Npar
            write(car,fmt='(i3)') k
            car=adjustl(car)
            fmtt='('//trim(car)//'f12.3'//')'

            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='LINEAR') then
               write(unit=lun, fmt=trim(fmtt)) vec_backgd(i)%RV(1:k)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Spline Interpolation
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='SPLINE') then
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
            k=Vec_backgd(i)%Npar
            write(car,fmt='(i3)') k
            car=adjustl(car)
            fmtt='('//trim(car)//'f12.3'//')'
            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='SPLINE') then
               write(unit=lun, fmt=fmtt) vec_backgd(i)%RV(1:k)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Polynomial
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='POLYNOM') then
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
            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='POLYNOM') then
                do j=1,Vec_backgd(i)%NPar
                   line = trim(line)//'  '//String_Real(vec_backgd(i)%RV(j),10)
                end do
            end if
         end do
         write(unit=lun, fmt="(a)") " "//trim(line)
         write(unit=lun, fmt="(a)") " "
      end if

      !> Chebychev
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='CHEBYCHEV') then
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
            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='CHEBYCHEV') then
               do j=1, Vec_backgd(i)%Npar
                  line = trim(line)//'  '//String_Real(vec_backgd(i)%RV(j),10)
               end do
            end if
         end do
         write(unit=lun, fmt="(a)") " "//trim(line)
         write(unit=lun, fmt="(a)") " "
      end if

      !> Peaks_pVoigt
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='PKS_PVOIGT') then
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
            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='PKS_PVOIGT') then
               j=j+1
               write(car,fmt='(i3)') j
               car=adjustl(car)
               write(unit=lun, fmt="(a,5x,f8.3, f12.3, 4x,f8.3)") " peak"//trim(car),vec_backgd(i)%RV(1:3)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

      !> Peaks_Split_pVoigt
      info=.false.
      do i=1, NP_backgd
         if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='PKS_SPLITPVOIGT') then
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
            if (Vec_backgd(i)%IV(1) ==IPatt .and. vec_backgd(i)%Str=='PKS_SPLITPVOIGT') then
               j=j+1
               write(car,fmt='(i3)') j
               car=adjustl(car)
               write(unit=lun, fmt="(a,5x,f8.3,7x,f12.3, f10.2, 2x, f10.2)") " peak"//trim(car),vec_backgd(i)%RV(1:4)
            end if
         end do
         write(unit=lun, fmt="(a)") " "
      end if

   End Subroutine Write_InfoBlock_Backgd

   !!----
   !!---- Subroutine Write_InfoBlock_ExcludedRegions
   !!----
   !!----    Write the information about Excluded Regions in file associated with
   !!----    logical unit "iunit".
   !!----    If no argument is passed the standard output (iunit=6) is used
   !!----
   !!---- Update: May - 2023
   !!
   Module Subroutine Write_InfoBlock_ExcludedRegions(IPatt, Iunit)
      !---- Arguments ----!
      integer,             intent(in) :: IPatt
      integer, optional,   intent(in) :: Iunit

      !---- Local variables ----!
      integer :: i, j, lun

      lun=6
      if (present(iunit)) lun=iunit

      if (NP_ExReg <= 0) return
      if (.not. any(Vec_ExReg%IV(1) == IPatt)) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i4)") " Excluded Regions for Pattern: ",IPatt
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,a)")"  Region             Start          End"

      j=0
      do i=1,NP_ExReg
         if (Vec_ExReg(i)%IV(1) /= IPatt) cycle

         j=j+1
         write(unit=lun,fmt="(i6,5x,2f15.3)") j, Vec_ExReg(i)%RV(1:2)
      end do

   End Subroutine Write_InfoBlock_ExcludedRegions

   !!----
   !!---- SUBROUTINE Get_SubBlock_CommPatterns
   !!----
   !!----    Determine the zone of Pattern Blocks into a Command Zone
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Get_SubBlock_CommPatterns(ffile, N_ini, N_end, Bl_Patt, NPatt, C_Patt)
      !---- Arguments ----!
      type(File_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: N_ini
      integer,                            intent(in)     :: N_end
      type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Patt
      integer,                            intent(out)    :: Npatt
      type(BlockInfo_Type), dimension(:), intent(in out) :: C_Patt

      !---- Local Variables ----!
      integer               :: i, iv
      integer, dimension(2) :: ind
      character(len=60)     :: StrName

      !> Init
      NPatt=0

      i=N_ini

      do while(i < n_end)
         call Get_SubBlock_KEY('PATTERN', ffile, i, n_end, Ind, StrName)
         if (all(ind > 0)) then
            NPatt=NPatt+1

            call get_num(strName, vet, ivet, iv)
            if (iv > 0) then
               C_Patt(NPatt)%StrName=Bl_Patt(ivet(1))%StrName
            else
               C_Patt(NPatt)%StrName=trim(StrName)
            end if
            call clear_error()

            C_Patt(NPatt)%BlName='COM_PATTERN'
            C_Patt(NPatt)%IBl=2
            C_Patt(NPatt)%Nl=Ind

            i=Ind(2)+1

         else
            exit
         end if
      end do

   End Subroutine Get_SubBlock_CommPatterns

   !!----
   !!---- SUBROUTINE Get_SubBlock_CommPhases
   !!----
   !!----    Determine the zone of Pattern Blocks into a Command Zone
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Get_SubBlock_CommPhases(ffile, N_ini, N_end, Bl_Phas, NPhas, C_Phas)
      !---- Arguments ----!
      type(File_type),                    intent(in)     :: ffile
      integer,                            intent(in)     :: N_ini
      integer,                            intent(in)     :: N_end
      type(BlockInfo_Type), dimension(:), intent(in)     :: Bl_Phas
      integer,                            intent(out)    :: Nphas
      type(BlockInfo_Type), dimension(:), intent(in out) :: C_Phas

      !---- Local Variables ----!
      integer               :: i, iv
      integer, dimension(2) :: ind
      character(len=60)     :: StrName

      !> Init
      NPhas=0

      i=N_ini

      do while(i < n_end)
         call Get_SubBlock_KEY('PHASE', ffile, i, n_end, Ind, StrName)
         if (all(ind > 0)) then
            NPhas=NPhas+1

            call get_num(strName, vet, ivet, iv)
            if (iv > 0) then
               C_Phas(NPhas)%StrName=Bl_Phas(ivet(1))%StrName
            else
               C_Phas(NPhas)%StrName=trim(StrName)
            end if
            call clear_error()

            C_Phas(NPhas)%BlName='COM_PHASE'
            C_Phas(NPhas)%IBl=1
            C_Phas(NPhas)%Nl=Ind

            i=Ind(2)+1

         else
            exit
         end if
      end do
   End Subroutine Get_SubBlock_CommPhases

   !!----
   !!---- SUBROUTINE Get_Blocks_Filetype
   !!----
   !!----
   !!---- June - 2023
   !!----
   Module Subroutine Get_Blocks_Filetype(ffile, NComm, Bl_Comm, NPatt, Bl_Patt, &
                                         NPhas, Bl_Phas, NCPatt, BlC_Patt, NCPhas, BlC_Phas)
      !---- Arguments ----!
      type(File_type),                               intent(in)  :: ffile
      integer,                            optional,  intent(out) :: NComm    ! Number of Commands zone
      type(BlockInfo_Type),               optional,  intent(out) :: Bl_Comm  ! Command block info
      integer,                            optional,  intent(out) :: NPatt    ! Number of Patterns zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Patt  ! Patterns block info
      integer,                            optional,  intent(out) :: NPhas    ! Number of Phases zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: Bl_Phas  ! Phases block info
      integer,                            optional,  intent(out) :: NCPatt   ! Number of Patterns into Commands zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Patt ! Patterns block info
      integer,                            optional,  intent(out) :: NCPhas   ! Number of Phases into Commands zone
      type(BlockInfo_Type), dimension(:), optional,  intent(out) :: BlC_Phas ! Phases block info

      !---- Local Variables ----!
      integer              :: n_ini, n_end, nbc
      type(BlockInfo_Type) :: BC

      !> Init
      call clear_error()

      nbc=0

      call Get_Block_Commands(ffile, N_Ini, N_End)
      if (Err_CFML%IErr /= 0) return

      if (n_ini > 0 .and. n_end >= n_ini) then
         BC%Nl(1)=n_ini
         BC%Nl(2)=n_end
         BC%IBl=0
         BC%BlName='COMMAND'
         Nbc=1
      end if

      !> Command Zone
      if (present(NComm) .and. present(Bl_Comm)) then
         NComm=0
         if (nbc > 0) then
            Bl_Comm= BC
            NComm=1
         end if
      end if

      !> Patterns Zone
      if (present(NPatt) .and. present(Bl_Patt)) then
         NPatt=0

         n_ini=1
         n_end=ffile%nlines
         if (Nbc > 0) then
            call Get_Block_Patterns(ffile, n_ini, n_end, NPatt, Bl_Patt, BC%Nl)
         else
            call Get_Block_Patterns(ffile, n_ini, n_end, NPatt, Bl_Patt)
         end if
      end if

      !> Phases Zone
      if (present(NPhas) .and. present(Bl_Phas)) then
         NPhas=0

         n_ini=1
         n_end=ffile%nlines
         if (Nbc > 0) then
            call Get_Block_Phases(ffile, n_ini, n_end, NPhas, Bl_Phas, BC%Nl)
         else
            call Get_Block_Phases(ffile, n_ini, n_end, NPhas, Bl_Phas)
         end if
      end if

      !> Patterns sublocks into Command zone
      if (present(NCPatt) .and. present(BlC_Patt) .and. present(Bl_Patt)) then
         NCPatt=0

         if (nbc > 0) then
            n_ini=BC%Nl(1)
            n_end=BC%Nl(2)
            call Get_SubBlock_CommPatterns(ffile, N_ini, N_end, Bl_Patt, NCPatt, BlC_Patt)
         end if
      end if

      !> Phases sublocks into Command zone
      if (present(NCPhas) .and. present(BlC_Phas) .and. present(Bl_Phas)) then
         NCPhas=0

         if (nbc > 0) then
            n_ini=BC%Nl(1)
            n_end=BC%Nl(2)
            call Get_SubBlock_CommPhases(ffile, N_ini, N_end, Bl_Phas, NCPhas, BlC_Phas)
         end if
      end if

   End Subroutine Get_Blocks_FileType

End SubModule Format_Blocks