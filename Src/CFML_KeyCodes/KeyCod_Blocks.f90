!!
Submodule (CFML_KeyCodes) KeyCod_Blocks
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
     integer, dimension(3)            :: ivet
     character(len=:), allocatable    :: car, str
     real, dimension(3)               :: vet

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

End SubModule KeyCod_Blocks