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
   Module Subroutine Get_Block_KEY(Key, ffile, N_Ini, N_End, Nb, Ind, ID_Str)
      !---- Arguments ----!
      character(len=*),              intent(in)  :: Key         ! 'Pattern','Molec',....
      Type(file_type),               intent(in)  :: ffile
      integer,                       intent(in)  :: n_ini
      integer,                       intent(in)  :: n_end
      integer,                       intent(out) :: Nb          ! Number of blocks
      integer, dimension(:,:),       intent(out) :: Ind         ! 1:Start; 2: End
      character(len=*),dimension(:), intent(out) :: ID_Str      ! String identification

     !---- Local Arguments ----!
     logical                          :: Debug=.false.

     integer                          :: i,j,k,n,nc,iv
     character(len=40)                :: car,str

     !> Init
     Ind=0
     ID_Str=' '

     car=u_case(trim(key))
     k=0

     i=N_ini
     do while(i <= N_end)
        line=adjustl(ffile%line(i)%Str_tmp)

        !> No comments, No blank lines
        if (line(1:1) =='!') then
           i=i+1
           cycle
        end if
        if (line(1:1) ==' ') then
           i=i+1
           cycle
        end if

        !> Purge comments
        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)

        !> Block type
        j=index(u_case(line),'%'//trim(car))
        if (j <=0) then
           i=i+1
           cycle
        end if

        call cut_string(line)

        !> Identification String
        call get_words(line, dire, nc)
        if ( nc /= 1) then
           call set_error(-1, " You have to give an identification name in Block definition")
           return
        end if

        str=adjustl(dire(1))

        do n=i+1,n_end
           line=adjustl(ffile%line(n)%Str_tmp)
           if (line(1:1) =='!') cycle
           if (line(1:1) ==' ') cycle

           j=index(u_case(line),'%END_'//trim(car))
           if (j <= 0) cycle

           !> Max dimension for Ind and ID_Str. Pay attention
           k=k+1
           Ind(1,k)=i+1
           Ind(2,k)=n-1
           ID_Str(k)=adjustl(str)

           i=n
           exit
        end do
        i=i+1
     end do

     Nb = k

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
         line=adjustl(ffile%line(i)%Str_tmp)
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)

         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (n_Ini == 0) then
            j=index(u_case(line),'COMMAND')
            if (j > 0) then
               n_ini=i+1
               cycle
            end if
         end if

         !> N_end point the previous line from end Command zone
         if (n_ini > 0 .and. i >= n_ini) then
            j=index(u_case(line),'END_COMM')
            if (j > 0) then
               n_End=i-1
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