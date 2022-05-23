!!
Submodule (CFML_KeyCodes) KeyCod_Blocks
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE GET_BLOCK_KEY
   !!----
   !!----      %Pattern N
   !!----      .....
   !!----      %EndPattern
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Get_Block_KEY(Key, ffile, N_Ini, N_End, Nkey, Ind)
      !---- Arguments ----!
      character(len=*),        intent(in)  :: Key         ! 'Pattern','Molec',....
      Type(file_type),         intent(in)  :: ffile
      integer,                 intent(in)  :: n_ini
      integer,                 intent(in)  :: n_end
      integer,                 intent(out) :: Nkey
      integer, dimension(:,:), intent(out) :: Ind    ! (1,-):Start; (2,-): End

     !---- Local Arguments ----!
     logical                          :: Debug=.false.
     integer                          :: i,j,k,n,nc,iv,kmax
     character(len=3)                 :: car

     !> Init
     car=u_case(key)

     NKey=0
     Ind=0
     kmax=0

     i=N_ini
     do while(i <= N_end)
        line=adjustl(ffile%line(i)%str)

        if (line(1:1) =='!') then
           i=i+1
           cycle
        end if
        if (line(1:1) ==' ') then
           i=i+1
           cycle
        end if

        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)

        j=index(u_case(line),'%'//car)
        if (j <=0) then
           i=i+1
           cycle
        end if

        call cut_string(line)

        k=0
        if (len_trim(line) ==0) then
           k=1
        else
           call get_words(line, dire, nc)
           call get_num(dire(1), vet, ivet, iv)
           if (iv < 1) then
              call set_error(-1, " You have to give the number indentification in Block definition")
              return
           end if
           k=ivet(1)

           !> Check if k > Dim_MAX
           if (size(ind,dim=2) < k) then
              call set_error(1, " You are given an outrange for number indentification in Block definition")
              return
           end if
        end if
        kmax=max(kmax,k)

        do n=i+1,n_end
           line=adjustl(ffile%line(n)%str)
           if (line(1:1) =='!') cycle
           if (line(1:1) ==' ') cycle

           j=index(u_case(line),'%END'//car)
           if (j <= 0) cycle

           Ind(1,k)=i+1
           Ind(2,k)=n-1

           i=n
           exit
        end do
        i=i+1
     end do

     do i=1, kmax
        if (Ind(1,i) == 0) cycle

        if (Ind(1,i) > 0 .and. Ind(2,i) ==0) then
           call set_error(1,"Error in Block definition! Please, check it")
           NKey=0
           return

        else if (Ind(1,i) > Ind(2,i) ) then
           !> Empty block
           Ind(1,i) =0
           Ind(2,i) =0

        else
           NKey=NKey+1
        end if
     end do

     !> Debug
     if (debug .and. nKey > 0) then
        write(unit=*,fmt='(a,i3)') 'Number of Blocks readed: ',NKey
        do i=1,kmax
           if (Ind(1,i)==0) cycle
           write(unit=*,fmt='(a,i3,2x,a,i5,2x,a,i5)') 'Key: '//car, i, 'Ini: ',Ind(1,i), 'End: ',Ind(2,i)
        end do
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
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         j=index(line,'!')
         if (j > 0) line=line(:j-1)

         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         !> N_ini point the next line into Command zone
         if (n_Ini == 0) then
            j=index(u_case(line),'COMMA')
            if (j > 0) then
               n_ini=i+1
               cycle
            end if
         end if

         !> N_end point the previous line from end Command zone
         if (n_ini > 0 .and. i >= n_ini) then
            j=index(u_case(line),'END COM')
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