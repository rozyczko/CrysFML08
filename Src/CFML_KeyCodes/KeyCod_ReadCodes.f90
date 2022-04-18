!!
Submodule (CFML_KeyCodes) KeyCod_ReadCodes
   implicit none

   Contains
   !!--++
   !!--++ SUBROUTINE READ_REFCODES_ATM
   !!--++
   !!--++    Subroutine for treatment of Codes controls
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Read_RefCodes_ATM(ffile, n_ini, n_end, Spg, Atlist)
      !---- Arguments ----!
      Type(file_type),    intent(in)     :: ffile
      integer,            intent(in)     :: n_ini
      integer,            intent(in)     :: n_end
      class (SpG_type),   intent(in)     :: Spg
      type(AtList_Type),  intent(in out) :: AtList

      !---- Local variables ----!
      integer, parameter :: NMAX_GEN = 20

      character(len=40),dimension(NMAX_GEN) :: dir_gen, dir_loc, dir_lab
      integer                               :: ndir, nloc, nc
      integer                               :: i,j,k,nlong
      integer, dimension(NMAX_GEN)          :: Ind_gen, Ind_loc,Iph

      !> Init
      call clear_error()

      do i=n_ini,n_end
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX .....
               call cut_string(line,nlong)

               !> general directives
               call split_genrefcod_atm(line,ndir,Ind_gen, dir_gen)

               !> Locals  directives
               call Split_LocRefCod_ATM(line, nloc, dir_loc, Ind_loc, dir_lab, IPh)

               if (ndir > 0 .and. nloc > 0) then
                  call set_error(1, "Bad format for FIX directive!")
                  return
               end if

            case ("VARY")    ! VARY .....
               call cut_sting(line,nlong)

               !> general directives
               call split_general_atm(line,ndir,Ind_gen, dir_gen)

               !> Locals  directives
               call Split_LocRefCod_ATM(line, nloc, dir_loc, Ind_loc, dir_lab, IPh)

               if (ndir > 0 .and. nloc > 0) then
                  call set_error(1, "Bad format for FIX directive!")
                  return
               end if

            case ("AFIX") ! AFIX ang sigma    (Angles restraints)
               call cut_string(line,nlong)

            case ("DFIX") ! DFIX d sigma      (Distance restraints)
               call cut_string(line,nlong)

            case ("TFIX") ! TFIX ang sigma    (Torsion angle restraints)
               call cut_string(line,nlong)

         end select
      end do

   End Subroutine Read_RefCodes_ATM

   !!--++
   !!--++ SUBROUTINE SPLIT_GENREFCOD_ATM
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Split_GenRefCod_ATM(String, Nc, Ikeys, Keys)
      !---- Arguments ----!
      character(len=*),                         intent(in)  :: String
      integer,                                  intent(out) :: Nc
      integer, dimension(:),                    intent(out) :: IKeys
      character(len=*), dimension(:), optional, intent(out) :: Keys

      !---- Local Variables ----!
      integer :: i,j,n,iv

      !> Init
      Nc=0; Ikeys=0
      if (present(keys)) Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)
      loop1: do i=1,n
         do j=1,NKEY_ATM
            if (trim(KEY_ATM(j)) == trim(u_case(dire(i)))) then
               nc=nc+1
               ikeys(nc)=j
               if (present(keys)) keys(nc)=trim(key_atm(j))
               cycle loop1
            end if
         end do ! Key_atm

         !> Phases or Molecule reference
         if (dire(i)(1:2) == 'PH') then
            nc=nc+1
            if (present(keys)) keys(nc)=dire(i)
            call get_num(dire(i)(3:),vet,ivet,iv)
            if (iv ==1) ikeys(nc)=-ivet(1)

         else if (dire(i)(1:3) == 'MOL') then
            nc=nc+1
            if (present(keys)) keys(nc)=dire(i)
            call get_num(dire(i)(4:),vet,ivet,iv)
            if (iv ==1) ikeys(nc)=-ivet(1)
         end if

      end do loop1

   End Subroutine Split_GenRefCod_ATM

   !!--++
   !!--++ SUBROUTINE SPLIT_LOCREFCOD_ATM
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Split_LocRefCod_ATM(String, Nc, Keys, Ikeys, AtLab, IPh)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String
      integer,                        intent(out) :: Nc
      character(len=*), dimension(:), intent(out) :: Keys
      integer,          dimension(:), intent(out) :: Ikeys
      character(len=*), dimension(:), intent(out) :: AtLab
      integer,          dimension(:), intent(out) :: IPh

      !---- Local Variables ----!
      integer           :: i,j,n,iv
      character(len=40) :: str

      !> Init
      Nc=0
      Keys=" "
      Ikeys=0
      AtLab=" "
      IPh=0

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
               if (iv ==1) iph(nc)=ivet(1)

            else if (str(j+1:j+3) == 'MOL') then
               call get_num(str(j+4:),vet,ivet,iv)
               if (iv ==1) iph(nc)=ivet(1)

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

End SubModule KeyCod_ReadCodes
