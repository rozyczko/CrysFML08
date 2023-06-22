!!
Submodule (CFML_KeyCodes) KeyCod_Molec
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE READ_REFCODES_MOL
   !!----
   !!----
   !!---- Update: 14/06/2023
   !!
   Module Subroutine Read_RefCodes_MOL(ffile, n_ini, n_end, IPhas, N_Mol, Mol, G)
      !---- Arguments ----!
      Type(file_type),                   intent(in)     :: ffile
      integer,                           intent(in)     :: n_ini
      integer,                           intent(in)     :: n_end
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      integer                 :: i, k

      !> Init
      call clear_error()

      do i=n_ini,n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) <=0) cycle
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_MOL(line, IPhas, N_Mol, Mol, G)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_MOL(line, IPhas, N_Mol, Mol, G)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               !call ReadCode_EQUAL_MOL()  ! Por hacer
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_MOL

   !!----
   !!---- SUBROUTINE READCODE_FIX_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_MOL(String, IPhas, N_Mol, Mol, G)
      !---- Arguments ----!
      character(len=*),                  intent(in)     :: String
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      integer :: i, j, jj, im, n, k, n_dir, nlong
      integer :: n1, n2, iKey, IPh, jMol

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

      !> Get words
      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> word(i) Is a general directive?
         n1 = index_Key_Phas(dire(i))

         !> next directive
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> word(i) is a directive: centre, orient, tls, ...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> last instruction
                  k=iphas
                  im=1  ! Check?

                  select case (n1)
                     case (34:48)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)
                        !call GPList_to_Molec()      ! Por hacer
                        !call GPList_from_Molec()    ! Por hacer

                     case (52:54)

                  end select

               else
                  do n=i+1, n_dir
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol)

                     k=iphas
                     if (iph > 0) k=iPh

                     im=1
                     if (jmol > 0) im=jmol

                     !> ikey debe ser 0

                     select case (n1)
                        case (34:48)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)
                           !call GPList_to_Molec()      ! Por hacer
                           !call GPList_from_Molec()    ! Por hacer

                        case (52:54)

                     end select
                  end do

                  i=n_dir
               end if

            else
               if (i+1 == n_dir) then
                  k=iphas
                  im=1

                  select case (n1)
                     case (34:48)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)
                        !call GPList_to_Molec()      ! Por hacer
                        !call GPList_from_Molec()    ! Por hacer

                     case (52)

                  end select

               else
                  do n=i+1, j-1
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol)

                     k=iphas
                     if (iph > 0) k=iPh

                     im=1
                     if (jmol > 0) im=jmol

                     !> ikey debe ser 0

                     select case (n1)
                        case (1:7)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)
                           !call GPList_to_Molec()      ! Por hacer
                           !call GPList_from_Molec()    ! Por hacer

                        case (52)

                     end select
                  end do
               end if

               i=j-1
            end if

         else
            !> word(i) is local instruction: xc_, ...
            call Get_InfoKey_StrMol(dire(i), iKey, IPh, jMol)

            k=iphas
            if (iph > 0) k=iPh

            im=1
            if (jmol > 0) im=jmol

            select case (ikey)
               case (34:48)
                  call Set_RefCodes_MOL('FIX', ikey, k, im, G=G)
                  !call GPList_to_Molec()      ! Por hacer
                  !call GPList_from_Molec()    ! Por hacer

               case (52:54)
                  do j=i+1, n_dir
                     !> Check for atom label
                     n2=Index_Atlab_on_Molecule(dire(j),Mol(jmol))
                     if (n2 > 0) then
                        call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(jmol)%Atname(n2), G)
                     end if
                  end do
                  !call GPList_to_Molec()      ! Por hacer
                  !call GPList_from_Molec()    ! Por hacer

            end select

         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_FIX_MOL

   !!----
   !!---- SUBROUTINE READCODE_VARY_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_MOL(String, Iphas, N_Mol, Mol, G)
      !---- Arguments ----!
      character(len=*),                  intent(in)     :: String
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      integer :: i, j, jj, im, n, k, n_dir, nlong
      integer :: n1, n2, n3, iKey, IPh, jMol


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

      !> Get words
      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> word(i) Is a general directive?
         n1 = index_Key_Phas(dire(i))

         !> next directive
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> word(i) is a directive: centre, orient, tls, ...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> last instruction
                  k=iphas
                  im=1

                  select case (n1)
                     case (34:48)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)
                        !call GPList_to_Molec()      ! Por hacer
                        !call GPList_from_Molec()    ! Por hacer

                     case (52)

                  end select

               else
                  do n=i+1, n_dir
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol)

                     k=iphas
                     if (iph > 0) k=iPh

                     im=1
                     if (jmol > 0) im=jmol

                     !> ikey debe ser 0

                     select case (n1)
                        case (34:48)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)
                           !call GPList_to_Molec()      ! Por hacer
                           !call GPList_from_Molec()    ! Por hacer

                     end select
                  end do

                  i=n_dir
               end if

            else
               if (i+1 == n_dir) then
                  k=iphas
                  im=1

                  select case (n1)
                     case (34:48)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)
                        !call GPList_to_Molec()      ! Por hacer
                        !call GPList_from_Molec()    ! Por hacer

                     case (52)

                  end select

               else
                  do n=i+1, j-1
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol)

                     k=iphas
                     if (iph > 0) k=iPh

                     im=1
                     if (jmol > 0) im=jmol

                     !> ikey debe ser 0

                     select case (n1)
                        case (1:7)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)
                           !call GPList_to_Molec()      ! Por hacer
                           !call GPList_from_Molec()    ! Por hacer

                        case (52)

                     end select
                  end do
               end if

               i=j-1
            end if

         else
            !> word(i) is local instruction: xc_, ...
            call Get_InfoKey_StrMol(dire(i), iKey, IPh, jMol)

            k=iphas
            if (iph > 0) k=iPh

            im=1
            if (jmol > 0) im=jmol

            select case (ikey)
               case (13) ! UISO
                  if (i == n_dir) then
                     !> Todos los atomos en molex
                     do j=1, Mol(jmol)%natoms
                        call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                              Mol(jmol)%Atname(j), G=G)
                     end do

                  else
                     do j=i+1, n_dir
                        !> Check for atom label
                        n2=Index_Atlab_on_Molecule(dire(j),Mol(jmol))
                        if (n2 > 0) then
                           call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(jmol)%Atname(n2), G)
                        else
                           n3=0
                           !> Check for chemical symbol
                           do n=1, Mol(jmol)%natoms
                              if (trim(dire(j)) == trim(Mol(jmol)%AtSymb(n))) then
                                 n3=n
                                 exit
                              end if
                           end do
                           if (n3 > 0) then
                              do n=1, Mol(jmol)%natoms
                                 if (trim(Mol(jmol)%AtSymb(n)) == &
                                     trim(Mol(jmol)%AtSymb(n3))) then

                                    call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(jmol)%Atname(n), G)

                                 end if
                              end do
                           end if

                        end if
                     end do
                  end if

                  !call GPList_to_Molec()      ! Por hacer
                  !call GPList_from_Molec()    ! Por hacer
                  exit


               case (34:48)
                  call Set_RefCodes_MOL('VARY', ikey, k, im, G=G)
                  !call GPList_to_Molec()      ! Por hacer
                  !call GPList_from_Molec()    ! Por hacer

               case (52:54)
                  do j=i+1, n_dir
                     !> Check for atom label
                     n2=Index_Atlab_on_Molecule(dire(j),Mol(jmol))
                     if (n2 > 0) then
                        call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(jmol)%Atname(n2), G)
                     end if
                  end do
                  !call GPList_to_Molec()      ! Por hacer
                  !call GPList_from_Molec()    ! Por hacer

            end select

         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_VARY_MOL



   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_MOL
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_MOL(Keyword, Npar, IPhas, IMol, Lab, G)
      !---- Arguments ----!
      character(len=*),           intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                    intent(in)     :: NPar        ! Specific parameter A,B,C,...
      integer,                    intent(in)     :: IPhas
      integer,                    intent(in)     :: IMol
      character(len=*), optional, intent(in)     :: Lab
      type(GenParList_Type),      intent(in out) :: G

      !---- Local variables ----!
      character(len=4) :: cdire, c_ph, c_mol
      character(len=10):: labc

      !> Init
      call clear_error()

      write(c_ph,fmt='(i3)') iphas
      write(c_mol,fmt='(i3)') imol
      c_ph=adjustl(c_ph)
      c_mol=adjustl(c_mol)
      line=trim(c_mol)//'_PHAS'//trim(c_ph)

      Labc=''
      if (present(lab)) Labc=trim(lab)

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (13)
                  call Fix_GPList_Par(G,'UISO_'//trim(Labc)//'_MOL'//trim(line))

               case (34) ! XC
                  call Fix_GPList_Par(G,'XC_MOL'//trim(line))

               case (35) ! YC
                  call Fix_GPList_Par(G,'YC_MOL'//trim(line))

               case (36) ! ZC
                  call Fix_GPList_Par(G,'ZC_MOL'//trim(line))

               case (37) ! CENTRE
                  call Fix_GPList_Par(G,'XC_MOL'//trim(line))
                  call Fix_GPList_Par(G,'YC_MOL'//trim(line))
                  call Fix_GPList_Par(G,'ZC_MOL'//trim(line))

               case (38) ! THE
                  call Fix_GPList_Par(G,'THE_MOL'//trim(line))

               case (39) ! PHI
                  call Fix_GPList_Par(G,'PHI_MOL'//trim(line))

               case (40) ! CHI
                  call Fix_GPList_Par(G,'CHI_MOL'//trim(line))

               case (41) ! ORIENT
                  call Fix_GPList_Par(G,'THE_MOL'//trim(line))
                  call Fix_GPList_Par(G,'PHI_MOL'//trim(line))
                  call Fix_GPList_Par(G,'CHI_MOL'//trim(line))

               case (42) ! T
                  call Fix_GPList_Par(G,'T_MOL'//trim(line))

               case (43) ! L
                  call Fix_GPList_Par(G,'L_MOL'//trim(line))

               case (44) ! S
                  call Fix_GPList_Par(G,'S_MOL'//trim(line))

               case (45) ! TL
                  call Fix_GPList_Par(G,'T_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L_MOL'//trim(line))

               case (46) ! LS
                  call Fix_GPList_Par(G,'L_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S_MOL'//trim(line))

               case (47) ! TS
                  call Fix_GPList_Par(G,'T_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S_MOL'//trim(line))

               case (48) ! TLS
                  call Fix_GPList_Par(G,'T_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S_MOL'//trim(line))

               case (52) ! DIST
                  call Fix_GPList_Par(G,'DIST_'//trim(Labc)//'_MOL'//trim(line))

               case (53) ! BANG
                  call Fix_GPList_Par(G,'BANG_'//trim(Labc)//'_MOL'//trim(line))

               case (54) ! TORS
                  call Fix_GPList_Par(G,'TORS_'//trim(Labc)//'_MOL'//trim(line))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (13)
                  call Vary_GPList_Par(G,'UISO_'//trim(Labc)//'_MOL'//trim(line))

               case (34) ! XC
                  call Vary_GPList_Par(G,'XC_MOL'//trim(line))

               case (35) ! YC
                  call Vary_GPList_Par(G,'YC_MOL'//trim(line))

               case (36) ! ZC
                  call Vary_GPList_Par(G,'ZC_MOL'//trim(line))

               case (37) ! CENT
                  call Vary_GPList_Par(G,'XC_MOL'//trim(line))
                  call Vary_GPList_Par(G,'YC_MOL'//trim(line))
                  call Vary_GPList_Par(G,'ZC_MOL'//trim(line))

               case (38) ! THE
                  call Vary_GPList_Par(G,'THE_MOL'//trim(line))

               case (39) ! PHI
                  call Vary_GPList_Par(G,'PHI_MOL'//trim(line))

               case (40) ! CHI
                  call Vary_GPList_Par(G,'CHI_MOL'//trim(line))

               case (41) ! ORIENT
                  call Vary_GPList_Par(G,'THE_MOL'//trim(line))
                  call Vary_GPList_Par(G,'PHI_MOL'//trim(line))
                  call Vary_GPList_Par(G,'CHI_MOL'//trim(line))

               case (42) ! T
                  call Vary_GPList_Par(G,'T_MOL'//trim(line))

               case (43) ! L
                  call Vary_GPList_Par(G,'L_MOL'//trim(line))

               case (44) ! S
                  call Vary_GPList_Par(G,'S_MOL'//trim(line))

               case (45) ! TL
                  call Vary_GPList_Par(G,'T_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L_MOL'//trim(line))

               case (46) ! LS
                  call Vary_GPList_Par(G,'L_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S_MOL'//trim(line))

               case (47) ! TS
                  call Vary_GPList_Par(G,'T_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S_MOL'//trim(line))

               case (48) ! TLS
                  call Vary_GPList_Par(G,'T_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S_MOL'//trim(line))

               case (52) ! DIST
                  call Vary_GPList_Par(G,'DIST_'//trim(Labc)//'_MOL'//trim(line))

               case (53) ! BANG
                  call Vary_GPList_Par(G,'BANG_'//trim(Labc)//'_MOL'//trim(line))

               case (54) ! TORS
                  call Vary_GPList_Par(G,'TORS_'//trim(Labc)//'_MOL'//trim(line))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_MOL

End Submodule KeyCod_Molec