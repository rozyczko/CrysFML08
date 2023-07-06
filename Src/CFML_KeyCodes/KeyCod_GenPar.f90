!!
Submodule (CFML_KeyCodes) KeyCod_GenPar
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE Allocate_GenParList
   !!----
   !!----
   !!---- Update: 13/05/2022
   !!
   Module Subroutine Allocate_GPList(NDMax, G)
      !---- Arguments ----!
      integer,               intent(in)     :: NDMax
      type(GenParList_Type), intent(in out) :: G

      !---- Local Arguments ----!
      integer :: i

      if (NDMax == 0) then
         G%ND_Max=0
         G%NPar=0
         if (allocated(G%Par)) deallocate(G%Par)
         return
      end if

      G%ND_Max=NDMax
      if (allocated(G%Par)) deallocate(G%Par)
      allocate(G%Par(NDMax))

      !> Initialize
      do i=1,NDMax
         G%Par(i)%Ip=0
         G%Par(i)%Nam=" "
         G%Par(i)%Ext=" "
         G%Par(i)%L=0
         G%Par(i)%M=0.0_cp
         G%Par(i)%Val=0.0_cp
         G%Par(i)%Sig=0.0_cp
         G%Par(i)%Vlim=0.0_cp
         G%Par(i)%BCond=.false.
      end do

      G%NPar=0

   End Subroutine Allocate_GPList

   !!--++
   !!--++ Subroutine Del_RefCode_GenParList
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the GenParList type
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Del_RefCode_GPList(G, NCode)
      !---- Arguments ----!
      type(GenParList_Type), intent(in out) :: G
      integer,               intent(in)     :: NCode

      !---- Local Variables ----!
      integer :: i,j,nc

      !> Delete the NPar Parameter
      do i=1,G%NPar
         if (G%Par(i)%L == Ncode) then
            G%Par(i)%Ip=0
            G%Par(i)%Nam=" "
            G%Par(i)%Ext=" "
            G%Par(i)%L=0
            G%Par(i)%M=1.0_cp
            G%Par(i)%Vlim=0.0_cp
            G%Par(i)%BCond=.false.
         end if
      end do

      nc=G%NPar
      i=1
      do while(i <= nc)
         if (G%Par(i)%L == 0) then
            do j=i+1, nc
               G%Par(j-1)=G%Par(j)
            end do
            nc=nc-1
            cycle
         end if
         i=i+1
      end do

      G%NPar=nc

   End Subroutine Del_RefCode_GPList

   !!--++
   !!--++ SUBROUTINE FIX_GenParList_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine FIX_GPList_Par(G, CodeNam)
      !---- Arguments ----!
      type(GenParList_Type), intent(in out) :: G
      character(len=*),      intent(in)     :: CodeNam

      !---- Local Variables ----!
      integer :: i,nc

      !> Check
      if (G%Npar ==0) return
      if (len_trim(CodeNam) <=0) return

      !> Start
      nc=0
      do i=1,G%NPar
         if (trim(CodeNam) /= trim(G%Par(i)%Nam))  cycle
         nc=G%Par(i)%L
         exit
      end do
      if (nc ==0) return

      call Del_RefCode_GPList(G, nc)

   End Subroutine FIX_GPList_Par

   !!--++
   !!--++ SUBROUTINE VARY_GenParList_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine VARY_GPList_Par(G, CodeNam, Value, Sig, Mult, Vlim, Bc)
      !---- Arguments ----!
      type(GenParList_Type),                 intent(in out) :: G
      character(len=*),                      intent(in)     :: CodeNam
      real(kind=cp),               optional, intent(in)     :: Value
      real(kind=cp),               optional, intent(in)     :: Sig
      real(kind=cp),               optional, intent(in)     :: Mult
      real(kind=cp), dimension(2), optional, intent(in)     :: Vlim
      logical,                     optional, intent(in)     :: Bc

      !---- Local Variables ----!
      logical       :: is_new, bcc
      integer       :: i,j,nmax
      real(kind=cp) :: m,v,s
      real(kind=cp), dimension(2)::v2

      !> Check
      if (G%ND_MAX == 0) return     ! No está dimensionado
      if (len_trim(CodeNam) <= 0) return

      !> Is new element in the List?
      is_new=.true.
      do i=1,G%NPar
         if (trim(CodeNam) /= trim(G%Par(i)%Nam) ) cycle
         is_new=.false.
         exit
      end do

      !> Add
      v=0.0_cp
      if (present(value)) v=value
      s=0.0_cp
      if (present(sig)) s=sig
      m=1.0_cp
      if (present(mult)) m=mult
      v2=0.0_cp
      if (present(vlim)) v2=vlim
      bcc=.false.
      if (present(bc)) bcc=bc

      if (G%Npar == 0) then
         nmax=0
      else
         nmax=maxval(G%Par(1:G%NPar)%L)
      end if

      if (is_new) then
         !> Check
         if (G%NPar+1 > G%ND_Max) then
            call set_error(1, 'Maximum number of relations founded in GenParList!')
            return
         end if

         i=G%Npar+1

         G%Par(i)%Nam=trim(Codenam)
         G%Par(i)%L=nmax+1
         G%Par(i)%M=m
         G%Par(i)%Val=v
         G%Par(i)%Sig=s
         G%Par(i)%Vlim=v2
         G%Par(i)%Bcond=bcc

         G%Npar=i

      end if

   End Subroutine VARY_GPList_Par

   !!----
   !!---- SUBROUTINE GPList_to_Cell
   !!----
   !!----
   !!---- Update: 19/05/22
   !!
   Module Subroutine GPList_to_Cell(G, Ip, Cell)
      !---- Arguments ----!
      type(GenParList_Type), intent(in)    :: G
      integer,               intent(in)    :: Ip
      class(cell_Type),      intent(in out):: Cell

      !---- Local Arguments ----!
      character(len=40) :: car
      integer           :: i,j,npos,iv

      select type (Cell)
         type is (Cell_LS_Type)
            cell%lcell=0; cell%lang=0

            do i=1,G%Npar
               car=trim(G%Par(i)%Nam)
               npos=index(car,'PHAS')
               call get_num(car(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(car,'_')
               car=car(:npos-1)
               j= index_key_phas(trim(car))
               select case (j)
                  case (1:3)
                     cell%lcell(j)=G%Par(i)%L

                  case (4:6)
                     cell%lang(j-3)=G%Par(j)%L
               end select
            end do

         type is (Cell_GLS_Type)
            cell%lcell=0; cell%lang=0

            do i=1,G%Npar
               car=trim(G%Par(i)%Nam)
               npos=index(car,'PHAS')
               call get_num(car(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(car,'_')
               car=car(:npos-1)
               j= index_key_phas(trim(car))
               select case (j)
                  case (1:3)
                     cell%lcell(j)=G%Par(i)%L

                  case (4:6)
                     cell%lang(j-3)=G%Par(i)%L
               end select
            end do

      end select

   End Subroutine GPList_to_Cell

   !!----
   !!---- SUBROUTINE GPList_to_Molec
   !!----
   !!----
   !!---- Update: may - 2022
   !!
   Module Subroutine GPList_to_Molec(M, Im, Mol)
      !---- Arguments ----!
      type(GenParList_Type), intent(in)   :: M
      integer,                 intent(in)   :: Im
      type(Molecule_type),     intent(inout):: Mol

      !---- Local Arguments ----!
      character(len=30) :: ccc
      integer           :: i,npos,iv

      do i=1,M%Npar
         ccc=trim(M%Par(i)%Nam)
         npos=index(ccc,'MOL')
         call get_num(ccc(npos+3:),vet,ivet,iv)
         if (ivet(1) /= im) cycle

         npos=index(ccc,'_')
         ccc=ccc(:npos-1)
         !do j=1, NKEY_MOL
         !   if (trim(ccc) /= trim(KEY_MOL(j))) cycle
         !
         !   select case (j)
         !      case (1:3)
         !         mol%lxcentre(j)=M%Par(i)%L
         !
         !      case (5:7)
         !         mol%lorient(j-4)=M%Par(i)%L
         !   end select
         !end do

      end do

   End Subroutine GPList_to_Molec

   !!----
   !!---- SUBROUTINE WriteInfo_GenParList
   !!----
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine WriteInfo_GPList(G, Iunit)
      !---- Arguments ----!
      type(GenParList_Type), intent(in) :: G
      integer, optional,     intent(in) :: Iunit

      !---- Local variables ----!
      integer :: i,lun

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      if (G%NPar <= 0) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i5)") " Number of Refinable Parameters: ",G%NPar
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,a)")" N.Par      Name                            Value     Sigma    ",&
                                     " L.Bound   U.Bound  Multiplier    Code  B.C."
      do i=1,G%NPar
         write(unit=lun,fmt="(i4,8x,a28,5f10.5, 4x, i5, 3x,l2)") i,  &
               G%Par(i)%Nam, G%Par(i)%Val, G%Par(i)%Sig, G%Par(i)%Vlim,&
               G%Par(i)%M, G%Par(i)%L, G%Par(i)%BCond
      end do
      write(unit=lun, fmt="(a)") " "

   End Subroutine WriteInfo_GPList

   !!----
   !!---- SUBROUTINE Update_GenParList_from_AtomList
   !!----
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine GPList_from_AtmList(AtList, IPh, G)
      !---- Arguments ----!
      type(Atlist_Type),     intent(in)     :: AtList
      integer,               intent(in)     :: IPh   ! Phase
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer :: i, j, n
      character(len=20) :: str1, str2
      character(len=3)  :: car

      !> Init
      write(unit=car,fmt='(i3)') iPh
      car=adjustl(car)

      !> First delete all information from atomic current phase
      do i=1,AtList%natoms
         associate (A => AtList%atom(i))
            str1=trim(A%lab)//'_PHAS'//trim(car)  ! Atom label + Phase

            !> Exist in the List?
            j=1
            do while(j <= G%NPar)
               str2=trim(G%Par(j)%Nam)
               n=index(trim(str2), trim(str1))

               if (n > 0) then
                  call Del_RefCode_GPList(G, G%Par(j)%L)
                  cycle
               end if
               j=j+1
            end do
         end associate
      end do

      do i=1,AtList%natoms
         associate (A => AtList%atom(i))
            str1=trim(A%lab)//'_PHAS'//trim(car)  ! Atom label + Phase

            select type (A)
               type is (Atm_Ref_Type)
                  if (A%l_x(1) > 0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='X_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_x(1)
                     G%Par(G%NPar)%M=A%m_x(1)
                     G%Par(G%NPar)%Val=A%x(1)
                     G%Par(G%NPar)%Vlim=[0.0_cp, 1.0_cp]
                     G%Par(G%NPar)%Sig=A%x_std(1)
                     G%Par(G%NPar)%BCond=.true.
                  end if

                  if (A%l_x(2) > 0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='Y_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_x(2)
                     G%Par(G%NPar)%M=A%m_x(2)
                     G%Par(G%NPar)%Val=A%x(2)
                     G%Par(G%NPar)%Vlim=[0.0_cp, 1.0_cp]
                     G%Par(G%NPar)%Sig=A%x_std(2)
                     G%Par(G%NPar)%BCond=.true.
                  end if

                  if (A%l_x(3) > 0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='Z_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_x(3)
                     G%Par(G%NPar)%M=A%m_x(3)
                     G%Par(G%NPar)%Val=A%x(3)
                     G%Par(G%NPar)%Vlim=[0.0_cp, 1.0_cp]
                     G%Par(G%NPar)%Sig=A%x_std(3)
                     G%Par(G%NPar)%BCond=.true.
                  end if

                  if (A%l_occ > 0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='OCC_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_occ
                     G%Par(G%NPar)%M=A%m_occ
                     G%Par(G%NPar)%Val=A%occ
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_U_iso > 0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='UISO_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u_iso
                     G%Par(G%NPar)%M=A%m_u_iso
                     G%Par(G%NPar)%Val=A%u_iso
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(1) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U11_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(1)
                     G%Par(G%NPar)%M=A%m_u(1)
                     G%Par(G%NPar)%Val=A%u(1)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(2) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U22_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(2)
                     G%Par(G%NPar)%M=A%m_u(2)
                     G%Par(G%NPar)%Val=A%u(2)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(3) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U33_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(3)
                     G%Par(G%NPar)%M=A%m_u(3)
                     G%Par(G%NPar)%Val=A%u(3)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(4) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U12_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(4)
                     G%Par(G%NPar)%M=A%m_u(4)
                     G%Par(G%NPar)%Val=A%u(4)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(5) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U13_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(5)
                     G%Par(G%NPar)%M=A%m_u(5)
                     G%Par(G%NPar)%Val=A%u(5)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if

                  if (A%l_u(6) >0) then
                     G%Npar=G%Npar+1

                     G%Par(G%NPar)%Nam='U23_'//trim(str1)
                     G%Par(G%NPar)%L=A%l_u(6)
                     G%Par(G%NPar)%M=A%m_u(6)
                     G%Par(G%NPar)%Val=A%u(6)
                     G%Par(G%NPar)%Vlim=0.0_cp
                     G%Par(G%NPar)%Sig=0.0_cp
                     G%Par(G%NPar)%BCond=.false.
                  end if
            end select

         end associate
      end do

   End Subroutine GPList_from_AtmList

   !!----
   !!---- Subroutine GPList_from_Cell
   !!----
   !!----
   !!---- June - 2023
   !!
   Module Subroutine GPList_from_Cell(Cell, IPh, G)
      !---- Arguments ----!
      class(cell_Type),      intent(in)     :: Cell
      integer,               intent(in)     :: IPh   ! Phase
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer          :: i, n, ic
      character(len=3) :: car

      !> Delete all references to cell parameters
      write(unit=car, fmt='(i3)') IPh
      car=adjustl(car)

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /= 'A_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /='B_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /='C_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /='ALPHA_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /='BETA_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      do i=1,G%Npar
         if (trim(G%Par(i)%Nam) /='GAMMA_'//'PHAS'//trim(car) ) cycle
         call Del_RefCode_GPList(G, G%Par(i)%L)
         exit
      end do

      ic=0
      n=G%Npar
      if (n > 0) ic=maxval(G%Par(1:n)%L)

      select type (Cell)
         type is (Cell_LS_Type)
            if (cell%lcell(1) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='A_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(1)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(1)
               G%Par(n)%Sig=cell%scell(1)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lcell(2) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='B_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(2)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(2)
               G%Par(n)%Sig=cell%scell(2)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lcell(3) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='C_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(3)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(3)
               G%Par(n)%Sig=cell%scell(3)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(1) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='ALPHA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(1)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(1)
               G%Par(n)%Sig=cell%sang(1)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(2) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='BETA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(2)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(2)
               G%Par(n)%Sig=cell%sang(2)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(3) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='GAMMA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(3)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(3)
               G%Par(n)%Sig=cell%sang(3)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

         type is (Cell_GLS_Type)
            if (cell%lcell(1) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='A_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(1)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(1)
               G%Par(n)%Sig=cell%scell(1)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lcell(2) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='B_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(2)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(2)
               G%Par(n)%Sig=cell%scell(2)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lcell(3) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='C_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lcell(3)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%cell(3)
               G%Par(n)%Sig=cell%scell(3)
               G%Par(n)%Vlim=0.0_cp
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(1) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='ALPHA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(1)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(1)
               G%Par(n)%Sig=cell%sang(1)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(2) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='BETA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(2)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(2)
               G%Par(n)%Sig=cell%sang(2)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

            if (cell%lang(3) > 0) then
               n=n+1

               G%Par(n)%Ip(2)=IPh
               G%Par(n)%Nam='GAMMA_'//'PHAS'//trim(car)
               G%Par(n)%L=ic + cell%lang(3)
               G%Par(n)%M=1.0_cp
               G%Par(n)%Val=cell%ang(3)
               G%Par(n)%Sig=cell%sang(3)
               G%Par(n)%Vlim=[0.0_cp, 180.0_cp]
               G%Par(n)%BCond=.false.
            end if

      end select
      G%Npar=n

   End Subroutine GPList_from_Cell

   !!----
   !!---- Subroutine Update_GPList_Code
   !!----
   !!---- Reorder the GenParList_Type to avoid empty Code numbers
   !!----
   !!---- June - 2023
   !!
   Module Subroutine Update_GPList_Code(G)
      !---- Argument ----!
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer :: i, j, k, n, ic_min, ic_max

      n = count(G%Par(1:G%NPar)%L > 0)
      if (n ==0) return

      ic_min=minval(G%Par(1:G%NPar)%L, mask=G%Par(1:G%NPar)%L > 0)
      ic_max=maxval(G%Par(1:G%NPar)%L)

      n=0
      do i=ic_min, ic_max
         k=count(G%Par(1:G%NPar)%L ==i)
         if (k > 0) n=n+1

         do j=1,G%NPar
            if (G%Par(j)%L == i) G%Par(j)%L=n
         end do
      end do

   End Subroutine Update_GPList_Code

   !!----
   !!---- Subroutine GPList_to_AtmList
   !!----
   !!---- June - 2023
   !!
   Module Subroutine GPList_to_AtmList(G, IPh, AtList)
      !---- Arguments ----!
      type(GenParList_Type), intent(in)     :: G
      integer,               intent(in)     :: IPh
      type(Atlist_Type),     intent(in out) :: AtList

      !---- Local Variables ----!
      integer :: i, j, n, nc, iv, npos
      character(len=60) :: str, lab

      nc=maxval(G%Par(1:G%NPar)%L)

      !> Init values in AtmList
      do i=1,AtList%natoms
         associate (A => AtList%atom(i))
            select type(A)
               type is (Atm_Ref_Type)
                  A%l_x=0
                  A%l_occ=0
                  A%l_u_iso=0
                  A%l_u=0

            end select
         end associate
      end do

      do i=1,G%NPar
         str=trim(G%Par(i)%Nam)

         !> Select the active phase to work
         npos=index(str,'_PHAS')
         call get_num(str(npos+5:),vet,ivet,iv)
         if (ivet(1) /= iph) cycle

         str=str(:npos-1)
         npos=index(str,'_')
         if (npos == 0) cycle

         !> Select atom (using Label))
         lab=str(npos+1:)
         n = Index_AtLab_on_AtList(trim(lab), iph, Atlist)

         !n=0
         !do j=1,AtList%natoms
         !   if (trim(AtList%atom(j)%Lab) /= trim(lab)) cycle
         !   n=j
         !   exit
         !end do

         if (n == 0) then
            call set_error(1,'Label atom not found! from GenParList, Please check it: '//trim(lab))
            return
         end if

         !> Select parameter to refine
         str=u_case(str(:npos-1))
         j = index_Key_Phas(trim(str))

         select case (j)
            case (8) ! X
               call Vary_XYZ_Atm(Atlist%atom(n), nc, 1)

            case (9) ! Y
               call Vary_XYZ_Atm(Atlist%atom(n), nc, 2)

            case (10)! Z
               call Vary_XYZ_Atm(Atlist%atom(n), nc, 3)

            case (12)! OCC
               call Vary_OCC_Atm(Atlist%atom(n),nc)

            case (13)! UISO
               call Vary_U_Atm(Atlist%atom(n), nc, 0)

            case (15:20) ! U's
               call Vary_U_Atm(Atlist%atom(n), nc, j-14)
         end select

      end do ! General Vector

   End Subroutine GPList_to_AtmList

   !!----
   !!---- Subroutine Get_InfoKey_StrPhas
   !!----     Get information about the Instructiion
   !!----
   !!----     Str format: DIR_LABEL_PHAS[N]
   !!----        Ex: OCC_O1_PHAS1, OCC_O1, O1, O
   !!----
   !!---- June 2023
   !!
   Module Subroutine Get_InfoKey_StrPhas(Str, iKey, IPh, Lab)
      !---- Arguments ----!
      character(len=*), intent(in) :: Str
      integer,          intent(out):: iKey   ! Index on KEY_PHAS
      integer,          intent(out):: IPh    ! Phase in str
      character(len=*), intent(out):: Lab    ! Atom label

      !---- Local Variables ----!
      integer :: i, n, iv

      !> Init
      iKey=0; iPh=0
      Lab=' '

      !> copy
      line=adjustl(Str)

      !> Phase
      n = index(u_case(line),'_PHAS')
      if (n > 0) then
         call get_num(line(n+5:), vet, ivet, iv)
         if (iv == 1) Iph=ivet(1)
         line=line(:n-1)
      end if

      !> Directive
      n = index(line, '_')
      if (n == 0) then
         lab=trim(line) ! Label of atom

      else
         !> check if line contain a directive (looking Key_Phas)
         i = index_key_Phas(u_case(line(:n-1)))
         if ( i > 0) then
            iKey=i
            line=line(n+1:)
         end if

         lab = trim(line)
      end if

   End Subroutine Get_InfoKey_StrPhas

   !!----
   !!---- Subroutine Get_InfoKey_StrMol
   !!----     Get information about the Instructiion
   !!----
   !!----     Str format: DIR_MOL[N]_PHAS[M]
   !!----        Ex: XC_MOL1_PHAS2, XC_MOL1,...
   !!----
   !!---- June 2023
   !!
   Module Subroutine Get_InfoKey_StrMol(Str, iKey, IPh, IMol)
      !---- Arguments ----!
      character(len=*), intent(in) :: Str
      integer,          intent(out):: iKey   ! Index on KEY_PHAS
      integer,          intent(out):: IPh    ! Phase in str
      integer,          intent(out):: IMol   ! Molecule in str

      !---- Local Variables ----!
      integer :: i, n, iv

      !> Init
      iKey=0; iPh=0; IMol=0

      !> copy
      line=adjustl(Str)

      !> Phase
      n = index(u_case(line),'_PHAS')
      if (n > 0) then
         call get_num(line(n+5:), vet, ivet, iv)
         if (iv == 1) Iph=ivet(1)
         line=line(:n-1)
      end if

      !> Molecule
      n = index(u_case(line),'MOL')
      if (n > 0) then
         call get_num(line(n+3:), vet, ivet, iv)
         if (iv == 1) IMol=ivet(1)
         line=line(:n-1)
      end if

      !> Check if there is a '_'
      n = index(line,'_', back=.true.)
      if (n > 0) line=line(:n-1)

      !> Directive
      i = index_key_Phas(u_case(trim(line)))
      if ( i > 0) iKey=i

   End Subroutine Get_InfoKey_StrMol

   !!----
   !!---- Subroutine Get_InfoKey_StrPatt
   !!----
   !!----      Get information about the Instruction
   !!----      Str format: DIR_LABEL_PATT[N]
   !!----
   !!----      Ex: A_PATT1, A,...
   !!----
   !!---- June 2023
   !!
   Module Subroutine Get_InfoKey_StrPatt(Str, iKey, IPat)
      !---- Arguments ----!
      character(len=*), intent(in) :: Str
      integer,          intent(out):: iKey   ! Index on KEY_PHAS
      integer,          intent(out):: IPat   ! Pattern in str

      !---- Local Variables ----!
      integer :: i, n, iv

      !> Init
      iKey=0; iPat=0

      !> copy
      line=adjustl(Str)

      !> Pattern
      n = index(u_case(trim(line)),'_PATT')
      if (n > 0) then
         call get_num(line(n+5:), vet, ivet, iv)
         if (iv == 1) Ipat=ivet(1)
         line=line(:n-1)
      end if

      !> Directive
      line=u_case(line)

      do i=1,NKEY_PATT
         if (trim(line) /= trim(KEY_PATT(i)) ) cycle
         iKey=i
         exit
      end do

   End Subroutine Get_InfoKey_StrPatt

   !!----
   !!---- Function Index_GPList
   !!----
   !!----
   !!---- June 2023
   !!
   Module Function Index_GPList(CodeNam, G) Result(Ind)
      !---- Arguments ----!
      character(len=*),      intent(in) :: CodeNam
      type(GenParList_Type), intent(in) :: G
      integer                           :: Ind

      !---- Local Variables ----!
      integer :: i

      !> Init
      Ind=0
      if (len_trim(Codenam)<= 0) return

      do i=1, G%Npar
         if (trim(CodeNam) /= trim(G%Par(i)%Nam)) cycle
         ind=i
         exit
      end do

   End Function Index_GPList

   !!----
   !!---- Function Index_KEY_PHAS
   !!----
   !!----
   !!---- June 2023
   !!
   Module Function Index_KEY_PHAS(String) Result(Ind)
      !---- Arguments ----!
      character(Len=*), intent(in) :: String
      integer                      :: Ind

      !---- Local Variables ----!
      integer           :: i
      character(len=40) :: str

      !> Init
      Ind=0
      if (len_trim(String)<= 0) return

      str=adjustl(string)
      str=u_case(str)

      do i=1, NKEY_PHAS
         if (trim(str) /= trim(KEY_PHAS(i)) ) cycle
         ind=i
         exit
      end do

   End Function Index_KEY_PHAS

   !!----
   !!---- Function Index_KEY_PATT
   !!----
   !!----
   !!---- June 2023
   !!
   Module Function Index_KEY_PATT(String) Result(Ind)
      !---- Arguments ----!
      character(Len=*), intent(in) :: String
      integer                      :: Ind

      !---- Local Variables ----!
      integer           :: i
      character(len=40) :: str

      !> Init
      Ind=0
      if (len_trim(String)<= 0) return

      str=adjustl(string)
      str=u_case(str)

      do i=1, NKEY_PATT
         if (trim(str) /= trim(KEY_PATT(i)) ) cycle
         ind=i
         exit
      end do

   End Function Index_KEY_PATT

End SubModule KeyCod_GenPar
