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
   Module Subroutine Allocate_GenParList(NDMax, R)
      !---- Arguments ----!
      integer,               intent(in)     :: NDMax
      type(GenParList_Type), intent(in out) :: R

      !---- Local Arguments ----!
      integer :: i

      if (NDMax == 0) then
         R%ND_Max=0
         R%NPar=0
         if (allocated(R%Par)) deallocate(R%Par)
         return
      end if

      R%ND_Max=NDMax
      if (allocated(R%Par)) deallocate(R%Par)
      allocate(R%Par(NDMax))

      !> Initialize
      do i=1,NDMax
         R%Par(i)%Nam=" "
         R%Par(i)%Ext=" "
         R%Par(i)%L=0
         R%Par(i)%M=0.0_cp
         R%Par(i)%Val=0.0_cp
         R%Par(i)%Sig=0.0_cp
      end do

      R%NPar=0

   End Subroutine Allocate_GenParList

   !!--++
   !!--++ Subroutine Del_RefCode_GenParList
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the GenParList type
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Del_RefCode_GenParList(R, NPar)
      !---- Arguments ----!
      type(GenParList_Type), intent(in out) :: R
      integer,                 intent(in)   :: NPar

      !---- Local Variables ----!
      logical :: deleted
      integer :: i,j,k

      deleted=.false.

      !> Delete the NPar Parameter
      k=0
      do i=1,R%NPar
         if (R%Par(i)%L == NPar) then
            R%Par(i)%L=0
            R%Par(i)%M=0.0_cp
            deleted=.true.
            k=k+1
         end if
      end do

      !> Updating Variables
      do i=1,R%Npar
         if (R%Par(i)%L > NPar) then
            R%Par(i)%L=R%Par(i)%L-1
         end if
      end do

      R%Npar=R%NPar-k

      !> Updating Vec_Vectors
      if (deleted) call Del_Element_in_VRef(NPar)

   End Subroutine Del_RefCode_GenParList

   !!--++
   !!--++ SUBROUTINE FIX_GenParList_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine FIX_GenParList_Par(R, CodeNam)
      !---- Arguments ----!
      type(GenParList_Type), intent(in out) :: R
      character(len=*),        intent(in)     :: CodeNam

      !---- Local Variables ----!
      integer :: i,nc

      !> Check
      if (R%Npar ==0) return
      if (len_trim(CodeNam) <=0) return

      !>
      do i=1,R%NPar
         if (trim(u_case(CodeNam)) /= trim(u_case(R%Par(i)%Nam)) ) cycle

         if (R%Par(i)%L /=0) then
            nc=R%Par(i)%L
            call Del_RefCode_GenParList(R,nc)
         end if
      end do

   End Subroutine FIX_GenParList_Par

   !!--++
   !!--++ SUBROUTINE VARY_GenParList_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine VARY_GenParList_Par(R, CodeNam, Value, Sig, Mult)
      !---- Arguments ----!
      type(GenParList_Type), intent(in out) :: R
      character(len=*),        intent(in)     :: CodeNam
      real(kind=cp), optional, intent(in)     :: Value
      real(kind=cp), optional, intent(in)     :: Sig
      real(kind=cp), optional, intent(in)     :: Mult

      !---- Local Variables ----!
      logical       :: is_new
      integer       :: i,j
      real(kind=cp) :: m,v,s

      !> Check
      if (R%ND_Max ==0) return
      if (len_trim(CodeNam) <=0) return

      !>
      is_new=.true.
      do i=1,R%NPar
         if (trim(u_case(CodeNam)) /= trim(u_case(R%Par(i)%Nam)) ) cycle
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

      if (is_new) then
         !> Check
         if (R%NPar+1 > R%ND_Max) then
            call set_error(1, 'Maximum number of relations founded!')
            return
         end if

         i=R%Npar+1
         j=NP_Ref+1

         R%Par(i)%Nam=trim(Codenam)
         R%Par(i)%L=j
         R%Par(i)%M=m
         R%Par(i)%Val=v
         R%Par(i)%Sig=s

         Vec_RefPar(j)=v
         Vec_RefParSTD(j)=s
         Vec_NamePar(j)=trim(Codenam)
         Vec_LimPar(:,j)=0.0_cp
         Vec_BCond(j)=0
         Vec_PointPar(j)=i

         R%Npar=i
         NP_Ref=j
      end if

   End Subroutine VARY_GenParList_Par

   !!----
   !!---- SUBROUTINE GPList_to_Cell
   !!----
   !!----
   !!---- Update: 19/05/22
   !!
   Module Subroutine GPList_to_Cell(Ph, Ip, Cell)
      !---- Arguments ----!
      type(GenParList_Type), intent(in)   :: Ph
      integer,                 intent(in)   :: Ip
      class(cell_Type),        intent(inout):: Cell

      !---- Local Arguments ----!
      character(len=30) :: ccc
      integer           :: i,j,npos,iv

      select type (Cell)
         type is (Cell_LS_Type)
            do i=1,Ph%Npar
               ccc=trim(Ph%Par(i)%Nam)
               npos=index(ccc,'PHAS')
               call get_num(ccc(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(ccc,'_')
               ccc=ccc(:npos-1)
               do j=1, NKEY_PHAS
                  if (trim(ccc) /= trim(KEY_PHAS(j))) cycle
                  select case (j)
                     case (1:3)
                        cell%lcell(j)=ph%Par(i)%L

                     case (4:6)
                        cell%lang(j-3)=ph%Par(j)%L
                  end select
               end do
            end do

         type is (Cell_GLS_Type)
            do i=1,Ph%Npar
               ccc=trim(Ph%Par(i)%Nam)
               npos=index(ccc,'PHAS')
               call get_num(ccc(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(ccc,'_')
               ccc=ccc(:npos-1)
               do j=1, NKEY_PHAS
                  if (trim(ccc) /= trim(KEY_PHAS(j))) cycle
                  select case (j)
                     case (1:3)
                        cell%lcell(j)=ph%Par(i)%L

                     case (4:6)
                        cell%lang(j-3)=ph%Par(i)%L
                  end select
               end do
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
      integer           :: i,j,npos,iv

      do i=1,M%Npar
         ccc=trim(M%Par(i)%Nam)
         npos=index(ccc,'MOL')
         call get_num(ccc(npos+3:),vet,ivet,iv)
         if (ivet(1) /= im) cycle

         npos=index(ccc,'_')
         ccc=ccc(:npos-1)
         do j=1, NKEY_MOL
            if (trim(ccc) /= trim(KEY_MOL(j))) cycle

            select case (j)
               case (1:3)
                  mol%lxcentre(j)=M%Par(i)%L

               case (5:7)
                  mol%lorient(j-4)=M%Par(i)%L
            end select
         end do

      end do

   End Subroutine GPList_to_Molec

End SubModule KeyCod_GenPar