!!
Submodule (CFML_KeyCodes) KeyCod_Relat
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE Allocate_RelationsType
   !!----
   !!----
   !!---- Update: 13/05/2022
   !!
   Module Subroutine Allocate_RelationList(NDMax, R)
      !---- Arguments ----!
      integer,                 intent(in)     :: NDMax
      type(RelationList_Type), intent(in out) :: R

      !---- Local Arguments ----!
      integer :: i

      if (NDMax ==0) then
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
         R%Par(i)%Name=" "
         R%Par(i)%Ext=" "
         R%Par(i)%L=0
         R%Par(i)%M=0.0_cp
         R%Par(i)%Val=0.0_cp
         R%Par(i)%Sig=0.0_cp
      end do

      R%NPar=0

   End Subroutine Allocate_RelationList

   !!--++
   !!--++ Subroutine Del_RefCode_RelationList
   !!--++
   !!--++    Delete the number of Refinable Parameter (NPar) on the RelationList type
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Del_RefCode_RelationList(R, NPar)
      !---- Arguments ----!
      type(RelationList_Type), intent(in out) :: R
      integer,                 intent(in)     :: NPar

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

   End Subroutine Del_RefCode_RelationList

   !!--++
   !!--++ SUBROUTINE FIX_RELATIONLIST_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine FIX_RelationList_Par(R, CodeNam)
      !---- Arguments ----!
      type(RelationList_Type), intent(in out) :: R
      character(len=*),        intent(in)     :: CodeNam

      !---- Local Variables ----!
      integer :: i,nc

      !> Check
      if (R%Npar ==0) return
      if (len_trim(CodeNam) <=0) return

      !>
      do i=1,R%NPar
         if (trim(u_case(CodeNam)) /= trim(u_case(R%Par(i)%Name)) ) cycle

         if (R%Par(i)%L /=0) then
            nc=R%Par(i)%L
            call Del_RefCode_RelationList(R,nc)
         end if
      end do

   End Subroutine FIX_RelationList_Par

   !!--++
   !!--++ SUBROUTINE VARY_RELATIONLIST_PAR
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine VARY_RelationList_Par(R, CodeNam, Value, Sig, Mult)
      !---- Arguments ----!
      type(RelationList_Type), intent(in out) :: R
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
         if (trim(u_case(CodeNam)) /= trim(u_case(R%Par(i)%Name)) ) cycle
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

         R%Par(i)%Name=trim(Codenam)
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

   End Subroutine VARY_RelationList_Par

   !!----
   !!---- SUBROUTINE RList_to_Cell
   !!----
   !!----
   !!---- Update: 19/05/22
   !!
   Module Subroutine RList_to_Cell(Ph, Ip, Cell)
      !---- Arguments ----!
      type(RelationList_Type), intent(in)   :: Ph
      integer,                 intent(in)   :: Ip
      class(cell_Type),        intent(inout):: Cell

      !---- Local Arguments ----!
      character(len=30) :: ccc
      integer           :: i,j,npos,iv

      select type (Cell)
         type is (Cell_LS_Type)
            do i=1,Ph%Npar
               ccc=trim(Ph%Par(i)%Name)
               npos=index(ccc,'PHAS')
               call get_num(ccc(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(ccc,'_')
               ccc=ccc(:npos-1)
               do j=1, 7 !NKEY_PHAS
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
               ccc=trim(Ph%Par(i)%Name)
               npos=index(ccc,'PHAS')
               call get_num(ccc(npos+4:),vet,ivet,iv)
               if (ivet(1) /= ip) cycle

               npos=index(ccc,'_')
               ccc=ccc(:npos-1)
               do j=1, 7 !NKEY_PHAS
                  if (trim(ccc) /= trim(KEY_PHAS(j))) cycle
                  select case (j)
                     case (1:3)
                        cell%lcell(j)=ph%Par(i)%L

                     case (4:6)
                        cell%lang(j-3)=ph%Par(j)%L
                  end select
               end do
            end do
      end select

   End Subroutine RList_to_Cell

End SubModule KeyCod_Relat