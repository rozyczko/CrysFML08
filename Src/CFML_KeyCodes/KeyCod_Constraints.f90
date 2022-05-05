!!
Submodule (CFML_KeyCodes) KeyCod_Constraints
   implicit none

   Contains

   !!--++
   !!--++  SUBROUTINE GET_ATOMBET_CTR
   !!--++
   !!--++  Subroutine to get the appropriate constraints in the refinement codes of
   !!--++  anisotropic atomic displacement(thermal) parameters.
   !!--++
   !!--++  New algorithm based in the Wigner theorem.
   !!--++  The matrix Bet = Sum { R Beta RT} displays the symmetry constraints to be
   !!--++  applied to the anisotropic temperature factors. The sum runs over all rotational
   !!--++  symmetry operators of the stabilizer of the particular atom position in the given
   !!--++  space group.
   !!--++
   !!--++  There are a total of 29 kind of relations that may appear in the Bet matrix:
   !!--++
   !!--++     1    A A A 0   0   0  -> m-3m, -43m, 432, m-3,23, 3[111].2[001]
   !!--++     2    A A C 0   0   0  -> 4/mmm, -42m, 4mm, 422, 4/m, -4,4, 4[001]
   !!--++     3    A B A 0   0   0  -> 4[010]
   !!--++     4    A B B 0   0   0  -> 4[100]
   !!--++     5    A A A D   D   D  -> -3m, 3m, 32, -3, 3   3[111]
   !!--++     6    A A A D  -D  -D  -> 3[11-1]
   !!--++     7    A A A D  -D   D  -> 3[1-11]
   !!--++     8    A A A D   D  -D  -> 3[-111]
   !!--++     9    A A C A/2 0   0  -> 6/mmm, -6m2, 6mm, 622, 6/m, 6,-6,-3m, 32,-3, 3:  h 3[001]
   !!--++    10    A B C 0   0   0  -> mmm, mm2, 222  2[001] 2[100]
   !!--++    11    A A C D   0   0  -> 2[001], 2[110]    w
   !!--++    12    A B A 0   E   0  -> 2[010], 2[101]
   !!--++    13    A B B 0   0   F  -> 2[100], 2[011]
   !!--++    14    A B C B/2 0   0  -> 2[001], 2[100]    h
   !!--++    15    A B C A/2 0   0  -> 2[001], 2[010]    h
   !!--++    16    A B C D   0   0  -> 2/m, m, 2: 2[001] w
   !!--++    17    A B C 0   E   0  -> 2[010]
   !!--++    18    A B C 0   0   F  -> 2[100]
   !!--++    19    A A C D   E  -E  -> 2[110]            w
   !!--++    20    A A C D   E   E  -> 2[1-10]           w
   !!--++    21    A B A D   E  -D  -> 2[101]
   !!--++    22    A B A D   E   D  -> 2[10-1]
   !!--++    23    A B B D  -D   F  -> 2[011]
   !!--++    24    A B B D   D   F  -> 2[01-1]
   !!--++    25    A B C B/2 F/2 F  -> 2[100]            h
   !!--++    26    A B C A/2 0   F  -> 2[210]            h
   !!--++    27    A B C B/2 E   0  -> 2[120]            h
   !!--++    28    A B C A/2 E   E/2-> 2[010]            h
   !!--++    29    A B C D   E   F  -> 1, -1
   !!--++
   !!--++   Updated: April - 2022
   !!
   Module Subroutine Get_AtomBet_CTR(X, Betas, Spgr, Codini, Icodes, Multip, Ord, Ss, Ipr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),             intent(in    ) :: X          !Atom position (fractional coordinates)
      real(kind=cp), dimension(6),             intent(in out) :: Betas      !Anisotropic temperature factors
      type(SpG_type),                          intent(in    ) :: Spgr       !Space Group
      integer,                                 intent(in out) :: Codini     !Last attributed parameter
      integer, dimension(6),                   intent(in out) :: Icodes     !codewords for betas only number
      real(kind=cp), dimension(6),             intent(in out) :: Multip     !Multipliers
      integer,                       optional, intent(in    ) :: Ord        !Order of the stabilizer
      integer, dimension(:),         optional, intent(in    ) :: Ss         !Pointer to SymmOp. of stabilizer
      integer,                       optional, intent(in    ) :: Ipr        !Printing unit for debug

      !---- Local variables ----!
      real(kind=cp),     parameter      :: EPSS=0.01_cp

      character (len=1), dimension(6)   :: cdd
      integer                           :: i,j,order
      integer,           dimension(48)  :: ss_ptr
      integer,           dimension(6)   :: codd
      integer,           dimension(3,3) :: Rsym
      real(kind=cp),     dimension(3,3) :: bet,bett,Rs,Rot
      real(kind=cp),     dimension(6)   :: cod
      real(kind=cp),     dimension(3,48):: atr

      !> Init
      cod=real(icodes)

      do j=1,6
         if (cod(j) < 1.0 .and. abs(multip(j)) > EPSS)  then
            codini=codini+1
            cod(j) = real(codini)
         end if
      end do

      if (present(ord) .and. present(ss)) then
         order=ord
         ss_ptr(1:order) = ss(1:ord)
      else
         call get_stabilizer(x,Spgr,order,ss_ptr,atr)
      end if

      bet=reshape((/17.0, 7.0,3.0,  &
                    7.0,13.0,5.0,  &
                    3.0, 5.0,11.0/),(/3,3/))
      bett=bet
      if (order > 1 ) then
         do j=2,order
            !Rsym=Spgr%SymOp(ss_ptr(j))%Rot
            Rs=Spgr%Op(ss_ptr(j))%Mat
            bett=bett+ matmul(Rs,matmul(bet,transpose(Rs)))
         end do
      end if
      Rsym=nint(1000.0*bett)
      codd=[Rsym(1,1),Rsym(2,2),Rsym(3,3),Rsym(1,2),Rsym(1,3),Rsym(2,3)]
      cdd=['a','b','c','d','e','f']
      multip=1.0_cp

      !> Search systematically all the possible constraints
      if (codd(1) == codd(2) .and. codd(1) == codd(3)) then ! a a a
         if (codd(4) == codd(5) .and. codd(4) == codd(6) ) then ! a a a d d d
            if (codd(4) == 0) then
               cdd=['a','a','a','0','0','0']     ! 1 A A A 0   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
               betas(4:6)=0.0_cp
               betas(2:3)=betas(1)
               cod(2:3)=cod(1); cod(4:6)=0.0_cp

            else
               cdd=['a','a','a','d','d','d']     ! 5 A A A D   D   D
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(5:6)=betas(4)
               betas(2:3)=betas(1)
               cod(2:3)=cod(1); cod(5:6)=cod(4)
            end if

         else if (codd(4) == -codd(5) .and. codd(4) == -codd(6) ) then !a a a d -d -d
            cdd=['a','a','a','d','d','d']       ! 6 A A A D  -D  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, -1.0_cp]
            betas(5:6)=-betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)=cod(4)

         else if (codd(4) == -codd(5) .and. codd(4) ==  codd(6) ) then !a a a d -d  d
            cdd=['a','a','a','d','d','d']       ! 7 A A A D  -D   D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, 1.0_cp]
            betas(5)=-betas(4); betas(6)=betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)= cod(4)

         else if (codd(4) ==  codd(5) .and. codd(4) == -codd(6) ) then !a a a d  d -d
            cdd=['a','a','a','d','d','d']       ! 8 A A A D   D  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
            betas(6)=-betas(4); betas(5)=betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)= cod(4)
         end if

      else if (codd(1) == codd(2)) then ! a a c
         if (codd(4) == codd(5) .and. codd(4) == codd(6) .and. codd(4) == 0) then ! a a c 0 0 0
            cdd=['a','a','c','0','0','0']     ! 2 A A C 0   0   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
            betas(4:6)=0.0
            betas(2)=betas(1)
            cod(2)=cod(1); cod(4:6)= 0.0_cp

         else if (codd(5) == codd(6) .and. codd(5) == 0) then ! a a c x 0 0
            if (codd(4) == codd(1)/2) then
               cdd=['a','a','c','a','0','0']     ! 9 A A C A/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(1)*0.5_cp
               betas(2)=betas(1)
               cod(2)=cod(1); cod(4)= cod(1); cod(5:6)=0.0_cp

            else
               cdd=['a','a','c','d','0','0']     !11 A A C D   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp
               betas(2)=betas(1)
               cod(2)=cod(1); cod(5:6)=0.0_cp
            end if

         else
            if (codd(5) == codd(6)) then  ! a a c d e e
               cdd=['a','a','c','d','e','e']     !20 A A C D   E   E
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(6)=betas(5)
               betas(2)=betas(1)
               cod(2)=cod(1); cod(6)=cod(5)

            else if (codd(5) == -codd(6)) then  ! a a c d e -e
               cdd=['a','a','c','d','e','e']     !19 A A C D   E  -E
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
               betas(6)=-betas(5)
               betas(2)=betas(1)
               cod(2)=cod(1); cod(6)=cod(5)
            end if
         end if

      else if (codd(1) == codd(3)) then ! a b a
         if (codd(4) == codd(6)) then    ! a b a d x d
            if (codd(4) == 0) then  ! a b a 0 x 0
               if (codd(5) == 0) then ! a b a 0 0 0
                  cdd=['a','b','a','0','0','0']     ! 3 A B A 0   0   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
                  betas(4:6)=0.0_cp
                  betas(3)=betas(1)
                  cod(3)=cod(1); cod(4:6)=0.0_cp

               else                  ! a b a 0 e 0
                  cdd=['a','b','a','0','e','0']     !12 A B A 0   E   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 1.0_cp, 0.0_cp]
                  betas(4)=0.0_cp;  betas(6)=0.0_cp
                  betas(3)=betas(1)
                  cod(3)=cod(1); cod(4)=0.0_cp;  cod(6)=0.0_cp
               end if

            else  !! a b a d e d
               cdd=['a','b','a','d','e','d']       !22 A B A D   E   D
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(6)=betas(4)
               betas(3)=betas(1)
               cod(3)=cod(1); cod(6)=cod(4)
            end if

         else if (codd(4) == -codd(6)) then ! a b a d e -d
            cdd=['a','b','a','d','e','d']         !21 A B A D   E  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
            betas(6)=-betas(4)
            betas(3)=betas(1)
            cod(3)=cod(1); cod(6)=cod(4)
         end if

      else if (codd(2) == codd(3)) then ! a b b
         if (codd(4) == codd(5)) then    ! a b b d d x
            if (codd(4) == 0) then  ! a b b 0 0 x
               if (codd(6) == 0) then ! a b b 0 0 0
                  cdd=['a','b','b','0','0','0']     ! 4 A B B 0   0   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
                  betas(4:6)=0.0_cp
                  betas(3)=betas(2)
                  cod(3)=cod(2); cod(4:6)=0.0_cp

               else                  ! a b b 0 0 f
                  cdd=['a','b','b','0','0','f']     !13 A B B 0   0   F
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 1.0_cp]
                  betas(4:5)=0.0_cp
                  betas(3)=betas(2)
                  cod(3)=cod(2); cod(4:5)=0.0_cp
               end if

            else  !! a b b d d f
               cdd=['a','b','b','d','d','f']       !24 A B B D   D   F
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(5)=betas(4)
               betas(3)=betas(2)
               cod(3)=cod(2); cod(5)=cod(4)
            end if

         else if (codd(4) == -codd(5)) then ! a b b d -d e
            cdd=['a','b','b','d','d','f']         !23 A B B D  -D   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, 1.0_cp]
            betas(5)=-betas(4)
            betas(3)=betas(2)
            cod(3)=cod(2); cod(5)=cod(4)
         end if

      else !Now a /= b /= c
         if (codd(4) == codd(5) .and. codd(4) == 0) then ! a b c 0 0 x
            if (codd(6) == 0) then ! a b c 0 0 0
               cdd=['a','b','c','0','0','0']          !10 A B C 0   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
               betas(4:6)=0.0_cp
               cod(4:6)=0.0_cp

            else
               cdd=['a','b','c','0','0','f']          !18 A B C 0   0   F
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 1.0_cp]
               betas(4:5)=0.0_cp
               cod(4:5)=0.0_cp
            end  if

         else if (codd(5) == codd(6) .and. codd(5) == 0) then  ! a b c x 0 0
            if (codd(4) == codd(1)/2) then ! a b c a/2 0 0
               cdd=['a','b','c','a','0','0']          !15 A B C A/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(1)*0.5_cp
               cod(4)=cod(1); cod(5:6)=0.0_cp

            else if(codd(4) == codd(2)/2) then    !a b c b/2 0 0
               cdd=['a','b','c','b','0','0']          !14 A B C B/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(2)*0.5_cp
               cod(4)=cod(2); cod(5:6)=0.0_cp

            else
               cdd=['a','b','c','d','0','0']          !16 A B C D   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp
               cod(5:6)=0.0_cp
            end if

         else if (codd(4) == codd(6) .and. codd(4) == 0) then !a b c 0 e 0
            cdd=['a','b','c','0','e','0']            !17 A B C 0   E   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 1.0_cp, 0.0_cp]
            betas(4)=0.0_cp; betas(6)=0.0_cp
            cod(4)=0.0_cp; cod(6)=0.0_cp

         else if (codd(4) == codd(1)/2 .and. codd(5) == 0) then !a b c a/2 0 f
            cdd=['a','b','c','a','0','f']            !26 A B C A/2 0   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 1.0_cp]
            betas(4)=betas(1)*0.5_cp; betas(5)=0.0_cp
            cod(4)=cod(1); cod(5)=0.0_cp

         else if (codd(4) == codd(2)/2 .and. codd(6) == 0) then !a b c b/2 e 0
            cdd=['a','b','c','b','e','0']            !27 A B C B/2 E   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 1.0_cp, 0.0_cp]
            betas(4)=betas(2)*0.5_cp; betas(6)=0.0_cp
            cod(4)=cod(2); cod(6)=0.0_cp

         else if (codd(4) == codd(2)/2 .and. codd(5) == codd(6)/2) then !a b c b/2 f/2 f
            cdd=(/'a','b','c','b','f','f'/)            !25 A B C B/2 F/2 F
            multip=(/1.0,1.0,1.0,0.5,0.5,1.0/)
            betas(4)=betas(2)*0.5; betas(5)=betas(6)*0.5
            cod(4)=cod(2); cod(5)=cod(6)

         else if(codd(4) == codd(1)/2 .and. codd(6) == codd(5)/2) then !a b c a/2 e e/2
            cdd=['a','b','c','a','e','e']            !28 A B C A/2 E   E/2
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 1.0_cp, 0.5_cp]
            betas(4)=betas(1)*0.5_cp; betas(6)=betas(5)*0.5_cp
            cod(4)=cod(1); cod(6)=cod(5)

         else
            cdd=['a','b','c','d','e','f']            !29 A B C D   E   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
         end if
      end if

      do j=1,6
         if (multip(j) < EPSS .or. cdd(j) == "0" ) then
            icodes(j) = 0

         else
            icodes(j) = nint(cod(j))
         end if
      end do

      if (present(Ipr)) then
         write(Ipr,'(a,6i5)')           '     Codes on Betas       :  ',Icodes
         write(Ipr,'(a,6(a,1x),6f7.3)') '     Codes and multipliers:  ',cdd,multip
         write(Ipr,'(a)')               '     Beta_TOT matrix:  '
         do i=1,3
            write(Ipr,'(a,3f12.4)')       '                      ',bett(i,:)
         end do
      end if
   End Subroutine Get_AtomBet_CTR

   !!--++
   !!--++  Subroutine Get_Atompos_Ctr
   !!--++
   !!--++     (Private)
   !!--++     Subroutine to get the appropriate constraints in the refinement codes of
   !!--++     atoms positions. The algorithm is based in an analysis of the symbol generated
   !!--++     for the symmetry elements of the operators belonging to the stabilizer of the
   !!--++     atom position. This routine operates with splitted codes in the sense of
   !!--++     FullProf rules
   !!--++
   !!--++     Updated: April - 2022
   !!
   Module Subroutine Get_AtomPos_CTR(X, Spgr, Codini, ICodes, Multip, Ord, Ss, Att,Ipr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),            intent(in)     :: X
      type(SpG_type),                         intent(in)     :: Spgr
      integer,                                intent(in out) :: Codini
      integer,       dimension(3),            intent(in out) :: ICodes
      real(kind=cp), dimension(3),            intent(in out) :: Multip
      integer,                       optional,intent(in)     :: Ord
      integer, dimension(:),         optional,intent(in)     :: Ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: Att
      integer,                       optional,intent(in)     :: Ipr

      !---- Local variables ----!
      real(kind=cp),     parameter     :: EPSS=0.001_cp

      integer                          :: i,j,k,order,L,L1,L2,ipar,j1
      integer,          dimension(3,3) :: RSym
      integer,          dimension(48)  :: ss_ptr
      real(kind=cp),    dimension(3,48):: atr
      real(kind=cp),    dimension(3)   :: tr

      character(len=40)                :: symbol,tsymbol,sym_symb
      character(len=10), dimension(3)  :: nsymb
      character(len=3),  dimension(3)  :: ssymb

      type(Symm_Oper_Type)             :: Op

      if (present(ord) .and. present(ss) .and. present(att)) then
         order=ord
         ss_ptr(1:order) = ss(1:ord)
         atr(:,1:order)  = att(:,1:ord)
      else
         call get_stabilizer(x,Spgr,order,ss_ptr,atr)
      end if

      !> If codes were not assigned with explicit number
      !> attribute numbers bigger than initial Codini
      do j=1,3
         if (Icodes(j) < 1  .and. abs(multip(j)) > EPSS)  then
            codini = codini+1
            Icodes(j) = codini
         end if
      end do

      ssymb=["  x","  y","  z"]

      if (present(Ipr)) write(unit=Ipr,fmt='(/a,3f10.5)')  ' => Atom Position:',x

      if (order > 1 ) then  !A constraint in position must exist
         if (present(Ipr)) write(unit=Ipr,fmt='(a)')   ' => List of symmetry element of the stabilizer without identity:'
         do k=2,order
            symbol=" "
            !Rsym=Spgr%SymOp(ss_ptr(k))%Rot
            !tr=Spgr%SymOp(ss_ptr(k))%tr + atr(:,k)
            Rsym=Spgr%Op(ss_ptr(k))%Mat(1:3,1:3)
            tr=Spgr%Op(ss_ptr(k))%Mat(1:3,4)
            tr=tr+atr(:,k)

            !call Get_SymSymb(Rsym,tr,Sym_Symb)
            !call symmetry_symbol(Sym_Symb,tsymbol)
            Sym_Symb=Get_Symb_from_OP(Rsym,tr)
            tsymbol=symmetry_symbol(Rsym,tr)

            i=index(tsymbol,";")
            if (i /= 0) then
               symbol=tsymbol(1:i-1)
               !call Read_Xsym(tsymbol(i+1:),1,Rsym,Tr,.false.)
               Op=Get_Op_from_Symb(tsymbol(i+1:))
               tr=Op%Mat(1:3,4)

               if (sum(abs(x-tr)) < EPSS) then
                  ssymb=["  0","  0","  0"]
                  if (present(Ipr)) then
                     write(unit=Ipr,fmt="(a,i2,a,t20,a,t55,a,t90,4a)") "     Operator ",k,": ", &
                     trim(Sym_Symb),trim(tsymbol),"  ssymb:" ,(ssymb(j)//"  ",j=1,3)
                  end if
                  cycle
               end if

            else
               symbol=tsymbol
            end if

            ipar=index(symbol,")")              !Translation element appears before position
            L =index(symbol(ipar+1:)," ")+ipar  !Position of the first blank after translation
            L1=index(symbol(ipar+1:),",")+ipar  !Position of the first comma after translation
            L2=index(symbol(L1+1:),",")+L1      !Position of the second comma
            if (L1 == 0) L1=1
            if (L2 == 0) L2=1
            if (L  == 0) L=1

            !> Construct a new symbol that estabish automatically the constraints
            nsymb = (/symbol(L+1:L1-1),symbol(L1+1:L2-1),symbol(L2+1:)/)
            do i=1,3
               do j=1,10  !Delete unwanted symbols (keep only x,y,z,2 and -
                  if (nsymb(i)(j:j) == " ") cycle
                  if (nsymb(i)(j:j) /= "x" .and. nsymb(i)(j:j) /= "y" .and. &
                      nsymb(i)(j:j) /= "z" .and. nsymb(i)(j:j) /= "-" .and. &
                      nsymb(i)(j:j) /= "2" ) nsymb(i)(j:j)=" "
               end do
               if (len_trim(nsymb(i))  == 0 .or. (index(nsymb(i),"x") == 0 .and. &
                   index(nsymb(i),"y") == 0 .and. index(nsymb(i),"z") == 0  ) ) then
                  ssymb(i)="  0"
                  cycle
               end if

               !> Now remove 2s on the right of x,y, or z
               j1=index(nsymb(i),"2")
               if ( j1 /= 0) then
                  if (len_trim(nsymb(i)) == j1) nsymb(i)=nsymb(i)(1:j1-1)
               end if

               !> Now remove -s on the right of x,y, or z
               j1=index(nsymb(i),"-")
               if ( j1 /= 0) then
                  if (len_trim(nsymb(i)) == j1) nsymb(i)=nsymb(i)(1:j1-1)
               end if
               nsymb(i)= adjustl(nsymb(i))
            end do

            if (ssymb(1) /= "  0" .and. ssymb(1) /= "  a") then
               ssymb(1)= nsymb(1)
               ssymb(1)= adjustr(ssymb(1))
            end if

            if (ssymb(2) /= "  0" .and. ssymb(2) /= "  a" .and. ssymb(2) /= "  b" .and. &
               ssymb(2) /= " -a" .and. ssymb(2) /= " 2a"   ) then
               ssymb(2) = nsymb(2)
               ssymb(2) = adjustr(ssymb(2))
            end if

            if (ssymb(3) /= "  0" .and. ssymb(3) /= "  a" .and. ssymb(3) /= "  b" .and. &
               ssymb(3) /= "  c" .and. ssymb(3) /= " 2a" .and. ssymb(3) /= " 2b" .and. &
               ssymb(3) /= " -a" .and. ssymb(3) /= " -b") then
               ssymb(3) = nsymb(3)
               ssymb(3) = adjustr(ssymb(3))
            end if

            do i=1,3
               if (ssymb(i)(3:3) == "x")  ssymb(i)(3:3) = "a"
            end do
            do i=1,3
               if (ssymb(i)(3:3) == "y")  ssymb(i)(3:3) = "b"
            end do
            do i=1,3
               if (ssymb(i)(3:3) == "z")  ssymb(i)(3:3) = "c"
            end do
            if (present(Ipr)) then
               write(unit=Ipr,fmt="(a,i2,a,t20,a,t55,a,t90,4a)") "     Operator ",k,": ", &
               trim(Sym_Symb),trim(tsymbol),"  Ssymb:" ,(ssymb(j)//"  ",j=1,3)
            end if

         end do !do k=1,order  over operators of the stabilizer

      else
         ssymb=["  a","  b","  c"]

      end if  !order > 1

      do i=1,3                  !Fixing codes
         if (ssymb(i)=="  0") then
            Icodes(i)=0
            multip(i)=0.0
         end if
      end do

      if (index(ssymb(1),"a") /= 0) then
         do i=2,3  !Fixing codes
            if (index(ssymb(i),"-a") /= 0) then
               Icodes(i)=Icodes(1)
               multip(i)=-multip(1)

            else if (index(ssymb(i),"a") /= 0) then
               Icodes(i)=Icodes(1)
               multip(i)=multip(1)

               if (index(ssymb(i),"2") /= 0) then
                  multip(i)=2.0* multip(1)

               else if (index(ssymb(1),"2") /= 0) then
                  multip(i)=0.5* multip(1)
               end if
            end if
         end do

      else  !the x-coordinate is fixed, analyse y and z
         if (index(ssymb(2),"b") /= 0 .and. index(ssymb(3),"b") /= 0) then
            Icodes(3)=Icodes(2)
            if (ssymb(2) == ssymb(3)) then
               multip(3)= multip(2)

            else if (ssymb(3) == " -b" .and. ssymb(2) == "  b") then
               multip(3)= -multip(2)

            else if (ssymb(3) == "  b" .and. ssymb(2) == " -b") then
               multip(3)= -multip(2)
            end if
         end if
      end if

      do j=1,3
         if (abs(multip(j)) < EPSS) then
            Icodes(j) = 0
         end if
      end do

      if (present(Ipr)) then
         write(unit=Ipr,fmt="(a,3i5)")    "     Codes positions: ",Icodes
         write(unit=Ipr,fmt="(a,3f5.1)")  "     Multipliers    : ",multip
         write(unit=Ipr,fmt="(5a)")       "     Codes   string : ( ",(ssymb(j),j=1,3) ," )"
      end if
   End Subroutine Get_AtomPos_CTR

End SubModule KeyCod_Constraints