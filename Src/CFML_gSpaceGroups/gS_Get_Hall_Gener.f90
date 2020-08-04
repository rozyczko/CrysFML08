!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) SPG_Generators_from_Hall
   implicit none
   Contains
   !!----
   !!---- GET_GENERATORS_FROM_HALL
   !!----
   !!----    Magnetic Hall symbols interpretation based on descriptions on
   !!----    (1) http://cci.lbl.gov/sginfo/hall_symbols.html and
   !!----    (2) International Tables for Crystallography (2010). Vol. B, Appendix 1.4.2, pp. 122�134
   !!----
   !!---- 09/05/2019, updated 30/07/2020 (JRC)
   !!
   Module Subroutine Get_Generators_from_Hall(Hall, Gen, Ngen, R_Shift)
      !---- Arguments ----!
      character(len=*),                            intent(in)  :: Hall      ! Hall symbol
      character(len=*), dimension(:), allocatable, intent(out) :: Gen       ! String generators
      integer,                                     intent(out) :: Ngen      ! Number of genertaors
      logical, optional,                           intent(in)  :: R_Shift    ! .True. to give the shift vector in free format

      !---- Local Variables ----!
      character(len=11), parameter :: L ="PABCIRSTFHX"  !adding the symbol H used in ref. (2), putting X for unknown lattice
      character(len=13), parameter :: T ="ABCNUVWD12345"
      character(len=6),  parameter :: A ="XYZ^"//'"*'
      character(len=6),  parameter :: N ="123406"
      character(len=13), parameter :: AT='abcnuvwdABCIS'

      integer, dimension(3,3), parameter  :: X_1   = reshape([ 1, 0, 0,  0, 1, 0,  0, 0, 1],[3,3])
      integer, dimension(3,3), parameter  :: Y_1   = reshape([ 1, 0, 0,  0, 1, 0,  0, 0, 1],[3,3])
      integer, dimension(3,3), parameter  :: Z_1   = reshape([ 1, 0, 0,  0, 1, 0,  0, 0, 1],[3,3])

      integer, dimension(3,3), parameter  :: X_2   = reshape([ 1, 0, 0,  0,-1, 0,  0, 0,-1],[3,3])
      integer, dimension(3,3), parameter  :: Y_2   = reshape([-1, 0, 0,  0, 1, 0,  0, 0,-1],[3,3])
      integer, dimension(3,3), parameter  :: Z_2   = reshape([-1, 0, 0,  0,-1, 0,  0, 0, 1],[3,3])

      integer, dimension(3,3), parameter  :: X_3   = reshape([ 1, 0, 0,  0, 0, 1,  0,-1,-1],[3,3])
      integer, dimension(3,3), parameter  :: Y_3   = reshape([-1, 0,-1,  0, 1, 0,  1, 0, 0],[3,3])
      integer, dimension(3,3), parameter  :: Z_3   = reshape([ 0, 1, 0, -1,-1, 0,  0, 0, 1],[3,3])

      integer, dimension(3,3), parameter  :: X_4   = reshape([ 1, 0, 0,  0, 0, 1,  0,-1, 0],[3,3])
      integer, dimension(3,3), parameter  :: Y_4   = reshape([ 0, 0,-1,  0, 1, 0,  1, 0, 0],[3,3])
      integer, dimension(3,3), parameter  :: Z_4   = reshape([ 0, 1, 0, -1, 0, 0,  0, 0, 1],[3,3])

      integer, dimension(3,3), parameter  :: X_6   = reshape([ 1, 0, 0,  0, 1, 1,  0,-1, 0],[3,3])
      integer, dimension(3,3), parameter  :: Y_6   = reshape([ 0, 0,-1,  0, 1, 0,  1, 0, 1],[3,3])
      integer, dimension(3,3), parameter  :: Z_6   = reshape([ 1, 1, 0, -1, 0, 0,  0, 0, 1],[3,3])

      integer, dimension(3,3), parameter  :: X_2P  = reshape([-1, 0, 0,  0, 0,-1,  0,-1, 0],[3,3])
      integer, dimension(3,3), parameter  :: Y_2P  = reshape([ 0, 0,-1,  0,-1, 0, -1, 0, 0],[3,3])
      integer, dimension(3,3), parameter  :: Z_2P  = reshape([ 0,-1, 0, -1, 0, 0,  0, 0,-1],[3,3])

      integer, dimension(3,3), parameter  :: X_2PP = reshape([-1, 0, 0,  0, 0, 1,  0, 1, 0],[3,3])
      integer, dimension(3,3), parameter  :: Y_2PP = reshape([ 0, 0, 1,  0,-1, 0,  1, 0, 0],[3,3])
      integer, dimension(3,3), parameter  :: Z_2PP = reshape([ 0, 1, 0,  1, 0, 0,  0, 0,-1],[3,3])

      integer, dimension(3,3), parameter  :: XYZ_3 = reshape([ 0, 1, 0,  0, 0, 1,  1, 0, 0],[3,3])

      integer, dimension(4,4), parameter :: IDENTIDAD = reshape([1, 0, 0, 0, &
                                                                 0, 1, 0, 0, &
                                                                 0, 0, 1, 0, &
                                                                 0, 0, 0, 1],[4,4])

      integer,             parameter :: PMAX= 5  ! Maximum number of Operators

      integer, dimension(PMAX)       :: Ni       ! rotation symbol for each operator
      integer, dimension(PMAX)       :: Ai       ! axis symbol for each operator
      integer, dimension(3,PMAX)     :: Ti       ! traslation for each symbol
      logical, dimension(PMAX)       :: Tr       ! time reversal

      character(len=20)              :: str
      character(len=3)               :: car
      character(len=10),dimension(5) :: dire
      integer                        :: i,j,n1,n2,nt,iv,signo
      integer                        :: ilat, axis
      integer, dimension(3)          :: ishift, v_trans, a_latt
      integer, dimension(3)          :: ivet
      real(kind=cp), dimension(3)    :: vet
      logical                        :: centro,free,shift

      type(rational), dimension(4,4) :: sn, snp
      type(rational), dimension(3)   :: sh
      type(symm_oper_type)           :: op

      logical, parameter             :: pout=.false.

      !> Init
      ngen=0
      free=.false.
      shift=.false.
      if (present(R_Shift)) free=R_Shift

      !> Copy
      str=adjustl(Hall)

      !>
      !> Shift origin (N1 N2 N3)
      !>
      ishift=0
      sh=0//1

      n1=index(str,'(')
      n2=index(str,')')
      nt=len_trim(str)
      if ((n1 == 0 .and. n2 > 0) .or. (n1 > 0 .and. n2 == 0) .or. (n1 > n2)) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Error with shift origin format!"
         return
      end if

      if (n1 > 0) then
         call get_num(str(n1+1:n2-1), vet, ivet, iv)
         if (iv /= 3) then
            err_CFML%IErr=1
            err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Error with shift origin format!"
            return
         end if
         shift=.true.

         if (.not. free) then
            ishift=ivet
            ishift=mod(ishift +24, 12)
            sh=real(ishift)/12.0_cp
         else
            sh=vet
         end if

         if (n2+1 > nt) then
            str=str(:n1-1)
         else
            str=str(:n1-1)//' '//str(n2+1:)
         end if
      end if

      !>
      !> Lattice traslational (L)
      !>
      centro=.false.
      ilat=0

      if (str(1:1)=='-') then
         centro=.true.
         str=adjustl(str(2:))  !Removing the initial sign from the Hall-symbol
      end if

      ilat=index(L,u_case(str(1:1)))
      if (ilat == 0) then
         err_CFML%IErr=1
         err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Unknown lattice traslational symmetry!"
         return
      end if
      str=adjustl(str(2:))  !Removing the lattice symbol for the Hall-symbol

      !>
      !> Anti-traslational operators
      !>
      a_latt=0
      car=" "
      n1=index(str,"1'")
      if (n1 > 1) then
        if( str(n1-1:n1-1) == ' ') then
          if (len_trim(str) > n1+1) then
             car=str(n1+2:)
             do i=1,len_trim(car)
                n2=index(AT,car(i:i))
                if (n2 == 0) then
                   err_CFML%IErr=1
                   err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Unknown anti-lattice traslational symmetry!"
                   return
                end if
             end do
             select case (trim(car))
                case ("a")
                   a_latt=[6,0,0]
                case ("b")
                   a_latt=[0,6,0]
                case ("c")
                   a_latt=[0,0,6]
                case ("ab","ba")
                   a_latt=[6,6,0]
                case ("ac","ca")
                   a_latt=[6,0,6]
                case ("bc","cb")
                   a_latt=[0,6,6]
                case ("n","abc","acb","bca","bac","cab","cba")
                   a_latt=[6,6,6]
                case ("u")
                   a_latt=[3,0,0]
                case ("v")
                   a_latt=[0,3,0]
                case ("w")
                   a_latt=[0,0,3]
                case ("uv","vu")
                   a_latt=[3,3,0]
                case ("uw","wu")
                   a_latt=[3,0,3]
                case ("vw","wv")
                   a_latt=[0,3,3]
                case ("d","uvw","uwv","vuw","vwu","wuv","wvu")
                   a_latt=[3,3,3]
                case ("A")
                   a_latt=[0,6,6]
                case ("B")
                   a_latt=[6,0,6]
                case ("C")
                   a_latt=[6,6,0]
                case ("I")
                   if (ilat ==6) then  ! R Lattice
                      a_latt=[0,0,6]
                   else
                      a_latt=[6,6,6]
                   end if
                case ("S")
                   if (ilat ==9) then ! F Lattice
                      a_latt=[6,6,6]
                   else
                      a_latt=[0,0,6]
                   end if
             end select
             str=adjustl(str(:n1-1))
          end if
        end if
      end if

      str=u_case(str)

      !>
      !> Operators
      !>
      dire=" "
      Ni=0; Ai=0; Ti=0
      call Allocate_Op(4, Op)  ! 4 is Dimension

      call get_words(str, dire, iv) !dire constains the items separated by blanks in the stripped (without L-symbol) Hall symbol
      if (iv ==0 ) then
         err_CFML%IErr=1
         err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Hall symbol format is wrong, please check it!"
         return
      end if

      do i=1, iv
         if (pout) then
            print*,' '
            print*,'  ---> Operator description: ',i, trim(dire(i))
         end if

         !> Ni (Rotation)
         signo=1
         if (dire(i)(1:1)=='-') then
            signo=-1
            dire(i)=adjustl(dire(i)(2:))
         end if

         j=index(N,dire(i)(1:1))
         if (j == 0 .or. j==5) then
            err_CFML%IErr=1
            err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Wrong proper/improper rotation symbol!"
            return
         end if
         Ni(i)=j
         dire(i)=adjustl(dire(i)(2:))  !Direction axis + Translation symbol

         if (pout) print*,'  ---> Rotation order: ', signo*Ni(i)

         !> Ai (axis)
         axis=0
         j=index(A,dire(i)(1:1))
         if (j == 0) then
            !> When the axis symbol can be omitted
            select case (i)
               case (1) !> First operator
                  axis=3 ! c axis

               case (2) !> Second operator (precedence rules)
                  if (Ni(i) == 2) then
                     if (abs(Ni(i-1)) == 2 .or. abs(Ni(i-1)) == 4) then
                        axis=1 ! a axis

                     elseif (abs(Ni(i-1))==3 .or. abs(Ni(i-1))==6) then
                        axis=7 ! a-b
                     end if
                  end if

               case (3) !> Third operator
                  if (abs(Ni(i)) == 3) then
                     axis=10 ! a+b+c
                  end if
            end select

         else
            !Now the axis symbol is explicitly given
            select case (j)
               case (1:3)
                  axis=j  ! a or b or c

               case (4) !^ (^ replaces ' in the original Hall-symbols)
                  if (Ni(i) /= 2) then
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: The rotation order of the operator should be 2!"
                     return
                  end if
                  select case (Ai(i-1))
                     case (1)
                        axis=9 ! b-c
                     case (2)
                        axis=8 ! a-c
                     case (3)
                        axis=7 ! a-b
                  end select

               case (5) !"
                  if (Ni(i) /= 2) then
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: The rotation order of the operator should be 2!"
                     return
                  end if
                  select case (Ai(i-1))
                     case (1)
                        axis=6 ! b+c
                     case (2)
                        axis=5 ! c+a
                     case (3)
                        axis=4 ! a+b
                  end select
               case (6) !*
                  axis=10 !a+b+c
            end select
            dire(i)=adjustl(dire(i)(2:))
         end if

         !> Axis could be 0 only if order the last operator is 1
         if (axis==0 .and. Ni(i) /= 1) then
            err_CFML%IErr=1
            err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: The axis symbol (A) in Hall is wrong!"
            return
         end if
         Ai(i)=axis

         if (pout) print*,'  ---> Axis: ',Ai(i)

         !> Symbol T
         v_trans=0
         do
            if (len_trim(dire(i))==0) exit

            j=index(T,dire(i)(1:1))
            if (j==0) exit
            select case (j)
               case (1) ! a
                  v_trans=v_trans+[6,0,0]
               case (2) ! b
                  v_trans=v_trans+[0,6,0]
               case (3) ! c
                  v_trans=v_trans+[0,0,6]
               case (4) ! n
                  v_trans=v_trans+[6,6,6]
               case (5) ! u
                  v_trans=v_trans+[3,0,0]
               case (6) ! v
                  v_trans=v_trans+[0,3,0]
               case (7) ! w
                  v_trans=v_trans+[0,0,3]
               case (8) ! d
                  v_trans=v_trans+[3,3,3]

               case (9) ! 1
                  select case (Ni(i))
                     case (3) ! 1/3
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[4,0,0]
                           case (2)
                              v_trans=v_trans+[0,4,0]
                           case (3)
                              v_trans=v_trans+[0,0,4]
                        end select

                     case (4) ! 1/4
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[3,0,0]
                           case (2)
                              v_trans=v_trans+[0,3,0]
                           case (3)
                              v_trans=v_trans+[0,0,3]
                        end select

                     case (6) ! 1/6
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[2,0,0]
                           case (2)
                              v_trans=v_trans+[0,2,0]
                           case (3)
                              v_trans=v_trans+[0,0,2]
                        end select
                  end select

               case (10)! 2
                  select case (Ni(i))
                     case (3) ! 2/3
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[8,0,0]
                           case (2)
                              v_trans=v_trans+[0,8,0]
                           case (3)
                              v_trans=v_trans+[0,0,8]
                        end select

                     case (6) ! 2/6
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[4,0,0]
                           case (2)
                              v_trans=v_trans+[0,4,0]
                           case (3)
                              v_trans=v_trans+[0,0,4]
                        end select
                  end select

               case (11)! 3
                  select case (Ni(i))
                     case (4) ! 3/4
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[9,0,0]
                           case (2)
                              v_trans=v_trans+[0,9,0]
                           case (3)
                              v_trans=v_trans+[0,0,9]
                        end select
                  end select

               case (12)! 4
                  select case (Ni(i))
                     case (6) ! 4/6
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[8,0,0]
                           case (2)
                              v_trans=v_trans+[0,8,0]
                           case (3)
                              v_trans=v_trans+[0,0,8]
                        end select
                  end select

               case (13)! 5
                  select case (Ni(i))
                     case (6) ! 5/6
                        select case (Ai(i))
                           case (1)
                              v_trans=v_trans+[10,0,0]
                           case (2)
                              v_trans=v_trans+[0,10,0]
                           case (3)
                              v_trans=v_trans+[0,0,10]
                        end select
                  end select
            end select
            dire(i)=adjustl(dire(i)(2:))
         end do
         Ti(:,i)=v_trans

         if (pout) print*,'  --->Traslation: ',Ti(:,i)

         !> Time reversal
         Tr(i)=.false.
         if (len_trim(dire(i)) > 0) then
            if (dire(i)(1:1) /="'") then
               err_CFML%IErr=1
               err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Check the MHall symbol!"
               return
            end if
            Tr(i)=.true.

            if (pout) then
               if (tr(i)) then
                  print*,'  ---> Primed operator'
               end if
            end if
         end if

         !> Save the signo in rotation
         Ni(i)=Ni(i)*signo
      end do

      !> In the original paper it was not commented but it is possible to
      !> define a shift using the last operator with rotation order -1
      !> and given additional traslation
      !if (Ni(iv)== -1 .and. (.not. tr(iv))) then
      !   shift=.true.
      !   ishift=mod(-Ti(:,iv) + 24, 12)
      !   sh=real(ishift)/12.0_cp
      !
      !   iv=iv-1
      !end if
      !if (pout) print*,'  ---> Shift: ', rational_string(sh)

      !> Allocate Gen
      nt=iv
      if (centro) nt=nt+1
      if (any(a_latt > 0)) nt=nt+1
      select case (ilat)
         case (2:5)
            nt=nt+1
         case (6:8,10)
            nt=nt+2
         case (9)
            nt=nt+3
      end select
      if (nt == 0) then
         err_CFML%IErr=1
         err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Please check the Hall symbol!"
         return
      end if

      if (allocated(gen)) deallocate(gen)
      allocate(gen(nt))
      gen=" "

      !> Generators
      ngen=0
      do i=1,iv
         sn=identidad
         signo=sign(1,Ni(i))

         select case (abs(Ni(i)))
            case (1)
               select case (Ai(i))
                  case (0)
                     !if (.not. tr(i)) then
                     !   err_CFML%IErr=1
                     !   err_CFML%Msg="Get_Generators_from_MHall@GSPACEGROUPS: Check symbol!"
                     !   return
                     !end if
                     sn=sign(1,Ni(i))*identidad
                  case (1)
                     sn(1:3,1:3)=signo*X_1
                  case (2)
                     sn(1:3,1:3)=signo*Y_1
                  case (3)
                     sn(1:3,1:3)=signo*Z_1
                  case default
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Rotation order 1, Incompatible axis!"
                     return
               end select

            case (2)
               select case (Ai(i))
                  case (1)
                     sn(1:3,1:3)=signo*X_2
                  case (2)
                     sn(1:3,1:3)=signo*Y_2
                  case (3)
                     sn(1:3,1:3)=signo*Z_2
                  case (4)
                     sn(1:3,1:3)=signo*Z_2PP
                  case (5)
                     sn(1:3,1:3)=signo*X_2PP
                  case (6)
                     sn(1:3,1:3)=signo*Y_2PP
                  case (7)
                     sn(1:3,1:3)=signo*Z_2P
                  case (8)
                     sn(1:3,1:3)=signo*X_2P
                  case (9)
                     sn(1:3,1:3)=signo*Y_2PP
                  case default
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Rotation order 2, Incompatible axis!"
                     return
               end select

            case (3)
               select case (Ai(i))
                  case (1)
                     sn(1:3,1:3)=signo*X_3
                  case (2)
                     sn(1:3,1:3)=signo*Y_3
                  case (3)
                     sn(1:3,1:3)=signo*Z_3
                  case (10)
                     sn(1:3,1:3)=signo*XYZ_3
                  case default
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Rotation order 3, Incompatible axis!"
                     return
               end select

            case (4)
               select case (Ai(i))
                  case (1)
                     sn(1:3,1:3)=signo*X_4
                  case (2)
                     sn(1:3,1:3)=signo*Y_4
                  case (3)
                     sn(1:3,1:3)=signo*Z_4
                  case default
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Rotation order 4, Incompatible axis!"
                     return
               end select

            case (6)
               select case (Ai(i))
                  case (1)
                     sn(1:3,1:3)=signo*X_6
                  case (2)
                     sn(1:3,1:3)=signo*Y_6
                  case (3)
                     sn(1:3,1:3)=signo*Z_6
                  case default
                     err_CFML%IErr=1
                     err_CFML%Msg="Get_Generators_from_Hall@GSPACEGROUPS: Rotation order 4, Incompatible axis!"
                     return
               end select
         end select
         sn(1:3,4)=Ti(:,i)/12.0_cp

         if (shift) then
            snp=identidad
            snp(1:3,4)=sh
            sn=matmul(snp,sn)

            snp(1:3,4)=-sh
            sn=matmul(sn,snp)
            sn(1:3,4)=rational_modulo_lat(sn(1:3,4))
         end if

         op%mat=sn
         op%Time_Inv=1
         if (tr(i)) op%Time_Inv=-1

         ngen=ngen+1
         gen(ngen)=Get_Symb_from_Op(op)
      end do

      !> Centro symmetric
      if (centro) then
         sn=-identidad

         if (shift) then
            snp=identidad
            snp(1:3,4)=sh
            sn=matmul(snp,sn)

            snp(1:3,4)=-sh
            sn=matmul(sn,snp)
            sn(1:3,4)=rational_modulo_lat(sn(1:3,4))
         end if

         op%mat=sn
         op%Time_Inv=1

         ngen=ngen+1
         gen(ngen)=Get_Symb_from_Op(op)
      end if

      !> Anti-traslation
      if (any(a_latt > 0)) then
         sn=identidad
         sn(4,4)=1
         sn(1:3,4)=a_latt/12.0_cp

         op%mat=sn
         op%Time_Inv=-1

         ngen=ngen+1
         gen(ngen)=Get_Symb_from_Op(op)
      end if

      if (ilat > 1 .and. ilat < 11) then
         do i=1,3 ! Loops
            select case (i)
               case (1) ! First loop
                  select case (ilat)
                     case (2)
                        sn=identidad
                        sn(1:3,4)=[0//1, 1//2, 1//2]

                     case (3)
                        sn=identidad
                        sn(1:3,4)=[1//2, 0//1, 1//2]

                     case (4)
                        sn=identidad
                        sn(1:3,4)=[1//2, 1//2, 0//1]

                     case (5)
                        sn=identidad
                        sn(1:3,4)=[1//2, 1//2, 1//2]

                     case (6)
                        sn=identidad
                        sn(1:3,4)=[2//3, 1//3, 1//3]

                     case (7)
                        sn=identidad
                        sn(1:3,4)=[1//3, 1//3, 2//3]

                     case (8)
                        sn=identidad
                        sn(1:3,4)=[1//3, 2//3, 1//3]

                     case (9)
                        sn=identidad
                        sn(1:3,4)=[0//1, 1//2, 1//2]

                     case (10)
                        sn=identidad
                        sn(1:3,4)=[2//3, 1//3, 0//1]

                  end select

               case (2) ! Second loop
                  select case (ilat)
                     case (6)
                        sn=identidad
                        sn(1:3,4)=[1//3, 2//3, 2//3]

                     case (7)
                        sn=identidad
                        sn(1:3,4)=[2//3, 2//3, 1//3]

                     case (8)
                        sn=identidad
                        sn(1:3,4)=[2//3, 1//3, 2//3]

                     case (9)
                        sn=identidad
                        sn(1:3,4)=[1//2, 0//1, 1//2]

                     case (10)
                        sn=identidad
                        sn(1:3,4)=[1//3, 2//3, 0//1]
                  end select

               case (3) ! Third loop
                  if (ilat ==9) then
                     sn=identidad
                     sn(1:3,4)=[1//2, 1//2, 0//1]
                  end if
            end select

            if (shift ) then
               snp=identidad
               snp(1:3,4)=sh
               sn=matmul(snp,sn)

               snp(1:3,4)=-sh
               sn=matmul(sn,snp)
               sn(1:3,4)=rational_modulo_lat(sn(1:3,4))
            end if

            op%mat=sn
            op%Time_Inv=1

            ngen=ngen+1
            gen(ngen)=Get_Symb_from_Op(op)

            if (ilat < 6) exit              ! only one cycle
            if (ilat < 9 .and. i==2) exit   ! two cycles
            if (ilat == 10 .and. i==2) exit ! two cycles, H-centring

         end do
      end if

      if (pout) then
         print*,' '
         print*,'  ---> List of Possible Generators <---'
         print*,'---------------------------------------'
         do i=1,ngen
            print*,'  ---> Generator: '//trim(gen(i))
         end do
         print*,' '
      end if

   End Subroutine Get_Generators_from_Hall

   !!----
   !!---- SEARCH_HALL_OPERATORS
   !!----    Procedure that search operators that define the Hall symbol
   !!----
   !!---- 29/05/2019
   !!
   Module Function Search_Hall_Operators(G, Ishift) Result(Str)
      !---- Arguments ----!
      class(spg_type),                 intent(in)  :: G
      integer, dimension(3), optional, intent(in)  :: Ishift
      character(len=:), allocatable                :: Str

      !---- Local Variables ----!
      Type :: H_Oper
         integer         :: N     =0
         character(len=1):: Axis  =" "
         character(len=5):: Tras  =" "
         character(len=1):: Prime =" "
      End Type H_Oper

      integer,               parameter :: MAX_DIV   =12  ! Multiplo of 3 and 4



      integer,               parameter :: MAX_H_OPER=10  ! Max. number of operators to select
      integer,               parameter :: MAX_NC    =3   ! Max number of iterations on Traslation part

      integer, dimension(3), parameter :: TR_A=[MAX_DIV/2, 0, 0]
      integer, dimension(3), parameter :: TR_B=[0, MAX_DIV/2, 0]
      integer, dimension(3), parameter :: TR_C=[0, 0, MAX_DIV/2]
      integer, dimension(3), parameter :: TR_N=[MAX_DIV/2, MAX_DIV/2, MAX_DIV/2]
      integer, dimension(3), parameter :: TR_U=[MAX_DIV/4, 0, 0]
      integer, dimension(3), parameter :: TR_V=[0, MAX_DIV/4, 0]
      integer, dimension(3), parameter :: TR_W=[0, 0, MAX_DIV/4]
      integer, dimension(3), parameter :: TR_D=[MAX_DIV/4, MAX_DIV/4, MAX_DIV/4]

      character(len=2)                   :: c_rot
      character(len=1)                   :: c_axis,c_prime
      character(len=5)                   :: c_tras
      integer                            :: i,k,n,nt,nc,np
      integer                            :: j1,j2,j3,j4
      integer, dimension(3)              :: itr, itr2
      integer, dimension(MAX_H_OPER)     :: iord

      real(kind=cp), dimension(3)        :: rtr

      type(rational), dimension(3)       :: tr, ax
      type(rational), dimension(4,4)     :: p1,p2,p
      type(H_Oper), dimension(MAX_H_OPER):: HSymb
      type(Symm_Oper_Type)               :: Op


      !> Init
      Str="  "

      nt=0
      call Allocate_Op(4, Op)

      if (present(IShift)) then
         call Rational_Identity_Matrix(p1)
         call Rational_Identity_Matrix(p2)

         p1(1:3,4)=[ Ishift(1)//MAX_DIV,  Ishift(2)//MAX_DIV,  Ishift(3)//MAX_DIV]
         p2(1:3,4)=[-Ishift(1)//MAX_DIV, -Ishift(2)//MAX_DIV, -Ishift(3)//MAX_DIV]
      end if

      main: do i=1,G%multip
         Op=G%op(i)
         if (present(IShift)) then
            p=matmul(op%mat,p1)
            op%mat=matmul(p2,p)
         end if

         tr=Op%Mat(1:3,4)

         !> Purge
         if (Is_OP_Inversion_Centre(Op) .and. rational_is_nullvector(tr)) cycle
         if (Is_OP_Lattice_Centring(Op)) cycle
         if (Is_OP_1_Prime(Op)) cycle
         if (Is_OP_Minus_1_Prime(Op)) cycle
         if (Is_OP_Anti_Lattice(Op)) cycle

         !> Rotation
         n=get_rotation_order(Op%Mat(1:3,1:3))

         !> Axis
         c_axis=" "
         ax=[0//1,0//1,0//1]
         if (abs(n) > 1) then
            ax=get_rotation_axis(Op%Mat(1:3,1:3))
            if (err_CFML%Ierr /= 0) then
               call clear_error()
               cycle
            end if
         end if

         if (rational_equal(ax,[1//1, 0//1, 0//1])) then
           c_axis="x"
         else if (rational_equal(ax,[0//1, 1//1, 0//1])) then
            c_axis="y"
         else if (rational_equal(ax,[0//1, 0//1, 1//1])) then
            c_axis="z"
         else if (rational_equal(ax,[1//1, -1//1, 0//1]) .or. &    ! a-b, a-c, b-c
                  rational_equal(ax,[1//1, 0//1, -1//1]) .or. &
                  rational_equal(ax,[0//1, 1//1, -1//1])) then
            c_axis="^"
         else if (rational_equal(ax,[-1//1, 1//1, 0//1]) .or. &    ! -a+b, -a+c, -b+c
                  rational_equal(ax,[-1//1, 0//1, 1//1]) .or. &
                  rational_equal(ax,[0//1, -1//1, 1//1])) then
            c_axis="^"
         else if (rational_equal(ax,[1//1, 1//1, 0//1]) .or. &      ! a+b, a+c,b+c
                  rational_equal(ax,[1//1, 0//1, 1//1]) .or. &
                  rational_equal(ax,[0//1, 1//1, 1//1])) then
            c_axis='"'
         else if (rational_equal(ax,[1//1, 1//1, 1//1])) then
            c_axis="*"
         end if

         !> traslation
         tr=Op%Mat(1:3,4)
         rtr=tr
         itr=nint(rtr*real(MAX_DIV))
         itr=mod(itr+2*MAX_DIV, MAX_DIV)
         c_tras=" "

         nc=0
         do
            if (all(itr ==0)) exit
            select case (abs(n))
               case (3)
                  np=MAX_DIV/3
                  itr2=mod(itr,np)
                  if (all(itr2 ==0)) then
                     select case (c_axis)
                        case ('x')
                           itr2=itr-[2*np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if

                        case ('y')
                           itr2=itr-[0,2*np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if

                        case ('z')
                           itr2=itr-[0,0,2*np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,0,np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if
                        end select
                  else if (any(mod(itr,MAX_DIV/4) /= 0)) then
                     cycle main
                  end if

               case (4)
                  !> 41 is equivalent to traslation u, v, w or d
                  !> 43 is equivalent to traslation au, vw or cw

               case (6)
                  np=MAX_DIV/6
                  itr2=mod(itr,np)
                  if (all(itr2 ==0)) then
                     select case (c_axis)
                        case ('x')
                           itr2=itr-[5*np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'5'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[4*np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'4'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[2*np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[np,0,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if

                        case ('y')
                           itr2=itr-[0,5*np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'5'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,4*np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'4'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,2*np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,np,0]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if

                        case ('z')
                           itr2=itr-[0,0,5*np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'5'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,0,4*np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'4'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,0,2*np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'2'
                              itr=itr2
                              cycle
                           end if

                           itr2=itr-[0,0,np]
                           if (all(itr2 >= 0)) then
                              c_tras=trim(c_tras)//'1'
                              itr=itr2
                              cycle
                           end if

                     end select
                  else if (any(mod(itr,MAX_DIV/4) /=0)) then
                     cycle main
                  end if
            end select

            itr2=itr-TR_A
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'a'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_B
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'b'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_C
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'c'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_N
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'n'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_U
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'u'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_V
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'v'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_W
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'w'
               itr=itr2
               cycle
            end if

            itr2=itr-TR_D
            if (all(itr2 >= 0)) then
               c_tras=trim(c_tras)//'d'
               itr=itr2
               cycle
            end if

            !> Control for Maximum iterations
            if (nc == MAX_NC) exit
            nc=nc+1
         end do
         if (any (itr /= 0)) cycle main

         !> Primed
         c_prime=" "
         if (Op%Time_Inv == -1) c_Prime="'"

         !> Purge
         if (nt > 0) then
            do k=1,nt
               if (n == Hsymb(k)%N .and. c_axis == Hsymb(k)%axis) cycle main
            end do
         end if

         if (n == -1) then
            select case (G%Spg_lat)
               case ("A")
                  k=index(c_tras,'bc')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

               case ("B")
                  k=index(c_tras,'ac')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

               case ("C")
                  k=index(c_tras,'ab')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

               case ("F")
                  k=index(c_tras,'ab')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

                  k=index(c_tras,'ac')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

                  k=index(c_tras,'bc')
                  if (k > 0) then
                     if (len_trim(c_tras)==2) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+2:)
                  end if

               case ("I")
                  k=index(c_tras,'n')
                  if (k > 0) then
                     if (len_trim(c_tras)==1) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+1:)
                  end if

                  k=index(c_tras,'abc')
                  if (k > 0) then
                     if (len_trim(c_tras)==3) cycle main
                  else
                     c_tras=c_tras(:k-1)//c_tras(k+3:)
                  end if
            end select
         end if

         !> Some changes
         k=index(c_tras,'abc')
         if (k > 0) then
            if (len_trim(c_tras)==3) then
               c_tras="n"
            else
               c_tras=c_tras(:k-1)//"n"//c_tras(k+3:)
            end if
         end if
         k=index(c_tras,'uvw')
         if (k > 0) then
            if (len_trim(c_tras)==3) then
               c_tras="d"
            else
               c_tras=c_tras(:k-1)//"d"//c_tras(k+3:)
            end if
         end if
         k=index(c_tras,'21')
         if (k > 0) then
            if (len_trim(c_tras)==2) then
               c_tras="c"
            else
               c_tras=c_tras(:k-1)//"c"//c_tras(k+3:)
            end if
         end if

         if (abs(n) == 1 .and. c_prime=="'") cycle
         if (n == -1 .and. len_trim(c_tras) ==0) cycle

         nt=nt+1
         HSymb(nt)%N=n
         Hsymb(nt)%axis=c_axis
         Hsymb(nt)%Tras=c_tras
         Hsymb(nt)%Prime=c_prime

         if (nt == MAX_H_OPER) exit
      end do main

      !> First operator
      iord=0
      do i=1,nt
         if (abs(HSymb(i)%N) == 1) cycle
         if (HSymb(i)%axis =="z") then
            iord(i)=1
         end if
      end do
      k=count(iord > 0)
      j1=0
      if (k > 0) then
         n=0
         do i=1,nt
            if (iord(i)/=1) cycle
            if (n < abs(HSymb(i)%N)) then
               n=abs(HSymb(i)%N)
               j1=i
            end if
         end do
      end if
      if (j1==0) then
         j1=1
         iord(1)=1
      end if

      !> Second operator
      j2=0
      if (nt > 1) then
         n=abs(Hsymb(j1)%N)
         select case (n)
            case (2,4)
               do i=1,nt
                  if (abs(HSymb(i)%N) == 1) cycle
                  if (iord(i)/=0) cycle
                  if (HSymb(i)%axis =="x") then
                     iord(i)=2
                     j2=i
                     exit
                  end if
               end do

            case (3,6)
               do i=1,nt
                  if (abs(HSymb(i)%N) == 1) cycle
                  if (iord(i)/=0) cycle
                  if (HSymb(i)%axis =="^") then
                     iord(i)=2
                     j2=i
                     exit
                  end if
               end do
         end select
         if (j2 == 0) then
            do i=1,nt
               if (iord(i)/=0) cycle
               j2=i
               iord(i)=2
               exit
            end do
         end if
      end if

      !> Third operator
      j3=0
      if (nt > 2) then
         do i=1,nt
            if (iord(i)/=0) cycle
            if (abs(HSymb(i)%N) /= 3) cycle
            if (HSymb(i)%axis=="*") then
               j3=i
               iord(i)=3
               exit
            end if
         end do
         if (j3 ==0) then
            do i=1,nt
               if (iord(i)/=0) cycle
               if (HSymb(i)%N /= -1) cycle
               j3=i
               iord(i)=3
               exit
            end do
         end if
      end if

      !> Fourth operator
      j4=0
      if (nt > 3) then
         do i=1,nt
            if (iord(i)/=0) cycle
            if (HSymb(i)%N /= -1) cycle
            if (HSymb(i)%prime=="'") cycle
            j4=i
            iord(i)=4
            exit
         end do
      end if

      !> Symbol
      write(unit=c_rot,fmt='(i2)') HSymb(j1)%N
      if (HSymb(j1)%axis=="z") HSymb(j1)%axis=" "
      str=trim(c_rot)//trim(HSymb(j1)%axis)//trim(HSymb(j1)%tras)//trim(HSymb(j1)%prime)

      if (j2 > 0) then
         write(unit=c_rot,fmt='(i2)') HSymb(j2)%N
         if (HSymb(j2)%axis=="x" .or. HSymb(j2)%axis=="^") HSymb(j2)%axis=" "
         str=trim(str)//"  "//trim(c_rot)//trim(HSymb(j2)%axis)//trim(HSymb(j2)%tras)//trim(HSymb(j2)%prime)
      end if

      if (j3 > 0) then
         write(unit=c_rot,fmt='(i2)') HSymb(j3)%N
         if (HSymb(j3)%axis=="*") HSymb(j3)%axis=" "
         str=trim(str)//"  "//trim(c_rot)//trim(HSymb(j3)%axis)//trim(HSymb(j3)%tras)//trim(HSymb(j3)%prime)
      end if

      if (j4 > 0) then
         write(unit=c_rot,fmt='(i2)') HSymb(j4)%N
         str=trim(str)//"  "//trim(c_rot)//trim(HSymb(j4)%axis)//trim(HSymb(j4)%tras)//trim(HSymb(j4)%prime)
      end if

   End Function Search_Hall_Operators

   !!----
   !!---- GET_HALL_FROM_GENERATORS
   !!----     Procedure that obtain the hall symbol from a list of generators
   !!----     that create a Space group.
   !!----
   !!---- 29/05/2019
   !!
   Module Function Get_Hall_from_Generators(Ngen, Gen, Ishift) Result(Hall)
      !---- Arguments ----!
      integer,                         intent(in) :: NGen
      character(len=*), dimension(:),  intent(in) :: Gen
      integer, dimension(3), optional, intent(in) :: ishift
      character(len=:), allocatable               :: Hall

      !---- Local Variables ----!
      type(spg_type)                 :: Grp
      type(rational), dimension(3,3) :: Identidad
      type(rational), dimension(4,4) :: Mat
      type(rational), dimension(3)   :: tr

      character(len=2)               :: c_latt
      character(len=1)               :: c_alatt
      character(len=5)               :: car_prime
      character(len=8), dimension(5) :: car_op,nc_lat,nc_alat
      character(len=40)              :: str_hall
      integer                        :: prime,n,iv,Invt
      integer                        :: n1,n2,n_lat,n_alat

      logical                        :: pout=.false.

      if (pout) then
         print*,'Generators'
         print*,'-----------'
         do n=1,ngen
            print*,'Gen:',n,trim(gen(n))
         end do
      end if

      !> Init
      Hall="  "

      if (ngen <=0) then
         Err_CFML%Ierr=1
         Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Number of generators is zero!"
      end if

      call Init_SpaceGroup(Grp)
      call Rational_Identity_Matrix(identidad)

      !Examine the input generators to determine non-conventional lattice of anti-lattice type
      !and add the (anti)lattice generators to the Hall symbol
      n_lat=0; n_alat=0
      do n=1,ngen
        call Get_Mat_From_Symb(gen(n), Mat, Invt)
        if(rational_equal(Mat(1:3,1:3),identidad)) then
          tr=Mat(1:3,4)
          tr=rational_modulo_lat(tr)
          if (rational_equal(tr,[1//2, 0//1, 0//1])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1a"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'a"
             end if
             cycle
          end if
          if (rational_equal(tr,[0//1, 1//2,  0//1])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1b"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'b"
             end if
             cycle
          end if
          if (rational_equal(tr,[0//1,  0//1, 1//2])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1c"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'c"
             end if
             cycle
          end if
          if (rational_equal(tr,[1//4,  0//1, 0//1])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1u"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'u"
             end if
             cycle
          end if
          if (rational_equal(tr,[0//1, 1//4,  0//1])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1v"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'v"
             end if
             cycle
          end if
          if (rational_equal(tr,[0//1,  0//1, 1//4])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1w"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'w"
             end if
             cycle
          end if
          if (rational_equal(tr,[1//4,  1//4, 1//4])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1d"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'd"
             end if
             cycle
          end if
          if (rational_equal(tr,[0//1,  1//4, 1//4])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1vw"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'vw"
             end if
             cycle
          end if
          if (rational_equal(tr,[1//4, 0//1,  1//4])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1uw"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'uw"
             end if
             cycle
          end if
          if (rational_equal(tr,[1//4,  1//4, 0//1])) then
             if(invt == 1) then
               n_lat=n_lat+1
               nc_lat(n_lat)="1uv"
             else
               n_alat=n_alat+1
               nc_alat(n_alat)="1'uv"
             end if
             cycle
          end if
        end if
      end do

      !> Constructor
      call Group_Constructor(gen,Grp)
      if (Err_CFML%Ierr /= 0) return


      call Identify_PointGroup(Grp)
      if (Err_CFML%Ierr /= 0) return

      call Identify_LaueClass(Grp)
      if (Err_CFML%Ierr /= 0) return

      call Identify_Crystal_System(Grp)
      if (Err_CFML%Ierr /= 0) return

      if (pout) then
         write(*,"(a,2i3)") " => N_lat & N_alat: ",n_lat,n_alat
         do n=1,n_lat
           write(*,"(i3,tr4,a)") n,nc_lat(n)
         end do
         do n=1,n_alat
           write(*,"(i3,tr4,a)") n,nc_alat(n)
         end do
         print*,'Raw Group'
         print*,'-----------'
         call Write_SpaceGroup_Info(Grp)
      end if

      !> Lattice Type
      Grp%SPG_lat=Get_Lattice_Type(ngen,gen)

      c_latt=" "
      select case (Grp%centred)
          case (0,1)
             c_latt=Grp%SPG_lat
          case (2)
             c_latt="-"//Grp%SPG_lat
      end select
      if (len_trim(c_latt) <=0) then
         Err_CFML%Ierr=1
         Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Impossible to determine the Lattice symbol!"
         return
      end if

      !> Anti_lattice
      c_alatt=" "
      do n=1,Grp%multip
         !> Conditions
         if (.not. Is_OP_Anti_Lattice(Grp%op(n))) cycle

         !> Traslation
         tr=Grp%op(n)%Mat(1:3,4)
         tr=rational_modulo_lat(tr)

         if (rational_equal(tr,[1//2, 0//1, 0//1])) then
            c_alatt="a"
            exit
         end if

         if (rational_equal(tr,[0//1, 1//2, 0//1])) then
            c_alatt="b"
            exit
         end if

         if (rational_equal(tr,[0//1, 0//1, 1//2])) then
            if (Grp%Laue == "-1") then
               c_alatt='S'
            else
               if (Grp%SPG_lat=="R") then
                  c_alatt="I"
               else
                  c_alatt="c"
               end if
            end if
            exit
         end if

         if (rational_equal(tr,[0//1, 1//2, 1//2]) .and. &
             (Grp%SPG_lat/="A" .and. Grp%SPG_lat/="F" )) then
            c_alatt="A"
            exit
         end if

         if (rational_equal(tr,[1//2, 0//1, 1//2]) .and. &
             (Grp%SPG_lat/="B" .and. Grp%SPG_lat/="F" )) then
            c_alatt="B"
            exit
         end if

         if (rational_equal(tr,[1//2, 1//2, 0//1]) .and. &
             (Grp%SPG_lat/="C" .and. Grp%SPG_lat/="F" )) then
            c_alatt="C"
            exit
         end if

         if (rational_equal(tr,[1//2, 1//2, 1//2])) then
            select case (Grp%SPG_lat)
               case ("P")
                  c_alatt="I"

               case ("F")
                  c_alatt="S"
            end select
            exit
         end if
      end do

      !> Searching operators
      car_prime=" "
      car_op=" "
      select case (trim(u_case(Grp%crystalsys)))
         case ("TRICLINIC")
            prime=Search_OnePrime_Operator(Grp)

            !> Prime
            car_op(1)=" "
            car_prime=" "
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
               case default
                  car_op(1)="1"
            end select

         case ("MONOCLINIC")
            str_hall=Search_Hall_Operators(Grp)
            call get_words(str_hall,car_op,iv)
            if (iv ==0) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Monoclinic spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            !> Extra conditions
            n1=index(car_op(1),'-')
            n2=index(car_op(1),"'")
            if (prime == -1 .and. n1 > 0 .and. n2==0) then
               car_op(1)=car_op(1)(n1+1:)
               car_op(1)=trim(car_op(1))//"'"
            end if

            car_op(2:)=" "

         case ("ORTHORHOMBIC")
            str_Hall=Search_Hall_Operators(Grp)
            call get_words(str_hall,car_op,iv)
            if (iv < 2) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Orthorhombic spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            do n=1,2
               n1=index(car_op(n),'-')
               n2=index(car_op(n),"'")
               if (prime == -1 .and. n1 > 0 .and. n2==0) then
                  car_op(n)=car_op(n)(n1+1:)
                  car_op(n)=trim(car_op(n))//"'"
               end if
            end do

            do n=3,iv
               n1=index(car_op(n),'-1')
               if (n1 == 0) car_op(n)=" "
            end do

         case ("TETRAGONAL")
            str_Hall=Search_Hall_Operators(Grp)
            call get_words(str_hall,car_op,iv)
            if (iv < 1) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Tetragonal spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            do n=1,2
               n1=index(car_op(n),'-')
               n2=index(car_op(n),"'")
               if (prime == -1 .and. n1 > 0 .and. n2==0) then
                  car_op(n)=car_op(n)(n1+1:)
                  car_op(n)=trim(car_op(n))//"'"
               end if
            end do

            do n=3,iv
               n1=index(car_op(n),'-1')
               if (n1 ==0) car_op(n)=" "
            end do

         case ("TRIGONAL")
            if (present(ishift)) then
               str_Hall=Search_Hall_Operators(Grp,ishift)
            else
               str_Hall=Search_Hall_Operators(Grp)
            end if
            call get_words(str_Hall,car_op,iv)
            if (iv < 1) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Trigonal spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            !> Rule: -2xxx + (-1') => 2xxx' + (-1')
            do n=1,2
               n1=index(car_op(n),'-')
               n2=index(car_op(n),"'")
               if (prime == -1 .and. n1 > 0 .and. n2==0) then
                  car_op(n)=car_op(n)(n1+1:)
                  car_op(n)=trim(car_op(n))//"'"
               end if
            end do

            !> Rule: 3 + (-1') => -3' (Only one operator)
            if (len_trim(car_op(2)) ==0) then
               if (car_op(1) =='3' .and. index(car_op(1),"'")==0 .and. prime==-1) then
                  car_op(1)="-3'"
                  prime=0
                  car_prime=" "
               end if
            end if

            car_op(3:)=" "

         case ("HEXAGONAL")
            if (present(ishift)) then
               str_Hall=Search_Hall_Operators(Grp,ishift)
            else
               str_Hall=Search_Hall_Operators(Grp)
            end if
            call get_words(Str_Hall,car_op,iv)
            if (iv < 1) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Hexagonal spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            !> Rule: -2xxx + (-1') => 2xxx' + (-1')
            do n=1,2
               n1=index(car_op(n),'-')
               n2=index(car_op(n),"'")
               if (prime == -1 .and. n1 > 0 .and. n2==0) then
                  car_op(n)=car_op(n)(n1+1:)
                  car_op(n)=trim(car_op(n))//"'"
               end if
            end do

            car_op(3:)=" "

         case ("CUBIC")
            str_Hall=Search_Hall_Operators(Grp)
            call get_words(str_hall,car_op,iv)
            if (iv < 3) then
               Err_CFML%Ierr=1
               Err_CFML%Msg="Get_Hall_from_SpG@GSPACEGROUPS: Not operators for Cubic spacegroup!"
               return
            end if

            !> Prime operator
            prime=Search_OnePrime_Operator(Grp)
            select case (prime)
               case (-1)
                  car_prime="-1'"
               case (1)
                  car_prime="1'"
            end select

            do n=1,3
               n1=index(car_op(n),'-')
               n2=index(car_op(n),"'")
               if (prime == -1 .and. n1 > 0 .and. n2==0) then
                  car_op(n)=car_op(n)(n1+1:)
                  car_op(n)=trim(car_op(n))//"'"
               end if
            end do

            do n=4,iv
               n1=index(car_op(n),'-1')
               if (n1 == 0) car_op(n)=" "
            end do
      end select

      !> ----
      !> Constructing the Hall symbol
      !> ----

      !> Lattice + anti-lattice
      Hall=trim(c_latt)

      !> Redundat centre of symmetry?
      if(Hall(1:1) == "-") then !Centre of symmetry at the origin
        do n=1,3
           n1=index(car_op(n),"-1")  !Redundant centre of symmetry
           if(n1 /= 0) car_op(n)=" "
        end do
      end if

      !> Raw Hall symbol
      Hall=trim(Hall)//' '//trim(car_op(1))//' '//trim(car_op(2))//' '//trim(car_op(3))

      !> Completing Hall symbol with non-conventional lattice centrings
      if(n_lat /= 0) then
         do n=1,n_lat
           Hall=trim(Hall)//' '//trim(nc_lat(n))
         end do
      end if

      Hall=trim(Hall)//' '//trim(car_prime)

      !> Completing Hall symbol with non-conventional anti-translations
      if (c_alatt /= " ") then
         Hall=trim(Hall)//" 1'"//c_alatt
      else if (n_alat /= 0) then
         do n=1,n_alat
           Hall=trim(Hall)//' '//trim(nc_alat(n))
         end do
      end if

      !> Completing Hall symbol with provided shift of origin
      if (present(ishift)) then
         if (any(ishift /= 0)) then
            str_hall=" "
            write(unit=str_hall,fmt='(3i3)') ishift
            str_hall=adjustl(str_hall)
            Hall=trim(Hall)//' ('//trim(str_hall)//')'
         end if
      end if

   End Function Get_Hall_from_Generators

End SubModule SPG_Generators_from_Hall