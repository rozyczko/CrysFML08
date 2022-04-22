!!
Submodule (CFML_KeyCodes) KeyCod_Restraints
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE ALLOCATE_RESTRAINTS_VEC
   !!----
   !!----    Allocate vectors Ang_Rest, Dist_Rest, Tor_Rest.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Allocate_Restraints_Vec(Ffile, N_ini, N_end, NDfix, NAfix, NTFix)
      !---- Arguments ----!
      Type(file_type),   intent( in) :: Ffile
      integer, optional, intent( in) :: N_ini
      integer, optional, intent( in) :: N_end
      integer, optional, intent(out) :: NDfix
      integer, optional, intent(out) :: NAfix
      integer, optional, intent(out) :: NTfix

      !---- Local variables ----!
      integer :: i,j,nc,nr,iv1,iv2,npos
      integer :: l_ini, l_end

      if (allocated(Ang_Rest)) deallocate(Ang_Rest)
      if (allocated(Dis_Rest)) deallocate(Dis_Rest)
      if (allocated(Tor_Rest)) deallocate(Tor_Rest)

      NP_Rest_Dis=0
      NP_Rest_Ang=0
      NP_Rest_Tor=0

      if (present(NDfix)) NDfix=0
      if (present(NAfix)) NAfix=0
      if (present(NTfix)) NTfix=0

      call clear_error()

      l_ini=1
      l_end=ffile%nlines
      if (present(n_ini)) l_ini=n_ini
      if (present(n_end)) l_end=n_end

      !> Dimension for DFIX
      nr=0
      do i=l_ini,l_end
         line=adjustl(ffile%line(i)%str)
         if (u_case(line(1:4)) /= "DFIX") cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         call cut_string(line)
         call get_words(line,dire,nc)

         !> value
         call get_num(dire(1),vet,ivet,iv1)
         if (iv1 /= 1) then
            call set_error(1," Bad arguments on DFIX directive: "//trim(line))
            return
         end if

         !> Sigma (optional)
         call get_num(dire(2),vet,ivet,iv2)
         if (iv2 == 1) then
            nc=(nc-2)
         else
            nc=(nc-1)
         end if
         if (mod(nc,2) /= 0) then
            call set_error(1," Bad arguments on DFIX directive: "//trim(line))
            return
         end if
         nr=nr+nc/2
      end do

      if (nr > 0) then
         allocate(Dis_Rest(nr))
         dis_rest%IPh =0
         dis_rest%obs  =0.0_cp
         dis_rest%cal  =0.0_cp
         dis_rest%Sig  =0.0_cp
         dis_rest%P(1) =0
         dis_rest%P(2) =0
         dis_rest%Code =" "
      end if
      if (present(NDfix)) NDfix=nr

      !> Dimension for AFIX
      nr=0
      do i=l_ini,l_end
         line=adjustl(ffile%line(i)%str)
         if (u_case(line(1:4)) /= "AFIX") cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         call cut_string(line)
         call get_words(line,dire,nc)

         !> value
         call get_num(dire(1),vet,ivet,iv1)
         if (iv1 /= 1) then
            call set_error(1," Bad arguments on AFIX directive: "//trim(line))
            return
         end if

         !> Sigma (optional)
         call get_num(dire(2),vet,ivet,iv2)
         if (iv2 == 1) then
            nc=(nc-2)
         else
            nc=(nc-1)
         end if
         if (mod(nc,3) /= 0) then
            call set_error(1," Bad arguments on AFIX directive: "//trim(line))
            return
         end if
         nr=nr+nc/3
      end do

      if (nr >0) then
         allocate(Ang_Rest(nr))
         ang_rest%IPh  =0
         ang_rest%Obs  =0.0_cp
         ang_rest%Cal  =0.0_cp
         ang_rest%Sig  =0.0_cp
         ang_rest%P(1) = 0
         ang_rest%P(2) = 0
         ang_rest%P(3) = 0
         ang_rest%Code(1) =" "
         ang_rest%Code(2) =" "
      end if
      if (present(NAfix)) NAfix=nr

      !> Dimension for TFIX
      nr=0
      do i=l_ini,l_end
         line=adjustl(ffile%line(i)%str)
         if (u_case(line(1:4)) /= "TFIX") cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         call cut_string(line)
         call get_words(line,dire,nc)

         !> value
         call get_num(dire(1),vet,ivet,iv1)
         if (iv1 /= 1) then
            call set_error(1," Bad arguments on TFIX directive: "//trim(line))
            return
         end if

         !> Sigma (optional)
         call get_num(dire(2),vet,ivet,iv2)
         if (iv2 == 1) then
            nc=(nc-2)
         else
            nc=(nc-1)
         end if
         if (mod(nc,4) /= 0) then
            call set_error(1," Bad arguments on TFIX directive: "//trim(line))
            return
         end if
         nr=nr+nc/4
      end do

         if (nr > 0) then
            allocate(Tor_Rest(nr))
            tor_rest%IPh  =0
            tor_rest%Obs  =0.0_cp
            tor_rest%Cal  =0.0_cp
            tor_rest%Sig  =0.0_cp
            tor_rest%p(1) =0
            tor_rest%p(2) =0
            tor_rest%p(3) =0
            tor_rest%p(4) =0
            tor_rest%Code(1) =" "
            tor_rest%Code(2) =" "
            tor_rest%Code(3) =" "
         end if
         if (present(NTfix)) NTfix=nr

   End Subroutine Allocate_Restraints_Vec

   !!----
   !!---- SUBROUTINE GET_DFIX_LINE
   !!----
   !!----    Get Distance Restraints relations for Free atoms type
   !!----    Line: Dist [sig] At1a At1b At2a At2b......
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Get_DFIX_Line(String, AtList, IPhase)
      !---- Arguments ----!
      character(len=*),  intent(in) :: String
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: IPhase

      !---- Local variables ----!
      integer, parameter              :: NP=20

      integer,dimension(2,NP)         :: p
      integer                         :: i,j,k,nc,iv,nr,npos
      integer                         :: iph
      real(kind=cp)                   :: dis,sig
      character(len=8), dimension(NP) :: symtrans=" "
      character(len=8)                :: symcode=" "

      !> Init
      call clear_error()

      if (len_trim(string) == 0 .or. .not.allocated(dis_rest) ) return

      !> Words
      call get_words(string,dire,nc)

      !> Value
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1," Error in DFIX line: "//trim(string))
         return
      end if
      dis=vet(1)

      !> Sigma
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.02
         k=2
      else
         sig=max(vet(1),0.0001_cp)
         k=3
      end if

      nr=0
      do i=k,nc,2
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in DFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         ivet=0
         associate(A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i))) == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (all(ivet(1:2) > 0) ) exit
            end do
         end associate

         if (any(ivet(1:2) == 0)) then
            call set_error(1,"Some of the atom names in"//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:2)
         symtrans(nr)=trim(symcode)
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations
      iph=1
      if (present(IPhase)) iph=IPhase

      k=NP_Rest_Dis
      do i=1,nr
         k=k+1
         dis_rest(k)%IPh=iph
         dis_rest(k)%Obs=dis
         dis_rest(k)%Cal=0.0_cp
         dis_rest(k)%sig=sig
         dis_rest(k)%p = p(:,i)
         dis_rest(k)%Code=symtrans(i)
      end do
      NP_Rest_Dis=NP_Rest_Dis+nr

   End Subroutine Get_DFIX_Line

   !!----
   !!---- SUBROUTINE GET_AFIX_LINE
   !!----
   !!----     Get Distance Restraints relations for Free atoms type
   !!----     Line: Angle [sig] At1a At1b At1c At2a At2b At2c....
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Get_AFIX_Line(String, AtList, IPhase)
      !---- Arguments ----!
      character(len=*),  intent(in) :: String
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: IPhase

      !---- Local variables ----!
      integer, parameter                :: NP=30

      integer, dimension(3,NP)          :: p=0
      integer                           :: i,j,k,iv,nc,npos,iph,nr
      character(len=8), dimension(2,NP) :: symtrans=" "
      character(len=8), dimension(2)    :: symcode=" "
      real(kind=cp)                     :: ang,sig

      !> Init
      call clear_error()

      if (len_trim(string) == 0 .or. .not. allocated(ang_rest)) return

      !> words
      call get_words(string, dire, nc)

      !> Angle
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1,"Error in AFIX line: "//trim(string))
         return
      end if
      ang=vet(1)

      !> Sigma
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.2
         k=2
      else
         sig=max(vet(1),0.001_cp)
         k=3
      end if

      nr=0
      do i=k,nc,3
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in AFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode(1)=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         npos=index(dire(i+2),"_")
         if (npos /=0) then
            symcode(2)=dire(i+2)(npos+1:)
            dire(i+2)=dire(i+2)(:npos-1)
         end if

         ivet=0
         associate(A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i)))   == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (trim(u_case(dire(i+2))) == trim(u_case(A(j)%Lab))) ivet(3)=j
               if (all(ivet(1:3) > 0) ) exit
            end do
         end associate

         if (any(ivet(1:3) == 0)) then
            call set_error(1,"Some of the atom names in "//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:3)
         symtrans(:,nr)=symcode
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations
      iph=1
      if (present(IPhase)) iph=IPhase
      k=NP_Rest_Ang
      do i=1,nr
         k=k+1
         ang_rest(k)%Iph=iph
         ang_rest(k)%Obs=ang
         ang_rest(k)%Cal=0.0_cp
         ang_rest(k)%Sig=sig
         ang_rest(k)%p = p(:,i)
         ang_rest(k)%Code=symtrans(:,i)
      end do
      NP_Rest_Ang=NP_Rest_Ang+nr

   End Subroutine Get_AFIX_Line

   !!----
   !!---- SUBROUTINE GET_TFIX_LINE
   !!----
   !!----    Get Torsion Restraints relations for Free atoms type
   !!----    Line: Torsion_Angle [sig] At1a At1b At1c At1d ...
   !!----
   !!---- Update: April - 2020
   !!
   Module Subroutine Get_TFIX_Line(String, AtList, IPhase)
      !---- Arguments ----!
      character(len=*),  intent(in) :: String
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: IPhase

      !---- Local variables ----!
      integer, parameter                :: NP=30

      integer, dimension(4,NP)          :: p=0
      integer                           :: i,j,k,iv,nc,npos,iph,nr
      character(len=8), dimension(3,NP) :: symtrans=" "
      character(len=8), dimension(3)    :: symcode=" "
      real(kind=cp)                     :: tor,sig

      !> Init
      call clear_error()

      if (len_trim(string) == 0 .or. .not. allocated(tor_rest)) return

      !> Words
      call get_words(string,dire,nc)

      !> Angle
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1,"Error in TFIX line: "//trim(string))
         return
      end if
      tor=vet(1)

      !> Sigma
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.2
         k=2
      else
         sig=max(vet(1),0.02_cp)
         k=3
      end if

      nr=0
      do i=k,nc,4
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in TFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode(1)=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         npos=index(dire(i+2),"_")
         if (npos /=0) then
            symcode(2)=dire(i+2)(npos+1:)
            dire(i+2)=dire(i+2)(:npos-1)
         end if

         npos=index(dire(i+3),"_")
         if (npos /=0) then
            symcode(3)=dire(i+3)(npos+1:)
            dire(i+3)=dire(i+3)(:npos-1)
         end if

         ivet=0
         associate (A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i)))   == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (trim(u_case(dire(i+2))) == trim(u_case(A(j)%Lab))) ivet(3)=j
               if (trim(u_case(dire(i+3))) == trim(u_case(A(j)%Lab))) ivet(4)=j
               if (all(ivet(1:4) > 0) ) exit
            end do
         end associate
         if (any(ivet(1:4) == 0)) then
            call set_error(1,"Some of the atom names in "//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:4)
         symtrans(:,nr)=symcode
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations
      iph=1
      if (present(IPhase)) iph=IPhase
      k=NP_Rest_Tor
      do i=1,nr
         k=k+1
         tor_rest(k)%IPh=iph
         tor_rest(k)%Obs=tor
         tor_rest(k)%Cal=0.0_cp
         tor_rest(k)%Sig=sig
         tor_rest(k)%p = p(:,i)
         tor_rest(k)%Code=symtrans(:,i)
      end do
      NP_Rest_Tor=NP_Rest_Tor+nr

   End Subroutine Get_TFIX_Line

End SubModule KeyCod_Restraints