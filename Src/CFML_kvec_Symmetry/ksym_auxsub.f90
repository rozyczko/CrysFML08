SubModule (CFML_kvec_Symmetry) ksym_auxsub
   implicit none
   Contains
    !---------------------!
    !---- Subroutines ----!
    !---------------------!
    !!----
    !!---- Module Subroutine Latsym(Symb,Numl,Latc)
    !!----    character (len=*),                       intent(in)  :: SYMB  !  In -> Space Group H-M/Hall symbol
    !!----    integer, optional,                       intent(in)  :: numL  !  Number of centring vectors
    !!----    real(kind=cp),optional, dimension(:,:),  intent(in)  :: latc  !  Centering vectors
    !!----
    !!--<<        Inlat  Lattice type & associated translations
    !!----          1     P: { 000 }
    !!----          2     A: { 000;  0  1/2 1/2 }+
    !!----          3     B: { 000; 1/2  0  1/2 }+
    !!----          4     C: { 000; 1/2 1/2  0  }+
    !!----          5     I: { 000; 1/2 1/2 1/2 }+
    !!----          6     R: { 000; 2/3 1/3 1/3; 1/3 2/3 2/3   } +
    !!----          7     F: { 000;  0  1/2 1/2; 1/2  0  1/2; 1/2 1/2  0 } +
    !!----          8     Z: { 000;  user-given centring vectors } +
    !!-->>
    !!----    Provides the Lattice type of the S.G. SYMB. Also gives the index (Inlat)
    !!----    of the lattice, the multiplicity (Nlat) and the fractionnal lattice translations
    !!----    ((Ltr(in,j)j=1,3),in=1,Nlat).
    !!----
    !!---- Update: February - 2005, January 2014 (JRC)
    !!
    Module Subroutine LatSym(SYMB,numL,latc)
       !---- Argument ----!
       character(len=*),                        intent(in)  :: SYMB
       integer, optional,                       intent(in)  :: numL
       real(kind=cp),optional, dimension(:,:),  intent(in)  :: latc  !general vector (JRC, Jan2014)

       !---- Local variables ----!
       character(len=1)                        :: LAT
       character(len=:), allocatable           :: SYMBB
       integer                                 :: i

       call Clear_Error()
       symbb=adjustl(symb)
       do i=1,len_trim(symbb)
          if (symbb(i:i) == "-" .or. symbb(i:i) == " ") cycle
          lat=symbb(i:i)
          exit
       end do

       nlat=1
       ltr(:,1)=0.0
       select case (lat)
          case ("P","p")
             lat="P"
             nlat=1
             inlat=1

          case ("A","a")
             lat="A"
             nlat=2
             inlat=2
             ltr(1,2)=0.0
             ltr(2,2)=0.5
             ltr(3,2)=0.5

          case ("B","b")
             lat="B"
             nlat=2
             inlat=3
             ltr(1,2)=0.5
             ltr(2,2)=0.0
             ltr(3,2)=0.5

          case ("C","c")
             lat="C"
             nlat=2
             inlat=4
             ltr(1,2)=0.5
             ltr(2,2)=0.5
             ltr(3,2)=0.0

          case ("I","i")
             lat="I"
             nlat=2
             inlat=5
             ltr(:,2)=0.5

          case ("R","r")
             lat="R"
             nlat=3
             inlat=6
             ltr(1,2)=2.0/3.0
             ltr(2,2)=1.0/3.0
             ltr(3,2)=1.0/3.0
             ltr(1,3)=1.0/3.0
             ltr(2,3)=2.0/3.0
             ltr(3,3)=2.0/3.0

          case ("F","f")
             lat="F"
             nlat=4
             inlat=7
             ltr(1,2)=0.5
             ltr(2,2)=0.5
             ltr(3,2)=0.0
             ltr(1,3)=0.5
             ltr(2,3)=0.0
             ltr(3,3)=0.5
             ltr(1,4)=0.0
             ltr(2,4)=0.5
             ltr(3,4)=0.5

          case ("Z","z","X","x")
             if(present(numL) .and. present(latc)) then
              lat="Z"
              nlat=numL+1
              !nlat=min(nlat,12) !restriction removed in January 2014
              inlat=8
              do i=2,nlat
                ltr(:,i)=latc(:,i-1)
              end do
             else
               Err_CFML%flag=.true.
               Err_CFML%Msg="Unconventional Lattice Symbol Z needs centring vectors"
             end if
          case default
             Err_CFML%flag=.true.
             Err_CFML%Msg="Wrong Lattice Symbol "//LAT
       end select
    End Subroutine Latsym

    !!----
    !!---- Module Subroutine Read_Xsym(Info,Istart,Sim,Tt,ctrl)
    !!----    character (len=*),                     intent( in)    :: Info   !  In -> String with the symmetry symbol
    !!----                                                                             in the form: SYMM  x,-y+1/2,z
    !!----    integer,                               intent(in)     :: istart !  In -> Starting index of info to read in.
    !!----    integer, dimension(3,3),               intent(out)    :: sim    ! Out -> Rotational part of S.O.
    !!----    real(kind=cp), optional, dimension(3), intent(out)    :: tt     ! Out -> Traslational part of S.O.
    !!----
    !!----
    !!----    Read symmetry or transformation operators in the form X,Y,Z, etc...
    !!----    Provides the rotational matrix and translation associated a to SYMM symbol
    !!----    in the Jones Faithful representation.
    !!----
    !!---- Update: June - 2011 (JRC, adding ctrl for controlling if a real symmetry operator is needed)
    !!
    Module Subroutine Read_Xsym(Info,Istart,Sim,Tt,ctrl)
       !---- Arguments ----!
       character (len=*),                     intent(in)     :: Info
       integer,                               intent(in)     :: istart
       integer, dimension(3,3),               intent(out)    :: sim
       real(kind=cp), optional, dimension(3), intent(out)    :: tt
       logical,       optional,               intent(in)     :: ctrl

       !---- Local variables ----!
       character (len=*), dimension(10), parameter :: ANUM=["1","2","3","4","5","6","7","8","9","0"]
       integer, dimension(10), parameter           :: NUM =[1,2,3,4,5,6,7,8,9,0]
       integer :: i,imax,nop,s,np,isl,ifound,ip,k,mod_istart,ST=0,I_P,ist
       real(kind=cp) :: t,a
       logical       :: control

       control=.true.
       if(present(ctrl)) control=ctrl
       call Clear_Error()
       imax=len_trim(info)
       if (present(tt)) tt=0.0
       sim = 0
       ist=istart
       do nop=1,3
          s=1
          t=0.0
          ip=0
          i_p=1
          np=0
          isl=0
          ifound=0
          mod_istart=0
          loop_string: do i=ist,imax
             if (info(i:i) == " ") cycle
             if (info(i:i) == "," .or. info(i:i) == "*") then
                mod_istart=1
                exit
             end if
             ifound=1
             if (info(i:i) == "X" .or. info(i:i) == "x") then
                sim(nop,1)=s*i_p
                i_p=1
                s=1
             else if (info(i:i) == "Y" .or. info(i:i) == "y") then
                sim(nop,2)=s*i_p
                i_p=1
                s=1
             else if(info(i:i) == "Z" .or. info(i:i) == "z") then
                sim(nop,3)=s*i_p
                i_p=1
                s=1
             else if(info(i:i) == "+") then
                s=1
             else if(info(i:i) == "-") then
                s=-1
             else if(info(i:i) == "/") then
                isl=1
             else if(info(i:i) == ".") then
                ip=1
             else
                st=s
                do k=1,10
                   if (info(i:i) == anum(k))  then
                      if (is_xyz(info(i+1:i+1))) then
                         i_p=num(k)
                         cycle loop_string
                      else
                         a=num(k)
                         if (isl == 1) then
                            t=t/a
                         else if(ip == 1) then
                            np=np+1
                            t=t+a/10**np
                         else
                            t=10.0*t+a
                         end if
                         cycle loop_string
                      end if
                   end if
                end do
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Invalid character... "//INFO(I:I)//" in operator string"
                return
             end if
          end do  loop_string   !end loop through the string (index:i= ist,imax)

          if (mod_istart == 1) then
             ist=i+1
          end if

          t=t*st
          if (present(tt)) tt(nop)=t

          if (ifound == 0) then
             Err_CFML%flag=.true.
             Err_CFML%Msg=" Blank operator field"
             return
          end if

       end do    !End external loop over the three expected items (index:NOP)

       if (determ3D(sim) == 0 .and. control) then      !Verify it is a suitable s.o.
          Err_CFML%flag=.true.
          Err_CFML%Msg=" The above operator is wrong: "//info
          return
       end if

       if (ifound == 1) return

       Err_CFML%flag=.true.
       Err_CFML%Msg=" The above operator is wrong: "//info

    End Subroutine Read_Xsym

    !!----
    !!---- Module Subroutine Read_Msymm(Info,Sim,P_Mag,ctrl)
    !!----    character (len=*),       intent( in) :: Info   !  In -> Input string with S.Op.
    !!----                                                            in the form: MSYM  u,w,w,p_mag
    !!----    integer, dimension(3,3), intent(out) :: sim    ! Out -> Rotation matrix
    !!----    real(kind=cp),           intent(out) :: p_mag  ! Out -> magnetic phase
    !!----    logical, optional,       intent(in)  :: ctrl   ! in  -> If provided and .true. an error condition
    !!----                                                            is raised if the det(Sim)=0
    !!----    Read magnetic symmetry operators in the form U,V,W, etc...
    !!----    Provides the magnetic rotational matrix and phase associated to a MSYM symbol
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Read_Msymm(Info,Sim,P_Mag,ctrl)
       !---- Arguments ----!
       character (len=*),       intent( in) :: Info
       integer, dimension(3,3), intent(out) :: sim
       real(kind=cp),           intent(out) :: p_mag
       logical, optional,       intent(in)  :: ctrl

       !---- Local variables ----!
       integer ::  i,imax,nop,s,ifound,j,ioerr,istart,mod_istart
       character(len=:), allocatable :: aux
       logical :: control

       control=.false.
       if(present(ctrl)) control=ctrl
       call Clear_Error()
       do j=len(Info),1,-1
          if (info(j:j) == ",") exit
       end do
       p_mag=0.0
       imax=j-1
       read(unit=info(j+1:),fmt=*,iostat=ioerr) p_mag
       if (ioerr /= 0) then
          p_mag=0.0
       end if
       sim = 0
       aux=adjustl(l_case(Info))
       if(aux(1:4) == "msym" .or. aux(1:4) == "dsym") then
         istart=6
       else
         istart=1
       end if

       do nop=1,3
          s=1
          mod_istart=0
          ifound=0
          do i=istart,imax
             if (aux(i:i) == " ") cycle
             if (aux(i:i) == "," .or. info(i:i) == "*") then
                mod_istart=1
                exit
             end if
             ifound=1
             if (aux(i:i) == "u" ) then
                sim(nop,1)=s
                s=1
             else if (aux(i:i) == "v") then
                sim(nop,2)=s
                s=1
             else if(aux(i:i) == "w") then
                sim(nop,3)=s
                s=1
             else if(aux(i:i) == "+") then
                s=1
             else if(aux(i:i) == "-") then
                s=-1
             else
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Invalid character... "//aux(I:I)//" in Sym. Op."
                return
             end if
          end do    !End loop through the string

          if (mod_istart == 1) then
            istart=i+1
          end if

          if (ifound == 0) then
             Err_CFML%flag=.true.
             Err_CFML%Msg=" Blank operator field "//info
             return
          end if
       end do    !End external loop over the three expected items

       if (determ3D(sim) == 0 .and. control) then      !Verify it is a suitable s.o.
          Err_CFML%flag=.true.
          Err_CFML%Msg=" The above operator is wrong "//info
          return
       end if

       if (ifound == 1) return

       Err_CFML%flag=.true.
       Err_CFML%Msg=" The above operator is wrong "//info

    End Subroutine Read_Msymm

    !!----
    !!---- Module Subroutine Get_SymSymb(Sim,Tt,Strsym)
    !!----    real(kind=cp)/integer, dimension(3,3), intent( in)    :: sim      !  In -> Rotational part of the S.O.
    !!----    real(kind=cp), dimension( 3),          intent( in)    :: tt       !  In -> Translational part of the S.O.
    !!----    character (len=*),                     intent(out)    :: Strsym   ! Out -> String in th form X,Y,-Z, ...
    !!----
    !!----    Obtain the Jones Faithful representation of a symmetry operator
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Module Subroutine Get_SymsymbI(Sim,Tt,Strsym)
    !!--++    integer, dimension(3,3),      intent( in)    :: sim      !  In -> Rotational part of the S.O.
    !!--++    real(kind=cp), dimension( 3), intent( in)    :: tt       !  In -> Translational part of the S.O.
    !!--++    character (len=*),            intent(out)    :: Strsym   ! Out -> String in th form X,Y,-Z, ...
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Obtain the Jones Faithful representation of a symmetry operator
    !!--++
    !!--++ Update: February - 2005, January-2014 (changed for a more robust algorithm,JRC)
    !!
    Module Subroutine Get_SymSymbI(X,T,Symb)
       !---- Arguments ----!
       integer,       dimension(3,3), intent( in) :: x
       real(kind=cp), dimension(3),   intent( in) :: t
       character (len=*),          intent(out) :: symb

       !---- Local Variables ----!
       character(len=*),dimension(3),parameter :: xyz=["x","y","z"]
       character(len= 30)              :: car
       character(len= 30),dimension(3) :: sym
       integer           :: i,j

       !---- Main ----!
       symb=" "
       do i=1,3
          sym(i)=" "
          do j=1,3
             if(x(i,j) == 1) then
                sym(i) = trim(sym(i))//"+"//xyz(j)
             else if(x(i,j) == -1) then
                sym(i) =  trim(sym(i))//"-"//xyz(j)
             else if(x(i,j) /= 0) then
               car=" "
               write(unit=car,fmt="(i3,a)") x(i,j),xyz(j)
               if(x(i,j) > 0) car="+"//trim(car)
               sym(i)=trim(sym(i))//pack_string(car)
             end if
          end do
          if (abs(t(i)) > eps_symm ) then
             car= string_fraction_2Dig(t(i))
             sym(i)=trim(sym(i))//trim(car)
          end if
          sym(i)=adjustl(sym(i))
          if(sym(i)(1:1) == "+")  then
            sym(i)(1:1) = " "
            sym(i)=adjustl(sym(i))
          end if
          sym(i)=pack_string(sym(i))
       end do
       symb=trim(sym(1))//","//trim(sym(2))//","//trim(sym(3))
    End Subroutine Get_SymSymbI

    !!--++
    !!--++  Module Subroutine Get_SymSymbR(X,T,Symb)
    !!--++     real(kind=cp),    dimension(3,3),    intent( in) :: x
    !!--++     real(kind=cp),    dimension(3),      intent( in) :: t
    !!--++     character (len=*),                   intent(out) :: symb
    !!--++
    !!--++     (OVERLOADED)
    !!--++     Returning a string for symmetry operators or for points, axes or plane give as
    !!--++     written in fractional form
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine Get_SymSymbR(X,T,Symb)
       !---- Arguments ----!
       real(kind=cp),    dimension(3,3), intent( in) :: x
       real(kind=cp),    dimension(3),   intent( in) :: t
       character (len=*),                intent(out) :: symb

       !---- Local Variables ----!
       character(len= 30):: car
       integer           :: i,j,k, np,npp,npos
       real(kind=cp)     :: suma

       !---- Main ----!
       symb=" "
       npos=1
       do i=1,3
          npp=0
          do j=1,3
             if (abs(x(i,j)) > 0.0 ) then
                car = string_fraction_2Dig(x(i,j))
                car=adjustl(car)
                if (abs(abs(x(i,j))-1.0) <= eps_symm) then
                     if (npp == 0) then
                        select case (car(1:2))
                           case ("-1")
                              car(2:)=car(3:)//"  "
                           case ("+1")
                              car=car(3:)//"  "
                        end select
                     else
                        car(2:)=car(3:)//"  "
                     end if
                else
                   if (npp == 0) then
                      if (car(1:1) =="+") then
                         car=car(2:)//"  "
                      end if
                   end if
                end if

                np=len_trim(car)
                select case (j)
                   case (1)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "x"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"x"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"x"
                      end if
                   case (2)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "y"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"y"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"y"
                      end if
                   case (3)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "z"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"z"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"z"
                      end if
                end select
                npos=len_trim(symb)+1
                npp=npos
             end if
          end do

          if (abs(t(i)) <= eps_symm .and. npp /= 0) then
             if (i < 3) then
                symb(npos:)=", "
                npos=len_trim(symb)+2
             end if
             cycle
          end if

          car= string_fraction_2Dig(t(i))
          car=adjustl(car)
          suma=0.0
          do j=1,3
             suma=suma+abs(x(i,j))
          end do
          np=len_trim(car)
          if (suma <= 3.0*eps_symm) then
             if (car(1:1) == "+") car=car(2:np)//" "
          end if

          if (i < 3) then
             symb(npos:)=car(1:np)//", "
             npos=len_trim(symb)+2
          else
             symb(npos:)=car(1:np)
          end if
       end do

       symb=pack_string(symb)

    End Subroutine Get_SymSymbR

End SubModule ksym_auxsub
