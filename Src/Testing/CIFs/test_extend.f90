 Module testing_extend
    implicit none
    Type, public :: Symm_Oper_Type
       integer                              :: Time_Inv =1         ! Time inversion for Magnetic groups
       integer                              :: Dt       =1         ! determinant of the submatrix (1:3,1:3), it should be 1 or -1
       integer, dimension(:,:), allocatable :: Mat                 ! Matrix operator
    End Type Symm_Oper_Type

    Type, public :: Group_Type
       integer                                         :: Multip =0       ! Multiplicity of the Group
       integer                                         :: D=0             ! Dimension of operator matrices (2:2D, 3:D3,...)
       type(Symm_Oper_Type), dimension(:), allocatable :: Op              ! Symmetry operator
       character(len=80),    dimension(:), allocatable :: Symb_Op         ! Symbol operator
    End Type Group_Type

    Type, public, extends(Group_Type)   :: SPG_Type
       integer                          :: num_lat  = 0      ! Number of lattice points in cell
       real,dimension(:,:), allocatable :: Lat_tr            ! Lattice traslations
    End Type SPG_Type

    Type, public, extends(Spg_Type):: SuperSpaceGroup_Type
       integer                              :: nk=0    ! (nk=1,2,3, ...) number of k-vectors
       integer                              :: nq=0    ! number of effective set of Q_coeff >= nk
       real,    allocatable,dimension(:,:)  :: kv      ! k-vectors (3,nk)
       real,    allocatable,dimension(:,:,:):: Om      ! Operator matrices (3+d+1,3+d+1,Multip) in real form to accelerate calculations
       real,    allocatable,dimension(:)    :: sintlim ! sintheta/lambda limits (nk)
       integer, allocatable,dimension(:)    :: nharm   ! number of harmonics along each k-vector
    End Type SuperSpaceGroup_Type

   contains

   Subroutine Allocate_group(d,m,SpG)
     integer, intent(in)            :: d,m
     class(Group_Type), intent(out) :: SpG
     integer :: i
     SpG%multip=m
     allocate(SpG%Op(m), SpG%Symb_Op(m))
     do i=1,m
       allocate(SpG%Op(i)%Mat(d,d))
       SpG%Op(i)%Mat(d,d)=0.0
     end do
   End Subroutine Allocate_group

   Subroutine setting_Spg(nmod,m,dd,nl,SpaceG)
      integer,         intent(in)     :: nmod,m,dd,nl
      class(SpG_type), intent(in out) :: SpaceG

          SpaceG%num_lat=nl
          allocate(SpaceG%Lat_tr(dd,nl))

          Select Type (Grp => SpaceG)

            type is (SuperSpaceGroup_Type)
                write(*,"(a,i3)") " => Allocating SuperSpace for : ",nmod
                Grp%nk=nmod                              !(d=1,2,3, ...) number of q-vectors
                if(Allocated(Grp%kv)) deallocate(Grp%kv)
                if(Allocated(Grp%sintlim)) deallocate(Grp%sintlim)
                if(Allocated(Grp%Om)) deallocate(Grp%Om)
                Allocate(Grp%kv(3,nmod),Grp%sintlim(nmod))
                write(*,"(a,i6,a)") " => Allocating Om for : ",Dd*Dd*Grp%Multip, "  elements"
                Allocate(Grp%Om(Dd,Dd,m))
                Grp%kv=0.0; Grp%sintlim=0.0; Grp%Om=0.0
                !Grp%nq=0     ! number of effective sets of Q_coeff >= nk
                !Grp%nharm=0  ! number of harmonics along each k-vector
          End Select

   End Subroutine setting_Spg

 End Module testing_extend


  Program test
    use testing_extend
    implicit none
    type(SuperSpaceGroup_Type) :: SpaceG
    integer :: m,d,nmod,nl
    m=192
    nmod=3
    d=3+nmod+1
    call Allocate_Group(d,m,SpaceG)
    call setting_Spg(nmod,m,d,nl,SpaceG)

  End Program test