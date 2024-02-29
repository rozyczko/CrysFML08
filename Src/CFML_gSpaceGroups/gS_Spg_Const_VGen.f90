!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gs_Spg_Const_VGen
   implicit none
   Contains

   !!----
   !!---- SPACEG_CONSTRUCTOR_GENV
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine SpaceG_Constructor_GenV(GenV, Spg, StrCode, set_inv)
      !---- Arguments ----!
      character(len=*),dimension(:),intent(in)     :: GenV
      class(Spg_Type),              intent(in out) :: Spg
      character(len=*),optional,    intent(in)     :: StrCode
      logical,         optional,    intent(in)     :: set_inv

      !--- Local variables ---!
      character(len=40),    dimension(:),  allocatable :: gen
      type(Symm_Oper_Type), dimension(:),  allocatable :: Op
      type(rational),       dimension(:),  allocatable :: centre_coord,anticentre_coord
      type(rational),       dimension(:,:),allocatable :: Lat_tr, aLat_tr
      type(rational),       dimension(:,:),allocatable :: Mat

      integer :: d,i,ngen,invt,multip,centred,anticentred,Numops,num_lat,num_alat,mag_type

      !> Init
      call Clear_Error()

      !> Initializes Grp
      call Init_SpaceGroup(Spg)
      d=Get_Dimension_SymmOp(genV(1))

      !> This routine delete the generator: x,y,z
      call Check_Gener(GenV,gen)
      if (Err_CFML%Ierr /= 0) return

      !> check
      ngen=0
      if (allocated(gen)) then
         d=Get_Dimension_SymmOp(gen(1))
         ngen = size(gen)
      end if

      Spg%generators_list="  "
      do i=1,ngen
         Spg%generators_list=trim(Spg%generators_list)//trim(gen(i))//";"
      end do
      Spg%generators_list=Spg%generators_list(1:len_trim(Spg%generators_list)-1)

      allocate(Op(maxnum_op))
      do i=1,maxnum_op
         call allocate_op(d,Op(i))
      end do

      !> Construct the list of the generators on top of Op.
      !> The identity is always the first operator
      if (allocated(Mat)) deallocate(Mat)
      allocate(Mat(d,d))
      do i=1,ngen
         call Get_Mat_From_Symb(gen(i),Mat,invt)
         if (Err_CFML%Ierr /= 0) return
         Op(i+1)%Mat=Mat
         Op(i+1)%time_inv=invt
      end do
      ngen=ngen+1

      !> Construct the raw Group
      call Get_OPS_from_Generators(ngen,Op,multip)
      if (Err_CFML%Ierr /= 0) return

      ! Allocate provisionally to Multip the lattice translations and anti-Translations
      if (allocated(Lat_tr)) deallocate(Lat_tr)
      if (allocated(aLat_tr)) deallocate(aLat_tr)
      allocate(Lat_tr(d-1,multip), aLat_tr(d-1,multip))
      if (allocated(centre_coord)) deallocate(centre_coord)
      if (allocated(anticentre_coord)) deallocate(anticentre_coord)
      allocate(centre_coord(d-1),anticentre_coord(d-1))

      call Reorder_Operators(multip, Op, centred, centre_coord, anticentred, anticentre_coord, &
                             Numops, num_lat, num_alat, Lat_tr, aLat_tr, mag_type)
      if (Err_CFML%Ierr /= 0) return

      Spg%multip=multip
      Spg%d=d
      if (allocated(Spg%Op)) deallocate(Spg%Op)
      call Allocate_Operators(d,multip,Spg%Op)
      Spg%Op(1:multip)=Op(1:multip)

      if (allocated(Spg%Symb_Op)) Deallocate(Spg%Symb_Op)
      allocate(Spg%Symb_Op(multip))
      do i=1,multip
         Spg%Symb_Op(i)=trim(Get_Symb_from_Op(Op(i)))
      end do

      if(present(set_inv)) then
        if(set_inv) Spg%inv = Get_Inv_OP(Op) !Construct the pointer of each operator to its inverse
      end if

      if (num_lat > 0) then
         if (allocated(Spg%Lat_tr)) Deallocate(Spg%Lat_tr)
         allocate(Spg%Lat_tr(1:d-1,1:Num_Lat))
      end if

      if (num_alat > 0) then
         if (allocated(Spg%aLat_tr)) Deallocate(Spg%aLat_tr)
         allocate(Spg%aLat_tr(1:d-1,1:Num_aLat))
      end if

      if (allocated(Spg%centre_coord)) Deallocate(Spg%centre_coord)
      if (allocated(Spg%anticentre_coord)) Deallocate(Spg%anticentre_coord)
      allocate(Spg%centre_coord(1:d-1))
      allocate(Spg%anticentre_coord(1:d-1))
      Spg%Numops           = Numops
      Spg%centred          = centred
      Spg%anticentred      = anticentred
      Spg%mag_type         = mag_type
      Spg%num_lat          = num_lat
      Spg%num_alat         = num_alat
      Spg%centre_coord     = centre_coord
      Spg%anticentre_coord = anticentre_coord
      if (num_lat  > 0)  Spg%Lat_tr = Lat_tr(1:d-1,1:Num_Lat)
      if (num_alat > 0)  Spg%aLat_tr=aLat_tr(1:d-1,1:Num_aLat)

      if (present(StrCode)) then
         if (trim(StrCode) /= 'xyz') then
            do i=1,Spg%Multip
               Spg%Symb_Op(i)=Get_Symb_from_Op(Spg%Op(i),StrCode)
            end do
         end if
      end if
   End Subroutine SpaceG_Constructor_GenV

End SubModule gs_Spg_Const_VGen