!!----
!!---- Program: CALC_SFAC
!!----          Example of simple program using CFML2008
!!----
!!---- Author: Juan Rodriguez-Carvajal
!!---- Revision: April 2022
!!
Program Calc_Structure_Factors
   !---- Use Modules ----!
   use CFML_GlobalDeps,                only: Err_CFML, clear_error                
   use CFML_Strings,                   only: U_Case, File_Type
   use CFML_gSpaceGroups,              only: SPG_Type, Write_SpaceGroup_Info
   use CFML_Atoms,                     only: AtList_Type, Write_Atom_List
   use CFML_Metrics,                   only: Cell_G_Type, Write_Crystal_Cell
   use CFML_Reflections,               only: Refl_Type,SRefl_Type, RefList_Type, Initialize_RefList, Gener_Reflections
   use CFML_IOForm,                    only: Read_Xtal_Structure
   use CFML_Structure_Factors,         only: Init_Structure_Factors, Structure_Factors, &
                                             Calc_StrFactor, Write_Structure_Factors

   !---- Variables ----!
   implicit none

   type (File_Type)                             :: fich_cfl
   type (AtList_Type)                           :: A
   type (Cell_G_Type)                           :: Cell
   type (RefList_Type)                          :: hkl
   class (SPG_Type),                allocatable :: SpG

   character(len=132)          :: line
   character(len=256)          :: filcod     ! Name of the input file
   character(len=15)           :: sinthlamb  ! String with stlmax (2nd cmdline argument)
   real                        :: stlmax     ! Maximum Sin(Theta)/Lambda
   real                        :: sn, sf2, Lambda
   integer                     :: MaxNumRef, lun, ier, i, Nref
   complex                     :: fc

   integer                     :: narg
   Logical                     :: esta, arggiven=.false.,sthlgiven=.false.

   !> Arguments on the command line
   narg=command_argument_count()

   if (narg > 0) then
      call get_command_argument(1,filcod)
      i=index(filcod,".")
      if (i/= 0) filcod=filcod(1:i-1)
      arggiven=.true.
   end if

   if (narg > 1) then
      call get_command_argument(2,sinthlamb)
      read(unit=sinthlamb,fmt=*,iostat=ier) stlmax
      if (ier == 0) sthlgiven=.true.
   end if

   !> Header of the Program
   write(unit=*,fmt="(/,/,6(a,/))")                                                  &
        "            ------ PROGRAM STRUCTURE FACTORS ------"                      , &
        "                ---- Version 1.0 April-2022 ----"                         , &
        "    *******************************************************************"  , &
        "    * Calculates structure factors reading a *.CFL or a *.CIF file    *"  , &
        "    *******************************************************************"  , &
        "                      (JRC- April 2022 )"
   write(unit=*,fmt=*) " "

   if (.not. arggiven) then
      write(unit=*,fmt="(a)", advance='no') " => Code of the file xx.cif(cfl) (give xx): "
      read(unit=*,fmt="(a)") filcod
      if (len_trim(filcod) == 0) stop
   end if
   
   if (.not. sthlgiven) then
      write(unit=*,fmt="(a)", advance='no') " => Maximum sinTheta/Lambda: "
      read(unit=*,fmt=*) stlmax
   end if

   open(newunit=lun,file=trim(filcod)//".sfa", status="replace",action="write")
   write(unit=lun,fmt="(/,/,6(a,/))")                                                  &
          "            ------ PROGRAM STRUCTURE FACTORS ------"                      , &
          "                ---- Version 1.0 April-2022----"                          , &
          "    *******************************************************************"  , &
          "    * Calculates structure factors reading a *.CFL or a *.CIF file    *"  , &
          "    *******************************************************************"  , &
          "                      (JRC- April 2022 )"

   inquire(file=trim(filcod)//".cif",exist=esta)
   if (esta) then
      call Read_Xtal_Structure(trim(filcod)//".cif", Cell, SpG, A)
   
   else
      inquire(file=trim(filcod)//".cfl",exist=esta)
      if ( .not. esta) then
         write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cif (or .cfl) does'nt exist!"
         stop
      end if
      call Read_Xtal_Structure(trim(filcod)//".cfl",Cell, SpG, A, FType=fich_cfl)
   end if

   if (err_CFML%Ierr /= 0) then
      write(unit=*,fmt="(a)") trim(err_CFML%Msg)
   
   else
      call Write_Crystal_Cell(Cell,lun)
      call Write_SpaceGroup_Info(SpG,lun)
      call Write_Atom_List(A,lun)   
      
      !> Creating a list of reflections
      call Gener_Reflections(Cell,stlmax,.false.,hkl,SpG)    
      if (HKL%Nref <=0 ) then                                         
         print*, " Problems generating a list of reflections!"    
         stop                                                     
      end if                                                      
                                                            
      
      !> Look for wavelength in CFL file
      lambda=0.70926 !Mo kalpha (used only for x-rays)
      do i=1,fich_cfl%nlines
         line=adjustl(fich_cfl%line(i)%str)
         if (u_case(line(1:6)) == "LAMBDA") then
            read(unit=line(7:),fmt=*,iostat=ier) lambda
            if (ier /= 0) lambda=0.70926
         end if
      end do

      !> Calculation for X-rays assume Mo-kalpha if Lambda not given
      call Init_Structure_Factors(hkl, A, Spg, 'XRay', lambda, lun)
      call Structure_Factors(hkl, A, SpG)
      call Write_Structure_Factors(hkl,lun)
      if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      
      !> Calculation for neutron scattering
      call clear_error()
      call Init_Structure_Factors(hkl, A, Spg, 'NUC', lun=lun)
      call Structure_Factors(hkl, A, SpG, 'NUC')
      call Write_Structure_Factors(hkl, lun, 'NUC')
      if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      
      !> Calculation for Electron Diffraction
      call clear_error()
      call Init_Structure_Factors(hkl, A, Spg, 'ELE',lun=lun)
      call Structure_Factors(hkl, A, SpG,'ELE')
      call Write_Structure_Factors(hkl,lun, 'ELE')
      if (err_CFML%Ierr /= 0) write(unit=*,fmt="(a)") trim(err_CFML%Msg)

      !> Test of another structure factor subroutine
      write(unit=lun,fmt="(/,a,/)") "   H   K   L   Mult  SinTh/Lda    |Fc|       Phase        F-Real      F-Imag      Num"
      select type (rr => hkl%ref)
         type is (srefl_type)
            do i=1, hkl%nref
               sn=rr(i)%s * rr(i)%s
               call Calc_StrFactor(i,sn,A,Spg,'P','N',sf2,fc=fc)
               write(unit=lun,fmt="(3i4,i5,5f12.5,i8,f12.5)") &
                     rr(i)%h, rr(i)%mult, rr(i)%S, rr(i)%Fc, &
                     rr(i)%Phase, real(fc), aimag(fc), i, sqrt(sf2)
            end do
      end select      

      write(unit=*,fmt="(a)") " Normal End of: PROGRAM STRUCTURE FACTORS "
      write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".sfa"
   end if

   close(unit=lun)
End Program Calc_Structure_Factors

