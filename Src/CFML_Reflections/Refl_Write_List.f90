!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Write_List
   implicit none
   Contains

   !!----
   !!---- WRITE_INFO_REFLIST
   !!----    Write information about the Reflection List
   !!----
   !!---- 24/06/2019
   !!
   Module Subroutine Write_Info_RefList(Reflex, Iunit, Mode)
      !---- Arguments ----!
      type(RefList_Type),         intent(in) :: Reflex
      integer,          optional, intent(in) :: Iunit
      character(len=*), optional, intent(in) :: Mode

      !---- Local variables ----!
      integer                              :: i,n,lun,d
      character(len=:), allocatable        :: forma
      character(len=*), dimension(0:2), parameter :: charRef = ["Nuclear ","Magnetic"," Nuc+Mag"]

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      if (present(mode)) then
         select case (l_case(mode(1:3)))
            case("neu")
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NEUTRONS)"
               write(unit=lun,fmt="(a)")     "    ==================================================="
            case("ele")
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(ELECTRONS)"
               write(unit=lun,fmt="(a)")     "    ===================================================="
            case default
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"
               write(unit=lun,fmt="(a)")     "    ================================================="
         end select

      else
         write(unit=lun,fmt="(a)")   "    LIST OF REFLECTIONS AND STRUCTURE FACTORS"
         write(unit=lun,fmt="(a)")   "    ========================================="
      end if

      if (reflex%nref <=0) then
         write(unit=lun,fmt="(/,a)")   "There is no reflection in the List!"
         return
      end if

      n=reflex%nref
      d=size(reflex%Ref(1)%h)

      Select Type (r => Reflex%Ref)
         type is (Refl_Type)
            if (present(mode)) then
               select case (l_case(mode(1:3)))
                  case("neu")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (NEUTRONS)"
                     write(unit=lun,fmt="(a)")     "    =============================="
                  case("ele")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (ELECTRONS)"
                     write(unit=lun,fmt="(a)")     "    ==============================="
                  case default
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS (X-RAYS)"
                     write(unit=lun,fmt="(a)")     "    ============================"
               end select
            else
               write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS"
               write(unit=lun,fmt="(a)")     "    ==================="
            end if


            forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a)"
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character'
              Case(4)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character'
              Case(5)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character'
              Case(6)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character'
            End Select

            do i=1,n
               write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag)
            end do

         type is (SRefl_Type)
            if (present(mode)) then
               select case (l_case(mode(1:3)))
                  case("neu")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NEUTRONS)"
                     write(unit=lun,fmt="(a)")     "    ==================================================="
                  case("ele")
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(ELECTRONS)"
                     write(unit=lun,fmt="(a)")     "    ===================================================="
                  case default
                     write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"
                     write(unit=lun,fmt="(a)")     "    ================================================="
               end select

            else
               write(unit=lun,fmt="(a)")   "    LIST OF REFLECTIONS AND STRUCTURE FACTORS"
               write(unit=lun,fmt="(a)")   "    ========================================="
            end if


            forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4)"
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'
                                                                             !123456789012123456789012123456789012123456789012
              Case(4)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'
              Case(5)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'
              Case(6)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'
            End Select

            do i=1,n
               write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                            r(i)%Fo,r(i)%SFo, r(i)%Fc, r(i)%Phase
            end do

         type is (MRefl_Type)
            write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(Nucler & Magnetic)"
            write(unit=lun,fmt="(a)")     "    ============================================================"

            forma="(i11,tr1, i4,i7,tr3,f15.5,tr3,a,4f12.4,tr3,14f12.4)"
            write(unit=forma(10:10),fmt="(i1)") d

            Select Case(d)
              Case(3)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)'
              Case(4)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)'
              Case(5)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)'
              Case(6)
                write(lun,'(a)')  &
                '     NumRef    h   k   l   m   n   p   Mult   sinTheta/Lambda   Character     |Fobs|    sig(Fobs)       |Fc|      Phase'// &
                '   |oMagIntVector|     sigma  Real(MsFx)  Real(MsFy)  Real(MsFz)  Imag(MsFx)  Imag(MsFy)  Imag(MsFz)  Real(MiVx)  Real(MiVy)  Real(MiVz)  Imag(MiVx)  Imag(MiVy)  Imag(MiVz)'
            End Select

            do i=1,n
               write(unit=lun,fmt=forma) i, r(i)%h, r(i)%mult, r(i)%S, charRef(r(i)%imag), &
                                            r(i)%Fo,r(i)%SFo, r(i)%Fc, r(i)%Phase, &
                                            r(i)%mIvo,r(i)%smIvo,real(r(i)%msF(:)),aimag(r(i)%msF(:)),&
                                            real(r(i)%mIv(:)),aimag(r(i)%mIv(:))
            end do

      End Select

   End Subroutine Write_Info_RefList

End SubModule Refl_Write_List