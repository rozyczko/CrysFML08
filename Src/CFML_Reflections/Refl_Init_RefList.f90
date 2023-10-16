!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Init_Reflist
   implicit none
   Contains

   !!----
   !!---- INITIALIZE_REFLIST(N, Reflex, Ctype, D)
   !!----    Initialize the Reflection List Variable
   !!----
   !!----   Ctype: Refl, SRefl, MRefl
   !!----
   !!----
   !!----
   !!----
   !!---- 24/06/2019
   !!
   Module Subroutine Initialize_RefList(N, Reflex, Ctype, D)
      !---- Arguments ----!
      integer,             intent(in)     :: N
      type(RefList_Type),  intent(in out) :: Reflex
      character(len=*),    intent(in)     :: Ctype   ! Refl, SRefl, MRefl
      integer, optional,   intent(in)     :: D       ! Dimension of the magnetic hkl

      !---- Local Variables ----!
      integer          :: i, Dd
      type(Refl_Type)  :: ref1
      type(SRefl_Type) :: ref2
      type(MRefl_Type) :: ref3

      if (allocated(reflex%ref)) deallocate(reflex%ref)
      select case (n)
         case (0)
             reflex%Nref=0

         case (1:)

            reflex%Nref=n
            select case (l_case(ctype))
               case ('srefl')
                  allocate(reflex%ref(n), source=ref2)

               case ('mrefl')
                  allocate(reflex%ref(n), source=ref3)

               case default
                  allocate(reflex%ref(n), source=ref1)
            end select

            Dd=3
            if (present(D)) Dd=d

            associate (r => reflex%ref)
               do i=1,n
                  allocate(reflex%ref(i)%h(dd))
               end do
               select type (r)
                 class is (Refl_Type)
                    do i=1,n
                       r(i)%h     =0
                       r(i)%mult  =0
                       r(i)%s     =0.0_cp
                       r(i)%Imag  =0
                       r(i)%Pcoeff=0
                    end do
               end select

               select type (r)
                 class is (SRefl_Type)
                    do i=1,n
                       r(i)%iph  =1
                       r(i)%Fo   =0.0_cp
                       r(i)%Fc   =0.0_cp
                       r(i)%sFo  =0.0_cp
                       r(i)%Phase=0.0_cp
                       r(i)%A    =0.0_cp
                       r(i)%B    =0.0_cp
                       r(i)%W    =1.0_cp
                    end do
               end select

               select type (r)
                 type is (MRefl_Type)
                    do i=1,n
                       r(i)%smIvo=0.0_cp
                       r(i)%msF=cmplx(0.0_cp,0.0_cp)
                       r(i)%mIv=cmplx(0.0_cp,0.0_cp)
                    end do
               end select
            end associate

      end select

   End Subroutine Initialize_RefList

End SubModule Refl_Init_Reflist