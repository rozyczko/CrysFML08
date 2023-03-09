Submodule (CFML_DiffPatt) DiffP_Add_Patterns

 implicit none

 Contains

    !!----
    !!---- ADD_PATTERNS
    !!----
    !!---- Add Patterns
    !!----
    !!---- 30/04/2019
    !!
    Module Subroutine Add_Patterns(Patterns, N, Active, Pat, step_int, VNorm)
        !---- Arguments ----!
        class(DiffPat_Type), dimension(:), intent(in)  :: Patterns
        integer,                           intent(in)  :: N
        logical,             dimension(:), intent(in)  :: Active
        class(DiffPat_Type),               intent(out) :: Pat
        real(kind=cp), optional,           intent(in)  :: step_int
        real(kind=cp), optional,           intent(in)  :: VNorm

        !---- Local Variables ----!
        integer                           :: i,j,npts,nc, np
        real(kind=cp)                     :: xmin,xmax,step,y,cnorm
        real(kind=cp), dimension(:), allocatable :: varI

        !> Checking
        if (N <= 0) return
        if (all(active) .eqv. .false.) return
        call clear_error()
        !> Initial values
        xmin=minval(Patterns(1:N)%xmin, mask= (active .eqv. .true.) )
        xmax=maxval(Patterns(1:N)%xmax, mask= (active .eqv. .true.) )

        if(present(step_int)) then
           step=step_int
           npts=nint((xmax-xmin)/step)+1
           np=maxval(Patterns(1:N)%npts, mask= (active .eqv. .true.) )
        else
           np =maxval(Patterns(1:N)%npts, mask= (active .eqv. .true.) )
           if (np  <= 1) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Add_Patterns@DIFFPAT: Number of Points in the new Pattern was zero! "
              return
           end if

           step=(xmax-xmin)/real(np -1)
           if (abs(step) <= epsilon(1.0_cp)) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Add_Patterns@DIFFPAT: Step size in the new Pattern was close to zero! "
              return
           end if
        end if

        if(allocated(varI)) deallocate(varI)
        allocate(varI(np))

        !> Allocating New Pat
        call Allocate_Pattern (Pat, npts)

        if (present(vnorm)) then
           cnorm=vnorm
        else
           cnorm=1.0
        end if

        do i=1,npts
           Pat%x(i)=xmin + (i-1)*step
           Pat%y(i)=0.0; Pat%sigma(i)=0.0
           nc=0
           do j=1,N
              if(Pat%x(i) < Patterns(j)%xmin) cycle      !This is to ensure that only points in range are treated
              if(Pat%x(i) > Patterns(j)%xmax) cycle
              np=Patterns(j)%npts
              if(Patterns(j)%SigVar) then
                varI(1:np)=Patterns(j)%sigma(1:np)*Patterns(j)%sigma(1:np)
              else
                varI(1:np)=Patterns(j)%sigma(1:np)
              end if
              nc=nc+1
              y=Linear_Interpol(Pat%x(i),Patterns(j)%x(1:np),Patterns(j)%y(1:np))
              Pat%y(i)=Pat%y(i) + y
              y=Linear_Interpol(Pat%x(i),Patterns(j)%x(1:np),varI(1:np)) !variance
              Pat%sigma(i)=Pat%sigma(i)+y
           end do
           !> control
           if (nc > 0) then
              Pat%y(i)=Pat%y(i)/real(nc)
              Pat%sigma(i)=sqrt(Pat%sigma(i)/real(nc))
           else
              Pat%y(i)=0.0
              Pat%sigma(i)=1.0
           end if
        end do
        Pat%SigVar=.true.
        Pat%xmin=xmin
        Pat%xmax=xmax
        Pat%step=step
        Pat%ymin=minval(Pat%y)
        Pat%ymax=maxval(Pat%y)
    End Subroutine Add_Patterns

End Submodule DiffP_Add_Patterns