Module test_missbehaviour_gfortran
  Implicit none
  private

  public :: Mypublic_proc

  Interface
    Module subroutine Mypublic_proc()
    End subroutine Mypublic_proc
  End Interface

  contains
    subroutine Print_Private()
      write(*,*) " => This is a print from a private subroutine"
    End subroutine Print_Private
End Module test_missbehaviour_gfortran