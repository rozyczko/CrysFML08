Submodule (test_missbehaviour_gfortran) public_proc
  Implicit none
  contains
    Module subroutine Mypublic_proc()
      call Print_Private()
    End subroutine Mypublic_proc
End Submodule public_proc