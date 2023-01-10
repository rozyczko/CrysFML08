! **************************************************************************
!
! CrysFML API
!
! @file      Src/Extensions/API_init.f90
! @brief     Initialisation functions for Fortran binding
!
! @homepage  https://code.ill.fr/scientific-software/crysfml
! @license   GNU LGPL (see LICENSE)
! @copyright Institut Laue Langevin 2020-now
! @authors   Scientific Computing Group at ILL (see AUTHORS), based on Elias Rabel work for Forpy
!
! **************************************************************************

module API_init
  use forpy_mod
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env
  
  implicit none

  PRIVATE
  type(PythonModule), save      :: mod_def
  type(PythonMethodTable), save :: method_table

CONTAINS
  ! Initialisation function for Python 3
  function PyInit_crysfml_api() bind(c, name="PyInit_crysfml_api") result(m)
  !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml_API
    type(c_ptr) :: m
    m = init()
  end function PyInit_crysfml_api

  ! Initialisation function for Python 2
  subroutine init_crysfml_api() bind(c, name="init_crysfml_api")
    type(c_ptr) :: m
    m = init()
  end subroutine init_crysfml_api

  ! Initialisation function
  function init() result(m)
    type(c_ptr) :: m
    integer :: ierror
    ierror = forpy_initialize()

    !--------------------------
    !Total number of method in the binding
    !--------------------------
    call method_table%init(1)
       
    m = mod_def%init("crysfml_symmetry", "A Python extension for crysFML symmetry", method_table)
  end function init


end module API_init
