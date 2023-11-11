!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!---- MODULE: CFML_Python
!!----   INFO: Subroutines related to Python interfacing
!!----
!!---- HISTORY
!!----    Update: 27/03/2023
!!----
!!
Module CFML_Python

   !---- Use Modules ----!
   Use CFML_GlobalDeps
   Use CFML_Atoms,        only: Atm_Cell_Type,Atm_Type,Atm_Ref_Type,Atm_Std_Type,Atlist_Type,Modatm_Ref_Type,Modatm_Std_Type
   Use CFML_DiffPatt,     only: DiffPat_Type,DiffPat_E_Type,DiffPat_G_Type
   Use CFML_gSpaceGroups, only: Symm_Oper_Type,Group_Type,SpG_Type,SuperSpaceGroup_Type
   Use CFML_Metrics,      only: Cell_Type,Cell_G_Type,Cell_LS_Type,Cell_GLS_Type,Twofold_Axes_Type
   Use CFML_Rational
   Use CFML_Reflections,  only: Refl_Type,SRefl_Type,Reflist_Type
   Use Forpy_Mod, str_forpy => str

   !---- Variables ----!
   implicit none

   private

   !---- List of public procedures ----!
   public :: Array_To_List,Check_Number_Of_Arguments,Get_Var_From_Item,Ndarray_To_Pointer,Pointer_To_Array,&
             Pointer_To_Array_Alloc,Wrap_Atm_Cell_Type,Wrap_Atm_Type,Wrap_Atlist_Type,Wrap_Symm_Oper_Type
   public :: Wrap_Group_Type,Wrap_Cell_Type,Wrap_Refl_Type,Wrap_Reflist_Type,Wrap_DiffPat_Type,&
             Wrap_Twofold_Axes_Type
   public :: Unwrap_Atm_Cell_Type,Unwrap_Atlist_Type,Unwrap_Atm_Type,Unwrap_Atm_Type_No_Alloc,&
             Unwrap_Cell_Type,Unwrap_Cell_G_Type,Unwrap_Dict_Item,Unwrap_Reflist_Type,Unwrap_Refl_Type,&
             Unwrap_Refl_Type_No_Alloc,Unwrap_Spg_Type,Unwrap_Dict_Item_String_Alloc,Unwrap_Twofold_Axes_Type

   !---- Overload ----!
   Interface Array_To_List
      module procedure Array_To_List_Str
   End Interface

   Interface Get_Var_From_Item
      module procedure Get_Dict_From_Item
      module procedure Get_Int32_From_Item
      module procedure Get_Logical_From_Item
      module procedure Get_Ndarray_From_Item
      module procedure Get_Real32_From_Item
      module procedure Get_String_From_Item
   End Interface Get_Var_From_Item

   Interface List_To_Array
      module procedure List_To_Array_Atm_Type
      module procedure List_To_Array_Logical
      module procedure List_To_Array_Refl_Type
      module procedure List_To_Array_Str
      module procedure List_To_Array_Symm_Oper_Type
   End Interface List_To_Array

   Interface Ndarray_To_Pointer
      module procedure Ndarray_Int32_1d_To_Pointer
      module procedure Ndarray_Int32_2d_To_Pointer
      module procedure Ndarray_Int64_2d_To_Pointer
      module procedure Ndarray_Int32_3d_To_Pointer
      module procedure Ndarray_Real32_1d_To_Pointer
      module procedure Ndarray_Real32_2d_To_Pointer
      module procedure Ndarray_Real32_3d_To_Pointer
   End Interface Ndarray_To_Pointer

   Interface Pointer_To_Array
      module procedure Pointer_Int32_1d_To_Array
      module procedure Pointer_Int32_2d_To_Array
      module procedure Pointer_Int32_3d_To_Array
      module procedure Pointer_Rational_1d_To_Array
      module procedure Pointer_Rational_2d_To_Array
      module procedure Pointer_Rational_3d_To_Array
      module procedure Pointer_Real32_1d_To_Array
      module procedure Pointer_Real32_2d_To_Array
      module procedure Pointer_Real32_3d_To_Array
   End Interface Pointer_To_Array

   Interface Pointer_To_Array_Alloc
      module procedure Pointer_Int32_1d_To_Array_Alloc
      module procedure Pointer_Int32_2d_To_Array_Alloc
      module procedure Pointer_Int32_3d_To_Array_Alloc
      module procedure Pointer_Rational_1d_To_Array_Alloc
      module procedure Pointer_Rational_2d_To_Array_Alloc
      module procedure Pointer_Rational_3d_To_Array_Alloc
      module procedure Pointer_Real32_1d_To_Array_Alloc
      module procedure Pointer_Real32_2d_To_Array_Alloc
      module procedure Pointer_Real32_3d_To_Array_Alloc
   End Interface Pointer_To_Array_Alloc

   Interface Rational_To_Ndarray
      module procedure Rational_To_Ndarray_1d
      module procedure Rational_To_Ndarray_2d
      module procedure Rational_To_Ndarray_3d
   End Interface Rational_To_Ndarray

   Interface Unwrap_Dict_Item
      module procedure Unwrap_Dict_Item_Dict
      module procedure Unwrap_Dict_Item_Int32
      module procedure Unwrap_Dict_Item_List
      module procedure Unwrap_Dict_Item_Logical
      module procedure Unwrap_Dict_Item_Ndarray_Int32_1d
      module procedure Unwrap_Dict_Item_Ndarray_Int32_2d
      module procedure Unwrap_Dict_Item_Ndarray_Int32_3d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_1d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_2d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_3d
      module procedure Unwrap_Dict_Item_Real32
      module procedure Unwrap_Dict_Item_String
   End Interface Unwrap_Dict_Item

   !---- Interface Zone ----!
   Interface

      Module Subroutine Array_To_List_Logical(procedure_name,var_name,arr,my_list,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         logical, dimension(:),                           intent(in)    :: arr
         type(list),                                      intent(inout) :: my_list
         integer,                                         intent(inout) :: ierror
      End Subroutine Array_To_List_Logical

      Module Subroutine Array_To_List_Str(procedure_name,var_name,arr,my_list,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         character(len=*), dimension(:),                  intent(in)    :: arr
         type(list),                                      intent(inout) :: my_list
         integer,                                         intent(inout) :: ierror
      End Subroutine Array_To_List_Str

      Module Subroutine Get_Dict_from_Item(procedure_name,var_name,item,di,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         type(dict),       intent(inout) :: di
         integer,          intent(inout) :: ierror
      End Subroutine Get_Dict_from_Item

      Module Subroutine Get_Int32_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         integer,          intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Int32_from_Item

      Module Subroutine Get_Logical_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         logical,          intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Logical_from_Item

      Module Subroutine Get_Ndarray_from_Item(procedure_name,var_name,item,nd,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         type(ndarray),    intent(inout) :: nd
         integer,          intent(inout) :: ierror
      End Subroutine Get_Ndarray_from_Item

      Module Subroutine Get_Real32_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         real,             intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Real32_from_Item

      Module Subroutine Get_String_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(object),                  intent(inout) :: item
         character(len=:), allocatable, intent(out)   :: var
         integer,                       intent(inout) :: ierror
      End Subroutine Get_String_from_Item

      Module Subroutine List_To_Array_Logical(procedure_name,var_name,my_list,arr,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         type(list),                                      intent(inout) :: my_list
         logical, dimension(:), allocatable,              intent(out)   :: arr
         integer,                                         intent(inout) :: ierror
      End Subroutine List_To_Array_Logical

      Module Subroutine List_To_Array_Atm_Type(procedure_name,var_name,my_list,arr,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         type(list),                                      intent(inout) :: my_list
         class(atm_type), dimension(:), allocatable,      intent(out)   :: arr
         integer,                                         intent(inout) :: ierror
      End Subroutine List_To_Array_Atm_Type

      Module Subroutine List_To_Array_Refl_Type(procedure_name,var_name,my_list,arr,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         type(list),                                      intent(inout) :: my_list
         class(refl_type),     dimension(:), allocatable, intent(out)   :: arr
         integer,                                         intent(inout) :: ierror
      End Subroutine List_To_Array_Refl_Type

      Module Subroutine List_To_Array_Str(procedure_name,var_name,my_list,arr,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         type(list),                                      intent(inout) :: my_list
         character(len=*), dimension(:), allocatable,     intent(out)   :: arr
         integer,                                         intent(inout) :: ierror
      End Subroutine List_To_Array_Str

      Module Subroutine List_To_Array_Symm_Oper_Type(procedure_name,var_name,my_list,arr,ierror)
         !---- Arguments ----!
         character(len=*),                                intent(in)    :: procedure_name
         character(len=*),                                intent(in)    :: var_name
         type(list),                                      intent(inout) :: my_list
         type(symm_oper_type), dimension(:), allocatable, intent(out)   :: arr
         integer,                                         intent(inout) :: ierror
      End Subroutine List_To_Array_Symm_Oper_Type

      Module Subroutine Ndarray_Int32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:), pointer,     intent(out)   :: p
         integer,                            intent(inout) :: ierror
      End Subroutine Ndarray_Int32_1d_To_Pointer

      Module Subroutine Ndarray_Int32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:,:), pointer,   intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Int32_2d_To_Pointer

      Module Subroutine Ndarray_Int64_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                         intent(in)    :: procedure_name
         character(len=*),                         intent(in)    :: var_name
         type(ndarray),                            intent(in)    :: nd
         integer(kind=8), dimension(:,:), pointer, intent(out)   :: p
         integer,                                  intent(inout) :: ierror
         character(len=1),                         intent(out)   :: order
      End Subroutine Ndarray_Int64_2d_To_Pointer

      Module Subroutine Ndarray_Int32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Int32_3d_To_Pointer

      Module Subroutine Ndarray_Real32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:), pointer,        intent(out)   :: p
         integer,                            intent(inout) :: ierror
      End Subroutine Ndarray_Real32_1d_To_Pointer

      Module Subroutine Ndarray_Real32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:,:), pointer,      intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Real32_2d_To_Pointer

      Module Subroutine Ndarray_Real32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:,:,:), pointer,    intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Real32_3d_To_Pointer

      Module Subroutine Pointer_Int32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         integer, dimension(:), pointer, intent(in)    :: p
         integer, dimension(:),          intent(out)   :: arr
         integer,                        intent(inout) :: ierror
      End Subroutine Pointer_Int32_1d_To_Array

      Module Subroutine Pointer_Int32_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         integer, dimension(:), pointer,     intent(in)    :: p
         integer, dimension(:), allocatable, intent(out)   :: arr
         integer,                            intent(inout) :: ierror
      End Subroutine Pointer_Int32_1d_To_Array_Alloc

      Module Subroutine Pointer_Int32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         integer, dimension(:,:), pointer, intent(in)    :: p
         integer, dimension(:,:),          intent(out)   :: arr
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(in)    :: order
      End Subroutine Pointer_Int32_2d_To_Array

      Module Subroutine Pointer_Int32_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         integer, dimension(:,:), pointer,     intent(in)    :: p
         integer, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(in)    :: order
      End Subroutine Pointer_Int32_2d_To_Array_Alloc

      Module Subroutine Pointer_Int32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         integer, dimension(:,:,:), pointer, intent(in)    :: p
         integer, dimension(:,:,:),          intent(out)   :: arr
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(in)    :: order
      End Subroutine Pointer_Int32_3d_To_Array

      Module Subroutine Pointer_Int32_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                       intent(in)    :: procedure_name
         character(len=*),                       intent(in)    :: var_name
         integer, dimension(:,:,:), pointer,     intent(in)    :: p
         integer, dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                                intent(inout) :: ierror
         character(len=1),                       intent(in)    :: order
      End Subroutine Pointer_Int32_3d_To_Array_Alloc

      Module Subroutine Pointer_Rational_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),             intent(in)    :: procedure_name
         character(len=*),             intent(in)    :: var_name
         real, dimension(:), pointer,  intent(in)    :: p
         type(rational), dimension(:), intent(out)   :: arr
         integer,                      intent(inout) :: ierror
      End Subroutine Pointer_Rational_1d_To_Array

      Module Subroutine Pointer_Rational_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                          intent(in)    :: procedure_name
         character(len=*),                          intent(in)    :: var_name
         real, dimension(:), pointer,               intent(in)    :: p
         type(rational), dimension(:), allocatable, intent(out)   :: arr
         integer,                                   intent(inout) :: ierror
      End Subroutine Pointer_Rational_1d_To_Array_Alloc

      Module Subroutine Pointer_Rational_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         real, dimension(:,:), pointer,  intent(in)    :: p
         type(rational), dimension(:,:), intent(out)   :: arr
         integer,                        intent(inout) :: ierror
         character(len=1),               intent(in)    :: order
      End Subroutine Pointer_Rational_2d_To_Array

      Module Subroutine Pointer_Rational_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                            intent(in)    :: procedure_name
         character(len=*),                            intent(in)    :: var_name
         real, dimension(:,:), pointer,               intent(in)    :: p
         type(rational), dimension(:,:), allocatable, intent(out)   :: arr
         integer,                                     intent(inout) :: ierror
         character(len=1),                            intent(in)    :: order
      End Subroutine Pointer_Rational_2d_To_Array_Alloc

      Module Subroutine Pointer_Rational_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         real, dimension(:,:,:), pointer,  intent(in)    :: p
         type(rational), dimension(:,:,:), intent(out)   :: arr
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(in)    :: order
      End Subroutine Pointer_Rational_3d_To_Array

      Module Subroutine Pointer_Rational_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                              intent(in)    :: procedure_name
         character(len=*),                              intent(in)    :: var_name
         real, dimension(:,:,:), pointer,               intent(in)    :: p
         type(rational), dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                                       intent(inout) :: ierror
         character(len=1),                              intent(in)    :: order
      End Subroutine Pointer_Rational_3d_To_Array_Alloc

      Module Subroutine Pointer_Real32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         real, dimension(:), pointer, intent(in)    :: p
         real, dimension(:),          intent(out)   :: arr
         integer,                     intent(inout) :: ierror
      End Subroutine Pointer_Real32_1d_To_Array

      Module Subroutine Pointer_Real32_1d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         real, dimension(:), pointer,     intent(in)    :: p
         real, dimension(:), allocatable, intent(out)   :: arr
         integer,                         intent(inout) :: ierror
      End Subroutine Pointer_Real32_1d_To_Array_Alloc

      Module Subroutine Pointer_Real32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         real, dimension(:,:), pointer, intent(in)    :: p
         real, dimension(:,:),          intent(out)   :: arr
         integer,                       intent(inout) :: ierror
         character(len=1),              intent(in)    :: order
      End Subroutine Pointer_Real32_2d_To_Array

      Module Subroutine Pointer_Real32_2d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                  intent(in)    :: procedure_name
         character(len=*),                  intent(in)    :: var_name
         real, dimension(:,:), pointer,     intent(in)    :: p
         real, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                           intent(inout) :: ierror
         character(len=1),                  intent(in)    :: order
      End Subroutine Pointer_Real32_2d_To_Array_Alloc

      Module Subroutine Pointer_Real32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         real, dimension(:,:,:), pointer, intent(in)    :: p
         real, dimension(:,:,:),          intent(out)   :: arr
         integer,                         intent(inout) :: ierror
         character(len=1),                intent(in)    :: order
      End Subroutine Pointer_Real32_3d_To_Array

      Module Subroutine Pointer_Real32_3d_To_Array_Alloc(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                    intent(in)    :: procedure_name
         character(len=*),                    intent(in)    :: var_name
         real, dimension(:,:,:), pointer,     intent(in)    :: p
         real, dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                             intent(inout) :: ierror
         character(len=1),                    intent(in)    :: order
      End Subroutine Pointer_Real32_3d_To_Array_Alloc

      Module Function Rational_To_Ndarray_1d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:), intent(in)  :: rational_arr
         integer,                      intent(out) :: ierror
         type(ndarray)                             :: nd_arr
      End Function Rational_To_Ndarray_1d

      Module Function Rational_To_Ndarray_2d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:,:), intent(in)  :: rational_arr
         integer,                        intent(out) :: ierror
         type(ndarray)                               :: nd_arr
      End Function Rational_To_Ndarray_2d

      Module Function Rational_To_Ndarray_3d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:,:,:), intent(in)  :: rational_arr
         integer,                          intent(out) :: ierror
         type(ndarray)                                 :: nd_arr
      End Function Rational_To_Ndarray_3d

      Module Subroutine Unwrap_Dict_Item_Dict(procedure_name,var_name,di,my_dict,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         type(dict),                  intent(inout) :: my_dict
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Dict

      Module Subroutine Unwrap_Dict_Item_Int32(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         integer,                     intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Int32

      Module Subroutine Unwrap_Dict_Item_List(procedure_name,var_name,di,my_list,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         type(list),                  intent(inout) :: my_list
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_List

      Module Subroutine Unwrap_Dict_Item_Logical(procedure_name,var_name,di,l,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         logical,                     intent(out)   :: l
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Logical

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d(procedure_name,var_name,di,p,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         type(dict),                     intent(inout) :: di
         integer, dimension(:), pointer, intent(out)   :: p
         integer,                        intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         type(dict),                       intent(inout) :: di
         integer, dimension(:,:), pointer, intent(out)   :: p
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(dict),                         intent(inout) :: di
         integer, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                 intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d(procedure_name,var_name,di,p,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         real, dimension(:), pointer, intent(out)   :: p
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(dict),                    intent(inout) :: di
         real, dimension(:,:), pointer, intent(out)   :: p
         integer,                       intent(inout) :: ierror
         character(len=1),              intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         type(dict),                      intent(inout) :: di
         real, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                         intent(inout) :: ierror
         character(len=1),                intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d

      Module Subroutine Unwrap_Dict_Item_Real32(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         real,                        intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Real32

      Module Subroutine Unwrap_Dict_Item_String(procedure_name,var_name,di,s,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         character(len=*),            intent(inout) :: s
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_String

      Module Subroutine Unwrap_Dict_Item_String_Alloc(procedure_name,var_name,di,my_str,ierror)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(dict),                    intent(inout) :: di
         character(len=:), allocatable, intent(inout) :: my_str
         integer,                       intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_String_Alloc

      ! #### CFML_Atoms ####

      Module Subroutine Unwrap_Atm_Cell_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                       intent(inout) :: py_var
         type(atm_cell_type), allocatable, intent(out)   :: for_var
         integer,                          intent(out)   :: ierror
      End Subroutine Unwrap_Atm_Cell_Type

      Module Subroutine Unwrap_Atlist_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         type(atlist_type),             intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Atlist_Type

      Module Subroutine Unwrap_Atm_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         class(atm_type), allocatable,  intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Atm_Type

      Module Subroutine Unwrap_Atm_Type_No_Alloc(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         class(atm_type),               intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Atm_Type_No_Alloc

      Module Subroutine Wrap_Atm_Cell_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         type(atm_cell_type), intent(in)    :: for_var
         type(dict),          intent(inout) :: py_var
         integer,             intent(out)   :: ierror
      End Subroutine Wrap_Atm_Cell_Type

      Module Subroutine Wrap_Atlist_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         type(atlist_type),  intent(in)    :: for_var
         type(dict),         intent(inout) :: py_var
         integer,            intent(out)   :: ierror
      End Subroutine Wrap_Atlist_Type

      Module Subroutine Wrap_Atm_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         class(atm_type), intent(in)    :: for_var
         type(dict),      intent(inout) :: py_var
         integer,         intent(out)   :: ierror
      End Subroutine Wrap_Atm_Type

      ! #### CFML_DiffPat ####

      Module Subroutine Wrap_DiffPat_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         class(diffpat_type), intent(in)    :: for_var
         type(dict),          intent(inout) :: py_var
         integer,             intent(out)   :: ierror
      End Subroutine Wrap_DiffPat_Type

      ! #### CFML_gSpaceGroups ####

      Module Subroutine Unwrap_Symm_Oper_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),           intent(inout) :: py_var
         type(symm_oper_type), intent(out)   :: for_var
         integer,              intent(out)   :: ierror
      End Subroutine Unwrap_Symm_Oper_Type

      Module Subroutine Unwrap_Spg_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                   intent(inout) :: py_var
         class(spg_type), allocatable, intent(out)   :: for_var
         integer,                      intent(out)   :: ierror
      End Subroutine Unwrap_Spg_Type

      Module Subroutine Wrap_Symm_Oper_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         type(symm_oper_type), intent(in)    :: for_var
         type(dict),           intent(inout) :: py_var
         integer,              intent(out)   :: ierror
      End Subroutine Wrap_Symm_Oper_Type

      Module Subroutine Wrap_Group_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         class(group_type),    intent(in)    :: for_var
         type(dict),           intent(inout) :: py_var
         integer,              intent(out)   :: ierror
      End Subroutine Wrap_Group_Type

      ! #### CFML_Metrics ####

      Module Subroutine Unwrap_Cell_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         class(cell_type), allocatable, intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Cell_Type

      Module Subroutine Unwrap_Cell_G_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                      intent(inout) :: py_var
         class(cell_g_type), allocatable, intent(out)   :: for_var
         integer,                         intent(out)   :: ierror
      End Subroutine Unwrap_Cell_G_Type

      Module Subroutine Unwrap_Twofold_Axes_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),              intent(inout) :: py_var
         type(twofold_axes_type), intent(out)   :: for_var
         integer,                 intent(out)   :: ierror
      End Subroutine Unwrap_Twofold_Axes_Type

      Module Subroutine Wrap_Cell_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         class(cell_type), intent(in)    :: for_var
         type(dict),       intent(inout) :: py_var
         integer,          intent(out)   :: ierror
      End Subroutine Wrap_Cell_Type

      Module Subroutine Wrap_Twofold_Axes_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         type(twofold_axes_type), intent(in)    :: for_var
         type(dict),              intent(inout) :: py_var
         integer,                 intent(out)   :: ierror
      End Subroutine Wrap_Twofold_Axes_Type

      ! #### CFML_Rational ####

      Module Subroutine Wrap_Rational(for_var,py_var,ierror)
         !---- Arguments ----!
         type(rational), intent(in)    :: for_var
         type(dict),     intent(inout) :: py_var
         integer,        intent(out)  :: ierror
      End Subroutine Wrap_Rational

      ! #### CFML_Reflections ####

      Module Subroutine Unwrap_Reflist_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         type(reflist_type),            intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Reflist_Type

      Module Subroutine Unwrap_Refl_Type(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         class(refl_type), allocatable, intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Refl_Type

      Module Subroutine Unwrap_Refl_Type_No_Alloc(py_var,for_var,ierror)
         !---- Arguments ----!
         type(dict),                    intent(inout) :: py_var
         class(refl_type),              intent(out)   :: for_var
         integer,                       intent(out)   :: ierror
      End Subroutine Unwrap_Refl_Type_No_Alloc

      Module Subroutine Wrap_Reflist_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         type(reflist_type),  intent(in)    :: for_var
         type(dict),          intent(inout) :: py_var
         integer,             intent(out)   :: ierror
      End Subroutine Wrap_Reflist_Type

      Module Subroutine Wrap_Refl_Type(for_var,py_var,ierror)
         !---- Arguments ----!
         class(refl_type), intent(in)    :: for_var
         type(dict),       intent(inout) :: py_var
         integer,          intent(out)  :: ierror
      End Subroutine Wrap_Refl_Type

   End Interface

   Contains

      Subroutine Check_Number_Of_Arguments(procedure_name,args,nmandatory,narg,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         type(tuple),      intent(inout) :: args
         integer,          intent(in)    :: nmandatory
         integer,          intent(out)   :: narg
         integer,          intent(out)   :: ierror

         ierror = args%len(narg)
         if (narg < nmandatory) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': insufficient number of arguments'
        end if
      End Subroutine Check_Number_Of_Arguments

End Module CFML_Python
