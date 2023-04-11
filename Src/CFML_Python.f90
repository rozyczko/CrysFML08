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
    Use CFML_Atoms,        only: Atm_Type, Atlist_Type
    Use CFML_Metrics,      only: Cell_Type, Cell_G_Type
    Use CFML_Reflections,  only: Refl_Type, SRefl_Type, Reflist_Type
    Use Forpy_Mod, str_forpy => str

    !---- Variables ----!
    implicit none

    private

    !---- List of public procedures ----!
    public :: Wrap_Atm_Type, Wrap_Atlist_Type, Wrap_Cell_Type, Wrap_Refl_Type, &
              Wrap_Reflist_Type

    !---- Interface Zone ----!
    Interface

       Module Subroutine Wrap_Atm_Type(for_var, dic_var)
        !---- Arguments ----!
        class(atm_type), intent(in)    :: for_var
        type(dict),      intent(inout) :: dic_var
       End Subroutine Wrap_Atm_Type

       Module Subroutine Wrap_Atlist_Type(for_var, dic_var)
        !---- Arguments ----!
        type(atlist_type),  intent(in)    :: for_var
        type(dict),         intent(inout) :: dic_var
       End Subroutine Wrap_Atlist_Type

       Module Subroutine Wrap_Cell_Type(for_var, dic_var)
        !---- Arguments ----!
        class(cell_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: dic_var
       End Subroutine Wrap_Cell_Type

       Module Subroutine Wrap_Refl_Type(for_var, dic_var)
        !---- Arguments ----!
        class(refl_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: dic_var
       End Subroutine Wrap_Refl_Type

       Module Subroutine Wrap_Reflist_Type(for_var, dic_var)
        !---- Arguments ----!
        type(reflist_type),  intent(in)   :: for_var
        type(dict),         intent(inout) :: dic_var
       End Subroutine Wrap_Reflist_Type

    End Interface

 End Module CFML_Python
