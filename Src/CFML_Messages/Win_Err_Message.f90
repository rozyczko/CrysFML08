!!----
!!----  CRYSFML + WINTERACTER
!!----
!!----
!!----
SubModule (CFML_Messages) Win_Err_Message
  Implicit none
   Contains

   !!----
   !!---- ERROR_MESSAGE
   !!----
   !!---- Print an error message on the screen and in "Iunit" if present
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Error_Message(Mess, Iunit, Routine, Fatal)
      !---- Arguments ----!
      character(len=*),            intent(in) :: Mess         ! Error information
      integer, optional,           intent(in) :: iunit        ! Write information on Iunit unit
      Character(Len =*), Optional, Intent(In) :: Routine      ! Added for consistency with the CFML_IO_Mess.f90 version.
      Logical,           Optional, Intent(In) :: Fatal        ! Added for consistency with the CFML_IO_Mess.f90 version.

      !> Init
      call WMessageBox(OKOnly, ExclamationIcon, CommonOK, trim(Mess),"CryFML: Error Message")

      if (present(iunit)) then
         write(unit=iunit,fmt="(tr1,a)") "****"
         write(unit=iunit,fmt="(tr1,a)") "**** ERROR: "//trim(Mess)
         write(unit=iunit,fmt="(tr1,a)") "****"
         write(unit=iunit,fmt="(tr1,a)") " "

         If (Present(Routine)) Then
            Write(Unit = iunit, Fmt = "(tr1,a)") "**** PROCEDURE: "//trim(Routine)
         End If

         If (Present(Fatal)) Then
            If (Fatal) Then
               write(unit=iunit,fmt="(tr1,a)") " "
               Write(Unit = iunit, Fmt = "(/tr1,a)") "**** The Program Will Stop Here."
               Stop
            End If
         End If
      end if

   End Subroutine Error_Message

      !!----
      !!---- SET_PYTHON_ERROR_FLAG
      !!----
      !!----    Set python error flag to EXCEPTION_ERROR if CFML_Err%Flag
      !!----    is different from zero. Raise a Python exception if raise is
      !!----    given
      !!----
      !!---- 27/03/2023
      !!
      Module Subroutine Set_Python_Error_Flag(ierror,proc_name)
         !---- Arguments ----!
         Integer,          Intent(inout)        :: ierror    ! Python error flag
         Character(Len=*), Intent(In), Optional :: proc_name ! Name of the calling procedure

         if (err_cfml%flag) ierror = EXCEPTION_ERROR
         if (present(proc_name)) call raise_exception(RuntimeError,proc_name//': '//trim(err_cfml%msg))

      End Subroutine Set_Python_Error_Flag


End SubModule Win_Err_Message