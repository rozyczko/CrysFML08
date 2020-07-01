!!----
SubModule (CFML_Messages) Wmess_StoppingMess
  Implicit none
   Contains

   !!----
   !!---- SUBROUTINE STOP_MESSAGE
   !!----
   !!----    Print an Stop on the screen
   !!----
   !!---- Update: 11/07/2015
   !!
   Module Subroutine Stop_Message(Mess, Title)
      !---- Argument ----!
      character (len=*),           intent(in) :: Mess      ! Message
      character (len=*), optional, intent(in) :: Title    ! Title in the Pop-up

      !---- Variable ----!
      character(:),allocatable :: ch_title

      ch_title=" "
      if (present(title)) ch_title=trim(title)

      call WMessageBox(YesNo, StopIcon, CommonYes, trim(Mess), trim(ch_title))

      return
   End Subroutine Stop_Message

End SubModule Wmess_StoppingMess