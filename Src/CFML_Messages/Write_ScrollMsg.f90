!!----
SubModule (CFML_Messages) ScrollMsg
   Contains

   !!----
   !!---- WRITE_SCROLL_TEXT
   !!----
   !!----    Print the string in a default output unit
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Write_Scroll_Text(Mess)
      !---- Argument ----!
      character(len=*), intent(in) :: Mess

      write(unit=*, fmt="(a)") trim(mess)

   End Subroutine Write_Scroll_Text

End SubModule ScrollMsg