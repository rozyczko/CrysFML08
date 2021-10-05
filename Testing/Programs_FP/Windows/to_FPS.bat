@echo off
if not x%1 == x goto CONT
cls
echo    TO_FPS: Adding programs to the FullProf Suite
echo    Syntax: to_fps [gfortran/ifort]
goto END
rem
:CONT
@echo "                                           "
@echo "-------------------------------------------"
@echo "           Building MHall                  "
@echo "-------------------------------------------"
@echo "                                           "
   call make_MHall %1
rem
@echo "                                           "
@echo "-------------------------------------------"
@echo "   Building TOF_fit_LM                     "
@echo "-------------------------------------------"
@echo "                                           "
   call make_TOF_LM %1
rem
:END