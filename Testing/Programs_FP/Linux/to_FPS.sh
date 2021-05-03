#!/bin/sh
# Script to compile the FullProf Suite Programs on Linux
#
echo "to_FPS : Adding programs to the FullProf Suite (ifort only)"
#
# Compiler Name
#
COMP="ifort"
# "#### MHall Program ####"

chmod +x make_hall.sh
make_hall.sh $COMP
##
# "#### TOF_fit_LM Program ####"
chmod +x make_tof_LM.sh
make_tof_LM.sh $COMP
