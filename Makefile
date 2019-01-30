#--------------------------------------------------------------------
# Makefile written for solar algorithms
# Written by Rajesh Prashanth Anandavadivel
# Time : Sun Oct  7 14:53:45 IST 2018
#--------------------------------------------------------------------
.SUFFIXES: .inc .f .f90
#
include make.inc

default :
	@( cd Src ; $(MAKE) all )
		
clean :
	@( cd Src ; $(MAKE) clean)	
	
