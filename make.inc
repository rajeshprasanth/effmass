#--------------------------------------------------------------------
# Makefile written for solar algorithms
# Written by Rajesh Prashanth Anandavadivel
# Time : Sun Oct  7 14:53:45 IST 2018
#--------------------------------------------------------------------
.SUFFIXES: .inc .f .f90
#
#
#
#---------------------------------#
# .f.o do not modify              #
#---------------------------------#
#
.f90.o:
	$(FC) $(FCFLAGS) $(MODULES) -c $*.f90 -o $*.o
#

#
#---------------------------------#
# Flags for Fortran modules       #
#---------------------------------#
#
MOD_FLAGS = -I
#
#---------------------------------#
# Fortran compiler       #
#---------------------------------#
#
FC = gfortran
#FC = ifort
#
#---------------------------------#
# Fortran and C compilers Flags   #
#---------------------------------#
#
#FCFLAGS = -g -O2 -ffree-form -W
FCFLAGS = -g -O2 #-Wunused 
#
#---------------------------------#
# Miscellaneous flags             #
#---------------------------------#
#
AR = ar
ARFLAGS = ruv
RANLIB = ranlib
INSTALL = /usr/bin/install -c
MAKEDIR = /bin/mkdir -p
LN_S = ln -s
SED = /bin/sed
AWK = gawk
GREP = /bin/grep
EGREP = /bin/grep -E
TAR = tar
WGET = wget
