#--------------------------------------------------------------------
# Makefile written effmass
# Written by Rajesh Prashanth Anandavadivel
# Time : Sun Oct  7 14:53:45 IST 2018
#--------------------------------------------------------------------
.SUFFIXES: .inc .f .f90
#
include ../make.inc



SRCS = version.f90 \
kinds.f90 \
constants.f90 \
date_and_tim.f90 \
global_variables.f90 \
error.f90 \
read_namelist.f90 \
read_card.f90 \
read_input.f90 \
summary.f90\
compute_reci_cell.f90 \
ordinate_conversion.f90 \
run_mode.f90 \
run_none.f90 \
run_generate.f90 \
run_calculate.f90 \
driver.f90

OBJS = version.o \
kinds.o \
constants.o \
date_and_tim.o \
global_variables.o \
error.o \
read_namelist.o \
read_card.o \
read_input.f90 \
summary.o \
compute_reci_cell.o \
ordinate_conversion.o \
run_mode.o \
run_none.o \
run_generate.o \
run_calculate.o \
driver.o

all: $(OBJS)
	#bash sysinfo.sh
	$(FC) $(FCFLAGS) $(OBJS) -o driver.x

clean:
	rm -f sysinfo.f90
	-/bin/rm -f *.o *.x *~ *.mod
