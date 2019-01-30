#!/usr/bin/env bash
#
# script to generate system information for compilation
# Written by Rajesh Prashanth Anandavadivel on Thu Jan 31 01:08:53 IST 2019
#
cat > sysinfo.f90 <<EOF
SUBROUTINE sysinfo()
  IMPLICIT NONE
  INTEGER :: unit_loc=6
  WRITE (unit_loc,'(3x,A,A)')"COMPLIER :: ", "$(grep -w ^FC ../make.inc|gawk -F= '{print $2}'|xargs)
  RETURN
END SUBROUTINE

EOF
