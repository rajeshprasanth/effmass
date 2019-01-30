SUBROUTINE version()
  IMPLICIT none
  INTEGER :: unit_loc=6
  INTEGER :: major=0
  INTEGER :: minor=0
  INTEGER :: rev=1

  WRITE (unit_loc,*)''
  WRITE (unit_loc,'(3x,A,I1,A,I1,A,I1)')'VERSION :: ',major,'.',minor,'.',rev
  !CALL sysinfo()
END SUBROUTINE
