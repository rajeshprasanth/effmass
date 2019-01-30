SUBROUTINE error(string,routine)
  IMPLICIT NONE
  !
  CHARACTER(LEN=*),INTENT(IN) :: string,routine
  WRITE(*,'(3x,a80)')'=------------------------------------------------------------------------------='
  WRITE(*,'(5x,A,A)')'Error in routine   :: ',TRIM(ADJUSTL(routine))
  WRITE(*,'(5x,A,A)')'Error in statement :: ',TRIM(ADJUSTL(string))
  !WRITE(*,'(5X,"Error in routine ",A," (" ,A, ")")' )TRIM(routine), TRIM(ADJUSTL(string))
  !WRITE(*,'(2x,a4)')'Fatal Error :: '//TRIM(string)
  WRITE(*,'(3x,a80)')'=------------------------------------------------------------------------------='
  STOP
END SUBROUTINE
