SUBROUTINE run_none()
  USE kinds
  USE global_variables
  IMPLICIT NONE
  INTEGER :: unit_loc=6

  call input_summary()
  WRITE(unit_loc,*)''
  WRITE(unit_loc,'(3x,A)')'RUN MODE :: DEFAULT'
  WRITE(unit_loc,*)''
  call end_summary ()

END SUBROUTINE
