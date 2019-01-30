SUBROUTINE run_mode()
  USE kinds
  USE global_variables
  IMPLICIT NONE

  IF ( TRIM(ADJUSTL(mode)) .EQ. TRIM(("none")) ) CALL run_none()
  IF ( TRIM(ADJUSTL(mode)) .EQ. TRIM(("generate")) ) CALL run_generate()
  IF ( TRIM(ADJUSTL(mode)) .EQ. TRIM(("calculate")) ) CALL run_calculate()

END SUBROUTINE
