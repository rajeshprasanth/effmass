MODULE read_namelist_module
	!
	USE kinds
  USE global_variables              ! Import all variables from global_variables module
  IMPLICIT NONE
  SAVE
  PRIVATE

  PUBLIC :: read_namelist

CONTAINS
  SUBROUTINE namelist_check()
    !
    IMPLICIT NONE

    INTEGER           :: i
    LOGICAL           :: allowed = .FALSE.
    !
    DO i = 1, SIZE( permit_mode )
         IF( TRIM(mode) == permit_mode(i) ) allowed = .TRUE.
    END DO
    IF( .NOT. allowed ) CALL error ("Value in mode not allowed","namelist_check()")
    !
    IF( ibands .LT. 0 ) CALL error ("Value of ibands should be > 1","namelist_check()")
    !
    IF( istep .LT. 0 ) CALL error ("Value of istep should be > 0","namelist_check()")
    !
		IF( alat .LE. 0 ) CALL error ("Value of istep should be > 0","namelist_check()")

		allowed = .FALSE.
		!write(*,*)allowed

    DO i = 1, SIZE( permit_stencil )
				!write(*,*)stencil
				 !allowed = .FALSE.
         IF( TRIM(stencil) == permit_stencil(i) ) allowed = .TRUE.
				 !
    END DO

    IF( .NOT. allowed ) CALL error ("Value for stencil not allowed","namelist_check()")
    !

    RETURN
  END SUBROUTINE

  SUBROUTINE read_namelist()
    !
    use global_variables
    INTEGER :: ios
    INTEGER :: unit_loc=5
    !
    ! Reading input file here
    !
    ios=0
    READ( unit_loc, input, iostat = ios )
		!write(*,*)stencil
    IF( ios /= 0 ) CALL error ('Bad namelist name found','read_namelist()')
    CALL namelist_check()
    !write (*,*) ibands

  END SUBROUTINE

END MODULE
