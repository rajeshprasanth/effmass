MODULE read_card_module
	!
	USE kinds
  USE global_variables              ! Import all variables from global_variables module
  IMPLICIT NONE
  SAVE
  PRIVATE

  PUBLIC :: read_card

CONTAINS
  SUBROUTINE card_check()
    !
    IMPLICIT NONE

    INTEGER           :: i
    LOGICAL           :: allowed = .FALSE.
    !
    DO i = 1, SIZE( permit_kpoint )
         IF( TRIM(kpoint_unit) == permit_kpoint(i) ) allowed = .TRUE.
    END DO
    IF( .NOT. allowed ) CALL error ("kpoint unit not allowed","card_check()")

    RETURN
  END SUBROUTINE

  SUBROUTINE read_card()
    !
    use global_variables
    INTEGER :: ios
    INTEGER :: unit_loc=5
    CHARACTER(LEN=256):: line
    LOGICAL :: terr=.FALSE.,tend=.FALSE.

    !
    ! Reading input file here
    !

30  READ (unit_loc, fmt='(A256)',END=10,ERR=15) line
    IF ( line == ' ' .OR. line(1:1) == '#' .OR. line(1:1) == '!' ) GO TO 30
    GO TO 20
15  CALL error ("Problem in reading card","read_card()")
10  tend = .TRUE.
20  CONTINUE
    IF ( tend .EQV. .FALSE. ) GO TO 35
    IF ( tend .EQV. .TRUE. ) CALL error ("No cards found","readcard()")
35  kpoint_unit = line
    CALL card_check ()
    IF ( TRIM(ADJUSTL(line)) == 'crystal' .OR. TRIM(ADJUSTL(line))  == 'CRYSTAL') THEN
      kpoint_unit = line
    ELSE IF ( TRIM(ADJUSTL(line)) == 'cartesian' .OR. TRIM(ADJUSTL(line))  == 'CARTESIAN') THEN
      kpoint_unit = line
    END IF
    !write(*,*)TRIM(ADJUSTL(line))
    !write (*,*) ibands
    READ (unit_loc,*) kpoint
    
  END SUBROUTINE

END MODULE
