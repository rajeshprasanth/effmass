SUBROUTINE splash()
	!
    IMPLICIT NONE
    	!
    CHARACTER(LEN=9) :: cdate, ctime
    INTEGER :: unit_loc=6
      !
    WRITE(unit_loc,'(3x,A)')'+==============================================================================+'
    WRITE(unit_loc,'(3x,A)')'|                            EFFECTIVE MASS CALCULATION                        |'
    WRITE(unit_loc,'(3x,A)')'+==============================================================================+'
    CALL version()
    CALL date_and_tim(cdate,ctime)
    WRITE (unit_loc,*)''
    WRITE (unit_loc,'(3x,a13,a9,a1,a9)')'Started on : ',cdate ,' ',ctime
    WRITE (unit_loc,*)''
    WRITE (unit_loc,'(3x,A)')"Reading from input file"
    WRITE (unit_loc,*)''

    RETURN
END  SUBROUTINE

SUBROUTINE input_summary()
    USE kinds
    USE global_variables
    USE read_namelist_module
    USE read_card_module
    !
    IMPLICIT NONE
    !
    !--------------------------------------------------------
    ! Local variables
    !--------------------------------------------------------
    !
    REAL (DP), DIMENSION(3,3) :: recicell
    INTEGER :: numkpts
    INTEGER :: unit_loc=6
    REAL (DP), DIMENSION(3) :: kpoint_cart, kpoint_crys
    !
    IF ( TRIM(ADJUSTL(stencil)) == TRIM(ADJUSTL("three")) ) numkpts = 21
    IF ( TRIM(ADJUSTL(stencil)) == TRIM(ADJUSTL("five")) ) numkpts = 29
    !
    WRITE (unit_loc,'(3x,A)')'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INPUT DUMP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
    WRITE (unit_loc,'(A)')''
    WRITE (unit_loc,'(3x,A,A)')'PREFIX :: ',TRIM(ADJUSTL(prefix))
    WRITE (unit_loc,'(A)')''
    WRITE (unit_loc,'(3x,A,a12)')'CALCULATION MODE                   :: ',TRIM(ADJUSTL(mode))
    WRITE (unit_loc,'(3x,A,i12)')'# OF BANDS (IBANDS)                :: ',ibands
    WRITE (unit_loc,'(3x,A,i12)')'# OF KPOINTS (IKPT)                :: ',numkpts
    WRITE (unit_loc,'(3x,A,a12)')'STENCIL POINT GENERATION METHOD    :: ',TRIM(ADJUSTL(stencil))
    WRITE (unit_loc,'(3x,A,e12.1)')'STEP SIZE FOR STENCIL GRID         :: ',istep
    WRITE (unit_loc,*)''
    WRITE (unit_loc,'(3x,A,f12.5,A)')'REAL SPACE CELL IN UNITS OF ALAT ( ALAT = ',alat,' BOHR )'
    WRITE (unit_loc,*)''
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'A1 = ( ',cell(1,:),' )'
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'A2 = ( ',cell(2,:),' )'
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'A3 = ( ',cell(3,:),' )'
    WRITE (unit_loc,*)''
  	CALL  compute_reci_cell(alat,cell,recicell)

  	WRITE (unit_loc,'(3x,A,f12.5,A)')'RECIPROCAL CELL IN UNITS OF 2*PI/ALAT ( ALAT = ',alat,' BOHR )'
    WRITE (unit_loc,*)''
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'B1 = ( ',recicell(1,:),' )'
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'B2 = ( ',recicell(2,:),' )'
    WRITE (unit_loc,'(9x,A,3f15.8,A)')'B3 = ( ',recicell(3,:),' )'
    WRITE (unit_loc,*)''

    IF ( (kpoint_unit .EQ. "crystal") .OR. (kpoint_unit .EQ. "CRYSTAL") ) THEN
      kpoint_crys = kpoint
      WRITE (unit_loc,'(3x,A)')'LIST OF KPOINTS ( CRYSTAL COORDINATES )'
  	  WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  		WRITE (unit_loc,'(i15,3f18.8)')1,kpoint_crys
  		WRITE (unit_loc,*)''
  		WRITE (unit_loc,'(3x,A)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT )'
  		WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  		CALL crys2cart_reciprocal (recicell,kpoint,kpoint_cart)
  		WRITE (unit_loc,'(i15,3f18.8)')1,kpoint_cart
  		WRITE (unit_loc,*)''

  	ELSE IF ( (kpoint_unit .EQ. "cartesian") .OR. (kpoint_unit .EQ. "CARTESIAN") ) THEN
      kpoint_cart = kpoint
  		WRITE (unit_loc,'(3x,A)')'LIST OF KPOINTS ( CRYSTAL COORDINATES )'
  	  CALL cart2crys_reciprocal (recicell,kpoint,kpoint_crys)
  	  WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  		WRITE (unit_loc,'(i5,3f18.8)')1,kpoint_crys
  		WRITE (unit_loc,*)''
  		WRITE (unit_loc,'(3x,A)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT )'
  		WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  		WRITE (unit_loc,'(i15,3f18.8)')1,kpoint_cart
  		WRITE (unit_loc,*)''
	  END IF

    RETURN


END SUBROUTINE

SUBROUTINE end_summary()
  CHARACTER(LEN=9) :: cdate, ctime
  INTEGER :: unit_loc=6
    !
  CALL date_and_tim(cdate,ctime)
  WRITE (unit_loc,'(3x,a13,a9,a1,a9)')'Completed on : ',cdate ,' ',ctime
  WRITE (unit_loc,*)''
  WRITE(*,'(3x,a80)')'=------------------------------------------------------------------------------='
  WRITE(*,'(9x,A)')'CALCULATION COMPLETED'
  WRITE(*,'(3x,a80)')'=------------------------------------------------------------------------------='


    RETURN


END SUBROUTINE
