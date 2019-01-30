



subroutine splash
	!
	IMPLICIT NONE
	!
	CHARACTER(LEN=9) :: cdate, ctime
	!
	WRITE(*,'(a80)')'+============================================================================+'
	WRITE(*,'(a80)')'|                         EFFECTIVE MASS CALCULATION                         |'
	WRITE(*,'(a80)')'+============================================================================+'
	CALL date_and_tim(cdate,ctime)
	WRITE (*,*)''
	WRITE (*,'(a15,a9,a1,a9)')'Started on : ',cdate ,' ',ctime
	WRITE (*,*)''

	return
end subroutine

SUBROUTINE readnamelist(filename,prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints)
	!
	USE kinds
	IMPLICIT NONE
	!
	!--------------------------------------------------------
	! Input file
	!--------------------------------------------------------
	!
	CHARACTER(LEN=512), INTENT(IN) :: filename
	!
	!--------------------------------------------------------
	! Namelist variables
	!--------------------------------------------------------
	!
	INTEGER, INTENT(OUT):: ibnd,ikpt
	REAL (DP), INTENT(OUT) :: istep,alat
	REAL (DP), DIMENSION(3), INTENT(OUT) :: in_kpoints
	REAL (DP), DIMENSION(3,3), INTENT(OUT) :: cell
	CHARACTER(LEN=512), INTENT(OUT) :: stencil, eigenvalue_file,mode,prefix,kpoint_unit
	!
	!--------------------------------------------------------
	! Local variables
	!--------------------------------------------------------
	!
	INTEGER :: stencil_point,i
	!CHARACTER(LEN=512) :: kpoint_unit
	!
	NAMELIST /input/ mode,prefix,ibnd,ikpt,stencil,istep,alat,cell,eigenvalue_file
	!
	!--------------------------------------------------------
	! Set default variables
	!--------------------------------------------------------
	!
	mode = 'generate'
	prefix = 'prefix'
	stencil = 'three'
	istep = 1.0E-3
	!
	OPEN (1,file=filename)
       	READ (1,nml=input)
       	READ (1,*)kpoint_unit
       	READ (1,*)in_kpoints
       	CLOSE (1)
       	!
  	RETURN
END SUBROUTINE

SUBROUTINE inputdump(prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell)

!SUBROUTINE inputdump(prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints,out_kpoints)
	USE kinds
	IMPLICIT NONE
	!
	!--------------------------------------------------------
	! Namelist variables
	!--------------------------------------------------------
	!
	INTEGER:: ibnd,ikpt
	REAL (DP):: istep,alat
!	REAL (DP), DIMENSION(3) :: in_kpoints
	!REAL (DP), DIMENSION(3), INTENT(OUT) :: out_kpoints
!	REAL (DP), DIMENSION(3):: out_kpoints

	REAL (DP), DIMENSION(3,3) :: cell,primcell,recicell
	CHARACTER(LEN=512) :: stencil, eigenvalue_file,mode,prefix!,kpoint_unit
	CHARACTER(LEN=512)::var
	!
	!--------------------------------------------------------
	! Local variables
	!--------------------------------------------------------
	!
	INTEGER :: stencil_point
!	REAL (DP), DIMENSION(3) :: in_kpoints_cart,in_kpoints_crys
	!

     	WRITE (*,'(a80)')'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> INPUT DUMP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
       	WRITE (*,'(a20)')''
       	WRITE (*,'(a8)')'PREFIX'
       	WRITE (*,'(a8)')'~~~~~~'
       	WRITE (*,'(a2,a64)')'  ',prefix
       	WRITE (*,'(a8)')'~~~~~~'
       	WRITE (*,'(a20)')''
       	WRITE (*,'(a40,i9)')'# OF BANDS (IBANDS)                :: ',ibnd
       	WRITE (*,'(a40,i9)')'# OF KPOINTS (IKPT)                :: ',ikpt

       	IF (trim(stencil) == 'three') THEN
       		stencil_point = 3
       	ELSE
       		stencil_point = 5
	END IF
	WRITE (*,'(a40,i9)')'# OF POINTS IN STENCIL GRID        :: ',stencil_point
     	WRITE (*,'(a40,e15.1)')'STEP SIZE FOR STENCIL GRID         :: ',istep
     	WRITE (*,*)''
      	WRITE (*,'(a44,f12.5,a10)')'REAL SPACE CELL IN UNITS OF ALAT ( ALAT = ',alat,'BOHR )'
      	WRITE (*,*)''
      	WRITE (*,'(a15,3f15.8,a3)')'A1 = ( ',cell(1,:),' )'
       	WRITE (*,'(a15,3f15.8,a3)')'A2 = ( ',cell(2,:),' )'
       	WRITE (*,'(a15,3f15.8,a3)')'A3 = ( ',cell(3,:),' )'
       	WRITE (*,*)''
				CALL  compute_reci_cell(alat,cell,recicell)

	      WRITE (*,'(a49,f12.5,a10)')'RECIPROCAL CELL IN UNITS OF 2*PI/ALAT ( ALAT = ',alat,'BOHR )'
        WRITE (*,*)''
      	WRITE (*,'(a15,3f15.8,a3)')'B1 = ( ',recicell(1,:),' )'
       	WRITE (*,'(a15,3f15.8,a3)')'B2 = ( ',recicell(2,:),' )'
       	WRITE (*,'(a15,3f15.8,a3)')'B3 = ( ',recicell(3,:),' )'
       	WRITE (*,*)''

!       	IF ( ( trim(kpoint_unit) .EQ. "crystal") .OR. ( trim(kpoint_unit) .EQ. "CRYSTAL") ) THEN
!	       	WRITE(*,'(a51,i5)')'LIST OF KPOINTS ( CRYSTAL COORDINATES )  IKPT :: ',ikpt
!	       	WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
!		WRITE(*,'(i15,3f18.8)')ikpt,in_kpoints
!		WRITE (*,*)''
!		WRITE(*,'(a72,i5)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT )  IKPT :: ',ikpt
!		WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
!		CALL crys2cart_reciprocal (recicell,in_kpoints,in_kpoints_cart)
!		WRITE (*,'(i15,3f18.8)')ikpt,in_kpoints_cart
!		out_kpoints = in_kpoints
!		WRITE (*,*)''

!	ELSE IF ( ( trim(kpoint_unit) .EQ. "cartesian") .OR. ( trim(kpoint_unit) .EQ. "CARTESIAN") ) THEN
!		WRITE(*,'(a51,i5)')'LIST OF KPOINTS ( CRYSTAL COORDINATES )  IKPT :: ',ikpt
!	       	CALL cart2crys_reciprocal (recicell,in_kpoints,in_kpoints_crys)
!	       	WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
!		WRITE(*,'(i5,3f18.8)')ikpt,in_kpoints_crys
!		WRITE (*,*)''
!		WRITE(*,'(a72,i5)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT )  IKPT :: ',ikpt
!		WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
!		WRITE (*,'(i15,3f18.8)')ikpt,in_kpoints
!		out_kpoints = in_kpoints_crys
!		WRITE (*,*)''
!	END IF

       	RETURN
END SUBROUTINE
