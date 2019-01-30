SUBROUTINE run_generate()
	USE kinds
	USE global_variables
	IMPLICIT NONE
	INTEGER :: unit_loc=6
	call input_summary()
	IF  ( (TRIM(ADJUSTL(stencil)) .EQ. "three") .OR. (TRIM(ADJUSTL(stencil)) .EQ. "THREE") ) THEN
		 WRITE(unit_loc,'(3x,A)')'RUN MODE :: GENERATE KPOINTS IN THREE POINT METHOD'
		 CALL gen_stencil_3()
	END IF
	!	 IF  ( (TRIM(ADJUSTL(stencil)) .EQ. "five") .OR. &
 	!	   &  (TRIM(ADJUSTL(stencil)) .EQ. "FIVE") ) CALL gen_stencil_3()
	call end_summary ()

	RETURN
END SUBROUTINE


SUBROUTINE gen_stencil_3()
	USE kinds
	USE global_variables
	IMPLICIT NONE
	INTEGER :: numkpts=21
	INTEGER :: i,j
	INTEGER :: unit_loc=6
	REAL (DP), DIMENSION(3,3) :: recicell
	REAL (DP), DIMENSION(21,3) :: trmat
	REAL (DP), DIMENSION(21,3) :: kmat,hmat
	REAL (DP), DIMENSION(21,3) :: kpointsmat,kpointsmat_cart, kpointsmat_crys
	!
	!
	data trmat(1,:)  /  1.0_DP,  0.0_DP,  0.0_DP /
	data trmat(2,:)  /  0.0_DP,  0.0_DP,  0.0_DP /
	data trmat(3,:)  / -1.0_DP,  0.0_DP,  0.0_DP /
	!----------------------------------------------------------------------
	data trmat(4,:)  /  0.0_DP,  1.0_DP,  0.0_DP /
	data trmat(5,:)  /  0.0_DP,  0.0_DP,  0.0_DP /
	data trmat(6,:)  /  0.0_DP, -1.0_DP,  0.0_DP /
	!----------------------------------------------------------------------
	data trmat(7,:)  /  0.0_DP,  0.0_DP,  1.0_DP /
	data trmat(8,:)  /  0.0_DP,  0.0_DP,  0.0_DP /
	data trmat(9,:)  /  0.0_DP,  0.0_DP, -1.0_DP /
	!----------------------------------------------------------------------
	data trmat(10,:)  /  1.0_DP,  1.0_DP,  0.0_DP /
	data trmat(11,:)  /  1.0_DP, -1.0_DP,  0.0_DP /
	data trmat(12,:)  / -1.0_DP,  1.0_DP,  0.0_DP /
	data trmat(13,:)  / -1.0_DP, -1.0_DP,  0.0_DP /
	!----------------------------------------------------------------------
	data trmat(14,:)  /  0.0_DP,  1.0_DP,  1.0_DP /
	data trmat(15,:)  /  0.0_DP, -1.0_DP,  1.0_DP /
	data trmat(16,:)  /  0.0_DP,  1.0_DP, -1.0_DP /
	data trmat(17,:)  /  0.0_DP, -1.0_DP, -1.0_DP /
	!----------------------------------------------------------------------
	data trmat(18,:)  /  1.0_DP,  0.0_DP,  1.0_DP /
	data trmat(19,:)  / -1.0_DP,  0.0_DP,  1.0_DP /
	data trmat(20,:)  /  1.0_DP,  0.0_DP, -1.0_DP /
	data trmat(21,:)  / -1.0_DP,  0.0_DP, -1.0_DP /
	!
	!write (*,*) numkpts
	DO i = 1, numkpts
		DO j = 1, 3
			kmat(i,j) = kpoint(j)
			hmat(i,j) = istep

		END DO
	END DO

	DO i = 1,numkpts
		kpointsmat(i,1) = kmat(i,1) + (hmat(i,1)* trmat (i,1))
		kpointsmat(i,2) = kmat(i,2) + (hmat(i,2)* trmat (i,2))
		kpointsmat(i,3) = kmat(i,3) + (hmat(i,3)* trmat (i,3))
	END DO

	CALL  compute_reci_cell(alat,cell,recicell)


	!write (*,*) numkpts
	DO i = 1, numkpts
		IF ( (kpoint_unit .EQ. "crystal") .OR. (kpoint_unit .EQ. "CRYSTAL") ) THEN
			kpointsmat_crys = kpointsmat
			CALL crys2cart_reciprocal (recicell,kpointsmat_crys(i,:),kpointsmat_cart(i,:))
		ELSE IF ( (kpoint_unit .EQ. "cartesian") .OR. (kpoint_unit .EQ. "CARTESIAN") ) THEN
			kpointsmat_cart = kpointsmat
			CALL cart2crys_reciprocal (recicell,kpointsmat_cart(i,:),kpointsmat_crys(i,:))
		END IF
	END DO

	WRITE (unit_loc,*)''
	WRITE (unit_loc,'(3x,A,I9)')'LIST OF KPOINTS ( CRYSTAL COORDINATES ) IKPT :: ',numkpts
	WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
	DO i = 1, numkpts
			WRITE (unit_loc,'(i15,3f18.8)')i,kpointsmat_crys(i,:)
	END DO
	WRITE (unit_loc,*)''
	WRITE (unit_loc,'(3x,A,I9)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT ) IKPT :: ',numkpts
	WRITE (unit_loc,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
	DO i = 1, numkpts
			WRITE (unit_loc,'(i15,3f18.8)')i,kpointsmat_cart(i,:)
	END DO
	WRITE (unit_loc,*)''
	WRITE (unit_loc,'(3x,A)')'WRITING GENERATED KPOINTS TO K_POINTS.crystal'
	WRITE (unit_loc,*)''
	OPEN (1,file="K_POINTS.crystal")
	WRITE (1,'(A)')'K_POINTS {CRYSTAL}'
	WRITE (1,'(i9)')numkpts
	DO i = 1, numkpts
			WRITE (1,'(3f18.8,i15)')kpointsmat_crys(i,:),1
	END DO
	CLOSE(1)
	WRITE (unit_loc,*)''
	WRITE (unit_loc,'(3x,A)')'WRITING GENERATED KPOINTS TO K_POINTS.cartesian'
	WRITE (unit_loc,*)''
	OPEN (2,file="K_POINTS.cartesian")
	WRITE (2,'(A)')'K_POINTS {tpiba}'
	WRITE (2,'(i9)')numkpts
	DO i = 1, numkpts
			WRITE (2,'(3f18.8,i15)')kpointsmat_crys(i,:),1
	END DO
	CLOSE(2)

	RETURN
END SUBROUTINE
