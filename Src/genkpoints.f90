SUBROUTINE genkpoints(prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints,out_kpoints)
	USE kinds
	IMPLICIT NONE
  INTEGER:: ibnd,ikpt,numkpts
  REAL (DP):: istep,alat
  REAL (DP), DIMENSION(3) :: in_kpoints,out_kpoints,out_kpoints_cart
  REAL (DP), DIMENSION(3,3) :: cell,primcell,recicell
  CHARACTER(LEN=512) :: stencil, eigenvalue_file,mode,prefix,kpoint_unit
  !
  !--------------------------------------------------------
  ! Local variables
  !--------------------------------------------------------
  !
  INTEGER:: i
  REAL (DP), DIMENSION(:,:), ALLOCATABLE:: kpoints_mat

  WRITE(*,*)'GENERATING KPOINTS WITH FINITE DIFFERENCE'
  IF (stencil .EQ. "three") THEN
      numkpts = 7
  END IF

  ALLOCATE (kpoints_mat(numkpts,3))

  kpoints_mat(1,:) =  (/out_kpoints(1)+istep,out_kpoints(2),out_kpoints(3)/)
  kpoints_mat(2,:) =  (/out_kpoints(1),out_kpoints(2),out_kpoints(3)/)
  kpoints_mat(3,:) =  (/out_kpoints(1)-istep,out_kpoints(2),out_kpoints(3)/)
  !
  kpoints_mat(4,:) =  (/out_kpoints(1)+istep,out_kpoints(2)+istep,out_kpoints(3)/)
  kpoints_mat(5,:) =  (/out_kpoints(1)+istep,out_kpoints(2)-istep,out_kpoints(3)/)
  kpoints_mat(6,:) =  (/out_kpoints(1)-istep,out_kpoints(2)+istep,out_kpoints(3)/)
  kpoints_mat(7,:) =  (/out_kpoints(1)-istep,out_kpoints(2)-istep,out_kpoints(3)/)
  WRITE (*,*)''
  WRITE(*,*)'# OF KPOINTS GENERATED :: ',numkpts
  WRITE (*,*)''
  WRITE (*,*)'--------------------------------------------------'
  WRITE(*,'(a51,i5)')'LIST OF KPOINTS ( CRYSTAL COORDINATES )  IKPT :: ',numkpts
  WRITE (*,*)'--------------------------------------------------'

  WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  DO i = 1,numkpts
    WRITE(*,'(i15,3f18.8)')i,kpoints_mat(i,:)
  END DO
  WRITE (*,*)''
  WRITE (*,*)'--------------------------------------------------'
  WRITE(*,'(a72,i5)')'LIST OF KPOINTS ( CARTESIAN COORDINATES UNITS OF 2*PI/ALAT )  IKPT :: ',numkpts
  WRITE (*,*)'--------------------------------------------------'
  WRITE(*,'(a15,a15,a18,a18)')'IKPT','KX','KY','KZ'
  DO i = 1,numkpts
    CALL  compute_reci_cell(alat,cell,recicell)
    CALL crys2cart_reciprocal (recicell,kpoints_mat(i,:),out_kpoints_cart)
		WRITE(*,'(i15,3f18.8)')i,out_kpoints_cart
  END DO

  DEALLOCATE (kpoints_mat)
  RETURN
END SUBROUTINE
