

SUBROUTINE cart2crys_reciprocal (cell,invect,outvect)
	!
	USE kinds
	USE constants
	!
	IMPLICIT NONE
	!
	INTEGER :: i
	REAL (DP), DIMENSION (3,3), INTENT(IN) :: cell		! Cell
	REAL (DP), DIMENSION (3), INTENT(IN) :: invect		! Input Vector
	REAL (DP), DIMENSION (3), INTENT(OUT):: outvect		! Output Vector
	DO i = 1, 3
		outvect (i) = cell (i, 1) * invect (1) + cell (i, 2) * invect (2) + cell (i, 3) * invect (3)
	ENDDO

END SUBROUTINE


SUBROUTINE crys2cart_reciprocal (cell,invect,outvect)
	!
	USE kinds
	USE constants
	!
	IMPLICIT NONE
	!
	INTEGER :: i
	REAL (DP), DIMENSION (3,3), INTENT(IN) :: cell		! Cell
	REAL (DP), DIMENSION (3), INTENT(IN) :: invect		! Input Vector
	REAL (DP), DIMENSION (3), INTENT(OUT):: outvect		! Output Vector
	DO i = 1, 3
		outvect (i) = cell (1, i) * invect (1) + cell (2, i) * invect (2) + cell (3, i) * invect (3)
	ENDDO

END SUBROUTINE

SUBROUTINE cart2frac_real (cell,invect,outvect)
	!
	USE kinds
	USE constants
	!
	IMPLICIT NONE
	!
	INTEGER :: i
	REAL (DP), DIMENSION (3,3), INTENT(IN) :: cell		! Cell
	REAL (DP), DIMENSION (3), INTENT(IN) :: invect		! Input Vector
	REAL (DP), DIMENSION (3), INTENT(OUT):: outvect		! Output Vector
	DO i = 1, 3
		outvect (i) = cell (i, 1) * invect (1) + cell (i, 2) * invect (2) + cell (i, 3) * invect (3)
	ENDDO

END SUBROUTINE


SUBROUTINE frac2cart_real (cell,invect,outvect)
	!
	USE kinds
	USE constants
	!
	IMPLICIT NONE
	!
	INTEGER :: i
	REAL (DP), DIMENSION (3,3), INTENT(IN) :: cell		! Cell
	REAL (DP), DIMENSION (3), INTENT(IN) :: invect		! Input Vector
	REAL (DP), DIMENSION (3), INTENT(OUT):: outvect		! Output Vector
	DO i = 1, 3
		outvect (i) = cell (1, i) * invect (1) + cell (2, i) * invect (2) + cell (3, i) * invect (3)
	ENDDO

END SUBROUTINE
