SUBROUTINE compute_reci_cell(alat,primcell,recicell)
	USE kinds
	USE CONSTANTS
	IMPLICIT NONE
	!
	!--------------------------------------------------------
	! Input variables
	!--------------------------------------------------------
	!
	REAL (DP) :: alat
	REAL (DP), DIMENSION(3,3), INTENT(IN) :: primcell
	!
	!--------------------------------------------------------
	! Output variables
	!--------------------------------------------------------
	!
	REAL (DP), DIMENSION(3,3), INTENT(OUT) :: recicell
	!
	!--------------------------------------------------------
	! Local variables
	!--------------------------------------------------------
	!
	REAL (DP) :: det,multiplier
	REAL (DP), DIMENSION(3) :: a1,a2,a3,b1,b2,b3,temp
	!
	a1 = primcell(1,:)*alat
	a2 = primcell(2,:)*alat
	a3 = primcell(3,:)*alat
	!==============================================================
	! 		| i		j		k	|
	! a2 x a3 = 	| a2(1)		a2(2)		a2(3)	|
	!		| a3(1)		a3(2)		a3(3)	|
	!--------------------------------------------------------------
	! 		| i		j		k	|
	! a3 x a1 = 	| a3(1)		a3(2)		a3(3)	|
	!		| a1(1)		a1(2)		a1(3)	|
	!--------------------------------------------------------------
	! 		| i		j		k	|
	! a1 x a2 = 	| a1(1)		a1(2)		a1(3)	|
	!		| a2(1)		a2(2)		a2(3)	|
	!==============================================================
	temp(1) = (a2(2)*a3(3)) - (a2(3)*a3(2))
	temp(2) = (a2(3)*a3(1)) - (a2(1)*a3(3))
	temp(3) = (a2(1)*a3(2)) - (a2(2)*a3(1))
	b1 = (tpi/dot_product(a1,temp))*temp
	!
	temp(1) = (a3(2)*a1(3)) - (a3(3)*a1(2))
	temp(2) = (a3(3)*a1(1)) - (a3(1)*a1(3))
	temp(3) = (a3(1)*a1(2)) - (a3(2)*a1(1))
	b2 = (tpi/dot_product(a2,temp))*temp
	!
	temp(1) = (a1(2)*a2(3)) - (a1(3)*a2(2))
	temp(2) = (a1(3)*a2(1)) - (a1(1)*a2(3))
	temp(3) = (a1(1)*a2(2)) - (a1(2)*a2(1))
	b3 = (tpi/dot_product(a3,temp))*temp
	!
	recicell(1,:)=b1/(tpi/alat)
	recicell(2,:)=b2/(tpi/alat)
	recicell(3,:)=b3/(tpi/alat)
	!
	RETURN
END SUBROUTINE
