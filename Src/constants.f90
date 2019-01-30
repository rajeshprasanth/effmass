
MODULE constants
	!
	USE kinds
	IMPLICIT NONE

	SAVE
	!
	! ... Mathematical constants
	!
	REAL(DP), PARAMETER :: pi     = 3.14159265358979323846_DP
	REAL(DP), PARAMETER :: tpi    = 2.0_DP * pi
	REAL(DP), PARAMETER :: rad2deg = 180/pi
	REAL(DP), PARAMETER :: deg2rad = pi/180
END MODULE
