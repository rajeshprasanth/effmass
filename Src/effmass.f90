program effmass
	use kinds
	use constants
	use global_variables
	implicit none

	!
	!REAL (DP), DIMENSION (3,3) :: cell
	!REAL (DP), DIMENSION (3) :: invect,in_kpoints,out_kpoints					! Input Vector
	!REAL (DP), DIMENSION (3) :: outvect=0
	!
	REAL (DP), DIMENSION (3) :: out_kpoints					! Input Vector

	!INTEGER :: ibnd,ikpt
	!REAL (DP) :: istep,alat
	!CHARACTER(LEN=512):: eigenvalue_file,stencil,mode,prefix,kpoint_unit

	!
	CALL splash
	!
	!CALL readnamelist('inp',prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints)
	!
	CALL read_namelist('inp',datablock)
	write(*,*)datablock%prefix
	!CALL inputdump(datablock%prefix,datablock%mode,datablock%ibnd,datablock%ikpt,datablock%stencil,datablock%istep,datablock%alat,datablock%eigenvalue_file,datablock%cell)

	!CALL inputdump(prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints,out_kpoints)
	!CALL genkpoints(prefix,mode,ibnd,ikpt,stencil,istep,alat,eigenvalue_file,cell,kpoint_unit,in_kpoints,out_kpoints)
	!write(*,*)ibnd,ikpt,trim(stencil),istep,alat,trim(eigenvalue_file)
	!
	!
	!CALL coordinates_converter (flag, iskpoint, isatom,cell, invector, outvector,return_code)

      	!WRITE(*,*)'LIST OF KPOINTS (units of 2*pi/alat)'
	!write(*,*)in_kpoints
	!WRITE(*,*)''
	!WRITE(*,*)'LIST OF KPOINTS (cryst. coord.)'
	!call frac2cart(cell,in_kpoints,outvect)
	!write(*,*) outvect
	!WRITE(*,*)''

	!WRITE(*,*)'LIST OF KPOINTS (units of cryst. coord.)'
	!write(*,*)in_kpoints
	!WRITE(*,*)''
	!WRITE(*,*)'LIST OF KPOINTS (units of 2*pi/alat)'
	!call cart2frac(cell,in_kpoints,outvect)
	!write(*,*) outvect
	!WRITE(*,*)''

end program effmass
