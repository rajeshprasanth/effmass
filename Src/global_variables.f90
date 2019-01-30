MODULE global_variables
  USE kinds
  SAVE
  !
  !--------------------------------------------------------
  ! Input file structures
  !--------------------------------------------------------
  !&system
  !   parameter1,
  !   parameter2,
  !   parameter3,
  !   parameter4,
  !/
  !card1
  !card2
  !
  !=------------------------------------------------------=!
  ! SYSTEM Namelist input parameters
  !=------------------------------------------------------=!
  !
  !Purpose :: This variable is used for naming the files
  CHARACTER(LEN=512) :: prefix = 'SYSTEM'
  !
  !Purpose :: This variable defines the mode in which the program
  !           shall run. List of modes available are listed below
  CHARACTER(LEN=32) :: mode = 'none'
  CHARACTER(len=80) :: permit_mode(6)
  DATA permit_mode / 'none', 'generate', 'calculate', 'NONE', 'GENERATE', 'CALCULATE'/
  !--------------------------------------------------------
  !
  ! Purpose :: This variable defines the number of bands used in
  !            used in the calculation
  INTEGER :: ibands = 1
  !--------------------------------------------------------
  !
  ! Purpose :: This variable is used for perturbating the kpoint
  !            or tell how much the kpoint should be adjusted.
  REAL (DP) :: istep = 0.1E-3
  !--------------------------------------------------------
  !
  ! Purpose :: lattice contant in units of BOHR
  REAL (DP) :: alat = 1.0_DP
  !--------------------------------------------------------
  !
  ! Purpose :: cell vectors in alat units of BOHR
  REAL (DP), DIMENSION(3,3):: cell = 0.0_DP
  !--------------------------------------------------------
  !
  ! Purpose :: Method used for generating the list of kpoints.
  !            List of methods available are listed below
  CHARACTER(LEN=512) :: stencil = 'three'
  CHARACTER(len=80) :: permit_stencil(4)
  DATA permit_stencil / 'three', 'five', 'THREE', 'FIVE' /
  !--------------------------------------------------------
  !
  ! Purpose :: Name of the file containing eigenvalues of bands
  !            generated by Quantum Espresso
  CHARACTER(LEN=512) :: eigenvalue_file
  !
  !=------------------------------------------------------=!
  ! kpoint input parameters
  !=------------------------------------------------------=!
  !
  ! Purpose :: unit of kpoint. List of units available are listed below
  CHARACTER(LEN=512) :: kpoint_unit = 'crystal'
  CHARACTER(len=80) :: permit_kpoint(4)
  DATA permit_kpoint / 'crystal', 'cartesian', 'CRYSTAL', 'CARTESIAN'/
  !--------------------------------------------------------
  !
  ! Purpose :: kpoint
  REAL (DP), DIMENSION(3):: kpoint = 0.0_DP
  !--------------------------------------------------------
  !
  ! Purpose :: timer
  real :: cpu_start, cpu_finish
  NAMELIST / input / prefix, &
                     mode, &
                     ibands, &
                     istep, &
                     alat, &
                     cell, &
                     stencil, &
                     eigenvalue_file
END MODULE global_variables
