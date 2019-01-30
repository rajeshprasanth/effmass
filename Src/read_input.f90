SUBROUTINE read_input()
  USE kinds
  USE global_variables
  USE read_namelist_module
  USE read_card_module
  CALL read_namelist()
  CALL read_card()

END SUBROUTINE
