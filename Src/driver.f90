program driver
  use kinds
  use global_variables
  USE read_namelist_module
  USE read_card_module
  
  call splash()
  call read_input()
  call run_mode()
  !call input_summary()

  stop
end program
