open Trs_bridge

let help () = 
  print_endline 
  "
  This program is intended to find and save ids of states containing defined patterns.
  Arg1 - states file (generated by Trackign_bigraph library).
  Arg2 - a csv file containing patterns to be found among states.
  Arg3 - name of the output file that results will be written to.
  "
let () = 
  if Array.length Sys.argv = 4 then
  let states_file = Sys.argv.(1)
  and patterns_file = Sys.argv.(2)
  and output_file = Sys.argv.(3) in
    Facade.extract_destination_states ~states_file ~patterns_file ~output_file
  else
    help () 
  
