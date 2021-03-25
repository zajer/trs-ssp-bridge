let help () = 
  print_endline 
  "
  This program normalizes an output of Tracking_bigraph library.
  Normalization unifies the output state of a transition with it's correspinding state in discovered states.
  They are not only isomorphic to each other, they are identical.
  
  If you see this message it means that you did not provide necessary number of arguments.
  Arguments required for this program are:
  Arg1 - the name of a file containing discovered states (generated by Trackign_bigraph library).
  Arg2 - the name of a file with transitions (generated by Trackign_bigraph library).
  Arg3 - the name of the normalized transitions file (generated by this program).
  "
  let () = 
    if Array.length Sys.argv = 4 then
    let states_file = Sys.argv.(1)
    and trans_file = Sys.argv.(2)
    and output_file = Sys.argv.(3) in
        Trs_bridge.Facade.normalize_tts ~states_file ~trans_file ~norm_trans_file:output_file
    else
        help ()