open OUnit2
module IntMap = Map.Make(Int)
open Trs_bridge
let norm_transition_1 _ = 
  let b0_template = "{(0, AT1:3),(1, A:0),(2, B:0),(3, AT1:3),(4, AT1:3),(5, AT2:1)}\n1 6 0\n100111\n011000\n000000\n000000\n000000\n000000\n000000\n({}, {ca1a2}, {(0, 1), (3, 1)})\n({}, {ca2a3}, {(3, 1), (4, 1)})\n({}, {ca2a4}, {(3, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1_template = "{(0, AT1:3),(1, AT1:3),(2, A:0),(3, B:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000100\n001000\n000000\n000000\n000000\n000000\n({}, {ca1a2}, {(0, 1), (1, 1)})\n({}, {ca2a3}, {(1, 1), (4, 1)})\n({}, {ca2a4}, {(1, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1_iso = "{(0, AT1:3),(1, AT1:3),(2, A:0),(3, B:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000000\n001100\n000000\n000000\n000000\n000000\n({}, {}, {(0, 1), (1, 1)})\n({}, {}, {(0, 1), (4, 1)})\n({}, {}, {(0, 1), (5, 1)})\n({}, {}, {(1, 1)})\n({}, {}, {(1, 1)})\n({}, {}, {(4, 1)})\n({}, {}, {(4, 1)})"
  in
  let lhs2init = Bigraph.Iso.of_list [(0, 1); (1, 2); (2, 0)]
  and res2init = Bigraph.Iso.of_list [(0, 1); (1, 0); (2, 2); (3, 3); (4, 4); (5, 5)] in
  let map_of_states = IntMap.singleton 0 (Bigraph.Big.of_string b0_template) |> IntMap.add 1 (Bigraph.Big.of_string b1_template) 
  and transition_to_norm = (1,0,"r1",lhs2init,res2init,Bigraph.Big.of_string b1_iso) in
  let (nt_from,nt_to,nt_label,nt_lhs2init,nt_residue2init,_) = Norm.normalize_transition map_of_states transition_to_norm in
  assert_equal nt_from 1 ;
  assert_equal nt_to 0;
  assert_equal nt_label "r1";
  assert_equal nt_lhs2init lhs2init;
  assert_equal (Bigraph.Iso.apply nt_residue2init 2) (Some 3);
  assert_equal (Bigraph.Iso.apply nt_residue2init 1) (Some 2)
let norm_transition_2 _ = 
  let b0_template = "{(0, AT1:3),(1, A:0),(2, B:0),(3, AT1:3),(4, AT1:3),(5, AT2:1)}\n1 6 0\n100111\n011000\n000000\n000000\n000000\n000000\n000000\n({}, {ca1a2}, {(0, 1), (3, 1)})\n({}, {ca2a3}, {(3, 1), (4, 1)})\n({}, {ca2a4}, {(3, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1_template = "{(0, AT1:3),(1, AT1:3),(2, A:0),(3, B:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000100\n001000\n000000\n000000\n000000\n000000\n({}, {ca1a2}, {(0, 1), (1, 1)})\n({}, {ca2a3}, {(1, 1), (4, 1)})\n({}, {ca2a4}, {(1, 1), (5, 1)})\n({}, {lc1a1}, {(0, 1)})\n({}, {lc1a3}, {(4, 1)})\n({}, {lc2a1}, {(0, 1)})\n({}, {lc2a3}, {(4, 1)})"
  and b1_iso = "{(0, AT1:3),(1, AT1:3),(2, A:0),(3, B:0),(4, AT1:3),(5, AT2:1)}\n1 6 0\n110011\n000000\n001100\n000000\n000000\n000000\n000000\n({}, {}, {(0, 1), (1, 1)})\n({}, {}, {(0, 1), (4, 1)})\n({}, {}, {(0, 1), (5, 1)})\n({}, {}, {(1, 1)})\n({}, {}, {(1, 1)})\n({}, {}, {(4, 1)})\n({}, {}, {(4, 1)})"
  in
  let lhs2init = Bigraph.Iso.of_list [(0, 1); (1, 2); (2, 0)]
  and res2init = Bigraph.Fun.of_list [(0, 1); (1, 0); (2, 2); (3, 3); (4, 4); (5, 5)] in
  let map_of_states = Hashtbl.create 10 
  and transition_to_norm = { Tracking_bigraph.TTS.in_state_idx=1;out_state_idx=0;react_label="r1";participants=lhs2init;residue=res2init;actual_out_state=(Bigraph.Big.of_string b1_iso)}in
  let _ = Hashtbl.add map_of_states 0 (Bigraph.Big.of_string b0_template) ; Hashtbl.add map_of_states 1 (Bigraph.Big.of_string b1_template) in
  let normalized_transition = Norm.normalize_single_transition map_of_states transition_to_norm in
  assert_equal normalized_transition.in_state_idx 1 ;
  assert_equal normalized_transition.out_state_idx 0;
  assert_equal normalized_transition.react_label "r1";
  assert_equal normalized_transition.participants lhs2init;
  assert_equal (Bigraph.Fun.apply normalized_transition.residue 2) (Some 3);
  assert_equal (Bigraph.Fun.apply normalized_transition.residue 1) (Some 2)
let suite =
  "Normalization" >::: [
    "Transition normalization test 1">:: norm_transition_1;
    "Transition normalization test 2">:: norm_transition_1;
    
]

let () =
  run_test_tt_main suite
