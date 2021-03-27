#!/bin/sh
NUMBER_OF_AGENTS=2
NUMBER_OF_STEPS=777
DEST_STATES=exmp_dst_states.csv
SSP_INPUT=exmp_ssp_input.csv
SSP_SOURCE=exmp_ssp_source.ml
WALK_OUTPUT=exmp_walk.csv

echo "Finding destinations states,generating a state space and a program that can utilize the two..."
dune exec --root=../.. ./bin/all_in_one.exe states.csv trans.csv patterns.csv 2 t2ts.csv ctrls.csv $DEST_STATES $SSP_INPUT $SSP_SOURCE ../../lib/ssp_template.txt
echo "done."
echo "Compiling the source code..."
ocamlfind ocamlopt -o exmp_program -linkpkg -package csv,ssp $SSP_SOURCE 
echo "done."
echo "Running the dynamically generated program..."
./exmp_program $SSP_INPUT  $DEST_STATES $NUMBER_OF_STEPS $WALK_OUTPUT
echo "done. The final results are saved in $WALK_OUTPUT"
