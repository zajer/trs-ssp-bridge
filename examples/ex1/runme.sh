#!/bin/sh
NUMBER_OF_AGENTS=2
NUMBER_OF_STEPS=777
FINAL_OUTPUT=walk.csv
echo "Looking for destination states..."
./find_dest_states.exe states.csv patterns.csv dest_states.csv
echo "done. Results have been written to dest_states.csv file"
echo "Normalizing transitions..."
./normalize_tts.exe states.csv trans.csv trans_normed.csv
echo "done. Normalized transitions are saved in trans_normed.csv file."
echo "Transforming TTS into a state space accepted by SSP library..."
./transform_normed_tts.exe states.csv trans_normed.csv t2ts.csv ctrls.csv ssp_input.csv
echo "done. Results are saved in ssp_input.csv"
echo "Generating source code of a program that find walk in the previously generated state space..."
./gen_ssp_source.exe states.csv $NUMBER_OF_AGENTS compileme.ml ssp_template.txt 
echo "done. Source code is saved in compileme.ml file."
echo "Compiling the source code..."
ocamlfind ocamlopt -o program -linkpkg -package csv,ssp compileme.ml 
echo "done."
echo "Running the dynamically generated program..."
./program ssp_input.csv  dest_states.csv $NUMBER_OF_STEPS $FINAL_OUTPUT
echo "done. The final results are saved in $FINAL_OUTPUT"
