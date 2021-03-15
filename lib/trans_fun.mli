open Tracking_bigraph
type mapped_states = (int, (int*int) list) Hashtbl.t
type trans_fun = {permutation_with_time_shift:(int*int) list; react_label:string}
type react_times = (string,int) Hashtbl.t

val transition_function_data : int list ->  Set.Make(Int).t -> int -> (int*int) list
val convert_trans_2_trans_fun : TTS.trans_exported -> mapped_states -> react_times -> trans_fun
val convert_states : TTS.state list -> Bigraph.Ctrl.t list -> mapped_states
val convert_transitions : TTS.state list -> TTS.trans_exported list -> react_times -> Bigraph.Ctrl.t list -> trans_fun list