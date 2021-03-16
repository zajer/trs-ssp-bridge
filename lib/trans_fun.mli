open Tracking_bigraph
type mapped_states = (int, (int*int) list) Hashtbl.t
type t = {permutation_with_time_shift:(int*int) list; react_label:string}
type paired_trans_fun = t*int
type react_times = (string,int) Hashtbl.t

val transition_function_data : int list ->  Set.Make(Int).t -> int -> (int*int) list
val convert_trans_2_trans_fun : TTS.trans_exported -> mapped_states -> react_times -> t
val convert_states : TTS.state list -> Bigraph.Ctrl.t list -> mapped_states
val convert_transitions : TTS.state list -> TTS.trans_exported list -> react_times -> Bigraph.Ctrl.t list -> paired_trans_fun list
val parconvert_transitions : TTS.state list -> TTS.trans_exported list -> react_times -> Bigraph.Ctrl.t list -> paired_trans_fun list
val export_trans_funs : paired_trans_fun list -> string -> unit