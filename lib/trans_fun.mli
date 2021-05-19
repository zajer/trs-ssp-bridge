open Tracking_bigraph
type mapped_states = (int, (int*int) list) Hashtbl.t
type t = Ssp.State.trans_fun_raw
type react_times = (string,int) Hashtbl.t

val transition_function_data : int list ->  Set.Make(Int).t -> int -> (int*int) list
val convert_trans_2_trans_fun : TTS.trans_exported -> int -> mapped_states -> react_times -> t
val convert_states : ?parallel:bool ->  TTS.state list -> Bigraph.Ctrl.t list -> mapped_states
val convert_transitions : TTS.state list -> TTS.trans_exported list -> react_times -> Bigraph.Ctrl.t list -> t list
val parconvert_transitions : TTS.state list -> TTS.trans_exported list -> react_times -> Bigraph.Ctrl.t list -> t list