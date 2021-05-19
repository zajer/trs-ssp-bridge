open Bigraph
open Tracking_bigraph
type mapped_states = Big.t option array
val normalize_single_transition : mapped_states -> TTS.trans_exported -> TTS.trans_exported
val normalize_transitions : mapped_states -> TTS.trans_exported list -> TTS.trans_exported list
val normalize_exported_tts : TTS.state list -> TTS.trans_exported list -> TTS.trans_exported list
val parnormalize_exported_tts : TTS.state list -> TTS.trans_exported list -> TTS.trans_exported list