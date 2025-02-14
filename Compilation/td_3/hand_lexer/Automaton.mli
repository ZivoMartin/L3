(** Module representing a deterministic finite state automaton (DFA).*)

type state
(** Type representing the states of an automaton. *)

val string_of_state : state -> string
(** Converts a state to a string. *)

type t
(** Type representing a DFA. *)

val get_initial_state : t -> state
(** Returns the initial state of the DFA in argument.*)

val is_final : t -> state -> bool
(** [is_final aut st] returns [yes] if [st] is a final state of [aut] and false [otherwise]. *)

val get_next_state : t -> state * char -> state option
(** [get_next_state aut (source,letter)] return [Some target] if the successor of state [source] by letter [letter] in [aut] is [target], and [None] if there is no successor by that letter.*)

val make : string * string list * (string * char * string) list -> t
(** [make (initial,finals,transitions)] creates an DFA that has [initial] as its initial state, [finals] as its set of finite states, and [transitions] as its set of transitions. If [transitions] contains several transitions with the same source and letter, it will only consider the first one.*)
