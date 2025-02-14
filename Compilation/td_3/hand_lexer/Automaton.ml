type state = string
type t = {initial_state : state; final_states : state list; transitions : ((state * char) * state) list}

let string_of_state s = s

let get_initial_state aut = aut.initial_state
let is_final aut state = List.mem state aut.final_states

let get_next_state aut (state,char) = List.assoc_opt (state,char) aut.transitions

let make (init,finals,trans) = {initial_state = init; final_states = finals; transitions = List.map (fun (a,b,c) -> ((a,b),c)) trans}
