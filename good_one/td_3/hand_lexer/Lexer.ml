let is_accepted automaton string =
  let rec step current_pos current_state =
    if current_pos = String.length string then
      Automaton.is_final automaton current_state
    else
      match
        Automaton.get_next_state automaton (current_state, string.[current_pos])
      with
      | None -> false
      | Some state -> step (current_pos + 1) state
  in
  step 0 (Automaton.get_initial_state automaton)

let get_lexeme (automaton : Automaton.t) (string : string) (start_pos : int) :
    int * Automaton.state option =
  ignore (automaton, string, start_pos);
  (0, None)
(* Vous devez renvoyer (pos,Some state) si la chaîne de caractères string.(start_pos)..string.(pos) est accepté par l’état state de automaton et qu’aucune chaîne plus longue (commençant en start_pos) n’est acceptée. Si aucune chaîne n’est acceptée, vous renverrez (start_pos,None).
   Vous pouvez vous inspirer de la fonction is_accepted (en fait elle est là pour ça). Par contre, il vous serait très difficile de l’utiliser comme boîte noire (elle ne renvoie pas assez d’informations). *)
