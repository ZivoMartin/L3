module CommandLine = struct
  let automaton = ref Automata_examples.subject_example

  let automaton_of_string = function
    | s -> List.assoc s Automata_examples.list_examples

  let arg_spec_list =
    [
      ( "-automaton",
        Arg.Symbol
          ( List.map fst Automata_examples.list_examples,
            fun s -> automaton := automaton_of_string s ),
        " determines which automaton is used for lexing the input. (default: \
         subject_example)" );
    ]

  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [string_to_lex]\n"

  let parse () =
    let res = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with
        | None -> res := Some a
        | Some _ -> raise (Arg.Bad "Too many strings to lex"))
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> s
end

let str = CommandLine.parse ()
let automaton = !CommandLine.automaton

let () =
  if Lexer.is_accepted automaton str then Format.printf "%s is accepted\n" str
  else Format.printf "%s is rejected\n" str

let first_prefix_length, final = Lexer.get_lexeme automaton str 0

let () =
  match final with
  | None -> Format.printf "No valid prefix\n"
  | Some st ->
      Format.printf "First valid prefix: length : %d : %s, case %s\n"
        first_prefix_length
        (String.sub str 0 first_prefix_length)
        (Automaton.string_of_state st)

let parse_full_str () =
  let rec aux pos acc =
    try
      let next, state = Lexer.get_lexeme automaton str pos in
      if next = pos then List.rev acc
      else
        aux next (((pos, next), state, String.sub str pos (next - pos)) :: acc)
    with _ -> List.rev acc
  in
  aux 0 []

let list = parse_full_str ()

let () =
  Format.printf "\nAll valid prefixes:\n";
  List.iter
    (fun ((b, e), st, s) ->
      Format.printf "(%d,%d) : %s, case %s\n" b e s
        (match st with
        | None -> "Rejected"
        | Some st -> Automaton.string_of_state st))
    list
