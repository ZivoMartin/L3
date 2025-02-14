let lexer_list =
  [
    ("subject_lexer", Subject_lexer.token);
    ("ab_lexer", Ab_star_lexer.token);
    ("even_odd_lexer", Even_odd_lexer.token);
    ("arith_lexer", Arith_lexer.token);
    ("decode_lexer", Decode_lexer.token);
    ("custom_lexer", Custom_lexer.token);
  ]

module CommandLine = struct
  let lex_function = ref Subject_lexer.token
  let lex_of_string = function s -> List.assoc s lexer_list

  let arg_spec_list =
    [
      ( "-lexer",
        Arg.Symbol
          (List.map fst lexer_list, fun s -> lex_function := lex_of_string s),
        " determines which lexer is used for lexing the input. (default: \
         subject_lexer)" );
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

let string = CommandLine.parse ()
let ic = Lexing.from_string string
let () = Format.printf "Lexing : %s\n" string

let rec lex_until_end () =
  let start_pos, end_pos, str, case = !CommandLine.lex_function ic in
  Format.printf "(%d,%d) : %s, case %s\n" start_pos end_pos str case;
  if case <> "EOF" then lex_until_end ()

let () = lex_until_end ()