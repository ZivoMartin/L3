module Command_line = struct
  let from_file = ref true
  let raw_print = ref false
  let testing = ref true

  let arg_spec_list =
    [
      ( "-from-stdin",
        Arg.Clear from_file,
        "if present, the argument is a string to parse, otherwise, it is a \
         file name containing the program" );
      ( "-raw-print",
        Arg.Set raw_print,
        "if present display will match exactly tokens names. By default, the \
         display is made with more concise notations (more code-like)." );
      ( "-no-test",
        Arg.Clear testing,
        "if present, only displays lexed token instead of comparing with \
         expected ones, for files which have test cases." );
    ]

  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " [program name]\n\
       Lexes the program passed in argument (by default, a file), and displays \
       the sequence of tokens associated with it.\n"

  let parse () =
    let filename = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !filename with
        | None -> filename := Some a
        | Some _ ->
            raise
              (Arg.Bad
                 ("unexpected argument `" ^ a
                ^ "' (multiple inputs are not allowed)")))
      usage_msg;
    match !filename with
    | Some a -> a
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
end

let arg = Command_line.parse ()

let string_of_token = function
  | Course_language.Parser.IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | L_PAR -> "("
  | R_PAR -> ")"
  | L_CUR_BRK -> "{"
  | R_CUR_BRK -> "}"
  | L_SQ_BRK -> "["
  | R_SQ_BRK -> "]"
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | MOD -> "%"
  | AND -> "&&"
  | OR -> "||"
  | NOT -> "!"
  | EQ -> "="
  | NEQ -> "≠"
  | LT -> "<"
  | GT -> ">"
  | LEQ -> "≤"
  | GEQ -> "≥"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | ASSIGN -> ":="
  | DEF -> "::="
  | DOT -> "."
  | PRINT -> "PRINT"
  | SIZE -> "SIZE"
  | RETURN -> "RETURN"
  | INT_TYP -> "int"
  | FLOAT_TYP -> "float"
  | BOOL_TYP -> "bool"
  | NULL_TYP -> "null"
  | VAR -> "Var"
  | ID s -> "ID(" ^ s ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | BOOL b -> "BOOL(" ^ if b then "true)" else "false)"
  | EOF -> "EOF"

let raw_string_of_token = function
  | Course_language.Parser.IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | L_PAR -> "L_PAR"
  | R_PAR -> "R_PAR"
  | L_CUR_BRK -> "L_CUR_BRK"
  | R_CUR_BRK -> "R_CUR_BRK"
  | L_SQ_BRK -> "L_SQ_BRK"
  | R_SQ_BRK -> "R_SQ_BRK"
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | ASSIGN -> "ASSIGN"
  | DEF -> "DEF"
  | DOT -> "DOT"
  | PRINT -> "PRINT"
  | SIZE -> "SIZE"
  | RETURN -> "RETURN"
  | INT_TYP -> "INT_TYP"
  | FLOAT_TYP -> "FLOAT_TYP"
  | BOOL_TYP -> "BOOL_TYP"
  | NULL_TYP -> "NULL_TYP"
  | VAR -> "VAR"
  | ID s -> "ID(\"" ^ s ^ "\")"
  | STRING s -> "STRING(\"" ^ s ^ "\")"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | BOOL b -> "BOOL(" ^ if b then "true)" else "false)"
  | EOF -> "EOF"

let _, lexbuf =
  if !Command_line.from_file then MenhirLib.LexerUtil.read arg
  else (arg, Lexing.from_string arg)

let token_printer =
  if !Command_line.raw_print then raw_string_of_token else string_of_token

let rec print_token_until_end () =
  try
    let token = Course_language.Lexer.token lexbuf in
    Format.printf "%s " (token_printer token);
    if token <> Course_language.Parser.EOF then print_token_until_end ()
    else Format.printf "\nSuccesfull end of lexing\n"
  with Course_language.Lexer.Error s ->
    Format.printf "\nEnd of lexing with error : %s\n" s

let compare_token_until_end list =
  let num_errors = ref 0 in
  let rec aux list =
    try
      let token = Course_language.Lexer.token lexbuf in
      let expected = List.hd list in
      if token <> expected then num_errors := !num_errors + 1;
      Format.printf "%s  ::  " (token_printer token);
      Format.printf
        (if token = expected then "\027[32m%s\027[0m" else "\027[31m%s\027[0m")
        (token_printer expected);
      Format.printf "\n";
      if token <> Course_language.Parser.EOF then aux (List.tl list)
      else (
        Format.printf "\nEnd of lexing\n";
        if list <> [ EOF ] then
          Format.printf "Generating too few tokens : there should be %d more\n"
            (List.length list - 1))
    with
    | Course_language.Lexer.Error s ->
        Format.printf "\nEnd of lexing with error : %s\n" s
    | Failure s ->
        if s = "hd" then Format.printf "\nGenerating more token than expected\n"
        else raise (Failure s)
  in
  Format.printf "Got  ::  Expected \n";
  aux list;
  if !num_errors > 0 then
    Format.printf "There are %d errors (displayed in red)\n" !num_errors

let () =
  if !Command_line.testing then
    match arg with
    | "programs/examples_lexing/all_tokens.p" ->
        compare_token_until_end Expected.all_tokens_expected
    | "programs/examples_lexing/commentaries.p" ->
        compare_token_until_end Expected.commentaries_expected
    | "programs/examples_lexing/float_and_bool.p" ->
        compare_token_until_end Expected.float_and_bool_expected
    | "programs/examples_lexing/plagiat1.p" ->
        compare_token_until_end Expected.plagiat1_expected
    | "programs/examples_lexing/plagiat2.p" ->
        compare_token_until_end Expected.plagiat2_expected
    | "programs/examples_lexing/toto.p" ->
        compare_token_until_end Expected.toto_expected
    | _ -> print_token_until_end ()
  else print_token_until_end ()
