type token =
  | IF
  | THEN
  | ELSE
  | WHILE
  | L_PAR (* ( *)
  | R_PAR (* )*)
  | L_CUR_BRK (* { *)
  | R_CUR_BRK (* } *)
  | L_SQ_BRK (* [ *)
  | R_SQ_BRK (* ] *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | COMMA
  | SEMICOLON
  | ASSIGN
  | DEF
  | DOT
  | PRINT
  | SIZE
  | RETURN
  | INT_TYP
  | FLOAT_TYP
  | BOOL_TYP
  | NULL_TYP
  | VAR
  | ID of string
  | STRING of string
  | INT of int
  | FLOAT of float
  | BOOL of bool
  | EOF

let string_of_token = function
  | IF -> "IF"
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
