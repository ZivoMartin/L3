{
    open Parser
    exception Error of string
}

(* Si vous utilisez plusieurs fois des expressions régulières, vous pouvez les nommer ici.*)
let digit = ['0'-'9']
let letter = ['a'-'z'] | '_'             
let number = digit digit*
               

(* Ici vous avez la règle principale. Il est possible d’en définir d’autres. La règle token s’appelle par [token lexbuf], lexbuf étant l’argument caché du lexeur (c’est le buffer de lecture). Cela appellera la règle APRÈS avoir reconnu le motif courant. C’est en particulier comme ça qu’on ignore des motifs (on reconnaît un motif, puis on se rappelle récursivement sur le suivant). *)
rule token = parse
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n' { Lexing.new_line lexbuf ; token lexbuf }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '/' { DIV }
    | '%' { MOD }
    | "&&" { AND }
    | "||" { OR }
    | '!' { NOT}
    | "==" { EQ }
    | "!=" { NEQ }
    | '<' { LT }
    | '>' { GT }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "if"              { IF }
    | "then"            { THEN }
    | "else"            { ELSE }
    | "while"            { WHILE }
    | '('            { L_PAR }
    | ')'            { R_PAR }
    | '{' { L_CUR_BRK } 
    | '}' { R_CUR_BRK } 
    | '[' { L_SQ_BRK } 
    | ']' { R_SQ_BRK } 
    | ',' { COMMA }
    | ';' { SEMICOLON }
    | ":=" { ASSIGN }
    | "::=" { DEF }
    | "." { DOT }
    | "print" { PRINT }
    | "size" { SIZE }
    | "return" { RETURN }
    | "int" { INT_TYP }
    | "float" { FLOAT_TYP }
    | "bool" { BOOL_TYP }
    | "null" { NULL_TYP }
    | "var" { VAR }
    | (letter (digit | letter)*) as s  { ID(s) }
    | letter as s  { ID(String.make 1 s) } 
    | ('\"' (([^ '\"']*) as s) '\"') { STRING(s) }
    | number as n { INT(int_of_string n) }
    | (number '.' number) as x { FLOAT(float_of_string x) }
    | "true" { BOOL(true) }
    | "false" { BOOL(false) }
    | eof { EOF }
    | _ as s            { raise (Error(String.make 1 s)) }
             

             
