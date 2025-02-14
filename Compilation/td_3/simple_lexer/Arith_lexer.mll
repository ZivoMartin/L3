
{
    exception Error of string
}

let digit = ['0' - '9']

rule token = parse
    | [' ' '\t' '\r']                       { token lexbuf }
    | '\n'                       { Lexing.new_line lexbuf; token lexbuf}
    | ("+" | "add") as s        { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"add" }
    | ("-" | "sub") as s        { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"sub" }
    | digit+ as s                { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"number" }
    | '('                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "(","Lpar" }
    | ')'                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), ")","Rpar" }
    | eof                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                    { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s))}