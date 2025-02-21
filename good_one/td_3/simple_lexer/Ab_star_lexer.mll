
{
    exception Error of string
}


rule token = parse
    | [' ' '\t' '\r']                       { token lexbuf }
    | '\n'                       { Lexing.new_line lexbuf; token lexbuf}
    | (("ab")* ) as s   { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"main" }
    | eof                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                    { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s))}