
{
    exception Error of string
}

let alphanum = ['0' - '9' 'a' - 'z' 'A' - 'Z']

rule token = parse
    | [' ' '\t' '\r']                       { token lexbuf }
    | '\n'                       { Lexing.new_line lexbuf; token lexbuf}
    | (alphanum alphanum)+ as s             { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"even"}
    | (alphanum (alphanum alphanum)*) as s  { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"odd"}
    | eof                                   { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                                { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }