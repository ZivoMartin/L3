
{
    exception Error of string
}

let alphanum = ['0' - '9' 'a' - 'z' 'A' - 'Z']

rule token = parse
    | [' ' '\t' '\r']                       { token lexbuf }
    | '\n'                       { Lexing.new_line lexbuf; token lexbuf}
    | (alphanum)* as s          { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,s }
    | '<'                        { let start_pos = Lexing.lexeme_start lexbuf in let str,res = decode lexbuf in start_pos, (Lexing.lexeme_end lexbuf), "<"^str,res }
    | eof                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                    { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s))}

and decode = parse
    | [' ' '\t' '\r']                       { decode lexbuf }
    | '\n'                       { Lexing.new_line lexbuf; decode lexbuf}
    | (alphanum)* as s          { let str,res = decode lexbuf in s^" "^str,Format.sprintf "%c%s" s.[0] res }
    | '>'                       { ">","" }
    | eof                       { raise (Error("Reached end of file while decoding"))}
    | _ as s                    { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s))}