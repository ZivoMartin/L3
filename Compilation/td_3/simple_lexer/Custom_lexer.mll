(*Faites ce que vous voulez ici*)

{
    exception Error of string
}


rule token = parse
    | eof                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                    { raise (Error(String.make 1 s)) }