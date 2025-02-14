

{
    exception Error of string
}


rule token = parse
    | ("a" (("a" | "ba")*) ) as s   { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"A" }
    | ("b" (("b" | "ab")*)) as s   { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), s,"B" }
    | eof                       { (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf), "","EOF"}
    | _ as s                    { raise (Error(String.make 1 s)) }