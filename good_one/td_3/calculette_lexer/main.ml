let ic = Lexing.from_string Sys.argv.(1)
let a = Lexer.token ic
let () = Format.printf "result: %d\n" a