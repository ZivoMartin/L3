

{
    exception Error of string
}

let digit = ['0' - '9']
let spaces = ' '*

rule token = parse
           | spaces ((digit*) as s)spaces { int_from_string }
            
          | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }
