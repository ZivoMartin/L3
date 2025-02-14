

{
    open Parser
    exception Error of string
}

(* Si vous utilisez plusieurs fois des expressions régulières, vous pouvez les nommer ici.*)
let digit = ['0'-'9']

(* Ici vous avez la règle principale. Il est possible d’en définir d’autres. La règle token s’appelle par [token lexbuf], lexbuf étant l’argument caché du lexeur (c’est le buffer de lecture). Cela appellera la règle APRÈS avoir reconnu le motif courant. C’est en particulier comme ça qu’on ignore des motifs (on reconnaît un motif, puis on se rappelle récursivement sur le suivant). *)
rule token = parse
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }
    | '+'               { ADD }
    | _ as s            { raise (Error(String.make 1 s)) }