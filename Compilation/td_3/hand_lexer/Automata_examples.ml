let subject_example =
  Automaton.make
    ( "init",
      [ "qa"; "qb" ],
      [
        ("init", 'a', "qa");
        ("qa", 'b', "qab");
        ("qab", 'a', "qa");
        ("qa", 'a', "qa");
        ("init", 'b', "qb");
        ("qb", 'a', "qba");
        ("qba", 'b', "qb");
        ("qb", 'b', "qb");
      ] )

let ab_star =
  Automaton.make ("main", [ "main" ], [ ("main", 'a', "a"); ("a", 'b', "main") ])

let odd_ab =
  Automaton.make
    ( "init",
      [ "odd" ],
      [
        ("init", 'a', "odd");
        ("init", 'b', "odd");
        ("odd", 'a', "init");
        ("odd", 'b', "init");
      ] )

let digits = List.init 10 (fun a -> (string_of_int a).[0])

let arith_lexer =
  Automaton.make
    ( "init",
      [ "number"; "add"; "sub"; "lpar"; "rpar"; "space" ],
      [
        ("init", '+', "add");
        ("init", 'a', "a");
        ("a", 'd', "ad");
        ("ad", 'd', "add");
        ("init", '-', "sub");
        ("init", 's', "s");
        ("s", 'u', "su");
        ("su", 'b', "sub");
        ("init", '(', "lpar");
        ("init", ')', "rpar");
        ("init", ' ', "space");
        ("space", ' ', "space");
      ]
      @ List.map (fun n -> ("init", n, "number")) digits
      @ List.map (fun n -> ("number", n, "number")) digits )

(* Put your other examples here !*)

let list_examples =
  [
    ("subject_example", subject_example);
    ("ab_star", ab_star);
    ("odd_ab", odd_ab);
    ("arith_lexer", arith_lexer);
  ]
