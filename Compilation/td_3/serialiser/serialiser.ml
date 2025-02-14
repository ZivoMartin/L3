(** Use this function to translate a list of integer of size 8 representing the integer as a list of bytes (between 0 and 255) that can be converted to char.*)
let int_list_of_int i =
  let rec normalise_list list =
    if List.length list >= 8 then list else normalise_list (0 :: list)
  in
  let rec aux rest acc =
    if rest = 0 then acc else aux (rest / 256) ((rest mod 256) :: acc)
  in
  if i >= 0 then normalise_list (aux i [])
  else
    let l = normalise_list (aux (i + max_int + 1) []) in
    (List.hd l + 64) :: List.tl l

(** Decodes an list of integers as an int. Use it to control your results.*)
let int_of_int_list l =
  let rec aux l acc =
    if l = [] then acc else aux (List.tl l) ((256 * acc) + List.hd l)
  in
  aux l 0

(** Converts a float in an list of integer of size 8 that can be converted to char (the content are between 0 and 255)*)
let int_list_of_float f = int_list_of_int Int64.(to_int (bits_of_float f))

(** Decodes the previous encoding.*)
let float_of_int_list l = Int64.(float_of_bits (of_int (int_of_int_list l)))

(** Encodes a string as a list of ints between 1 and 255. The list is terminated by a 0.*)
let int_list_of_string s =
  List.map int_of_char (List.of_seq (String.to_seq s)) @ [ 0 ]

(** Converts the content of file [source] to a int list, as described in the subject.
      You have first to open the source with open_in, and get a lexbuf with the module Lexing (see other files of this exercice session). Then you have to call the lexer and then converts each token to an int. Use the functions above to converts integers, floats and strings that are arguments of print_string.
      The only difficult point is for identifiers: you have to keep track of each identifier and attribute to each a unique integer (for exemple its position in the list of identifiers when ordering them by order of first appearance). You should support more than 255 identifiers, so you should have an encoding terminated by a particular number - e.g 0 or 255. Thus you cannot use the functions above for that.*)
let serialise_to_int_list source = failwith ("Au boulot" ^ source)

(** Detects if the programs of files [source1] and [source2] are identical up to renaming of identifiers. You should use the previous function for that.*)
let detect_plagiarism source1 source2 =
  failwith ("Au boulot" ^ source1 ^ " " ^ source2)

(** Serialises the content of file [source] and writes it in the formatter, with Format.printf. You should use [serialise_to_int_list] and converts the result to chars.*)
let serialise_to_channel formatter source =
  Format.fprintf formatter "Au boulot %s" source
