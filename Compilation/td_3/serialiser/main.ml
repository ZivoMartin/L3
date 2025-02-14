module CommandLine = struct
  type task = Detect | Display | Write

  let task = ref Display
  let serialised_file = ref "serialised"

  let string_of_task = function
    | Detect -> "detect"
    | Display -> "display"
    | Write -> "write"

  let task_of_string = function
    | "detect" -> Detect
    | "display" -> Display
    | "write" -> Write
    | _ -> failwith "wrong task string"

  let arg_spec_list =
    [
      ( "-task",
        Arg.Symbol
          ([ "detect"; "display"; "write" ], fun s -> task := task_of_string s),
        " determines what the program is doing: detect for plagiarism \
         detection of the two program in argument ; display to display the \
         serialisation ; write to write the binary serialisation in a file"
        ^ " (default: " ^ string_of_task !task ^ ")" );
      ( "-output",
        Arg.String (fun s -> serialised_file := s),
        Format.sprintf
          " sets the name of the file written to if task is write in \
           <NAME>.txt (default : %s)"
          !serialised_file );
    ]

  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [program name]\n"

  let parse () =
    let filename1 = ref None in
    let filename2 = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !filename1 with
        | None -> filename1 := Some a
        | Some _ -> (
            match !filename2 with None -> filename2 := Some a | Some _ -> ()))
      usage_msg;
    (!filename1, !filename2)
end

let filename1, filename2 = CommandLine.parse ()

let file1 =
  match filename1 with
  | None ->
      Arg.usage CommandLine.arg_spec_list CommandLine.usage_msg;
      exit 0
  | Some a -> a

let () =
  match !CommandLine.task with
  | CommandLine.Detect ->
      let file2 =
        match filename2 with
        | None -> raise (Arg.Bad "Only one program provided")
        | Some a -> a
      in
      if Serialiser.detect_plagiarism file1 file2 then
        Format.printf
          "The two programs are strictly equivalent. Alert Plagiarism\n"
      else Format.printf "The two program are different\n"
  | CommandLine.Display ->
      List.iter
        (fun i -> Format.printf "%d " i)
        (Serialiser.serialise_to_int_list file1)
  | CommandLine.Write ->
      let oc = open_out (!CommandLine.serialised_file ^ ".txt") in
      let formatter = Format.formatter_of_out_channel oc in
      Serialiser.serialise_to_channel formatter file1;
      close_out oc
