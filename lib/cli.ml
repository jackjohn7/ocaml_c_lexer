let read_file filename =
  let channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line channel in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let contents = read_lines [] in
  close_in channel;
  String.concat "\n" contents
