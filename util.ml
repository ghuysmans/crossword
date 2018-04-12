let read_lines () =
  let rec f acc =
    try
      f (read_line () :: acc)
    with End_of_file ->
      acc
  in
  f []
