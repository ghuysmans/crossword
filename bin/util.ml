let read_until_empty () =
  let rec f acc =
    try
      let l = read_line () in
      if l = "" then
        acc
      else
        f (l :: acc)
    with End_of_file ->
      acc
  in
  List.rev (f [])
