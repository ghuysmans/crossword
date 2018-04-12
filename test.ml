let () =
(*
  Util.read_lines () |>
  Sql.import_words

let () =
*)
  Grid.parse () |>
  Sql.of_grid |>
  print_endline
