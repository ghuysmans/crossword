let import () =
  Util.read_until_empty () |>
  Sql.import_words

let compile bare =
  if not bare then import ();
  let t = Grid.parse () in
  t |> List.iter (fun (Grid.Wrap {Grid.id; s; i; j; orientation; _}) ->
    let o = Grid.string_of_o orientation in
    Sql.print_endline (Printf.sprintf "#%d\\t(%d,%d)\\t%s\\t*%s" id i j o s
  ));
  print_endline (Sql.of_grid t)


open Cmdliner

let import_cmd =
  let doc = "import a dictionary" in
  Term.(const import $ const ()), Term.info "import" ~doc

let compile_cmd =
  let doc = "compile a crossword to SQL" in
  let bare =
    let doc = "the input is a single crossword" in
    Arg.(value & flag & info ~doc ["b"; "bare"])
  in
  Term.(const compile $ bare), Term.info "sql" ~doc


let () =
  let crossword =
    let doc = "a crossword solver" in
    Term.(ret @@ const @@ `Help (`Pager, None)),
    Term.info "crossword" ~doc
  in
  Term.(exit @@ eval_choice crossword [
    import_cmd;
    compile_cmd
  ])
