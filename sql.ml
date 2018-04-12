open Grid

let nth n = String.make 1 (Char.chr (65 + n))
let dict l = "d" ^ string_of_int l
let word id = "w" ^ string_of_int id
let field ~id i = word id ^ "." ^ nth i

let of_char c =
  (* FIXME prevent SQL injections *)
  Printf.sprintf "'%c'" c

let cond_of_t {id; s; inter; _} =
  (* w1.a='X' *)
  let rec f acc i =
    if i = String.length s then
      acc
    else if s.[i] = '.' then
      (* unknown *)
      f acc (i + 1)
    else
      (* fixed *)
      let c = field ~id i ^ " = " ^ of_char s.[i] in
      f (c :: acc) (i + 1)
  in
  (* w1.c=w9.a *)
  let j =
    inter |>
    List.filter (fun (p, _, p') -> p <= p') |> (* avoid duplicates *)
    List.map @@ fun (p, other, p') ->
      field ~id p ^ " = " ^ field ~id:other.id p'
  in
  f [] 0 @ j

let cc = String.concat ", "

let of_grid l =
  let f (sel, f, w) (Wrap ({id; s; _} as t)) =
    let rec fields acc i =
      if i = String.length s then
        List.rev acc (* user-readable output... *)
      else
        fields (field ~id i :: acc) (i + 1)
    in
    let f' = dict (String.length s) ^ " " ^ word id in
    sel @ fields [] 0, f' :: f, w @ cond_of_t t
  in
  let select, from, where = List.fold_left f ([], [], []) l in
  let where =
    match where with
    | [] -> ""
    | _ -> " WHERE " ^ String.concat " AND " where
  in
  "SELECT " ^ cc select ^ " FROM " ^ cc from ^ where

let insert_word w =
  let l = String.length w in
  let rec f (n, v) i =
    if i = l then
      (* we could actually reverse everything, but... *)
      List.rev n, List.rev v
    else
      let n' = nth i :: n in
      let v' = of_char w.[i] :: v in
      f (n', v') (i + 1)
  in
  let n, v = f ([], []) 0 in
  "INSERT INTO " ^ dict l ^ "(" ^ cc n ^ ") VALUES(" ^ cc v ^ ");"

let create_dictionary l =
  let rec f (n, k) i =
    if i = l then
      (* we could actually reverse everything, but... *)
      List.rev n, k
    else
      let k' = "xxx" :: k in (* FIXME read the docs! *)
      f ((nth i ^ " CHAR NOT NULL") :: n, k') (i + 1)
  in
  let n, k = f ([], []) 0 in
  "CREATE TABLE " ^ dict l ^ "(" ^ cc n ^ ") " ^ String.concat " " k

let import_words l =
  (* create tables *)
  List.map String.length l |>
  List.sort_uniq compare |>
  List.map create_dictionary |>
  List.iter print_endline;
  (* insert words *)
  List.map insert_word l |>
  List.iter print_endline
