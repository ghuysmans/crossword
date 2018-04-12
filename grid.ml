type ('ori, 'opp) o =
  | Horizontal: ([`Hor], [`Ver]) o
  | Vertical: ([`Ver], [`Hor]) o

let string_of_o: type a b. (a, b) o -> string = function
  | Horizontal -> "H"
  | Vertical -> "V"

type ('ori, 'opp) t = {
  id: int;
  i: int;
  j: int;
  mutable s: string;
  orientation: ('ori, 'opp) o;
  mutable inter: (int * ('opp, 'ori) t * int) list
}

let dump {id; s; i; j; orientation; inter} =
  let o = string_of_o orientation in
  Printf.printf "#%d\t(%d,%d)\t%s\t*%s\n" id i j o s;
  inter |> List.iter (fun (p, x, p') ->
    Printf.printf "\t%d->%d\t#%d\t*%s\n" p p' x.id x.s
  );
  print_newline ()

type w = Wrap: _ t -> w (* allows mixing Hor and Ver sites *)

let parse () =
  let id = ref 0 in (* a sequence number... *)
  let sites = ref [] in (* the result *)
  let l = read_line () in
  let width = String.length l in (* grid width (used by right-padding) *)
  let cur_ver = Array.make (width + 1) None in (* current vertical sites *)
  let rec f l i =
    let l' =
      if l = "" then
        "" (* read EOF or an empty line just once *)
      else
        try
          read_line ()
        with End_of_file ->
          ""
    in
    let rec g cur_hor j =
      if j = width + 1 then
        (* this last iteration flushed the last horizontal site *)
        ()
      else
        let c =
          (* we use a string because we'll concatenate it... *)
          if j < String.length l then
            String.sub l j 1
          else
            " "
        in
        if c = " " then (
          (* terminate any current vertical site *)
          sites :=
            (match cur_ver.(j) with
            | None -> !sites
            | Some x ->
              cur_ver.(j) <- None;
              (* x.len <- i - x.start; *)
              Wrap x :: !sites);
          (* terminate any current Horizontal site *)
          (match cur_hor with
          | None -> ()
          | Some x ->
            (* x.len <- j - x.start; *)
            sites := Wrap x :: !sites);
          (* process the next column *)
          g None (j + 1)
        )
        else
          let make i j s orientation =
            let ret = {id = !id; i; j; s; orientation; inter = []} in
            incr id; (* so, id is always a fresh sequence number *)
            ret
          in
          let add_intersection h v =
            (* add an edge to the undirected graph... *)
            let hp, vp = j - h.j, i - v.i in (* relative to the start... *)
            h.inter <- (hp, v, vp) :: h.inter;
            v.inter <- (vp, h, hp) :: v.inter
          in
          let ch' =
            let p = if j > 0 && j-1 < String.length l then l.[j-1] else ' ' in
            match cur_hor with
            | None when p <> ' ' ->
              let h = make i (j - 1) (String.sub l (j - 1) 2) Horizontal in
              (* manually handle the intersection since it's at j-1... *)
              (match cur_ver.(j - 1) with
              | None -> ()
              | Some v -> add_intersection h v);
              Some h
            | Some x ->
              (* concatenate the current character *)
              x.s <- x.s ^ c;
              cur_hor
            | v -> v
          in
          let c' = if j < String.length l' then l'.[j] else ' ' in
          cur_ver.(j) <-
            (match cur_ver.(j) with
            | None when c' <> ' ' ->
              Some (make i j c Vertical)
            | (Some x) as v ->
              (* concatenate the current character *)
              x.s <- x.s ^ c;
              v
            | v -> v);
          (* check for intersection *)
          (match ch', cur_ver.(j) with
          | Some h, Some v -> add_intersection h v
          | _ -> ());
          (* process the next column *)
          g ch' (j + 1)
    in
    (* process each column *)
    g None 0;
    (* continue or stop *)
    if l = "" then
      (* this last iteration flushed vertical sites *)
      ()
    else
      (* the current line is the new previous one *)
      f l' (i + 1)
  in
  (* process each row *)
  f l 0;
  (* don't forget the result... *)
  !sites
