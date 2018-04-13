(** SELECT solutions to the given grid *)
val of_grid: Grid.w list -> string

(** Insert a word into the right dictionary table *)
val insert_word: string -> string

(** Create an indexed dictionary table *)
val create_dictionary: int -> string

(** Create and populate indexed dictionary tables *)
val import_words: string list -> unit

(** Issue a dumb SELECT to print something *)
val print_endline: string -> unit
