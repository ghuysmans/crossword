(** An orientation's type contains both itself and its opposite, so we can just
    swap them to get the opposite orientation's type. *)
type ('ori, 'opp) o =
  | Horizontal: ([`Hor], [`Ver]) o
  | Vertical: ([`Ver], [`Hor]) o

val string_of_o: _ o -> string

(** A site is parametrized by its orientation, so that the compiler ensures
    that intersections only exist between perpendicular sites. *)
type ('ori, 'opp) t = private {
  id: int;
  i: int; (* row, 0-based *)
  j: int; (* column, 0-based *)
  mutable s: string;
  orientation: ('ori, 'opp) o;
  mutable inter: (int * ('opp, 'ori) t * int) list
}

(** Useful for debugging. *)
val dump: _ t -> unit

(** Parse stdin until EOF or an empty line. *)
type w = Wrap: _ t -> w
val parse: unit -> w list
