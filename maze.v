(* Maze generator in Coq
 * Joe Wingbermuehle
 * 2012-11-14
 *)

Require Import Coq.Lists.List.
Open Scope list_scope.

Require Import Coq.ZArith.BinIntDef.
Require Import Coq.ZArith.BinInt.
Open Scope Z_scope.

(* Declare a cell as either Empty or a Wall. *)
Inductive cell: Type := | E | W.

(* Set the ith item in a list of cells. *)
Fixpoint row_set (l: list cell) (i: nat) (v: cell): list cell :=
   match i, l with
   | _, nil             => nil
   | O, cons _ l'       => cons v l'
   | S i', cons o l'    => cons o (row_set l' i' v)
   end.

(* Get the ith item in a list of cells. *)
Fixpoint row_get (l: list cell) (i: nat): cell :=
   match i, l with
   | _, nil             => W
   | O, cons v _        => v
   | S i', cons _ l'    => row_get l' i'
   end.

(* Initialize a list of cells n long to be walls. *)
Fixpoint row_init (n: nat): list cell :=
   match n with
   | O      => nil
   | S n'   => cons W (row_init n')
   end.

(* Get the size of a row in the maze. *)
Definition row_size (l: list cell): nat := length l.

(* Declare a maze as being a list of lists of cells. *)
Definition maze: Type := list (list cell).

(* Initialize an empty maze that is x by y. *)
Fixpoint maze_init (x y: nat): maze :=
   match y with
   | O      => nil
   | S y'   => cons (row_init x) (maze_init x y')
   end.

(* Get a cell of a maze. *)
Fixpoint maze_get (m: maze) (x y: nat): cell :=
   match y, m with
   | _, nil          => W
   | O, cons r _     => row_get r x
   | S y', cons _ m' => maze_get m' x y'
   end.

(* Set a cell of a maze. *)
Fixpoint maze_set (m: maze) (x y: nat) (v: cell): maze :=
   match y, m with
   | _, nil          => nil
   | O, cons r m'    => cons (row_set r x v) m'
   | S y', cons r m' => cons r (maze_set m' x y' v)
   end.

(* Determine the height of a maze. *)
Fixpoint maze_height (m: maze): nat :=
   match m with
   | nil          => O
   | cons _ m'    => S (maze_height m')
   end.

(* Determine the width of a maze. *)
Definition maze_width (m: maze): nat :=
   match m with
   | nil          => O
   | cons r _     => row_size r
   end.

(* Generate a random number. *)
Definition rand (seed: Z): Z :=
   let a := 7 in
   let m := 524287 in
   let q := m / a in
   let r := m mod a in
   let g := a * (seed mod q) - r * (seed / q) in
   let h := (seed / q) - a * (seed / m) in
   g + m * h.

(* Update a coordinate to move in direction d. *)
Definition update (d x y: Z) :=
   match (d mod 4) with
   | 0   => (x, y - 1)
   | 1   => (x, y + 1)
   | 2   => (x - 1, y)
   | _   => (x + 1, y)
   end.

(* Determine if we can move in the specified direction. *)
Definition can_move (m: maze) (d x y: Z): bool :=
   let (x1, y1) := update d x y in
   let (x2, y2) := update d x1 y1 in
   let w := Z_of_nat (maze_width m) in
   let h := Z_of_nat (maze_height m) in
   match (x2 >? 0), (x2 <? w), (y2 >? 0), (y2 <? h) with
   | true, true, true, true   =>
      let (nx, ny) := (Zabs_nat x, Zabs_nat y) in
      let (nx1, ny1) := (Zabs_nat x1, Zabs_nat y1) in
      let (nx2, ny2) := (Zabs_nat x2, Zabs_nat y2) in
      match (maze_get m nx ny), (maze_get m nx1 ny1), (maze_get m nx2 ny2) with
      | E, W, W  => true
      | _, _, _            => false
      end
   | _, _, _, _ => false
   end.

(* Carve maze m.
 * t is a counter used to show progress (it should always be non-zero).
 * c is a counter used to try all directions.
 * d is the current direction (random number).
 * x and y are the starting coordinates.
 *)
Fixpoint carve (m: maze) (t c: nat) (d x y: Z): maze :=
   match t with
   | O => m
   | S t' =>
      match c with
      | O => m
      | S c' =>
         match (can_move m d x y) with
         | true =>
            let (x1, y1) := update d x y in
            let (nx1, ny1) := (Zabs_nat x1, Zabs_nat y1) in
            let m1 := maze_set m nx1 ny1 E in
            let (x2, y2) := update d x1 y1 in
            let (nx2, ny2) := (Zabs_nat x2, Zabs_nat y2) in
            let m2 := maze_set m1 nx2 ny2 E in
            let m3 := carve m2 t' 4 (rand d) x2 y2 in
            carve m3 t' c' d x y
         | false => carve m t' c' (d + 1) x y
         end
      end
   end.

(* Generate a maze. *)
Definition generate_maze (w h: nat): maze :=
   let seed := 12 in
   let m := maze_init w h in
   let m1 := maze_set m 1 1 E in
   let m2 := carve m1 (4 * w * h) 4 seed 1 1 in
   let m3 := maze_set m2 1 0 E in
   let m4 := maze_set m3 (w - 2) (h - 1) E in
   m4.

Eval compute in (generate_maze 11 11).

