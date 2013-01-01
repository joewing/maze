(* Maze generator in O'Caml by Joe Wingbermuehle
 * 20070605
 *)

type maze_element
   = Space
   | Wall
   | Border
;;

type maze = Maze of (int * int * maze_element array array)
;;

(* Display a maze. *)
let show_maze m =
   let show_element e =
      match e with
        Space  -> "  "
      | _      -> "[]" in
   let Maze (width, height, a) = m in
   for y = 0 to height - 1 do
      for x = 0 to width - 1 do
         print_string (show_element a.(x).(y))
      done;
      print_newline ();
   done
;;

(* Initialize a maze to be carved. *)
let initialize_maze width height =
   let a = Array.make_matrix width height Wall in
   for y = 0 to height - 1 do
      a.(0).(y) <- Border;
      a.(width - 1).(y) <- Border
   done;
   for x = 0 to width - 1 do
      a.(x).(0) <- Border;
      a.(x).(height - 1) <- Border
   done;
   a.(1).(1) <- Space;
   Maze (width, height, a)
;;

(* Carve starting at x, y. *)
let rec do_carve_maze m x y =
   let Maze (width, height, a) = m in
   let move_x x1 y1 x2 y2 =
      if a.(x1).(y1) == Wall && a.(x2).(y2) == Wall then begin
         a.(x1).(y1) <- Space;
         a.(x2).(y2) <- Space;
         do_carve_maze m x2 y2;
         true
      end else
         false
   in
   let move dir x y =
      match dir with
        0 -> move_x x (y - 1) x (y - 2)
      | 1 -> move_x x (y + 1) x (y + 2)
      | 2 -> move_x (x - 1) y (x - 2) y
      | _ -> move_x (x + 1) y (x + 2) y
   in
   let rec try_move dir count x y =
      if count < 4 then
         let status = move dir x y in
         if not status then
            let next_dir = (dir + 1) mod 4 in
            try_move next_dir (count + 1) x y
         else ()
      else ()
   in
   if a.(x).(y) == Space then begin
      a.(x).(y) <- Space;
      let dir = Random.int 4 in
      try_move dir 0 x y
   end else ()
;;

(* Carve an initialized maze. *)
let carve_maze m =
   let Maze (width, height, a) = m in
   for y = 0 to (height - 2) / 2 do
      for x = 0 to (width - 2) / 2 do
         do_carve_maze m (x * 2 + 1) (y * 2 + 1)
      done
   done;
   a.(0).(1) <- Space;
   a.(width - 1).(height - 2) <- Space
;;

(* Entry point for the maze generator. *)
let main () =
   Random.self_init ();
   let m = initialize_maze 39 23 in
   carve_maze m;
   show_maze m
;;

main () ;;

