(* Maze generator in SML
 * Joe Wingbermuehle
 * 2010-10-09
 *)

(* Signature of the maze generator. *)
signature MAZE =
sig
   type t
   val show       : t -> unit
   val generate   : t -> unit
   val initialize : int * int -> t
end

(* Structure used for generating mazes. *)
structure Maze :> MAZE =
struct

   (* The maze type. *)
   type t = int * int * int Array.array

   (* Get the array offset from the x and y coordinates. *)
   fun index(m, x, y) =
      let
         val (width, _, _) = m
      in
         y * width + x
      end

   (* Display a maze. *)
   fun show m =
      let
         val (width, height, a) = m
         fun display 0 = print "  "
           | display _ = print "[]"
         fun show_xy(x, y) =
            if x < width then (
               display(Array.sub(a, index(m, x, y)));
               show_xy(x + 1, y)
            ) else (
               print "\n";
               if y + 1 < height then
                  show_xy(0, y + 1)
               else
                  ()
            )
      in
         show_xy(0, 0)
      end

   (* Start carving a maze at x, y. *)
   fun carve(r, m, x, y) =
      let
         val (width, height, a) = m
         fun get_update 0 = (1, 0)
           | get_update 1 = (0, 1)
           | get_update 2 = (0-1, 0)
           | get_update _ = (0, 0-1)
         fun move(dir, cnt) =
            let
               val (dx, dy) = get_update(dir)
               val (x1, y1) = (x + dx, y + dy)
               val (x2, y2) = (x1 + dx, y1 + dy)
               val offset1 = index(m, x1, y1)
               val offset2 = index(m, x2, y2)
            in
               if x2 > 0 andalso x2 < width andalso y2 > 0 andalso y2 < height
                  andalso Array.sub(a, offset1) = 1
                  andalso Array.sub(a, offset2) = 1 then (
                  Array.update(a, offset1, 0);
                  Array.update(a, offset2, 0);
                  carve(r, m, x2, y2)
               ) else ();
               if cnt < 3 then move((dir + 1) mod 4, cnt + 1) else ()
            end
         val dir = Random.randNat r mod 4
      in
         move(dir, 0)
      end

   (* Generate a random maze. *)
   fun generate m =
      let
         val (width, height, a) = m
         val date = Date.fromTimeUniv (Time.now ())
         val r = Random.rand(Date.minute date, Date.second date)
      in
         Array.update(a, index(m, 1, 1), 0);
         carve(r, m, 1, 1);
         Array.update(a, index(m, 1, 0), 0);
         Array.update(a, index(m, width - 2, height - 1), 0)
      end

   (* Initialize an empty maze. *)
   fun initialize(width, height) =
      (width, height, Array.array(width * height, 1))

end

(* Generate and dispaly a random maze. *)
val () =
   let
      val m = Maze.initialize(39, 23)
   in
      Maze.generate m;
      Maze.show m;
      OS.Process.exit 0
   end

