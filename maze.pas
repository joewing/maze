(* Maze generator in Pascal.
 * Joe Wingbermuehle
 * 2010-09-29
 *)

program maze;
const

   (* Size of the maze, must be odd. *)
   width    = 39;
   height   = 23;

type

   CellType = (Space, Wall);
   MazeType = array [0 .. width - 1, 0 .. height - 1] of CellType;

var

   maze    : MazeType;

   (* Initialize the maze matrix. *)
   procedure init_maze;
   var
      x, y  : integer;
   begin
      for y := 0 to height - 1 do
         for x := 0 to width - 1 do
            maze[x, y] := Wall;
   end;

   (* Carve the maze starting at x, y. *)
   procedure carve_maze(x, y : integer);
   var
      x1, y1     : integer;
      x2, y2     : integer;
      dx, dy     : integer;
      dir, cnt   : integer;
   begin
      dir := random(4);
      cnt := 0;
      while cnt < 4 do
      begin
         dx := 0;
         dy := 0;
         case dir of
            0:    dx := 1;
            1:    dy := 1;
            2:    dx := -1;
            else  dy := -1;
         end;
         x1 := x + dx;
         y1 := y + dy;
         x2 := x1 + dx;
         y2 := y1 + dy;
         if (x2 > 0) and (x2 < width) and (y2 > 0) and (y2 < height) 
            and (maze[x1, y1] = Wall) and (maze[x2, y2] = Wall) then
         begin
            maze[x1, y1] := Space;
            maze[x2, y2] := Space;
            carve_maze(x2, y2);
         end;
         dir := (dir + 1) mod 4;
         cnt := cnt + 1;
      end;
   end;

   (* Generate the maze. *)
   procedure generate_maze;
   begin
      maze[1, 1] := Space;
      carve_maze(1, 1);
      maze[1, 0] := Space;
      maze[width - 2, height - 1] := Space;
   end;

   (* Show the maze. *)
   procedure show_maze;
   var
      x, y  : integer;
   begin
      for y := 0 to height - 1 do
      begin
         for x := 0 to width - 1 do
            if maze[x, y] = Space then
               write('  ')
            else
               write('[]');
         writeln('');
      end;
   end;

begin

   randomize;

   init_maze;
   generate_maze;
   show_maze;

end.

