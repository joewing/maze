--------------------------------------------------------------------------
-- Maze 19991109 <> 20020509 by Joe Wingbermuehle
-- This is a random maze generator
--------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line;

procedure Maze is

   Block    : constant String := "[]";       -- Block for maze
   Space    : constant String := "  ";       -- Blank space for maze
   Release  : constant String := "20020509"; -- Release date

   subtype Random_Range is Integer range 0 .. 3;
   package Random4 is new Ada.Numerics.Discrete_Random(Random_Range);

   type Array_Type is array(Positive range <>,
                            Positive range <>) of Integer range 0 .. 1;
   type Array_Pointer is access Array_Type;

   maze     : Array_Pointer;                    -- The maze array
   seed     : Random4.Generator;                -- Random number seed
   width    : Integer range 1 .. Integer'last;  -- Maze width
   height   : Integer range 1 .. Integer'last;  -- Maze height

   ------------------------------------------------------------------
   -- Display the maze
   ------------------------------------------------------------------
   procedure Display_Maze is
   begin
      for y in 1 .. height loop
         for x in 1 .. width loop
            if maze(x, y) = 1 then
               Put(Block);
            else
               Put(Space);
            end if;
         end loop;
         New_Line;
      end loop;   
   end Display_Maze;

   ------------------------------------------------------------------
   -- Initialize the maze array
   ------------------------------------------------------------------
   procedure Initialize_Board is
   begin
      for y in 1 .. height loop
         for x in 1 .. width loop
            maze(x, y) := 1;
         end loop;
      end loop;
   end Initialize_Board;

   ------------------------------------------------------------------
   -- Carve out a section of the maze
   ------------------------------------------------------------------
   procedure Carve_Maze(x : in Integer; y : in Integer) is

      d        : Integer range 0 .. 3; -- Direction
      c        : Integer range 0 .. 4; -- Number of directions tried
      dx, dy   : Integer;
      x1, y1   : Integer;
      x2, y2   : Integer;

   begin
      d := Random4.Random(seed);
      c := 0;
      while c < 4 loop
         dx := 0; dy := 0;
         case d is
            when 0 => dx := 1;
            when 1 => dy := 1;
            when 2 => dx := -1;
            when 3 => dy := -1;
         end case;
         x1 := x  + dx; y1 := y  + dy;
         x2 := x1 + dx; y2 := y1 + dy;
         if x2 > 1 and x2 < width and y2 > 1 and y2 < height then
            if maze(x1, y1) = 1 and maze(x2, y2) = 1 then
               maze(x1, y1) := 0;
               maze(x2, y2) := 0;
               Carve_Maze(x2, y2);
            end if;
         end if;
         c := c + 1;
         d := (d + 1) mod 4;
      end loop;
   end Carve_Maze;

   ------------------------------------------------------------------
   -- Generate the maze
   ------------------------------------------------------------------
   procedure Generate_Maze is
   begin
      Initialize_Board;
      maze(2, 2) := 0;
      Carve_Maze(2, 2);
      maze(2, 1) := 0;
      maze(width - 1, height) := 0;
   end Generate_Maze;

begin

   -- Display the header
   Put("Maze " & Release & " by Joe Wingbermuehle");
   New_Line;

   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line("usage: " & Ada.Command_Line.Command_Name
               & " <width> <height>");
      return;
   end if;

   -- Read the size from the command line
   begin
      width  := Integer'value(Ada.Command_Line.Argument(1));
      height := Integer'value(Ada.Command_Line.Argument(2));
   exception
      when others =>
         Put_Line("Invalid size");
         return;
   end;

   -- Create the maze array
   height := height * 2 + 3;
   width  := width * 2 + 3;
   maze   := new Array_Type(1 .. width, 1 .. height);

   -- Generate the maze
   Random4.Reset(seed, Integer(Seconds(Clock)));
   Generate_Maze;
   Display_Maze;

end Maze;

