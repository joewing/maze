\ Maze generator in Forth
\ Joe Wingbermuehle
\ 2010-10-04

\ Size of the maze, must be odd.
39 constant width
23 constant height

variable rand
variable maze width height 1 - * allot

\ Generate the next pseudo random number.
: random ( -- u )
   rand @ 1061 * 3251 + dup rand ! 64 /
;

\ Display the maze.
: show-maze ( -- )
   height 0 do
      width 0 do
         maze j width * i + + c@ 0= if
            ."   "
         else
            ." []"
         then
      loop
      cr
   loop
;

\ Carve the maze starting at the specified coordinates.
: carve-maze ( x y -- )
   0 random 4 mod 0 0 0 0 0 0
   { x y count dir dx dy x1 y1 x2 y2 }
   begin
      0 to dx 0 to dy
      dir 0 = if  1 to dx else
      dir 1 = if  1 to dy else
      dir 2 = if -1 to dx else
                 -1 to dy
      then then then
      x  dx + to x1 y  dy + to y1
      x1 dx + to x2 y1 dy + to y2
      x2 0> x2 width < and y2 0> y2 height < and and if
         maze y1 width * x1 + + c@ 1 =
         maze y2 width * x2 + + c@ 1 = and if
            0 maze y1 width * x1 + + c!
            0 maze y2 width * x2 + + c!
            x2 y2 recurse
      then then
      count 1 + to count
      dir 1 + 4 mod to dir
   count 3 > until
;

\ Generate the maze.
: generate-maze ( -- )
   height 0 do
      width 0 do
         1 maze j width * i + + c!
      loop
   loop
   1 1 carve-maze
   0 maze 1 + c!
   0 maze height 1 - width * width 2 - + + c!
;

time&date + + + + + rand !
generate-maze
show-maze
bye

