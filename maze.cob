       identification division.
       program-id. maze.
       author. Joe Wingbermuehle.
       date-written. 2010-09-26.

       environment division.

       data division.
       working-storage section.

       01  width        pic 99999 value 39.
       01  height       pic 99999 value 23.
       01  maze-array.
           05  maze-row occurs 39 times.
               10  maze-col occurs 23 times pic 9.

       01  seed         pic 9999999.
       01  x            pic 99999.
       01  y            pic 99999.
       01  lx           pic 99999.
       01  ly           pic 99999.
       01  x1           pic S99999.
       01  y1           pic S99999.
       01  x2           pic S99999.
       01  y2           pic S99999.
       01  dx           pic S9.
       01  dy           pic S9.
       01  dir          pic 9.
       01  cnt          pic 9.

       procedure division.

      * Generate and display a random maze.
       perform 100-initialize-maze
       perform 200-generate-maze
       perform 300-show-maze
       stop run.

      * Initialze the maze matrix.
       100-initialize-maze.
           perform varying y from 1 by 1 until y > height
              perform varying x from 1 by 1 until x > width
                 move 1 to maze-col(x, y)
              end-perform
           end-perform.

      * Generate a random maze.
       200-generate-maze.
           move function seconds-past-midnight to seed
           move function random(seed) to seed
           move 0 to maze-col(2, 2)
           perform varying y from 2 by 2 until y >= height
              perform varying x from 2 by 2 until x >= width
                  move x to lx
                  move y to ly
                  perform 400-carve-maze
              end-perform
           end-perform
           move 0 to maze-col(2, 1)
           move 0 to maze-col(width - 1, height).

      * Display the maze.
       300-show-maze.
           perform varying y from 1 by 1 until y > height
              perform varying x from 1 by 1 until x > width
                 if maze-col(x, y) = 0 then
                     display '  ' with no advancing
                 else
                     display '[]' with no advancing
                 end-if
              end-perform
              display ''
           end-perform.

      * Carve the maze starting at lx, ly
       400-carve-maze.
           compute dir = function random * 4.0
           move 0 to cnt
           perform until cnt > 3
               move 0 to dx
               move 0 to dy
               evaluate dir
                   when 0 move 1 to dx
                   when 1 move 1 to dy
                   when 2 move -1 to dx
                   when 3 move -1 to dy
               end-evaluate
               add lx to dx giving x1
               add ly to dy giving y1
               add x1 to dx giving x2
               add y1 to dy giving y2
               if      x2 > 1 and x2 < width
                   and y2 > 1 and y2 < height
                   and maze-col(x1, y1) = 1
                   and maze-col(x2, y2) = 1 then
                   move 0 to maze-col(x1, y1)
                   move 0 to maze-col(x2, y2)
                   move x2 to lx
                   move y2 to ly
                   compute dir = function random * 4.0
                   move 0 to cnt
               else
                   add 1 to cnt
                   add 1 to dir
                   if dir = 4 then move 0 to dir
               end-if
           end-perform.

