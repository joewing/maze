! Maze generator in Factor
! Joe Wingbermuehle 2013-11-15

USING: arrays io kernel locals math random sequences prettyprint ;
IN: maze

! Size of the maze (width and height must be odd).
CONSTANT: width 39
CONSTANT: height 23

! Initialize an empty maze array.
:: initialize-maze ( -- maze )
   width height * 0 <array> :> maze
   height iota [
      width * dup 1 swap maze set-nth
      1 swap width 1 - + maze set-nth
   ] each
   width iota [
      dup 1 swap maze set-nth
      1 swap height 1 - width * + maze set-nth
   ] each maze ;

! Get the direction deltas for the specified direction.
: get-dir ( dir -- dx dy )
   { { 1 0 } { -1 0 } { 0 1 } { 0 -1 } } nth
   dup 0 swap nth swap 1 swap nth ;

! Carve the maze starting at x, y.
:: carve-maze ( maze x y -- maze )
   1 y width * x + maze set-nth
   4 iota random :> d
   4 iota [
      d + 4 mod get-dir :> dx :> dy
      dx x + :> x1 dy y + :> y1
      dx x1 + :> x2 dy y1 + :> y2
      y1 width * x1 + maze nth 0 =
      y2 width * x2 + maze nth 0 = and
      [
         1 y1 width * x1 + maze set-nth
         maze x2 y2 carve-maze drop
      ] [ ] if
   ] each
   maze ;

! Display a maze.
:: display-maze ( maze -- )
   1 width 2 + maze set-nth
   1 height 2 - width * width 3 - + maze set-nth
   height iota [ :> y
      width iota [
         y width * + maze nth 1 =
         [ "  " ] [ "[]" ] if write
      ] each "" print
   ] each ;

! Generate and display a random maze.
: generate-maze ( -- ) initialize-maze 2 2 carve-maze display-maze ;

MAIN: generate-maze
