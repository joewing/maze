#!/usr/bin/tclsh
#
# Maze generator in TCL
# Joe Wingbermuehle
# 2010-04-11
#

# Size of the maze (must be odd)
set width   39
set height  23

# Show the maze.
proc show_maze {} {
   global maze width height
   set result {}
   for {set y 0} { $y < $height } { incr y } {
      for {set x 0} { $x < $width } { incr x } {
         if { $maze($x,$y) > 0 } {
            set result "$result\[]"
         } else {
            set result "$result  "
         }
      }
      set result "$result\n"
   }
   puts "$result"
}

# Initialize an empty maze.
proc init_maze {} {
   global maze width height
   for {set y 0} { $y < $height } { incr y } {
      for {set x 0} { $x < $width } { incr x } {
         set maze($x,$y) 1
      }
      set maze(0,$y) 2
      set maze([expr $width - 1],$y) 2
   }
   for {set x 0} { $x < $width } { incr x } {
      set maze($x,0) 2
      set maze($x,[expr $height - 1]) 2
   }
   set maze(1,1) 0
}

# Carve starting at x,y.
proc carve_maze { x y } {
   global maze
   if { $maze($x,$y) == 0 } {
      set dir [expr int(rand() * 4)]
      for {set tries 0} { $tries < 5 } { incr tries } {
         set deltax 0
         set deltay 0
         case $dir {
            0 { set deltax 1  }
            1 { set deltax -1 }
            2 { set deltay 1  }
            3 { set deltay -1 }
         }
         set x1 [expr $x + $deltax]
         set x2 [expr $x1 + $deltax]
         set y1 [expr $y + $deltay]
         set y2 [expr $y1 + $deltay]
         if { $maze($x1,$y1) == 1 && $maze($x2,$y2) == 1 } {
            set maze($x1,$y1) 0
            set maze($x2,$y2) 0
            set tries 0
            set dir [expr int(rand() * 4)]
            set x $x2
            set y $y2
         } else {
            set dir [expr ($dir + 1) % 4]
         }
      }
   }
}

# Generate a maze.
proc generate_maze {} {
   global maze width height
   init_maze
   for {set y 1} { $y < $height } { set y [expr $y + 2] } {
      for {set x 1} { $x < $width } { set x [expr $x + 2] } {
         carve_maze $x $y
      }
   }
   set maze(0,1) 0
   set maze([expr $width - 1],[expr $height - 2]) 0
}

generate_maze
show_maze

