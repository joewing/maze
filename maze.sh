#!/bin/bash
# Maze generator in bash.
# Joe Wingbermuehle
# 2013-04-21

# The size of the maze (must be odd).
MAZE_WIDTH=39
MAZE_HEIGHT=21

# Initialize the maze array.
function init_maze {
   for ((y=0; y<MAZE_HEIGHT; y++)) ; do
      for ((x=1; x<$((MAZE_WIDTH-1)); x++)) ; do
         maze[$((y * MAZE_WIDTH + x))]=0
      done
      maze[$((y * MAZE_WIDTH + 0))]=1
      maze[$((y * MAZE_WIDTH + (MAZE_WIDTH - 1)))]=1
   done
   for ((x=0; x<MAZE_WIDTH; x++)) ; do
      maze[$x]=1
      maze[$(((MAZE_HEIGHT - 1) * MAZE_WIDTH + x))]=1
   done
}

# Display the maze array.
function print_maze {
   for ((y=0; y<MAZE_HEIGHT; y++)) ; do
      for ((x = 0; x < MAZE_WIDTH; x++ )) ; do
         if [[ maze[$((y * MAZE_WIDTH + x))] -eq 0 ]] ; then
            echo -n "[]"
         else
            echo -n "  "
         fi
      done
      echo
   done
}

# Carve the maze starting at the specified offset.
function carve_maze {
   local index=$1
   local dir=$RANDOM
   local i=0
   maze[$index]=1
   while [ $i -le 4 ] ; do
      local offset=0
      case $((dir % 4)) in
         0) offset=1 ;;
         1) offset=-1 ;;
         2) offset=$MAZE_WIDTH ;;
         3) offset=$((-$MAZE_WIDTH)) ;;
      esac
      local index2=$((index + offset))
      if [[ maze[$index2] -eq 0 ]] ; then
         local nindex=$((index2 + offset))
         if [[ maze[$nindex] -eq 0 ]] ; then
            maze[$index2]=1
            carve_maze $nindex
            i=0
            dir=$RANDOM
            index=$nindex
         fi
      fi
      i=$((i + 1))
      dir=$((dir + 1))
   done
}

# Generate and display a maze.
init_maze
carve_maze $((2 * MAZE_WIDTH + 2))
maze[$((MAZE_WIDTH + 2))]=1
maze[$(((MAZE_HEIGHT - 2) * MAZE_WIDTH + MAZE_WIDTH - 3))]=1
print_maze

