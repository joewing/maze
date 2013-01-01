#!/usr/bin/python
# Maze generator in Python
# Joe Wingbermuehle
# 2010-10-06

import sys
import random

# The size of the maze (must be odd).
width    = 39
height   = 23

# The maze.
maze     = dict()

# Display the maze.
def display_maze():
   for y in range(0, height):
      for x in range(0, width):
         if maze[x][y] == 0:
            sys.stdout.write("  ")
         else:
            sys.stdout.write("[]")
      sys.stdout.write("\n")

# Initialize the maze.
def init_maze():
   for x in range(0, width):
      maze[x] = dict()
      for y in range(0, height):
         maze[x][y] = 1

# Carve the maze starting at x, y.
def carve_maze(x, y):
   dir = random.randint(0, 3)
   count = 0
   while count < 4:
      dx = 0
      dy = 0
      if   dir == 0:
         dx = 1
      elif dir == 1:
         dy = 1
      elif dir == 2:
         dx = -1
      else:
         dy = -1
      x1 = x + dx
      y1 = y + dy
      x2 = x1 + dx
      y2 = y1 + dy
      if x2 > 0 and x2 < width and y2 > 0 and y2 < height:
         if maze[x1][y1] == 1 and maze[x2][y2] == 1:
            maze[x1][y1] = 0
            maze[x2][y2] = 0
            carve_maze(x2, y2)
      count = count + 1
      dir = (dir + 1) % 4

# Generate the maze.
def generate_maze():
   random.seed()
   maze[1][1] = 0
   carve_maze(1, 1)
   maze[1][0] = 0
   maze[width - 2][height - 1] = 0

# Generate and display a random maze.
init_maze()
generate_maze()
display_maze()

