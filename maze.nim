# Maze generator in Nimrod
# Joe Wingbermuehle 2013-10-01

import math

# Width and height must be odd.
const width = 39
const height = 23

type MazeT = array[0 .. height - 1, array[0 .. width - 1, int]]

proc showMaze(maze: MazeT) =
   for y in countup(0, height - 1):
      for x in countup(0, width - 1):
         if maze[y][x] == 1:
            write(stdout, "[]")
         else:
            write(stdout, "  ")
      write(stdout, "\n")

proc initMaze(maze: ref MazeT) =
   for y in countup(0, height - 1):
      for x in countup(0, width - 1):
         maze[y][x] = 1
   for x in countup(0, width - 1):
      maze[0][x] = 0
      maze[height - 1][x] = 0
   for y in countup(0, height - 1):
      maze[y][0] = 0
      maze[y][width - 1] = 0

proc carveMaze(maze: ref MazeT, x, y: int) =
   maze[y][x] = 0
   let d = math.random(4)
   for i in countup(0, 3):
      var dx, dy: int
      case (d + i) mod 4
      of 0: dx = 1
      of 1: dx = -1
      of 2: dy = 1
      else: dy = -1
      let
         nx = x + dx
         ny = y + dy
         nx2 = x + 2 * dx
         ny2 = y + 2 * dy
      if maze[ny][nx] == 1 and maze[ny2][nx2] == 1:
         maze[ny][nx] = 0
         carveMaze(maze, nx2, ny2)

proc generateMaze(): MazeT =
   var Presult: ref MazeT
   new(Presult)
   initMaze(Presult)
   carveMaze(Presult, 2, 2)
   Presult[1][2] = 0
   Presult[height - 2][width - 3] = 0
   return Presult[]

showMaze(generateMaze())
