// Maze generator in Go
// Joe Wingbermuehle
// 2012-08-07

package main

import (
   "fmt"
   "math/rand"
   "time"
)

const (
   WALL     = 0
   SPACE    = 1
)

type Maze struct {
   width, height int
   data [][]byte
}

/** Create an empty maze.
 * @param w The width (must be odd).
 * @param h The height (must be odd).
 */
func NewMaze(w int, h int) *Maze {
   m := Maze { w, h, make([][]byte, h) }
   for y := range m.data {
      m.data[y] = make([]byte, w)
      for x := range m.data[y] {
         m.data[y][x] = WALL
      }
   }
   for x := 0; x < w; x++ {
      m.data[0][x], m.data[h - 1][x] = SPACE, SPACE
   }
   for y := 0; y < h; y++ {
      m.data[y][0], m.data[y][w - 1] = SPACE, SPACE
   }
   return &m
}

/** Start carving a maze at the specified coordinates. */
func CarveMaze(m *Maze, r *rand.Rand, x int, y int) {
   directions := [][]int { {1, 0}, {-1, 0}, {0, 1}, {0, -1} }
   d := r.Intn(4)
   for i := 0; i < 4; i++ {
      dx, dy := directions[d][0], directions[d][1]
      ax, ay := x + dx, y + dy
      bx, by := ax + dx, ay + dy
      if m.data[ay][ax] == WALL && m.data[by][bx] == WALL {
         m.data[ay][ax], m.data[by][bx] = SPACE, SPACE
         CarveMaze(m, r, bx, by) 
      }
      d = (d + 1) % 4
   }
}

/** Generate a maze. */
func GenerateMaze(m *Maze) {
   r := rand.New(rand.NewSource(time.Now().Unix()))
   m.data[2][2] = SPACE
   CarveMaze(m, r, 2, 2)
   m.data[1][2] = SPACE
   m.data[m.height - 2][m.width - 3] = SPACE
}

/** Show a generated maze. */
func ShowMaze(m *Maze) {
   for y := 0; y < m.height; y++ {
      for x := 0; x < m.width; x++ {
         if m.data[y][x] == WALL {
            fmt.Printf("[]")
         } else {
            fmt.Printf("  ")
         }
      }
      fmt.Printf("\n")
   }
}

func main() {
   m := NewMaze(39, 23)
   GenerateMaze(m)
   ShowMaze(m)
}

