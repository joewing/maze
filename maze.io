/* Maze generator in io
 * Joe Wingbermuehle
 * 2010-11-10
 */

// The maze object.
maze := Object clone

// Method to display the maze.
maze print := method(
   for(y, 0, height - 1,
      for(x, 0, width - 1,
         if((array at(y * width + x)) == 1, "[]", "  ") print
      )
      "" println
   )
   self
)

// Method to create an empty maze of w x h.
maze init := method(w, h,
   self width := w
   self height := h
   self rand := Random clone
   self array := Sequence clone setSize(width * height)
   for(x, 0, width * height, array atPut(x, 1))
   self
)

// Start carving the maze at x, y.
maze carve := method(x, y,
   d := rand value(4) floor
   c := 0
   while(c < 4,
      dx := 0
      dy := 0
      d switch(0, dx = 1,
               1, dy = 1,
               2, dx = -1,
               3, dy = -1)
      x1 := x + dx
      y1 := y + dy
      x2 := x1 + dx
      y2 := y1 + dy
      if(x2 > 0 and x2 < width and y2 > 0 and y2 < height,
         if(array at(y1 * width + x1) == 1,
            if(array at(y2 * width + x2) == 1,
               array atPut(y1 * width + x1, 0)
               array atPut(y2 * width + x2, 0)
               carve(x2, y2)
            )
         )
      )
      c = c + 1
      d = (d + 1) % 4
   )
)

// Generate a maze.
maze generate := method(
   array atPut(1 * width + 1, 0)
   carve(1, 1)
   array atPut(0 * width + 1, 0)
   array atPut((height - 1) * width + (width - 2), 0)
   self
)

// Generate and display a random maze.
maze init(39, 23) generate print

