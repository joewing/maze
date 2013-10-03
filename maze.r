# Maze generator in R
# Joe Wingbermuehle 2013-10-03

# Function to create an empty maze array of width by height.
init_maze <- function(width, height) {
   m <- rep(0, times = width * height)
   dim(m) <- c(width, height)
   m[1, 1:height] <- 1
   m[width, 1:height] <- 1
   m[1:width, 1] <- 1
   m[1:width, height] <- 1
   m
}

# Display a maze.
show_maze <- function(m) {
   for (y in 1:dim(m)[2]) {
      str <- ""
      for (x in 1:dim(m)[1]) {
         if(m[x, y] == 1) {
            str <- paste(str, "[]", sep = '')
         } else {
            str <- paste(str, "  ", sep = '')
         }
      }
      print(str)
   }
}

# Carve a maze starting at x, y.
carve_maze <- function(m, x, y) {
   m[x, y] <- 1
   d <- floor(3 * runif(1))
   xdirs <- c(1, -1, 0, 0)
   ydirs <- c(0, 0, 1, -1)
   for (i in 0:3) {
      j <- 1 + (i + d) %% 4
      dx <- xdirs[j]
      dy <- ydirs[j]
      nx1 <- x + dx
      nx2 <- nx1 + dx
      ny1 <- y + dy
      ny2 <- ny1 + dy
      if (m[nx1, ny1] == 0 && m[nx2, ny2] == 0) {
         m[nx1, ny1] <- 1
         m <- carve_maze(m, nx2, ny2)
      }
   }
   m
}

# Generate and display a random maze.
# Note that the width and height of the maze must be odd.
m <- init_maze(37, 23)
m <- carve_maze(m, 3, 3)
show_maze(m)

