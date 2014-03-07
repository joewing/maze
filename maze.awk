# Maze generator in AWK
# Joe Wingbermuehle 2014-03-06

# Create an empty maze array.
function init_maze(maze, width, height) {
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            if(x == 0 || y == 0) {
                maze[y * width + x] = 1
            } else if(x == width - 1 || y == height - 1) {
                maze[y * width + x] = 1
            } else {
                maze[y * width + x] = 0
            }
        }
    }
}

# Display the maze.
function show_maze(maze, width, height) {
    for(y = 0; y < height; y++) {
        line = ""
        for(x = 0; x < width; x++) {
            if(maze[y * width + x]) {
                printf "  "
            } else {
                printf "[]"
            }
        }
        printf "\n"
    }
}

# Carve the maze starting at x, y.
function carve_maze(maze, x, y, width, height) {
    dirx[0] = 1; dirx[1] = -1
    dirx[2] = 0; dirx[3] = 0
    diry[0] = 0; diry[1] = 0
    diry[2] = 1; diry[3] = -1
    maze[y * width + x] = 1
    d = int(rand() * 4)
    for(i = 0; i < 4; i++) {
        dx = dirx[d]; dy = diry[d]
        ax = x + dx; ay = y + dy
        bx = ax + dx; by = ay + dy
        if(maze[ay * width + ax] == 0 && maze[by * width + bx] == 0) {
            maze[ay * width + ax] = 1
            carve_maze(maze, bx, by, width, height)
            i = 0
        }
        d = (d + 1) % 4
    }
}

# Generate and display a random maze.
BEGIN {
    width = 39  # Must be odd.
    height = 23 # Must be odd.
    init_maze(maze, width, height)
    carve_maze(maze, 2, 2, width, height)
    maze[1 * width + 2] = 1
    maze[(height - 2) * width + width - 3] = 1
    show_maze(maze, width, height)
}
