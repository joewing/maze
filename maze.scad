// Maze Generator
// Joe Wingbermuehle
// 2018-02-19

// Size of each block in mm.
block_size = 5;

// Width and height in blocks.
// Width and height must be odd.
width = 25;
height = 25;

// Determine the next x, y positions based on the direction.
function next_x(dir, x) = (dir == 0 ? x + 1 : (dir == 1 ? x - 1 : x));
function next_y(dir, y) = (dir == 2 ? y + 1 : (dir == 3 ? y - 1 : y));

// Get a new direction.
function new_dir() = round(rands(0, 3, 1)[0]);

// Determine if a coordinate has been visited.
function is_visited(x, y, visited) = len(search(y * width + x, visited)) > 0;

// Visit the specified coordinate.
function visit(x, y, visited) = concat(visited, [y * width + x]);

// Draw the base.
module draw_base() {
   translate([block_size, block_size, 0]) {
      cube([block_size * width, block_size * height, block_size]);
   }
}

// Draw a block at the specified coordinate.
module block(x, y) {
   translate([x * block_size, y * block_size, block_size]) {
      cube([block_size, block_size, block_size]);
   }
}

// Get the next location and the intermediate step.
function take_step(dir, x, y) =
   let(x1 = next_x(dir, x), y1 = next_y(dir, y))
   [x1, y1, next_x(dir, x1), next_y(dir, y1)];

// Determine if it's valid to take a step.
function step_valid(visited, values) =
   let(
      x1 = values[0],
      y1 = values[1],
      x2 = values[2],
      y2 = values[3]
   )
   x2 > 0 && x2 < width && y2 > 0 && y2 < height
   && !is_visited(x1, y1, visited) && !is_visited(x2, y2, visited);

// Take the proposed step.
function carve(visited, values) =
   let(
      x1 = values[0],
      y1 = values[1],
      x2 = values[2],
      y2 = values[3],
      new_visited = visit(x1, y1, visit(x2, y2, visited))
   ) try_directions(new_dir(), 0, x2, y2, new_visited);

// Take the proposed step if it's valid.
function try_carve(dir, x, y, visited) =
   let(new_values = take_step(dir, x, y))
   step_valid(visited, new_values)
   ? carve(visited, new_values)
   : visited;

// From the current coordinate, attempt to move in all directions.
function try_directions(base_dir, count, x, y, visited) =
   let(dir = (base_dir + count) % 4)
   count < 4
   ? let(new_visited = try_carve(dir, x, y, visited))
      try_directions(base_dir, count + 1, x, y, new_visited)
   : visited;

// Generate and draw the maze.
module generate() {
   visited = try_directions(new_dir(), 0, 2, 2, []);
   for(y = [1 : height]) {
      for(x = [1 : width]) {
         if(!(x == 2 && y == 1) && !(x == width - 1 && y == height)) {
            if(!is_visited(x, y, visited)) {
               block(x, y);
            }
         }
      }
   }
}

draw_base();
generate();
