// Maze generator in Rust.
// 2014-01-11
// Joe Wingbermuehle

use std::rand;

struct Maze {
    width:  int,
    height: int,
    data:   ~[bool]
}

// Create an empty maze structure.
fn init_maze(width: int, height: int) -> ~Maze {
    let mut maze = ~Maze {
        width: width,
        height: height,
        data:   ~[]
    };
    for y in range(0, height) {
        for x in range(0, width) {
            let value =
                if y == 0 || y == height - 1 {
                    false
                } else if x == 0 || x == width - 1 {
                    false
                } else {
                    true
                };
            maze.data.push(value);
        }
    }
    maze
}

// Show the maze.
fn show_maze(maze: &Maze) {
    for y in range(0, maze.height) {
        for x in range(0, maze.width) {
            if maze.data[y * maze.width + x] {
                print("[]")
            } else {
                print("  ")
            }
        }
        println("")
    }
}

// Carve the maze starting at x, y.
fn carve_maze<R: std::rand::Rng>(rng: &mut R, x: int, y: int, maze: &mut Maze) {
    let xdirs = [ 1, -1, 0, 0 ];
    let ydirs = [ 0, 0, 1, -1 ];
    maze.data[y * maze.width + x] = false;
    let d = rng.gen_integer_range(0, 4);
    for i in range(0, 4) {
        let dx = xdirs[(d + i) % 4];
        let dy = ydirs[(d + i) % 4];
        let x2 = x + dx;
        let y2 = y + dy;
        let nx = x2 + dx;
        let ny = y2 + dy;
        if maze.data[y2 * maze.width + x2] &&
            maze.data[ny * maze.width + nx] {
            maze.data[y2 * maze.width + x2] = false;
            carve_maze(rng, nx, ny, maze);
        }
    }

}

// Generate a maze.
fn generate_maze(width: int, height: int) -> ~Maze {
    let mut maze = init_maze(width, height);
    let rng = rand::task_rng();
    carve_maze(rng, 2, 2, maze);
    maze.data[1 * width + 2] = false;
    maze.data[(height - 2) * width + (width - 3)] = false;
    maze
}

fn main() {
    let width = 39;     // Maze width; must be odd.
    let height = 23;    // Maze height; must be odd.
    let maze = generate_maze(width, height);
    show_maze(maze);
}
