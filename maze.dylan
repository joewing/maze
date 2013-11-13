Module:    maze
Author:    Joe Wingbermuehle
Date:      2013-11-12
Synopsis:  Maze generator

// A class to represent a maze.
define class <maze> (<object>)
  constant slot width :: <integer>, required-init-keyword: width:;
  constant slot height :: <integer>, required-init-keyword: height:;
  slot data :: <array>;
end class <maze>;

// Initialize the maze.
define method initialize(maze :: <maze>, #key)
  next-method();
  let dims = vector(maze.width, maze.height);
  maze.data := make(<array>, dimensions: dims, fill: #f);
  for (x from 0 below maze.width)
    maze.data[x, 0] := #t;
    maze.data[x, maze.height - 1] := #t;
  end for;
  for (y from 0 below maze.height)
    maze.data[0, y] := #t;
    maze.data[maze.width - 1, y] := #t;
  end for;
end method initialize;

// Carve the maze starting at x, y.
define method carve(maze :: <maze>, rand :: <random>,
                    #key x :: <integer> = 2, y :: <integer> = 2)
  maze.data[x, y] := #t;
  let xdirs = #[1, -1, 0, 0];
  let ydirs = #[0, 0, 1, -1];
  let d = random(4, random: rand);
  let i = 0;
  while (i < 4)
    let dx = xdirs[d];
    let dy = ydirs[d];
    let nx = x + 2 * dx;
    let ny = y + 2 * dy;
    if (~maze.data[x + dx, y + dy] & ~maze.data[nx, ny])
      maze.data[x + dx, y + dy] := #t;
      carve(maze, rand, x: nx, y: ny);
      d := random(4, random: rand);
      i := 0;
      x := nx;
      y := ny;
    else
      i := i + 1;
      d := modulo(d + 1, 4);
    end if;
  end while;
end method carve;

// Show the maze.
define method show(maze :: <maze>)
  maze.data[2, 1] := #t;
  maze.data[maze.width - 3, maze.height - 2] := #t;
  for (y from 0 below maze.height)
    for (x from 0 below maze.width)
      if (maze.data[x, y])
        format-out("  ");
      else
        format-out("[]");
      end if;
    end for;
    format-out("\n");
  end for;
end method show;

// Generate and display a random maze.
define function main(name :: <string>, arguments :: <vector>)
  let maze = make(<maze>, width: 39, height: 23);
  let rand = make(<random>);
  carve(maze, rand);
  show(maze);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
