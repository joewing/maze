/** Maze generator in D.
 * Joe Wingbermuehle
 * 20130420
 */

import std.stdio;
import std.random;

class Maze {

   private enum Cell { Wall, Space };

   /** Create a random maze. */
   this(uint w, uint h) {
      width    = w * 2 + 1;
      height   = h * 2 + 1;
      data.length = height;
      for(uint y = 0; y < height; y++) {
         data[y].length = width;
         if(y == 0 || y == height - 1) {
            data[y][] = Cell.Space;
         } else {
            data[y][0]           = Cell.Space;
            data[y][$ - 1]       = Cell.Space;
            data[y][1 .. $ - 1]  = Cell.Wall;
         }
      }
      carve(2, 2);
      data[1][2] = Cell.Space;
      data[$ - 2][$ - 3] = Cell.Space;
   }

   /** Carve the maze starting at x, y. */
   private void carve(uint x, uint y) {
      data[y][x] = Cell.Space;
      immutable dirs = [ [ 1, 0 ], [ -1, 0 ], [ 0, 1 ], [ 0, -1 ] ];
      uint direction = uniform(0, 4);
      for(uint i = 0; i < 4; i++) {
         immutable dx = dirs[direction][0];
         immutable dy = dirs[direction][1];
         immutable x2 = x  + dx;
         immutable y2 = y  + dy;
         immutable nx = x2 + dx;
         immutable ny = y2 + dy;
         if(data[y2][x2] == Cell.Wall && data[ny][nx] == Cell.Wall) {
            data[y2][x2] = Cell.Space;
            carve(nx, ny);
            direction = uniform(0, 4);
            x = nx;
            y = ny;
            i = 0;
         }
         direction = (direction + 1) % 4;
      }
   }

   /** Display the maze. */
   void print() {
      for(uint y = 0; y < height; y++) {
         for(uint x = 0; x < width; x++) {
            final switch(data[y][x]) {
            case Cell.Space:
               write("  ");
               break;
            case Cell.Wall:
               write("[]");
               break;
            }
         }
         writeln();
      }
   }

   private uint width;
   private uint height;
   private Cell[][] data;

}

void main() {
   Maze m = new Maze(19, 11);
   m.print();
}
