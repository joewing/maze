/* Maze generator in Scala
 * Joe Wingbermuehle
 * 2010-10-16
 */

object Maze {

   private var width:   Int = 0
   private var height:  Int = 0
   private var maze:    Array[Array[Int]] = null

   /* Show the maze. */
   def show {
      maze.foreach(row =>
         {
            row.foreach(block => print(if(block == 1) "[]" else "  "))
            println
         }
      )
   }

   /* Start carving at x, y. */
   private def carve(x: Int, y: Int) {

      def update_pos(dir: Int, x: Int, y: Int): (Int, Int) = dir match {
         case 0 => (x + 1, y + 0)
         case 1 => (x + 0, y + 1)
         case 2 => (x - 1, y + 0)
         case _ => (x + 0, y - 1)
      }

      var dir:    Int = (scala.math.random * 4.0).toInt
      var count:  Int = 0
      while(count < 4) {
         val (x1, y1) = update_pos(dir, x, y)
         val (x2, y2) = update_pos(dir, x1, y1)
         if(x2 > 0 && x2 < width && y2 > 0 && y2 < height) {
            if(maze(y1)(x1) == 1 && maze(y2)(x2) == 1) {
               maze(y1)(x1) = 0
               maze(y2)(x2) = 0
               carve(x2, y2)
            }
         }
         count += 1
         dir = (dir + 1) % 4
      }

   }

   /* Generate a maze. */
   def generate(w: Int, h: Int) {
      width = w
      height = h
      maze = Array.fill[Int](height, width)(1)
      maze(1)(1) = 0
      carve(1, 1)
      maze(0)(1) = 0
      maze(height - 1)(width - 2) = 0
   }

   /* Generate and display a random maze. */
   def main(args: Array[String]) {
      generate(39, 23)
      show
   }

}

