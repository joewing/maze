#!/usr/bin/ruby
# Maze generator in Ruby
# Joe Wingbermuehle
# 2011-06-07

class Maze

   # Generate a maze.
   def generate(width, height)
      @width = width
      @height = height
      @maze = Array.new
      for y in 0 ... @height
         for x in 0 ... @width
            @maze[y * @width + x] = 1
         end
      end
      @maze[1 * @width + 1] = 0
      carve(1, 1)
      @maze[0 * @width + 1] = 0
      @maze[(@height - 1) * @width + @width - 2] = 0
   end

   # Show the maze.
   def show
      for y in 0 ... @height
         for x in 0 ... @width
            chars = if @maze[y * @width + x] == 1 then
               [ "[", "]" ]
            else
               [ " ", " " ]
            end
            chars.each { |c| putc(c) }
         end
         puts
      end
   end

   private

   # Start carving at x, y.
   def carve(x, y)

      def update_pos(dir, x, y)
         case dir
         when 0
            return x + 1, y + 0
         when 1
            return x + 0, y + 1
         when 2
            return x - 1, y + 0
         when 3
            return x + 0, y - 1
         end
      end

      dir = rand(4)
      count = 0
      while count < 4
         x1, y1 = update_pos(dir, x, y)
         x2, y2 = update_pos(dir, x1, y1)
         if x2 > 0 and x2 < @width and y2 > 0 and y2 < @height
            if @maze[y1 * @width + x1] == 1 and @maze[y2 * @width + x2] == 1
               @maze[y1 * @width + x1] = 0
               @maze[y2 * @width + x2] = 0
               carve(x2, y2)
            end
         end
         count += 1
         dir = (dir + 1) % 4
      end

   end

end

mz = Maze.new
mz.generate(39, 23)
mz.show()

