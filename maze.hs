-- Maze generator and solver in Haskell
-- by Joe Wingbermuehle
-- 20070602 <> 20070603

module Main where

import Data.Map
import Random

data MazeElement
   = Space
   | Wall
   | Border
   | Solution
   | Marked
   deriving (Eq)

instance Show MazeElement where
   show Space = "  "
   show Wall  = "[]"
   show Border = show Wall
   show Marked = "++"

data Maze = Maze Int Int (Map Int MazeElement)

instance Show Maze where
   show (Maze width height fm) =
      let lst = Data.Map.toList fm in
      show_rows 0 width height lst
      where
         show_columns x width [] = ("", [])
         show_columns x width (e:es)
            | x < width =
               let (str, lst) = show_columns (x + 1) width es in
               let (key, value) = e in
               ((show value) ++ str, lst)
            | otherwise = ("\n", (e:es))
         show_rows y width height lst
            | y < height =
               let (str, nlst) = show_columns 0 width lst in
               str ++ (show_rows (y + 1) width height nlst)
            | otherwise = ""

-- Initialize an empty maze.
initialize_maze :: Int -> Int -> Maze
initialize_maze width height =
   let lst = [ (i, Wall) | i <- [ 0 .. width * height ] ] in
   let maze = Maze width height (Data.Map.fromList lst) in
   let maze_b = fill_rl 0 width height $ fill_tb 0 width height maze in
   set_element maze_b 1 1 Space
   where
      fill_rl y width height maze
         | y < height =
            let maze_l = set_element maze 0 y Border in
            let maze_r = set_element maze_l (width - 1) y Border in
            fill_rl (y + 1) width height maze_r
         | otherwise = maze
      fill_tb x width height maze
         | x < width =
            let maze_t = set_element maze x 0 Border in
            let maze_b = set_element maze_t x (height - 1) Border in
            fill_tb (x + 1) width height maze_b
         | otherwise = maze

-- Set a maze element.
set_element :: Maze -> Int -> Int -> MazeElement -> Maze
set_element maze x y value =
   let Maze width height fm = maze in
   let index = x + y * width in
   let nfm = Data.Map.insert index value (Data.Map.delete index fm) in
   Maze width height nfm

-- Get a maze element.
get_element :: Maze -> Int -> Int -> MazeElement
get_element maze x y =
   let Maze width height fm = maze in
   let index = x + y * width in
   Data.Map.findWithDefault Space index fm

-- Carve starting from the specified coordinates.
do_carve_maze :: IO Maze -> Int -> Int -> IO Maze
do_carve_maze maze x y =
   do
      temp <- maze
      let value = get_element temp x y
      dir <- getStdRandom $ randomR (0, 3)
      if value == Space then
         do
            let nmaze = set_element temp x y Space
            try_move (return nmaze) dir 0 x y
         else maze
   where
      move_x :: IO Maze -> Int -> Int -> Int -> Int -> IO (Bool, Maze)
      move_x maze x1 y1 x2 y2 =
         do
            temp <- maze
            let v1 = get_element temp x1 y1
            let v2 = get_element temp x2 y2
            if v1 == Wall && v2 == Wall then
               do
                  let m1 = set_element temp x1 y1 Space
                  let m2 = set_element m1 x2 y2 Space
                  m3 <- do_carve_maze (return m2) x2 y2
                  return (True, m3)
               else
                  do
                     temp <- maze
                     return (False, temp)
      move :: IO Maze -> Int -> Int -> Int -> IO (Bool, Maze)
      move maze 0 x y = move_x maze x (y - 1) x (y - 2)
      move maze 1 x y = move_x maze x (y + 1) x (y + 2)
      move maze 2 x y = move_x maze (x - 1) y (x - 2) y
      move maze _ x y = move_x maze (x + 1) y (x + 2) y
      try_move :: IO Maze -> Int -> Int -> Int -> Int -> IO Maze
      try_move maze dir count x y
         | count < 4 =
            do
               (status, nmaze) <- move maze dir x y
               if status == False then
                  do
                     let next_dir = mod (dir + 1) 4
                     try_move (return nmaze) next_dir (count + 1) x y
                  else do return nmaze
         | otherwise = maze

-- Carve starting from each possible position.
carve_maze :: IO Maze -> IO Maze
carve_maze maze =
   do
      (Maze width height _) <- maze
      m1 <- carve_maze_y maze 1 1 width height
      let m2 = set_element m1 0 1 Space
      return $ set_element m2 (width - 1) (height - 2) Space
   where
      carve_maze_x :: IO Maze -> Int -> Int -> Int -> Int -> IO Maze
      carve_maze_x maze x y width height
         | x < width =
            do
               let nmaze = do_carve_maze maze x y
               carve_maze_x nmaze (x + 2) y width height
         | otherwise = maze
      carve_maze_y :: IO Maze -> Int -> Int -> Int -> Int -> IO Maze
      carve_maze_y maze x y width height
         | y < height =
            do
               let nmaze = carve_maze_x maze x y width height
               carve_maze_y nmaze x (y + 2) width height
         | otherwise = maze

-- Solve the maze (if possible).
solve_maze :: Maze -> Maze
solve_maze maze =
   snd $ solve 1 1 maze
   where
      move maze x1 y1 x2 y2 =
         if x2 < 0 || y2 < 0 then (False, maze)
         else
            let e1 = get_element maze x1 y1 in
            let e2 = get_element maze x2 y2 in
            if e1 == Space && e2 == Space then
               solve x2 y2 $ set_element maze x1 y1 Marked
            else (False, maze)
      solve x y maze =
         let (Maze width height _) = maze in
         let nmaze = set_element maze x y Marked in
         if x == width - 2 && y == height - 2 then (True, nmaze)
         else
            let (s_left, m_left) = move nmaze (x + 1) y (x + 2) y in
            let (s_right, m_right) = move nmaze (x - 1) y (x - 2) y in
            let (s_up, m_up) = move nmaze x (y - 1) x (y - 2) in
            let (s_down, m_down) = move nmaze x (y + 1) x (y + 2) in
            if s_left then (s_left, m_left)
            else if s_right then (s_right, m_right)
            else if s_up then (s_up, m_up)
            else (s_down, m_down)

-- Entry point. Generate and display the maze.
main :: IO ()
main =
   do
      maze <- carve_maze $ return $ initialize_maze 39 23
      putStrLn $ show maze
      putStr $ show $ solve_maze maze

