; Maze generator in Clojure
; Joe Wingbermuehle
; 2013-10-08

; Initialize an empty maze matrix.
(defn init-maze [width height]
   (let [top_bottom  (vec (repeat width 1))
         middle      (vec (concat [1] (repeat (- width 2) 0) [1]))]
      (vec (concat [top_bottom] (repeat (- height 2) middle) [top_bottom]))))

; Display a maze.
(defn show-maze [maze]
   (dotimes [y (count maze)]
      (dotimes [x (count (get maze y))]
         (if (== (get (get maze y) x) 1)
            (print "[]")
            (print "  ")))
      (println)))

; Get/set maze elements.
(defn set-maze [maze x y v] (assoc maze y (assoc (get maze y) x v)))
(defn get-maze [maze x y] (get (get maze y) x))

; Carve a maze starting at x, y.
(defn carve-maze [maze x y i c]
   (let [dirs [[1 0] [-1 0] [0 1] [0 -1]]
         up1 (set-maze maze x y 1)
         d (get dirs (mod (+ i c) 4))
         dx (first d)
         dy (second d)
         nx  (+ x dx)
         ny  (+ y dy)
         nx2 (+ nx dx)
         ny2 (+ ny dy)]
      (if (and (= (get-maze up1 nx ny) 0) (= (get-maze up1 nx2 ny2) 0))
         (let [up2 (set-maze up1 nx ny 1)
               up3 (carve-maze up2 nx2 ny2 (rand-nth (range 0 4)) 0)]
            (if (< c 4) (carve-maze up3 nx2 ny2 i (+ c 1)) up3))
         (if (< c 4)
            (carve-maze up1 x y i (+ c 1))
            up1))))

; Generate a random maze.
; width and height must be odd.
(defn generate-maze [width height]
   (let [init     (init-maze width height)
         base     (carve-maze init 2 2 (rand-nth (range 0 4)) 0)
         top      (set-maze base 1 0 0)
         bottom   (set-maze top (- width 2) (- height 1) 0)]
      bottom))

; Generate and display a random maze.
(show-maze (generate-maze 39 23))

