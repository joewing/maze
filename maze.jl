; Maze generate in JL
; Joe Wingbermuehle

(define width 39)       ; Width of the maze (must be odd)
(define height 23)      ; Height of the maze (must be odd)
(define seed 17)        ; Random number seed.

; Generate a random number.
(define get-rand (lambda (s) (mod (+ (* 19 s) 1) 16383)))

; Set an element in a list.
(define set-element (lambda (lst n v)
   (if n
      (cons (head lst) (set-element (rest lst) (- n 1) v))
      (cons v (rest lst)))))

; Get a particular list element.
(define get-element (lambda (lst n)
   (if n
      (get-element (rest lst) (- n 1))
      (head lst))))

; Initialize the maze matrix.
(define init-maze (lambda (x y)
   (if (= y 1)
      (if (= x 1) (list 0) (cons 0 (init-maze (- x 1) y)))
      (if (= y height)
         (if (= x 1)
            (cons 0 (init-maze width (- y 1)))
            (cons 0 (init-maze (- x 1) y)))
         (if (= x 1)
            (cons 0 (init-maze width (- y 1)))
            (if (= x width)
               (cons 0 (init-maze (- x 1) y))
               (cons 1 (init-maze (- x 1) y))))))))

(define update-x (lambda (x d) (+ x (get-element (list 1 -1 0 0) (mod d 4)))))

(define update-y (lambda (y d) (+ y (get-element (list 0 0 1 -1) (mod d 4)))))

; Carve a maze.
(define carve-maze (lambda (maze rand x y c)
   (define maze (set-element maze (+ (* y width) x) 0))
   (define x1 (update-x x rand))
   (define y1 (update-y y rand))
   (define x2 (update-x x1 rand))
   (define y2 (update-y y1 rand))
   (if (and (> x2 0) (< x2 width) (> y2 0) (< y2 height)
            (get-element maze (+ (* y1 width) x1))
            (get-element maze (+ (* y2 width) x2)))
      (begin
         (define maze (set-element maze (+ (* y1 width) x1) 0))
         (define maze (carve-maze maze (get-rand rand) x2 y2 0))
         (if (< c 4) (carve-maze maze (+ rand 1) x y (+ c 1)) maze))
      (if (< c 4) (carve-maze maze (+ rand 1) x y (+ c 1)) maze))))

; Initialize and carve a maze.
(define generate-maze (lambda ()
   (define init (init-maze width height))
   (define carved (carve-maze init seed 2 2 0))
   (define temp (set-element carved (+ (* 1 width) 2) 0))
   (set-element temp (+ (* (- height 2) width) (- width 3)) 0)))

; Show a maze.
(define show-maze (lambda (maze x y)
   (define element (get-element maze (+ (* y width) x)))
   (if (= element 1) (print "[]") (print "  "))
   (if (= (+ x 1) width)
      (begin
         (print "\n")
         (if (< (+ y 1) height) (show-maze maze 0 (+ y 1))))
      (show-maze maze (+ x 1) y))))

(show-maze (generate-maze) 0 0)

