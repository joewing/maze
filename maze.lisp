;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLISP Maze 20030311 by Joe Wingbermuehle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The width and height of the maze. Both must be odd.
(defconstant *width* 39)
(defconstant *height* 21)
(defvar maze)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start carving the maze at a specific location.
(defun carve-maze (x y)
	(let ((d (random 4)))
		(dotimes (c 4)
         (let* (  (cd (mod (+ c d) 4))
                  (dv (cond
                        ((= cd 0) (list 1 0))
                        ((= cd 1) (list 0 1))
                        ((= cd 2) (list -1 0))
                        (t        (list 0 -1))))
                  (x1 (+ x (car dv)))
                  (y1 (+ y (cadr dv)))
                  (x2 (+ x1 (car dv)))
                  (y2 (+ y1 (cadr dv)))
               )
            (if (and (and (> x2 0) (< x2 *width*))
                     (and (> y2 0) (< y2 *height*)))
               (if (and (= (aref maze x1 y1) 1)
                        (= (aref maze x2 y2) 1))
                  (let ()
                     (setf (aref maze x1 y1) 0)
                     (setf (aref maze x2 y2) 0)
                     (carve-maze x2 y2)
                  )
               )
            )
         )
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate a maze
(defun generate-maze ()
   (setq *random-state* (make-random-state t))
   (setf (aref maze 1 1) 0)
   (carve-maze 1 1)
   (setf (aref maze 1 0) 0)
   (setf (aref maze (- *width* 1) (- *height* 2)) 0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display the maze
(defun display-maze ()
   (dotimes (y *height*)
      (dotimes (x *width*)
         (if (= (aref maze x y) 1)
            (princ "[]")
            (princ "  ")
         )
      )
      (terpri)
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create and display the maze.
(setq maze (make-array (list *width* *height*) :initial-element 1))
(generate-maze)
(display-maze)

