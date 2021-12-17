(defvar board '((5 3 0 0 7 0 0 0 0)
		(6 0 0 1 9 5 0 0 0)
		(0 9 8 0 0 0 0 6 0)
		(8 0 0 0 6 0 0 0 3)
		(4 0 0 8 0 3 0 0 1)
		(7 0 0 0 2 0 0 0 6)
		(0 6 0 0 0 0 2 8 0)
		(0 0 0 4 1 9 0 0 5)
		(0 0 0 0 8 0 0 7 9))
;;; 0  1  2  9  10 11 18 19 20
;;; 3  4  5  12 13 14 21 22 23

;;; ((0  1  2  3  4  5  6  7  8)
;;;  (9  10 11 12 13 14 15 16 17)
;;;  (18 19 20 21 22 23 24 25 26)
;;;  (27 28 29 30 31 32 33 34 35)
;;;  (36 37 38 39 40 41 42 43 44)
;;;  (45 46 47 48 49 50 51 52 53)
;;;  (0 6 0 0 0 0 2 8 0)
;;;  (0 0 0 4 1 9 0 0 5)
;;;  (0 0 0 0 8 0 0 7 9)))

(defun make-triples (l)
  (defun make-triples-iter (i l)
    (if (= i 0)
	nil
	(cons (car l) (make-triples-iter (- i 1) (cdr l)))))
  (if (null l)
      nil
      (cons (make-triples-iter 3 l) (make-triples (cdddr l)))))

(defun get-square (i board)
  (let ((triples (make-triples (flatten board))))
    (flatten
     (loop for j in (range 0 3)
	   collect (nth (+ (+ i (* (floor (/ i 3)) 6)) (* j 3)) triples)))))

(defun make-row ()
  (loop for k in (range 0 3)
       collect (flatten (loop for i in (range 0 3)
			      collect (loop for j in (range 0 3)
					    collect (+ (* i 9) j))))))

(defun print-board (board)
  (loop for row in board
	do (loop for num in row
		 do (write num)
		    (write-string " "))
	   (terpri)))

(defun flatten (board)
  (if (null board)
      nil
      (append (car board) (flatten (cdr board)))))

(defun reshape-board-to-squares (board)
  (let ((flat-board (flatten board)))
    flat-board))

(defun check-col (col)
  (sort col (lambda (x y) (< x y))))

(defun get-row (i board)
  (cond ((> i 8) nil)
	((= i 0) (car board))
	(t (get-row (- i 1) (cdr board)))))

(defun range (start end)
  (if (= start end)
      nil
      (cons start (range (+ start 1) end))))

(defun get-col (i board)
  (loop for row in board
	collect (nth i row)))


(defun solve (board)
  (
