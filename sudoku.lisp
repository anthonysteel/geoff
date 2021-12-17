(defun copy-board (board)
  (loop for row in board
	collect (loop for entry in row
		      collect entry)))

(defun flatten (board)
  (if (null board)
      nil
      (append (car board) (flatten (cdr board)))))

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
    (flatten (loop for j from 0 to 2
		   collect (nth (+ (+ i (* (floor (/ i 3)) 6)) (* j 3)) triples)))))

(defun get-all-squares (board)
  (loop for i from 0 to 8
	collect (get-square i board)))

(defun get-row (i board)
  (cond ((> i 8) nil)
	((= i 0) (car board))
	(t (get-row (- i 1) (cdr board)))))

(defun get-col (i board)
  (loop for row in board
	collect (nth i row)))

(defun get-all-rows (board)
  (loop for i from 0 to 8
	collect (get-row i board)))

(defun get-all-cols (board)
  (loop for i from 0 to 8
	collect (get-col i board)))

(defun duplicatep (l)
  (cond ((null l) nil)
	((and (> (car l) 0) (member (car l) (cdr l))) t)
	(t (duplicatep (cdr l)))))

(defun not-duplicatep (l)
  (not (duplicatep l)))
  
(defun is-valid (board)
  (let ((rows (get-all-rows board))
	(cols (get-all-cols board))
	(squares (get-all-squares board)))
    (every #'identity (mapcar 'not-duplicatep (append rows cols squares)))))

(defun is-not-valid (board)
  (not (is-valid board)))

(defun is-list-complete (l)
  (equal (loop for i from 1 to 9 collect i)
	 (sort (copy-list l) (lambda (x y) (< x y)))))

(defun is-complete (board)
  (let ((rows (get-all-rows board))
	(cols (get-all-cols board))
	(squares (get-all-squares board)))
    (every #'identity (mapcar 'is-list-complete (append rows cols squares)))))

(defun is-solved (board)
  (and (is-complete board)
       (is-valid board)))

(defun print-board (board)
  (loop for row in board
	do (loop for num in row
		 do (write num)
		    (write-string " "))
	   (terpri)))

(defun solve (board)
  (print-board board)
  (terpri)
  (let ((new-board (copy-board board)))
    (loop named outer for i from 0 to 8
	  do (loop for j from 0 to 8
		   do (if (= (nth j (nth i new-board)) 0)
			  (loop for k from 1 to 9
				do (setf (nth j (nth i new-board)) k)
				   (cond ((is-solved new-board) (return-from outer new-board))
					 ((is-valid new-board) (solve new-board)))))))))

(defun solve (board)
  (let ((new-board (make-new-board board)))
    (loop for row in board
	  do (loop for entry in row
		   do (if (= entry 0)
			  (loop for i from 1 to 9
				do (progn (setf entry i)
					  (cond ((is-solved new-board) (return new-board))
						((is-valid new-board) (solve new-board))
						(t (return))))))))))
