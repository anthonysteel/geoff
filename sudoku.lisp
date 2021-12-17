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

(defun get-all-squares (board) (loop for i from 0 to 8 collect (get-square i board)))

(defun get-row (i board) (nth i board))

(defun get-col (i board) (loop for row in board collect (nth i row)))

(defun get-all-rows (board) (loop for i from 0 to 8 collect (get-row i board)))

(defun get-all-cols (board) (loop for i from 0 to 8 collect (get-col i board)))

(defun duplicate? (l)
  (cond ((null l) nil)
	((and (> (car l) 0) (member (car l) (cdr l))) t)
	(t (duplicate? (cdr l)))))

(defun not-duplicate? (l) (not (duplicate? l)))
  
(defun is-valid? (board)
  (let ((rows (get-all-rows board))
	(cols (get-all-cols board))
	(squares (get-all-squares board)))
    (every #'identity (mapcar 'not-duplicate? (append rows cols squares)))))

(defun is-not-valid? (board) (not (is-valid? board)))

(defun is-list-complete? (l)
  (equal (loop for i from 1 to 9 collect i)
	 (sort (copy-list l) (lambda (x y) (< x y)))))

(defun is-complete? (board)
  (let ((rows (get-all-rows board))
	(cols (get-all-cols board))
	(squares (get-all-squares board)))
    (every #'identity (mapcar 'is-list-complete? (append rows cols squares)))))

(defun is-solved? (board) (and (is-complete? board) (is-valid? board)))

(defun print-board (board)
  (loop for row in board
	do (loop for num in row
		 do (write num)
		    (write-string " "))
	   (terpri)))

(defun get-elem (i j board) (nth j (nth i board)))

(defun set-elem (i j new-elem board) (setf (nth j (nth i board)) new-elem))

(defun solve (board)
  (print-board board)
  (terpri)
  (cond ((is-solved? board) board)
	((is-valid? board)
	 (loop for i from 0 to 8
	       do (loop for j from 0 to 8
			do (when (= (get-elem i j board) 0)
			     (loop for k from 1 to 9
				   do (let ((new-board (copy-board board)))
					(set-elem i j k new-board)
					(solve new-board)))))))))
