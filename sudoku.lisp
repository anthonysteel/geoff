(defun copy (board)
  (loop for row in board
	collect (loop for entry in row
		      collect entry)))

(defun print-board (board)
  (loop for row in board
	do (loop for num in row
		 do (write num)
		    (write-string " "))
	   (terpri))
  (terpri))

(defun get-cols (board)
  (loop for i from 0 to 8
	collect (loop for row in board
		      collect (nth i row))))

(defun get-rows-from (i j board)
  (loop for num from i to j
	collect (nth num board)))

(defun get-square (i j board)
  (loop for row in (get-rows-from (* i 3) (+ (* i 3) 2) board)
	append (loop for k from 0 to 2
		     collect (nth (+ k (* j 3)) row))))

(defun get-squares (board)
  (loop for i from 0 to 2
	append (loop for j from 0 to 2
		     collect (get-square i j board))))

(defun get-rows-cols-squares (board)
  (append board (get-all-cols board) (get-all-squares board)))

(defun has-duplicates? (l)
  (cond ((null l) nil)
	((= (first l) 0) (has-duplicates? (rest l))) ;; ignore repeated 0's
	(t (or (not (null (member (first l) (rest l)))) (has-duplicates? (rest l))))))

(defun reduce-and (l)
  (cond ((null l) t)
	(t (and (first l) (reduce-and (rest l))))))

(defun is-valid? (board)
  (reduce-and (loop for l in (get-rows-cols-squares board)
		  collect (not (has-duplicates? l)))))

(defun is-complete? (board)
  (let ((complete '(1 2 3 4 5 6 7 8 9)))
    (reduce-and (loop for l in (get-rows-cols-squares board)
		    collect (equal (sort (copy-list l) #'<)
				   complete)))))

(defun solve (board)
  (cond ((is-complete? board) (print-board board))
	(t (loop named outer for i from 0 to 8
		 do (loop for j from 0 to 8
			  do (if (= (nth j (nth i board)) 0)
				 (progn (loop for num from 1 to 9
					     do (let ((new-board (copy board)))
						  (setf (nth j (nth i new-board)) num)
						  (if (is-valid? new-board)
						      (solve new-board))))
					(return-from outer))))))))
