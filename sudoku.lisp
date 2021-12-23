(defun print-board (board)
  (loop for row in board
	do (loop for elem in row
		 do (write elem)
		    (write-string " "))
	   (terpri))
  (terpri))

(defun copy (board) (loop for row in board collect (copy-list row)))

(defun cols (board)
  (loop for i from 0 below 9
	collect (loop for row in board
		     collect (nth i row))))

(defun squares (board)
  (loop for i from 0 below 3
	append (loop for j from 0 below 3
		     collect (loop for m from (* i 3) below (+ (* i 3) 3)
				   append (loop for n from (* j 3) below (+ (* j 3) 3)
						collect (nth n (nth m board)))))))

(defun rows-cols-squares (board)
  (append board (cols board) (squares board)))

(defun duplicates? (l)
  (cond ((null l) nil)
	((not (null (member (first l) (rest l)))) t)
	(t (or nil (duplicates? (rest l))))))

(defun contains-1-to-9? (l)
  (equal (sort (copy-list l) (lambda (x y) (< x y)))
	 '(1 2 3 4 5 6 7 8 9)))

(defun remove-zeros (l) (remove-if (lambda (x) (= x 0)) l))

(defun and-each-element (l) (every #'identity l))

(defun valid? (board)
  (and-each-element (loop for l in (rows-cols-squares board)
			  collect (not (duplicates? (remove-zeros l))))))

(defun solved? (board)
  (and-each-element (loop for l in (rows-cols-squares board)
			  collect (contains-1-to-9? l))))

(defun solve (board)
  (cond ((solved? board) (print board))
	(t (loop named outer for i from 0 below 9
		 do (loop for j from 0 below 9
			  do (if (= (nth j (nth i board)) 0)
				 (progn (loop for k from 1 to 9
					      do (let ((new-board (copy board)))
						   (setf (nth j (nth i new-board)) k)
						   (if (valid? new-board)
						       (solve new-board))))
					(return-from solve))))))))

