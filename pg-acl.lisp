(defpackage :pg-acl
  (:use :common-lisp))

(in-package :pg-acl)


;; PG Book - Exercise 3.2
(defun ordered-union (seq-a seq-b &rest others)
  "Like union, but maintain order from original sequences
     Ex: (ordered-union '(a d c d) '(c a b c)) => (A D C B)"
  (let ((new-seq ())
	(seen (make-hash-table))
	(seqs (append (list seq-a seq-b) others)))
    (dolist (next-seq seqs (nreverse new-seq))
      (loop for el being the elements of next-seq do
	   (when (null (gethash el seen))
	     (setf (gethash el seen) 1)
	     (push el new-seq))))))


;; PG Book - Exercise 3.3
(defun occurrences (seq)
  "Returns occurrence map. 
     Ex: (a b a d a c d c a) => ((A . 4) (C . 2) (D . 2) (B . 1))"
  (let ((occ-table (make-hash-table))
	(occ ()))
    (loop for el being the elements of seq do
	 (if (null (gethash el occ-table))
	     (setf (gethash el occ-table) 1)
	     (incf (gethash el occ-table))))
    (loop for key being the hash-key of occ-table do
	 (push (cons key (gethash key occ-table)) occ))
    (sort occ #'> :key #'cdr)))

(defun occurrences-2 (lst)
  "Returns occurrence map. 
     Ex: (a b a d a c d c a) => ((A . 4) (C . 2) (D . 2) (B . 1))"
  (labels ((occurrences-r (lst occ)
	     (cond ((null lst) occ)
		   (t (let* ((el (car lst))
			     (entry (assoc el occ)))
			(if (null entry)
			    (push (cons el 1) occ)
			    (incf (cdr entry)))
			(occurrences-r (cdr lst) occ))))))
    (sort (occurrences-r lst ()) #'> :key #'cdr)))


       



;;PG Book - Exercise 4.1

(defun quarter-turn (square-arr)
  (let* ((dim (array-dimensions square-arr))
	 (n (car dim))
	 (counter 0)
	 (new-arr (make-array dim)))
    (dotimes (j n new-arr)
      (dotimes (k n)
	(let ((new-i (floor (/ counter n)))
	      (new-j (mod counter n))
	      (i (- n 1 k)))
	  (setf (aref new-arr new-i new-j) 
		(aref square-arr i j))
	  (incf counter))))))

(defun copy-square-array (arr)
  (let* ((dim (array-dimensions arr))
	 (new-arr (make-array dim))
	 (n (car dim)))
    (dotimes (i n new-arr)
      (dotimes (j n)
	(setf (aref new-arr i j) (aref arr i j))))))

(defun rotate-square (square-arr turns)
  (let ((new-arr (copy-square-array square-arr)))
    (dotimes (count (mod turns 4) new-arr)
      (setf new-arr (quarter-turn new-arr)))))

(defun print-square-arr (square-arr &optional (stream t))
  (let ((n (car (array-dimensions square-arr))))
    (dotimes (i n)
      (dotimes (j n)
	(format stream "~A  " (aref square-arr i j)))
      (format stream "~%"))))




;PG Book - Exercise 4.2a
(defun reduce-copy-list (lst)
  "Functional equivalent of copy-list"
  (reduce #'append lst :key #'list))

;PG Book - Exercise 4.2b
(defun reduce-reverse (lst)
  "Functional equivalent of reverse (for lists only)"
  (reduce #'(lambda (x y) (append y x)) lst :key #'list))



;PG Book - Exercise 4.3

(defstruct (3tree (:conc-name 3t-)
		  (:print-function print-3tree))
  (val nil)
  (left nil)
  (center nil)
  (right nil))

(defun print-3tree (3t stream depth)
  (format stream "#<val:~A  -  children: ~A, ~A, ~A>"
	  (3t-val 3t)
	  (3t-left 3t)
	  (3t-center 3t)
	  (3t-right 3t)))
	  
(defun copy-3t (3t)
  (and (not (null 3t))
       (make-3tree :val (3t-val 3t)
		   :left (copy-3t (3t-left 3t))
		   :center (copy-3t (3t-center 3t))
		   :right (copy-3t (3t-right 3t)))))

(defun 3t-search (3t val)
  (and (not (null 3t))
       (or (eql val (3t-val 3t))
	   (3t-search (3t-left 3t) val)
	   (3t-search (3t-center 3t) val)
	   (3t-search (3t-right 3t) val))))



;PG Book - Exercise 4.6
(defun assoc-to-hash-table (assoc-lst)
  (let ((ht (make-hash-table)))
    (dolist (entry assoc-lst ht)
      (setf (gethash (car entry) ht)
	    (cdr entry)))))

(defun hash-table-to-assoc (ht)
  (loop for key being the hash-keys of ht
     collect (cons key (gethash key ht))))



;PG Book - Exercise 5.1
(defun ex5.1-a (y)
  (funcall #'(lambda (x) (cons x x)) (car y)))
(defun ex5.1-b (x z)
  (apply #'(lambda (w y) (cons w y))
	   (funcall #'(lambda (w)
			(list w (+ w z))) (car x))))

;PG Book - Exercise 5.5
(defun precedes (val seq)
  (and (not (null seq))
       (< 1 (length seq))
       (let ((acc nil))
	 (dotimes (i (- (length seq) 1) (reverse acc))
	   (let ((this (elt seq i))
		 (next (elt seq (+ i 1))))
	     (if (equal val next)
		 (push this acc)))))))

(defun precedes-r (val seq)
  (labels ((pr (i len acc)
	     (cond ((>= i (- len 1)) acc)
		   (t (let ((this (elt seq i))
			    (next (elt seq (+ i 1))))
			(if (equal val next)
			    (pr (+ i 1) len (append acc (list this)))
			    (pr (+ i 1) len acc)))))))
    (pr 0 (length seq) nil)))


; PG Book - Exercise 5.6
(defun intersperse (obj lst)
  (let ((acc nil))
    (dolist (el lst (reverse acc))
      (if (not (null acc))
	  (push obj acc))
      (push el acc))))

(defun intersperse-r (obj lst)
  (cond ((null lst) nil)
	((null (cdr lst)) lst)
	(t (append (list (car lst) obj)
		   (intersperse-r obj (cdr lst))))))

;PG Book - Exercise 5.7a
(defun each-pair-diff-by (lst diff)
  (and (not (null lst))
       (> (length lst) 1)
       (= diff (- (cadr lst) (car lst)))
       (if (not (null (cddr lst)))
	   (each-pair-diff-by (cddr lst) diff)
	   t)))

;PG Book - Exercise 5.7b
(defun each-pair-diff-by-do (lst diff)
  (do ((x lst (cddr x)))
      ((null x) t)
    (if (or (null (cdr x))
	    (not (= diff (- (cadr x) (car x)))))
	(return nil))))

;PG Book - Exercise 5.7c
(defun each-pair-diff-by-mapc (lst diff)
  (let ((temp nil))
    (mapc #'(lambda (x)
	      (if (null temp) 
		  (setf temp x)
		  (if (not (= diff (- x temp))) 
		      (return-from each-pair-diff-by-mapc nil)
		      (setf temp nil)))) lst)
    (null temp)))
       

;PG Book - Exercise 5.8
(defun max-min (vec)
  "Return the max and min of the given sequence, as multiple values"
  (labels 
      ((max-min-r (vec i max-min-vals)
	 (if (>= i (length vec))
	     max-min-vals
	     (let ((val (aref vec i))
		   (max (first max-min-vals))
		   (min (second max-min-vals)))
	       (cond ((< val min) (max-min-r vec (+ i 1) (list max val)))
		     ((> val max) (max-min-r vec (+ i 1) (list val min)))
		     (t (max-min-r vec (+ i 1) max-min-vals)))))))
    (and (not (null vec))
	 (> (length vec) 0)
	 (let ((first (aref vec 0)))
	   (let ((max-min-list (max-min-r vec 0 (list first first))))
	     (values (first max-min-list) (second max-min-list)))))))	    

;PG Book - Exercise 6.3
(defun count-args (&rest rest)
  (length rest))

;PG Book - Exercise 6.4
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins))
	     (wins2 nil)
	     (max2 nil))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (>= score max)
	      (setf wins2 wins)
	      (setf max2 max)
	      (setf wins obj)
	      (setf max score))
	    (when (and (< score max)
		       (or (null max2) 
			   (> score max2))
		(setf wins2 obj)
		(setf max2 score)))))
	(values wins wins2))))
  
	
;PG Book - Exercise 6.5   
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun remove-if-filter (fn lst)
  (filter 
   #'(lambda (x)
       (if (not (funcall fn x)) x)) 
   lst))

;PG Book - Exercise 6.6
(let ((greatest-num nil))
  (defun greatest (num)
    (if (or (null greatest-num)
	    (> num greatest-num))
	(setf greatest-num num))
    greatest-num))

;PG Book - Exercise 6.7
(let ((last nil))
  (defun greater-than-last (num)
    (let ((last2 last))
      (setf last num)
      (and (not (null last2))
	   (> last last2)))))
	

;PG Book - Exercise 6.8
(defun expensive (arg)
  (format t "Expensive! ~A~%" arg)
  (+ arg 1))

(let ((seen (make-hash-table)))
  (defun frugal (arg)
    (let ((result (gethash arg seen)))
      (if (null result)
	  (setf (gethash arg seen) (expensive arg))
	  result))))

;PG Book - Exercise 6.9
(defun apply-octal (fn &rest args)
  (let ((*print-base* 8))
    (apply #'apply fn args)))
	

;PG Book - Exercise 7.1
(defun get-file-lines (fpath)
  (let ((lines ()))
    (with-open-file (stream fpath)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line lines)))
    (nreverse lines)))

;PG Book - Exercise 7.2
(defun make-string-vector ()
  (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))

(defun char-list (string)
  (let ((char-list ()))
    (loop for char being the elements of string
	 until (equal char :eof)
	 do (push char char-list))
    (nreverse char-list)))

(defun get-file-expressions (fpath)
  (let ((sexps ())
	(paren-count 0)
	(paren-started nil)
	(current-sexp (make-string-vector)))
    (dolist (line (get-file-lines fpath))
      (block this-line
	(dolist (char (char-list line))
	  (cond ((char= char #\;) (return-from this-line))
		((char= char #\() (incf paren-count))
		((char= char #\)) (decf paren-count)))
	  (vector-push-extend char current-sexp)
	  (when (> paren-count 0)
	    (setf paren-started t))
	  (when (< paren-count 0)
	    (error "Malformed file"))
	  (when (and (= paren-count 0) paren-started)
	    (push (read-from-string current-sexp) sexps)
	    ;(push current-sexp sexps)
	    (setf current-sexp (make-string-vector))
	    (setf paren-started nil)))))
	(nreverse sexps)))

;PG Book - Exercise 7.3
(defun copy-without-% (in-file out-file)
  (with-open-file (in-stream in-file)
    (with-open-file (out-stream out-file
				:direction :output
				:if-exists :supersede)
      (loop for line = (read-line in-stream nil) 
	 until (null line)
	 do (let ((out-line (make-string-vector)))
	      (dolist (char (char-list line))
		(if (not (char= char #\%))
		    (vector-push-extend char out-line)))
	      (write-line out-line out-stream))))))
      
;PG Book - Exercise 7.4
(defun pretty-print-float-arr (float-arr &optional (stream t))
  (let ((dim (array-dimensions float-arr)))
    (dotimes (i (car dim))
      (dotimes (j (cadr dim))
	(format stream "~10,2F" (aref float-arr i j)))
      (format t "~%"))))

	 
;PG Book - Exercise 10.1
(defun 10.1-forms ()
  (let ((x 'a)
	(y 'b)
	(z '(c d)))
    `((,z ,x z)
      (x ,y ,@z)
      ((,@z a) z))))

;PG Book - Exercise 10.2
(defmacro cond-if (test then &optional (else nil))
  `(cond (,test ,then)
	 (t ,else)))
		 
;PG Book - Exercise 10.3
(defmacro nth-expr (n &rest forms)
  (nth (- n 1) forms))

;PG Book - Exercise 10.4
(defmacro ntimes (n &rest body)
  `(labels ((ntimes-r (cur limit)
	     (if (>= cur limit)
		 nil
		 (progn ,@body
			(ntimes-r (+ 1 cur) limit)))))
    (ntimes-r 0 ,n)))

;PG Book - Exercise 10.5
(defmacro n-of (n form)
  (let ((lst-sym (gensym))
	(i-sym (gensym)))
    `(let ((,lst-sym ()))
      (dotimes (,i-sym ,n)
	(push ,form ,lst-sym))
      (nreverse ,lst-sym))))
      
;PG Book - Exercise 10.6
(defmacro revert-vars (var-list &rest body)
  `(let ,(mapcar #'(lambda (name) `(,name ,name)) var-list)
     ,@body))

;PG Book - Exercise 10.8
(defmacro double (x)
  (let ((result-sym (gensym)))
    `(let ((,result-sym ,x))
       (* 2 ,result-sym))))

;PG Book - Exercise 11.1
(defclass rectangle ()
  ((height :accessor rec-height
	   :initarg :height
	   :initform 0)
   (width :accessor rec-width
	  :initarg :width
	  :initform 0)))
(defclass circle ()
  ((radius :accessor circle-radius
	   :initarg :radius
	   :initform 0)))

(defmethod area ((x rectangle))
  (* (rec-height x) (rec-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

;PG Book - Exercise 11.5
(defparameter *area-global-counter* 0)
(defmethod area :after (x)
  (incf *area-global-counter*))

