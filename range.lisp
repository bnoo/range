(defmacro if-let (condition bindings &body body)
  "Like LET, but uses CONDITION to select alternatives from BINDINGS.

If CONDITION evaluates to non-NIL, bind the CAR of each element of BINDINGS
to its CADR in BODY, otherwise bind to its CADDR. Each element of BINDINGS
should be in the form (VAR TRUE-FORM FALSE-FORM)."
  (labels ((car-and (n)
	     #'(lambda (l)
		 (list (car l) (nth n l)))))
    `(if ,condition
	 (let ,(mapcar (car-and 1) bindings) ,@body)
	 (let ,(mapcar (car-and 2) bindings) ,@body))))

(defun generate-range (start end &optional (step 1))
  "Generates a list from START to END increasing by STEP.

STEP should always be positive; to generate a descending list call with END
less than START.

The generated list will include from START to at most 1 less than END, or for
decreasing lists no less than 1 more than END."
  (assert (> step 0) (step) "Step ~S not greater than zero." step)
  (if-let (< start end)
    ((reverse nil   t)
     (bottom  start (+ end   step))
     (top     end   (+ start step)))
    (let ((l (loop for i from bottom below top by step collecting i)))
      (if reverse (nreverse l) l))))

(set-macro-character #\} (get-macro-character #\)))

(set-dispatch-macro-character
 #\# #\{
 #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((spec (read-delimited-list #\} stream t)))
       `',(if (= (length spec) 1)
	      (generate-range 0 (car spec))
	      (apply #'generate-range spec)))))

;; #{5} => '(0 1 2 3 4 5)
;; #{10 2 2} => '(10 8 6 4)
;; etc
