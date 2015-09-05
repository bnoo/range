(ql:quickload :iterate)
(defpackage :picross
  (:use :cl :iterate))
(in-package :picross)

;;; Util

(defun digits (n)
  "Return the number of digits in N."
  (length (format nil "~s" n)))

(defun repeat-string (n string)
  "Return a string consisting of N copies of STRING."
  (with-output-to-string (out)
    (dotimes (i n)
      (princ string out))))

;;; Puzzle manipulation

(defclass puzzle ()
  ((columns :initarg  :column-hints
	    :accessor column-hints)
   (rows    :initarg  :row-hints
	    :accessor row-hints)
   (height  :initarg  :height
	    :reader   height)
   (width   :initarg  :width
	    :reader   width)
   (board   :initarg  :board
	    :reader   board
	    :initform (error "Must provide board"))))

(defun make-puzzle (column-hints row-hints)
  (let* ((height  (length row-hints))
	 (width   (length column-hints))
	 (board   (make-array (list height width) :element-type 'bit))
	 (columns (mapcar #'reverse column-hints)))
    (make-instance 'puzzle
		   :column-hints columns
		   :row-hints    row-hints
		   :height       height
		   :width        width
		   :board        board)))

(defun get-row (board i)
  (make-array (array-dimension board 1)
	      :displaced-to board
	      :displaced-index-offset (* i (array-dimension board 1))))

;;; Printing

(defun print-row-hint (width hint)
  (format t (format nil "~~~s<~{~a~^ ~}~~>|" width hint)))

(defun print-column-hints (indent width hints)
  (labels ((format-row (row)
	     (concatenate
	      'string
	      (repeat-string indent " ")
	      (format
	       nil (format
		    nil (format
			 nil "~~{~~~s<~~@[~~s~~]~~>~~}" width) row))))
	   (build-hints (hints acc)
	     (let ((current (mapcar #'first hints)))
	       (if (every #'null current)
		   acc
		   (build-hints (mapcar #'rest hints)
				(cons (format-row current) acc)))))
	   (make-separator ()
	     (concatenate 'string
			  (repeat-string indent " ")
			  (repeat-string (* width (length hints)) "-"))))
    (map nil #'write-line (build-hints hints (list (make-separator))))))

(defun print-row (column-width row)
  (format t (format nil "~~{~~~s<~~s~~>~~}~~%" column-width)
	  (coerce row 'list)))

(defun print-puzzle (puzzle)
  (labels ((hint-length (hint)
	     ;; We subtract 2 for the opening and closing brackets
	     (- (length (format nil "~s" hint)) 2))
	   (longest-row-hint (hints)
	     (apply #'max (mapcar #'hint-length hints))))
   (let* ((board        (board puzzle))
	  (rows         (row-hints puzzle))
	  (columns      (column-hints puzzle))
	  (column-width (1+ (digits (height puzzle))))
	  (x-hint-width (longest-row-hint rows)))
     (print-column-hints (1+ x-hint-width) column-width columns)
     (iter (for row in rows)
	   (for i upfrom 0)
	   (print-row-hint x-hint-width row)
	   (print-row column-width (get-row board i))))))

;;; Samples

(defvar b (make-puzzle '((2 2) (2 2) (2 2) (2 2) (2 2))
		       '((5) (5) () (5) (5))))

(defvar c (make-puzzle '((10) (8 1) (7 2) (6 3) (5 4) (1 1 1))
		       '((6) (5) (6) (5) (6)
			 (4) (3 1) (2 2) (1 3) (4))))

