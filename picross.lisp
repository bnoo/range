;;; -*- mode: Lisp; fill-column: 75; -*-
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

(defun curry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append args more-args))))

(defun concat (&rest strings)
  (apply (curry #'concatenate 'string) strings))

(defun array-slice (dimension)
  #'(lambda (arr i)
      (make-array (array-dimension arr dimension)
                  :displaced-to arr
                  :displaced-index-offset
                  (* i (array-dimension arr dimension)))))

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

(defun get-column (board i) (funcall (array-slice 0) board i))
(defun get-row    (board i) (funcall (array-slice 1) board i))

(defun columns (puzzle)
  (let ((board (board puzzle)))
    (iter (for x below (width puzzle))
          (collect (iter (for y below (height puzzle))
                         (format t "~a~%" (aref board y x))
                         (collect (aref board y x)))))))

(defun rows (puzzle)
  (let ((board (board puzzle)))
   (iter (for i below (height puzzle))
         (collect (get-row board i)))))

;;; Printing

(defun print-row-hint (width hint)
  (format t (format nil "~~~s<~{~a~^ ~}~~>|" width hint)))

(defun print-column-hints (indent width hints)
  (labels ((format-data (row)
             (format
              nil (format
                   nil (format
                        nil "~~{~~~s<~~@[~~s~~]~~>~~}" width) row)))
           (format-row (row)
             (concat (repeat-string indent " ")
                     (format-data row)))
           (build-hints (hints acc)
             (let ((current (mapcar #'first hints)))
               (if (every #'null current)
                   acc
                   (build-hints (mapcar #'rest hints)
                                (cons (format-row current) acc)))))
           (make-separator ()
             (concatenate 'string
                          (repeat-string indent " ")
                          (repeat-string (* width (length hints))
                                         "-"))))
    (map nil #'write-line
         (build-hints hints (list (make-separator))))))

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

;;; Verification

(defun verify-line (line hint)
  (labels ((test-hint (n hint)
             (= (first hint) n))
           (helper (current-block line hint)
             (cond ((null line)
                    (or (null hint)
                        (and (= (length hint) 1)
                             (test-hint current-block hint))))
                   ((null hint) (every #'zerop line))
                   ((zerop (first line))
                    (if (zerop current-block)
                        (helper 0 (rest line) hint)
                        (when (test-hint current-block hint)
                          (helper 0 (rest line) (rest hint)))))
                   ((= (first line) 1)
                    (helper (1+ current-block) (rest line) hint))
                   (t (error "This should never happen.")))))
    (helper 0 (coerce line 'list) hint)))

(defun verify-puzzle (puzzle)
  (let ((row-hints    (row-hints puzzle))
        (column-hints (mapcar #'reverse (column-hints puzzle))))
    (iter (for line in (append (rows puzzle) (columns puzzle)))
          (for hint in (append row-hints     column-hints))
          (unless (verify-line line hint)
            (return-from verify-puzzle nil)))
    t))

;;; Samples

(defvar b (make-puzzle '((2 2) (2 2) (2 2) (2 2) (2 2))
                       '((5) (5) () (5) (5))))

(defvar c (make-puzzle '((10) (8 1) (7 2) (6 3) (5 4) (1 1 1))
                       '((6) (5) (6) (5) (6)
                         (4) (3 1) (2 2) (1 3) (5))))

(defun reset-c ()
  (setf c (make-puzzle '((10) (8 1) (7 2) (6 3) (5 4) (1 1 1))
                       '((6) (5) (6) (5) (6)
                         (4) (3 1) (2 2) (1 3) (5)))))

(defun solve-c ()
  (let ((solution '((1  1  1  1  1  1)
                    (1  1  1  1  1  0)
                    (1  1  1  1  1  1)
                    (1  1  1  1  1  0)
                    (1  1  1  1  1  1)
                    (1  1  1  1  0  0)
                    (1  1  1  0  1  0)
                    (1  1  0  1  1  0)
                    (1  0  1  1  1  0)
                    (1  1  1  1  1  0))))
    (iter (for solved-row in solution)
          (for y below (height c))
          (iter (for space in solved-row)
                (for x below (width c))
                (setf (aref (board c) y x) space)))))

;;; Playing

(defun poke (space puzzle x y)
  (check-type space bit)
  (setf (aref (board puzzle) y x) space))

(defun toggle (puzzle x y)
  (symbol-macrolet ((space (aref (board puzzle) y x)))
    (setf space (if (zerop space) 1 0))))
