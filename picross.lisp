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
  "Concatenate STRINGS."
  (apply (curry #'concatenate 'string) strings))


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
  "Return a blank PUZZLE associated with COLUMN-HINTS and ROW-HINTS."
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

(defun get-row (puzzle i)
  "Return the Ith row of PUZZLE, with 0 being the topmost row."
  (let ((board (board puzzle)))
    (iter (for x below (width puzzle))
          (collect (aref board i x)))))

(defun get-column (puzzle i)
  "Return the Ith column of PUZZLE, with 0 being the leftmost column."
  (let ((board (board puzzle)))
    (iter (for y below (height puzzle))
          (collect (aref board y i)))))

(defun columns (puzzle)
  "Returns a list of all the columns in PUZZLE, left to right."
  (iter (for x below (width puzzle))
        (collect (get-column puzzle x))))

(defun rows (puzzle)
  "Returns a list of all the rows in PUZZLE, top to bottom."
  (iter (for y below (height puzzle))
        (collect (get-row puzzle y))))

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
   (let* ((rows         (row-hints puzzle))
          (columns      (column-hints puzzle))
          (column-width (1+ (digits (height puzzle))))
          (x-hint-width (longest-row-hint rows)))
     (print-column-hints (1+ x-hint-width) column-width columns)
     (iter (for row in rows)
           (for i upfrom 0)
           (print-row-hint x-hint-width row)
           (print-row column-width (get-row puzzle i))))))

;;; Verification

(defun verify-line (line hint)
  "Return T if LINE is described by HINT, otherwise NIL."
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
  "Return T if PUZZLE is completely solved, otherwise NIL."
  (let ((row-hints    (row-hints puzzle))
        (column-hints (mapcar #'reverse (column-hints puzzle))))
    (iter (for line in (append (rows puzzle) (columns puzzle)))
          (for hint in (append row-hints     column-hints))
          (unless (verify-line line hint)
            (return-from verify-puzzle nil)))
    t))

;;; Playing

(defun poke (space puzzle x y)
  "Set (X, Y) in PUZZLE to SPACE. SPACE must be 0 or 1."
  (check-type space bit)
  (setf (aref (board puzzle) y x) space))

(defun toggle (puzzle x y)
  "If (X, Y) in PUZZLE is 1, set it to 0; if it is 0, set it to 1."
  (symbol-macrolet ((space (aref (board puzzle) y x)))
    (setf space (if (zerop space) 1 0))))

