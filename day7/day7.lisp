(load "../common/common.lisp")

(defun reorder-list (list indexes)
  "Reorder the list in the order determined by indexes.
   Does not handle one of the indexes not indexing the list."
  (let
      ((res '())
       (rev-index (reverse indexes)))
    (dolist (i rev-index)
      (push (nth i list) res))
    res))

(defun infix-instruction (instruction)
  "Convert instruction into polish notation:
   '(instruction-name destination arg1 ...)."
  (let
      ((l (length instruction)))
    (cond
      ((= l 3) (reorder-list instruction '(1 2 0)))
      ((= l 4) (reorder-list instruction '(0 3 1)))
      ((= l 5) (reorder-list instruction '(1 4 0 2))))))

(defun parse-instruction (line)
  (infix-instruction
   (split-string line #\ )))

(defun main ()
  (let
      ((input (collect-input "input" #'parse-instruction)))
    (format t "~a~%" input)))

(main)
