(load "../common/common.lisp")

(defparameter +MAX+ 65535)

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

(defun +->+ (ht dest val)
  (setf (gethash dest ht) (parse-integer val))
  )

(defun +AND+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
        (mod
         (logand (gethash arg1 ht) (gethash arg2 ht))
         +MAX+)))

(defun +OR+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
        (mod
         (logior (gethash arg1 ht) (gethash arg2 ht))
         +MAX+)))

(defun +NOT+ (ht dest arg1)
  (setf (gethash dest ht)
        (mod
         (1+ (lognot (gethash arg1 ht)))
         +MAX+)))

(defun +LSHIFT+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
        (mod
         (ash (gethash arg1 ht) (parse-integer arg2))
         +MAX+)))

(defun +RSHIFT+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
        (mod
         (ash (gethash arg1 ht) (- 0 (parse-integer arg2)))
         +MAX+)))

(defun eval-instruction (instruction ht)
  (let
      ((dest (nth 1 instruction))
       (to-eval (cons (intern (format nil "+~a+" (car instruction))) (cons ht (cdr instruction)))))
    (eval to-eval)))

(defun part-1 (input)
  (let
      ((ht (make-hash-table :test 'equal)))
    (dolist (i input)
      (eval-instruction i ht))
    (format t "Part 1 result: ~a~%" (gethash "a" ht))))

(defun main ()
  (let
      ((input (collect-input "input" #'parse-instruction)))
    (format t "~a~%" input)
    (part-1 input)))

(main)
