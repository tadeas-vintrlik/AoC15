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

(defun get-arg-value (arg ht)
  (cond
    ((parse-integer arg :junk-allowed t) (parse-integer arg))
    (t (get-wire-value arg ht))))

(defun +->+ (ht dest val)
  (setf (gethash dest ht) (get-arg-value val ht)))

(defun +AND+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
         (logand (get-arg-value arg1 ht) (get-arg-value arg2 ht))))

(defun +OR+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
         (logior (get-arg-value arg1 ht) (get-arg-value arg2 ht))))

(defun +NOT+ (ht dest arg1)
  (setf (gethash dest ht)
         (1+ (mod (lognot (get-arg-value arg1 ht)) +MAX+))))

(defun +LSHIFT+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
         (ash (get-arg-value arg1 ht) (get-arg-value arg2 ht))))

(defun +RSHIFT+ (ht dest arg1 arg2)
  (setf (gethash dest ht)
         (ash (get-arg-value arg1 ht) (- 0 (get-arg-value arg2 ht)))))

(defun get-wire-value (name ht)

  (cond
    ;; If already a value in the hash table return it
    ((and (gethash name ht) (numberp (gethash name ht))) (gethash name ht))
    ;; Otherwise it needs to be computed
    (t (let*
        ((instruction (gethash name ht))
         (to-eval (cons
                  (intern (format nil "+~a+" (car instruction)))
                  (cons ht (cdr instruction)))))
        (eval to-eval)))))

(defun part-1 (input)
  (let
      ((ht (make-hash-table :test 'equal)))
    (dolist (i input)
      (cond
        ((and (string= (car i) "->") (parse-integer (nth 2 i) :junk-allowed t))
         (setf (gethash (nth 1 i) ht) (parse-integer (nth 2 i))))
        (t (setf (gethash (nth 1 i) ht) i))))
    (format t "Part 1 result: ~a~%" (get-wire-value "a" ht))
    (get-wire-value "a" ht)))

(defun part-2 (part-1-res input)
  (let
      ((ht (make-hash-table :test 'equal)))
    (dolist (i input)
      (cond
        ((and (string= (car i) "->") (parse-integer (nth 2 i) :junk-allowed t))
         (setf (gethash (nth 1 i) ht) (parse-integer (nth 2 i))))
        (t (setf (gethash (nth 1 i) ht) i))))
    (setf (gethash "b" ht) part-1-res)
    (format t "Part 2 result: ~a~%" (get-wire-value "a" ht)))
)

(defun main ()
  (let
      ((input (collect-input "input" #'parse-instruction)))
    (part-2 (part-1 input) input)))

(main)
