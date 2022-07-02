;; Returns value of character
(defun char-val (c)
  (if (string= c ")")
      -1
      1))

(defun part-1 (input)
  (format t "Part 1 result: ~d~%" 
          (reduce #'+ input)))

(defun part-2-help (input floor position)
  (if (= floor -1)
      (format t "Part 2 result: ~d~%" position)
      (part-2-help (cdr input) (+ floor (car input)) (1+ position))))

(defun part-2 (input)
  (part-2-help input 0 0))

(defun main ()
  (let
      ((input (mapcar 'char-val (loop :for c :across (read-line) :collect c))))
    (part-1 input)
    (part-2 input)))

;; Compile the code
(sb-ext:save-lisp-and-die "day1" :toplevel #'main :executable t)
