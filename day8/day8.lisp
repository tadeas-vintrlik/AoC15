(ql:quickload "cl-ppcre")

(defun collect-input-unescaped (filename)
  (with-open-file (file filename)
    (loop
      for x = (read-char file nil 'eof)
      with line = ""
      with res = '()
      while (not (equal 'eof x))
      do (cond
           ((char= #\Newline x) (progn (setf res (cons line res)) (setf line "")))
           (t (setf line (concatenate 'string line (coerce (vector x) '(string 1))))))
      finally (return res))))

(defun code-length (input)
  (reduce #'+ (mapcar #'length input)))

(defun unescaped-char-count (str)
  (- (length (cl-ppcre:regex-replace-all "\\\\x..|\\\\\"|\\\\\\\\" str "@")) 2))

(defun escaped-char-count (str)
  (+ (length (cl-ppcre:regex-replace-all "\"|\\" str "@@")) 2))

(defun unescaped-length (input)
  (reduce #'+ (mapcar #'unescaped-char-count input)))

(defun escaped-length (input)
  (reduce #'+ (mapcar #'escaped-char-count input)))

(defun part-1 (input)
  (format t "Part 1 solution: ~a~%" (- (code-length input) (unescaped-length input))))

(defun part-2 (input)
  (format t "Part 2 solution: ~a~%" (- (escaped-length input) (code-length input))))

(defun main()
  (let
      ((input (collect-input-unescaped "input")))
    (part-1 input)
    (part-2 input)))

(main)
