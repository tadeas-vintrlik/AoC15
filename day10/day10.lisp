(load "../common/common.lisp")
(ql:quickload "cl-ppcre")

(defun look-say-n (line n)
  (when (= 0 n)
    (return-from look-say-n line))
  (look-say-n
   (with-output-to-string (stream)
    (cl-ppcre:do-matches-as-strings (match "(.)\\1*" line)
      (format stream "~a~a" (length match) (string (char match 0)))))
   (1- n)))

(defun part-1 (input)
  (format t "Part 1 result: ~a~%" (length (look-say-n input 40))))

(defun part-2 (input)
  (format t "Part 2 result: ~a~%" (length (look-say-n input 50))))

(defun main()
  (let
      ((input (car (collect-input "input"))))
    (part-1 input)
    (part-2 input)))

(main)
