(load "~/quicklisp/setup.lisp")
(load "../common/common.lisp")
(ql:quickload "md5")

(defun first-5-bytes-zerop (hash)
  (=
   (+
    (logand 255 (aref hash 0))
    (logand 255 (aref hash 1))
    (logand 240 (aref hash 2))) 0))

(defun first-6-bytes-zerop (hash)
  (=
   (+
    (logand 255 (aref hash 0))
    (logand 255 (aref hash 1))
    (logand 255 (aref hash 2))) 0))

(defun part-help (input predicate part)
  "Helper function for both part predicate is the function to end the loop on.
Part is the number to printed in the result"
  (loop for i from 0
        for hash = (md5:md5sum-string (format nil "~a~d" input i))
        when (funcall predicate hash) return (format t "Part ~d result: ~d~%" part i)))

(defun part1 (input)
  (part-help input 'first-5-bytes-zerop 1))

(defun part2 (input)
  (part-help input 'first-6-bytes-zerop 2))

(defun main ()
  (let
      ((input (car (collect-input "input"))))
    (part1 input)
    (part2 input)))

(main)

