(load "../common/common.lisp")

(defun transform (coord direction)
  "Transform complex number according to the direction specified by character in (^, v, <, >)"
  (cond
    ((string= direction "^") (+ coord #C(0 1)))
    ((string= direction "v") (- coord #C(0 1)))
    ((string= direction ">") (+ coord #C(1 0)))
    ((string= direction "<") (- coord #C(1 0)))))

(defun part1 (input)
  (let
      ((ret '()))
    (loop for d across input
          for c = #C(0 0) then (transform c d)
          do (setf ret (adjoin c ret)))
    (format t "Part 1 result: ~a~%" (length ret))))

(defun part2 (input)
  (let
      ((res '(0)))
    (loop for d across input
          for i = 0 then (incf i) ; counter to determine whose turn it is
          for santa = (transform 0 d) then (if (evenp i) (transform santa d) santa)
          for robosanta = #C(0 0) then (if (oddp i) (transform robosanta d) robosanta)
          do (setf res (if (evenp i) (adjoin santa res) (adjoin robosanta res))))
    (format t "Part 2 result: ~a~%" (length res))))

(defun main ()
  (let*
      ((input (car (collect-input "input"))))
    (part1 input)
    (part2 input)))

(main)

