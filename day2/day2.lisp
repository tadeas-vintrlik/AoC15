(load "../common/common.lisp")

(defun parse-box-sizes (string)
  "Return list of length 3 with dimensions of the box."
  (let*
      ((i1 (position #\x string))
       (i2 (position #\x string :start (1+ i1))))
    (list
     (parse-integer-subseq string 0 i1)
     (parse-integer-subseq string (1+ i1) i2)
     (parse-integer-subseq string (1+ i2) (length string)))))

(defun parse-integer-subseq (string start end)
  "Parse integer in a subseq of string determined by start and end."
  (parse-integer
   (subseq string start end) :junk-allowed t))


(defun compute-box-area (size)
  (let
      ((l (car size))
       (w (nth 1 size))
       (h (nth 2 size)))
    (+
     (* 2 l w)
     (* 2 w h)
     (* 2 h l)
     (min
      (* l w)
      (* w h)
      (* l h)))))

(defun compute-ribbon-length (size)
  (let
      ((sorted (sort size #'<)))
    (+
     (* 2 (car sorted))
     (* 2 (nth 1 sorted))
     (* (car sorted) (nth 1 sorted) (nth 2 sorted)))))

(defun part1 (input)
  (format t "Part 1 result: ~a~%"
          (reduce #'+ (mapcar #'compute-box-area input))))

(defun part2 (input)
  (format t "Part 2 result: ~a~%"
          (reduce #'+ (mapcar #'compute-ribbon-length input))))

(defun main()
  (let
      ((input (collect-input "input" #'parse-box-sizes)))
    (part1 input)
    (part2 input)))

(main)
