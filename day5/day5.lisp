(load "../common/common.lisp")

(defun 3-vowelsp (string)
  (<= 3
      (+
       (count #\a string)
       (count #\e string)
       (count #\i string)
       (count #\o string)
       (count #\u string))))

(defun collect-double-letter (string)
  "Returns list of all substring of length 2."
  (loop for i from 1
        until (> (1+ i) (length string))
        collecting (format nil "~c~c" (char string (1- i)) (char string i))))

(defun two-double-letterp (string)
  "Checks if contains at least a pair of any two non-overlaping characters."
  (let
      ((double-letter-list (collect-double-letter string)))
    (loop for i from 0
          until (> (+ 2 i) (length double-letter-list))
          ;; Collecting from subseq + 2 to not include overlap
          when (find (nth i double-letter-list) (subseq double-letter-list (+ 2 i)) :test #'equal) collect (nth i double-letter-list))))

(defun sandwich-letterp (string)
  "Return if it contains two identical letters around any letter. Such as yxy."
  (loop for last2 = nil then last
        for last = nil then x
        for x across string
        when (and last2 last (char= last2 x)) return 't))

(defun double-letterp (string)
  "Return if it contains at least one double letter. Such as aa."
  (loop for last = nil then x
        for x across string
        when (and last (char= x last)) return 't))

(defun disallowedp (string)
  "Return if string is one of the disallowed."
  (or
   (string= string "ab")
   (string= string "cd")
   (string= string "pq")
   (string= string "xy")))

(defun not-contains-disallowedp (string)
  "Return if string contains any of the disallowed characters."
  (loop for last = nil then x
        for x across string
        when (and last (disallowedp (format nil "~c~c" last x))) return nil
          finally (return 't)))

(defun nice-string (string)
  (and
   (3-vowelsp string)
   (double-letterp string)
   (not-contains-disallowedp string)))

(defun nice-string-part2 (string)
  (and
   (sandwich-letterp string)
   (two-double-letterp string)))

(defun part-help (input nice-func part)
  (loop for x in input
        counting (funcall nice-func x) into ret
        finally (format t "Part ~d result: ~d~%" part ret)))

(defun part1 (input)
  (part-help input #'nice-string 1))

(defun part2 (input)
  (part-help input #'nice-string-part2 2))

(defun main ()
  (let
      ((input (collect-input "input")))
    (part1 input)
    (part2 input)))

(main)
