(load "../common/common.lisp")

(defun apply-instruction (hash-table instruction)
  (loop for x from (parse-integer (nth 1 instruction)) to (parse-integer (nth 3 instruction))
        do (loop for y from (parse-integer (nth 2 instruction)) to (parse-integer (nth 4 instruction))
                 do (cond
                      ((string= (car instruction) "on") (setf (gethash (complex x y) hash-table) t))
                      ((string= (car instruction) "off") (setf (gethash (complex x y) hash-table) nil))
                      ((string= (car instruction) "toggle") (setf (gethash (complex x y) hash-table) (not (gethash (complex x y) hash-table))))))))


(defun parse-ranges (line)
  (let*
      ((splitted (split-string line #\ ))
       (trimmed (if (string= (car splitted) "turn") (cdr splitted) splitted)))
    (append
     (list (first trimmed))
     (split-string (nth 1 trimmed) #\,)
     (split-string (nth 3 trimmed) #\,))))

(defun part-1 (data)
  (let
      ((ht (make-hash-table)))
    (dolist (d data)
      (apply-instruction ht d))
    (format t "Part 1 result: ~A~%"
            (loop for value being the hash-values of ht
                    using (hash-key key)
                  counting value))))


(defun main ()
  (let
      ((input (collect-input "input" #'parse-ranges)))
  (part-1 input)))

(main)
