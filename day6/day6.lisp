(load "../common/common.lisp")

(defun apply-instruction (hash-table instruction)
  (loop for x from (nth 1 instruction) to (nth 3 instruction)
        do (loop for y from (nth 2 instruction) to (nth 4 instruction)
                 do (let
                        ((c (complex x y)))
                      (cond
                        ((string= (car instruction) "on") (setf (gethash c hash-table) t))
                        ((string= (car instruction) "off") (setf (gethash c hash-table) nil))
                        ((string= (car instruction) "toggle") (setf (gethash c hash-table) (not (gethash c hash-table)))))))))

(defun apply-instruction-part2 (hash-table instruction)
  (loop for x from (nth 1 instruction) to (nth 3 instruction)
        do (loop for y from (nth 2 instruction) to (nth 4 instruction)
                 do (let
                        ((c (complex x y)))
                      (unless (gethash c hash-table)
                        (setf (gethash c hash-table) 0))
                      (cond
                        ((string= (car instruction) "on") (incf (gethash c hash-table)))
                        ((and (string= (car instruction) "off") (< 0 (gethash c hash-table)) (decf (gethash c hash-table))))
                        ((string= (car instruction) "toggle") (setf (gethash c hash-table) (+ 2 (gethash c hash-table)))))))))

(defun parse-ranges (line)
  (let*
      ((splitted (split-string line #\ ))
       (trimmed (if (string= (car splitted) "turn") (cdr splitted) splitted)))
    (append
     (list (first trimmed))
     (mapcar #'parse-integer (split-string (nth 1 trimmed) #\,))
     (mapcar #'parse-integer (split-string (nth 3 trimmed) #\,)))))

(defun part-1 (data)
  (let
      ((ht (make-hash-table)))
    (dolist (d data)
      (apply-instruction ht d))
    (format t "Part 1 result: ~A~%"
            (loop for value being the hash-values of ht
                    using (hash-key key)
                  counting value))))

(defun part-2 (data)
  (let
      ((ht (make-hash-table)))
    (dolist (d data)
      (apply-instruction-part2 ht d))
    (format t "Part 2 result: ~A~%"
            (loop for value being the hash-values of ht
                    using (hash-key key)
                  summing value))))

(defun main ()
  (let
      ((input (collect-input "input" #'parse-ranges)))
    (part-1 input)
    (part-2 input)))

(main)
