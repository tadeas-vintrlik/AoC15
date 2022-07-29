(defun map-three-increasing-straightp (string)
  (loop for last2 = nil then last
        for last = nil then x
        for x across string
        do (when (and x last last2 (three-increasing-straightp last2 last x))
             (return-from map-three-increasing-straightp t)))
  nil)

  
(defun three-increasing-straightp (c1 c2 c3)
  (let
      ((cc1 (char-code c1))
       (cc2 (char-code c2))
       (cc3 (char-code c3)))
    (and (= cc1 (1- cc2)) (= cc2 (1- cc3)))))

(defun forbidden-charactersp (str)
  (or
   (position #\i str)
   (position #\o str)
   (position #\l str)))

(defun two-double-letterp (string)
  "Return if it contains at least two double letter. Such as aa."
  (<= 2
     (loop for i = 0 then (1+ i)
           with max = (1- (length string))
           with ret = 0
           until (<= max i)
           finally (return ret)
           do (when (char= (char string i) (char string (1+ i)))
                (progn
                  (incf ret)
                  (incf i))))))

(defun valid-passwordp (string)
  (and
   (map-three-increasing-straightp string)
   (not (forbidden-charactersp string))
   (two-double-letterp string)))

(defun inc-char (c)
  (code-char (1+ (char-code c))))

(defun inc-string-r (str length index)
  (let*
      ((replace (inc-char (char str index)))
       (ret 
         (with-output-to-string (stream)
           (when (char= replace #\{)
             (setf replace #\a))
           (format stream "~a~a~a"
                   (subseq str 0 index)
                   (string replace)
                   (subseq str (1+ index) length)))))
    (when (char= replace #\a)
      (setf ret (inc-string-r ret length (1- index))))
    ret))

(defun inc-string (str)
  (let
      ((len (length str)))
    (inc-string-r str len (1- len))))

(defun part-x (str part)
  (loop with s = str
        until (valid-passwordp s)
        finally (progn
                  (format t "Part ~a result: ~a~%" part s)
                  (return-from part-x (inc-string s)))
        do (setf s (inc-string s))))

(defun main ()
  (with-open-file (in "input")
    (let
        ((input (read-line in)))
      (part-x (part-x input 1) 2))))

(main)
