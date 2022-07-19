(defun collect-input (file &optional clb)
  "Collect all lines from a file into a list. clb is a function to be called on each line before saving"
  (with-open-file (in file)
    (loop
      for line = (read-line in nil 'eof)
      until (eq line 'eof)
      collect (if clb
                  (funcall clb line)
                  line))))

(defun split-string (string delim)
  "Return a list of string split by delimiter."
  (let
      ((end (position delim string)))
    (if (not end)
        (list string)
        (append
         (list (subseq string 0 end))
         (split-string (subseq string (1+ end)) delim)))))

;;; Unit Testing Framework
;;; Taken from chapter 9 of Practical Common Lisp
;;; Slightly modified to show the value returned and expected
(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  "Macro building macro for generating symbols for list of names."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Expects lists of two where one is the expected value and other is the expression to test."
  `(combine-results
     ,@(loop for f in forms collect
             `(let
                  ((expected (eval (first ',f)))
                   (got-expr (second ',f))
                   (got-val (eval (second ',f))))
                (report-result got-expr expected got-val)))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (form expected got)
  "Report the results of a single test case. Called by 'check'."
  (let ((result (equal expected got)))
    (format t "~:[FAIL~;pass~] ... ~a: ~a ~:[(expected: ~a got: ~a)~;~]~%" result *test-name* form result expected got)
    result))

(deftest split-string-test ()
  (check
    ('("hello" "world") (split-string "hello world" #\ ))
    ('("hello" "world") (split-string "helloxworld" #\x))
    ('("helloxworld") (split-string "helloxworld" #\ ))
    ('("hello" "world" "") (split-string "hello world " #\ ))))
