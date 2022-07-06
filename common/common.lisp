(defun collect-input (file &optional clb)
  "Collect all lines from a file into a list. clb is a function to be called on each line before saving"
  (with-open-file (in file)
    (loop
      for line = (read-line in nil 'eof)
      until (eq line 'eof)
      collect (if clb
                  (funcall clb line)
                  line))))
