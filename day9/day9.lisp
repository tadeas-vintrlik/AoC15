(load "../common/common.lisp")
(ql:quickload "cl-ppcre")

(defun parse-distances (line)
  (cl-ppcre:register-groups-bind (place1 place2 distance)
      ("(\\w+) to (\\w+) = (\\d+)" line)
    (list place1 place2 distance)))

(defun load-graph (graph input)
  "Fill the hash-table graph with nodes from input.
The value stored for each node is a list of tuples containing the destination and the distance to it"
  (dolist (i input)
    (let
        ((place1 (nth 0 i))
         (place2 (nth 1 i))
         (dist   (nth 2 i)))
      (setf (gethash place1 graph) (cons (list place2 dist) (gethash place1 graph)))
      (setf (gethash place2 graph) (cons (list place1 dist) (gethash place2 graph))))))

(defun main ()
  (let
      ((input (collect-input "test" #'parse-distances))
       (graph (make-hash-table :test 'equal)))
   (load-graph graph input)))

(main)

