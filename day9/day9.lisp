(load "../common/common.lisp")
(ql:quickload "cl-ppcre")

(defun parse-distances (line)
  (cl-ppcre:register-groups-bind (place1 place2 distance)
      ("(\\w+) to (\\w+) = (\\d+)" line)
    (list place1 place2 distance)))

(defun load-graph (graph input)
  "Fill the hash-table graph with nodes from input.
Tgethash neighbour graphhe value stored for each node is a list of tuples containing the destination and the distance to it"
  (dolist (i input)
    (let
        ((place1 (nth 0 i))
         (place2 (nth 1 i))
         (dist   (parse-integer (nth 2 i))))
      (setf (gethash "start" graph) (adjoin (list place1 0) (gethash "start" graph) :test 'equal))
      (setf (gethash "start" graph) (adjoin (list place2 0) (gethash "start" graph) :test 'equal))
      (setf (gethash place1 graph) (cons (list place2 dist) (gethash place1 graph)))
      (setf (gethash place2 graph) (cons (list place1 dist) (gethash place2 graph))))))

(defun dfs-r (edge graph visited distance distances)
  "Recursive depth first search computing the distances of all Hamilton routes."
  (setf visited (cons edge visited))
  (let*
      ((neighbours (gethash edge graph))
       (unvisited (remove-if #'(lambda (x) (find (car x) visited :test 'equal)) neighbours)))
    (unless unvisited
      (return-from dfs-r (cons distance distances)))
    (dolist (un unvisited)
      (setf distances (dfs-r (car un) graph visited (+ distance (nth 1 un)) distances)))
    (return-from dfs-r distances)))

(defun part-1 (graph)
  (format t "Part 1 result: ~a~%" (apply #'min (dfs-r "start" graph (list "start") 0 '()))))

(defun part-2 (graph)
  (format t "Part 2 result: ~a~%" (apply #'max (dfs-r "start" graph (list "start") 0 '()))))


(defun main ()
  (let
      ((input (collect-input "input" #'parse-distances))
       (graph (make-hash-table :test 'equal)))
    (load-graph graph input)
    (part-1 graph)
    (part-2 graph)))

(main)

