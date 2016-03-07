(load "othello.el")

(defparameter *ip* "localhost")
(defparameter *port* '10005)

(defun othello()
  (setf *name* 'client)
  (setf my-stream (socket-connect *port* *ip*))
  (init-param)
  (print (if (= 1 *is-my-turn*) 2 1) my-stream)
  (start))
