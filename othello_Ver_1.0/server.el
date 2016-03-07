(load "othello.el")

(defparameter *port* '10005)
(defparameter my-socket nil)

(defun othello()
  (setf *name* 'server)
  (setf my-socket (socket-server *port*))
  (setf my-stream (socket-accept my-socket))
  (setf *is-my-turn* (read my-stream))
  (init-param)
  (start))
