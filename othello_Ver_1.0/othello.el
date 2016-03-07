(defparameter *board* (make-array 100))
(defparameter *is-my-turn* nil)
(defparameter *my-color* nil)
(defparameter *enemy-color* nil)
(defparameter my-stream nil)
(defparameter *name* nil)

(defun randval(n)
  (1+ (random (max 1 n))))

(defun start()
  (init-board)
  (game-loop))

;;ゲームループ
(defun game-loop()
  (show-board *board*)
  (if (game-endp)
	  (game-result *my-color*)
	(progn (if (= *is-my-turn* 1)
			   (my-turn)
			 (enemy-turn))
		   (game-loop))))

;;初期化
(defun init-board()
  (put-stone 44 'w)
  (put-stone 45 'b)
  (put-stone 54 'b)
  (put-stone 55 'w)
  (loop for i from 0
		until (> i 99)
		when (not (board-nump i))
		do (put-stone i 't)))

(defun init-param()
  (when (equal *name* 'client)
	(setf *is-my-turn* (randval 2)))
  (if (= 1 *is-my-turn*)
	  (progn (setf *my-color* 'b)
			 (setf *enemy-color* 'w))
	(progn (setf *my-color* 'w)
		   (setf *enemy-color* 'b))))


(defun show-board(board)
  (let ((x 0))
	(fresh-line)
	(princ "  12345678")
	(fresh-line)
	(princ "-+--------")
	(fresh-line)
	(mapc #'(lambda(m)
			  (if (and (> x 9)(< x 81)
					   (= 0 (mod x 10)))
				 (progn (fresh-line)
						(princ (/ x 10))
						(princ "|"))
			   t)
			 (if (board-nump x)
				 (cond ((equal m 'w)(princ "o"))
					   ((equal m 'b)(princ "x"))
					   (t           (princ ".")))
			   nil)
			 (incf x))
		  (coerce board 'list)))
  (fresh-line))

(defun my-turn()
  (format t "your-turn(~a)" (if (equal *my-color* 'b) 'x 'o))
  (let ((x (select-pos *my-color*)))
	(print x my-stream)
	(if (not (= x 0))
		(progn (put-stone x *my-color*)
			   (flip-stones x *my-color*))
	  (princ "pass"))
  (end-turn)))

(defun enemy-turn()
  (format t "enemy-turn(~a)" (if (equal *enemy-color* 'b) 'x 'o))
  (let ((x (read my-stream)))
	(if (not (= x 0))
		(progn (put-stone x *enemy-color*)
			   (flip-stones x *enemy-color*))
	  (princ "pass"))
  (end-turn)))

(defun end-turn()
  (if(= *is-my-turn* 1)
	  (setf *is-my-turn* 2)
	(setf *is-my-turn* 1)))


(defun put-stone(pos col)
  (setf (aref *board* pos) col))

(defun select-pos(col)
  (fresh-line)
  (princ ">>")
  (let((n (read)))
	(if (= n 0)
		n
	  (if (and (numberp n)
			   (board-nump n))
		  (if (and (not (null (member t (list-put-stonep n col))))
				   (equal (aref *board* n ) nil))
			  n
			(progn (princ "you can't put stone that cell.")
				   (select-pos col)))
		(progn (princ "that cell which doesn't exist.")
			   (select-pos col))))))

;;8方向それぞれで石が置けるかの判定のリスト
(defun list-put-stonep(pos col)
  (let ((dir '(-10 -9 1 11 10 9 -1 -11)))
	(mapcar (lambda(d)
			  ;;各方向dを元にリストlstを作る
			  ;;lst は中心点から指定された方向のセル番号を並べたリスト
			  (let ((lst (loop for i from 1
							   until (not (board-nump (+ pos (* d i)))) 
							   collect (+ pos (* d i)))))
				;;lstから盤面を見て石の配置を表すリストslstを作る
				(let ((slst
					   ;;一旦lにセル番号を元に作ったセルの中身のリストのを作る
					   (let ((l (mapcar (lambda(n)
										  (aref *board* n))
										lst)))
						 ;;先頭から見ていき何も入っていないセルが見つかるまでのリストを作る
						 ;;先頭がnilの時は何も入らない
						 (labels ((a(n)
									(if (null (car n))
										nil
									  (cons (car n)(a (cdr n))))))
								 (a l)))))
				  ;;slstから置けるかどうかの判定
				  ;;判定は先頭が相手の色の石であるかつリストの２番目以降に自分の色の石が存在している時にtになる
				  (let ((op-col (if (equal col 'w) 'b 'w)))
					;;slstがnilでないとき
					(if slst
						(and (equal (car slst) op-col)
							 (not (null (member col slst))))
					  nil)
					))))
			dir)))

;;石を返す処理
(defun flip-stones(pos col)
  (let ((dir '(-10 -9 1 11 10 9 -1 -11))
		(clist (list-put-stonep pos col)))
	(labels ((flip(dl cl)
				  ;;dirとclistの先頭の要素を取り出す
				  (let ((d (car dl))
						(c (car cl))
						(op-col (if (equal col 'w) 'b 'w))) 
					;;cがnilでなければ(その方向で石を返すことが可能)処理を行う
					(when c
					  ;;順番にたどっていき相手の石でなくなるまで自分の石に上書きを行う
					  (loop for i from 1
							until (> i 8)
							when (and (board-nump (+ pos (* d i)))
									  (equal op-col (aref *board* (+ pos (* d i)))))
							do (put-stone (+ pos (* d i)) col)))
					;;リストに2番め以降が存在するときに(a (cdr d)(cdr c))呼び出し
					(when (cdr cl)
					  (flip (cdr dl) (cdr cl))))))
			(flip dir clist))))

(defun board-nump(n)
  (and (> n 10)(< n 89)
	   (not (= (mod n 10) 0))
	   (not (= (mod n 10) 9))))

(defun game-endp()
  (let ((list (coerce *board* 'list)))
		(null (member nil list))))


(defun game-result(col)
  (let ((my-stone 0)(op-stone 0)
		(my-col (if (equal col 'w) 'w 'b))
		(op-col (if (equal col 'w) 'b 'w)))
	(loop for i from 0
		  below 100
		  do (let ((s (aref *board* i)))
			   (cond ((equal s my-col)(incf my-stone))
					 ((equal s op-col)(incf op-stone)))))
	(format t "Your-stone ~a~%Enemy-stone ~a~%" my-stone op-stone)))

(defun put-stones(list col)
  (mapc (lambda(s)
		  (put-stone s col))
		list))
