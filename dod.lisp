;;; ==========================================================================
;;; ゲーム情報
;;; ==========================================================================

;{{{
;;; ゲーム情報
(defparameter *num-players* 2)  ; プレイヤー数
(defparameter *max-dice* 3)     ; 1マスにおけるサイコロの最大数
(defparameter *board-size* 2)   ; 1辺のマスの数
(defparameter *board-hexnum* (* *board-size* *board-size*)) ; 盤上のマス数
;}}}

;;; 2x2のゲーム盤
; 座標
; [0][1]
; [2][3]
; リスト表現
; ((プレイヤーID サイコロ個数) ...)
; ((0 3) (0 3) (1 3) (1 1))
; [3][3]
; [3][1]


;;; ==========================================================================
;;; DIRTY IMPERATIVE
;;; ==========================================================================

;;; ゲーム盤初期化・ゲーム盤描画 ---------------------------------------------

(defun gen-board ();{{{
  "ゲーム盤をランダムに作る
   ret: ランダムで生成したゲーム盤(配列)"
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))
;}}}

(defun draw-board (board);{{{
  "ゲーム盤を画面に表示する
   board: ゲーム盤(配列)
   ret: -"
  ;; 行ごとに表示する
  (loop for y below *board-size*
        do (progn (fresh-line)
                  ;; 斜めに表示されるようにするため、行に合わせて空白を入れる
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        ;; 表示するマス目の情報をhexに代入する
                        for hex = (aref board (+ x (* *board-size* y)))
                        ;; 「プレイヤーID-サイコロの個数」のフォーマットでマスの情報を表示
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))
;}}}

;;; ゲームループ -------------------------------------------------------------

(defun play-vs-human (tree);{{{
  "対人専用ゲームループ
   tree: 現在のゲーム木
   ret: -"
  ;; ゲーム状態を表示
  (print-info tree)
  (if (caddr tree)
      ;; 有効な指し手があれば次の指し手を促す
      (play-vs-human (handle-human tree))
      ;; 有効な指し手が無ければ勝者を表示して終了する
      (announce-winner (cadr tree))))
;}}}

;;; ゲームの状態を表示する ---------------------------------------------------

(defun print-info (tree);{{{
  "現在のゲーム木が指すゲームの状態を表示する
   tree: 現在のゲーム木
   ret: -"
  (fresh-line)
  (format t "corrent player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))
;}}}

;;; 人間のプレイヤーからの入力を処理する -------------------------------------

(defun handle-human (tree);{{{
  "人間のプレイヤーに指し手を選んでもらう
   tree: ゲーム木
   ret: プレイヤーが選択した指し手に対応したゲーム木"
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   ;; 攻撃の指し手を表示
                   (format t "~a -> ~a" (car action) (cadr action))
                   ;; 手番を渡す指し手を表示
                   (princ "end turn"))))
    (fresh-line)
    ;; プレイヤーが選んだ指し手に対応するゲーム木を返す
    (cadr (nth (1- (read)) moves))))
;}}}

;;; 勝者をアナウンスする -----------------------------------------------------

(defun announce-winner (board);{{{
  "ゲームの勝者を表示する
   board: 現在のゲーム盤情報
   ret: -"
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        ;; 勝者が複数いる場合、全ての勝者の間でタイだったと表示する
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        ;; 勝者が1人のみいる場合、勝者はこのプレイヤーだったと表示する
        (format t "Ther winner is ~a" (player-letter (car w))))))
;}}}


;;; ==========================================================================
;;; CLEAN FUNCTION
;;; ==========================================================================

;;; ゲーム盤情報の型変換ユーティリティ ---------------------------------------

(defun board-array (lst);{{{
  "リストで表現されたゲーム盤を配列表現へと変える
   lst: ゲーム盤(リスト)
   ret: ゲーム盤(配列)"
  (make-array *board-hexnum* :initial-contents lst))
;}}}

(defun player-letter (n);{{{
  "ASCIIコードを該当する文字に変換する
   n: 1文字分のASCIIコード
   ret: 1文字"
  (code-char (+ 97 n)))
;}}}


;;; ゲームルールエンジン -----------------------------------------------------

;;; ゲームツリーの例
;;; CL-USER> (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
;;; (0                                   1回目:手番=プレイヤー0
;;;  #((0 1) (1 1) (0 2) (1 1))          1回目:ゲーム盤情報
;;;  (                                   2回目:有効なゲーム盤のリスト
;;;   ((2 3)                               2回目:行動=攻撃
;;;    (0                                  2回目:手番=プレイヤー0
;;;     #((0 1) (1 1) (0 1) (0 1))         2回目:ゲーム盤情報
;;;     (                                  3回目:有効なゲーム盤のリスト
;;;      (NIL                                3回目:行動=相手に指し手を渡す
;;;       (1                                 3回目:手番=プレイヤー1
;;;        #((0 1) (1 1) (0 1) (0 1))        3回目:ゲーム盤情報
;;;        NIL)))))))                          4回目:有効なゲーム盤が無い

;;; ゲームツリーの生成

(defun game-tree (board player spare-dice first-move);{{{
  "与えられた初期条件から、全ての可能な指し手を表現する木構造を作る
   この関数はゲーム開始時に1度だけ呼ばれる
   board: 盤面の情報
   player: 現在手番のプレイヤーID
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数
   first-move: 今のプレイヤーが手番を得て最初の指し手かどうか
   ret: 全体のゲーム木"
  (list player
        board
        ;; 攻撃をやめて相手に手番を渡す指し手
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          ;; 攻撃の指し手
                          (attacking-moves board player spare-dice))))
;}}}

;;; 相手に手番を渡す ---------------------------------------------------------

(defun add-passing-move (board player spare-dice first-move moves);{{{
  "指し手のリストに自分の手番を終了する動きを追加する
   board: 盤面の情報
   player: 現在手番のプレイヤーID
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数
   first-move: 今のプレイヤーが手番を得て最初の指し手かどうか
   moves: 現在までに集められた可能な指し手
   ret: プレイヤーが相手に指し手を渡してから後の全てのゲーム木"
  (if first-move
      ;; プレイヤーが手番を得てから最初の指し手なら、絶対攻撃せねばならないためmovesを返す
      moves
      ;; プレイヤーが手番を得てから2回目移行の指し手なら、新たな指し手を加える
      (cons (list
              ;; 指し手=自分の手番を終了する=nil
              nil
              ;; 手が指された後に起こりうる全てのパターンを持つゲーム木
              ;; プレイヤーはここで自分の手番を終了しているので、
              ;; 以降のゲーム木へはサイコロを補給した状態で遷移する
              (game-tree (add-new-dice board player (1- spare-dice))  ; 盤面の情報
                         (mod (1+ player) *num-players*)  ; 手番を他プレイヤーに変更
                         0  ; プレイヤーに補給されるサイコロの個数
                         t))  ; 今のプレイヤーが手番を得てからの最初の指し手
            moves)))
;}}}

;;; 攻撃の手を計算する -------------------------------------------------------

(defun attacking-moves (board cur-player spare-dice);{{{
  "可能な攻撃の指し手をゲーム木に追加する
   board: 現在のゲーム盤情報
   cur-player: 現在のプレイヤー
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数"
  (labels ((player (pos)
             ;; あるマス目のプレイヤーIDを返す
             (car (aref board pos)))
           (dice (pos)
             ;; あるマス目のサイコロの個数を返す
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                              (list (list src dst)
                                    (game-tree (board-attack board
                                                             cur-player
                                                             src
                                                             dst
                                                             (dice src))
                                               cur-player
                                               (+ spare-dice (dice dst))
                                               nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
              collect n))))
;}}}

;;; 隣接するマスを見つける----------------------------------------------------

(defun neighbors (pos);{{{
  "あるマスに隣接するマスを見つける
   pos: 現在のマス目
   ret: 隣接するマス目のリスト"
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))
;}}}

;;; 攻撃 ---------------------------------------------------------------------

(defun board-attack (board player src dst dice);{{{
  "マスsrcからマスdstを攻撃したときに何が起きるのかを計算する
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   src: 攻撃元のマス目
   dst: 攻撃先のマス目
   dice: srcにあるサイコロの個数"
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))
;}}}

;;; 補給 ---------------------------------------------------------------------

(defun add-new-dice (board player spare-dice);{{{
  "ゲーム盤にサイコロを足していく
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   spare-dice: 補給できるサイコロの個数"
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))
;}}}

;;; 勝者を決定する -----------------------------------------------------------

(defun winners (board);{{{
  "獲得マス数1位のプレイヤー全てをリストにして返す"
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))
;}}}

