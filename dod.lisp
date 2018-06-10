;;; ==========================================================================
;;; ゲーム情報
;;; ==========================================================================

;;; ゲーム情報 ;{{{
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

;;; ゲームループ(対人用) -----------------------------------------------------
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

;;; ゲームループ(対AI用) -----------------------------------------------------
(defun play-vs-computer (tree);{{{
  "AIに指し手を選んでもらう
   tree: ゲーム木
   ret: AIが選択した指し手に対応したゲーム木"
  ;; ゲームの状態を表示
  (print-info tree)
  ;; 可能な指し手が無ければ、勝者を表示してゲーム終了
  ;; プレイヤーIDが0(人間の手番)なら、人間に指し手を選択してもらう
  ;; プレイヤーIDが0以外(AIの手番)なら、AIに指し手を選択してもらう
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))
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
        (format t "The winner is ~a" (player-letter (car w))))))
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
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数
   ret: 可能な攻撃の指し手によるゲーム木"
  (labels ((player (pos)
             ;; あるマス目のプレイヤーIDを返す
             (car (aref board pos)))
           (dice (pos)
             ;; あるマス目のサイコロの個数を返す
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              ;; 着目中のマス目が現在のプレイヤーのマス目なら、
              ;; 隣接するマスへの攻撃の指し手によるゲーム木のリストを返す
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          ;; 隣接する攻撃先のマスが相手の陣で、
                          ;; かつ、自陣のサイコロが相手の陣のサイコロよりも多い場合、
                          ;; 攻撃指し手によるゲーム木を返す
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            ;; (((攻撃の指し手) (攻撃の指し手によるゲーム木)))
                            (list
                              (list (list src dst)
                                    (game-tree
                                      ;; 攻撃後のゲーム盤情報
                                      (board-attack board cur-player src dst (dice src))
                                      ;; 現在のプレイヤーID
                                      cur-player
                                      ;; 攻撃によって得られたサイコロの個数
                                      (+ spare-dice (dice dst))
                                      nil)))))
                        (neighbors src))))
            ;; ゲーム盤の全マス番号
            (loop for n below *board-hexnum*
                  collect n))))
;}}}

;;; 隣接するマスを見つける----------------------------------------------------
(defun neighbors (pos);{{{
  "あるマスに隣接するマスを見つける
   pos: 現在のマス目
   ret: 隣接するマス目のリスト"
  (let ((up (- pos *board-size*))  ; 上のマス
        (down (+ pos *board-size*)))  ; 下のマス
    (loop for p in (append (list up down)  ; 
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))  ; 左上のマス, 左のマス
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))  ; 右のマス, 右下のマス
          when (and (>= p 0) (< p *board-hexnum*))
          ;; ゲーム盤に収まっているマス目のみ収集する
          collect p)))
;}}}

;;; 攻撃 ---------------------------------------------------------------------
(defun board-attack (board player src dst dice);{{{
  "マスsrcからマスdstを攻撃したときのサイコロの移動を計算する
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   src: 攻撃元のマス目
   dst: 攻撃先のマス目
   dice: srcにあるサイコロの個数
   ret: 攻撃後のゲーム盤情報"
  (board-array (loop for pos from 0
                     for hex across board
                     ;; 攻撃元のマス目にはサイコロを1個残す
                     ;; 攻撃先のマス目には攻撃元にあったサイコロ-1個を置く
                     ;; その他のマス目には変化なし
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))
;}}}

;;; 補給 ---------------------------------------------------------------------
(defun add-new-dice (board player spare-dice);{{{
  "ゲーム盤にサイコロを足していく
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   spare-dice: 補給できるサイコロの個数
   ret: サイコロ追加後のゲーム盤情報"
  (labels ((f (lst n)
             ;; lst: ゲーム盤情報(リスト)
             ;; n: 補給できるサイコロの個数

             ;; ゲーム盤情報が無ければ、そのまま無し(nil)を返す
             ;; 補給できるサイコロが無ければ、ゲーム盤情報を返す
             ;; その他の場合、
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))  ; 現在のプレイヤーID
                            (cur-dice (cadar lst)))  ; 着目中のマスのサイコロの個数
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            ;; 着目中のマスが現在のプレイヤーのマス、かつ、
                            ;; マスにおけるサイコロの個数が上限でなければ、
                            ;; サイコロを追加して次のマスへ移動
                            ;; そうでなければ、サイコロを追加せずに次のマスへ移動
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
    ;; ゲーム盤情報をリストに変換して、
    ;; サイコロを追加して、
    ;; ゲーム盤情報を再び配列に戻す
    (board-array (f (coerce board 'list) spare-dice))))
;}}}

;;; 勝者を決定する -----------------------------------------------------------
(defun winners (board);{{{
  "獲得マス数1位のプレイヤー全てをリストにして返す
   board: ゲーム盤情報
   ret: ゲーム盤情報から算出した勝者のリスト"
  (let*
    ;; マスを所有しているプレイヤー達のIDのリスト(重複有り)
    ((tally (loop for hex across board
                  collect (car hex)))
     ;; マス所有者と所有マス数のコンスセル
     (totals (mapcar (lambda (player)
                       (cons player (count player tally)))
                     (remove-duplicates tally)))
     ;; 所有マス数の最大値
     (best (apply #'max (mapcar #'cdr totals))))
    ;; マスを最大数所有しているプレイヤーのIDのリスト
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))
;}}}

;;; ミニマックスアルゴリズムを用いたAI ---------------------------------------
(defun rate-position (tree player);{{{
  "ゲーム木のある節での特定のプレイヤーの点数を計算する
   tree: ゲーム木
   player: プレイヤーID
   ret: ゲーム木のある節での特定のプレイヤーの点数"
  (let ((moves (caddr tree)))  ; ある節で可能な指し手のリスト
    (if moves
        ;; 可能な指し手があるなら、各指し手について、
        ;; それ以降の全ての指し手を考慮した点数を計算する
        (apply (if (eq (car tree) player)
                   ;; プレイヤーなら点数の最大値を返す
                   #'max
                   ;; 敵なら点数の最小値を返す
                   #'min)
               ;; ゲーム木のある節の点数を計算する
               (get-ratings tree player))
        ;; 可能な指し手がないなら、そのゲーム盤情報から勝者を計算し、
        ;; 勝者にプレイヤーがいれば、1/勝者数を点数とする
        ;; 勝者にプレイヤーがいなければ、0を点数とする
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))
;}}}

(defun get-ratings (tree player);{{{
  "特定のプレイヤーに対して、その時に可能な指し手によるゲーム木の点数のリストを計算する
   tree: ゲーム木
   player: プレイヤーID
   ret: 特定のプレイヤーの、その時に可能な指し手の点数のリスト"
  (mapcar (lambda (move)
            ;; あるゲーム木における着目したプレイヤーの点数
            (rate-position (cadr move) player))
          ;; 可能な指し手によるゲーム木のリスト
          (caddr tree)))
;}}}

;;; AIのプレイヤーからの入力を処理する ---------------------------------------
(defun handle-computer (tree);{{{
  "tree: 現在のゲーム木
   ret: 現在のゲーム木で最も高い点数の指し手"
  ;; ratings: 現在のゲーム木(プレイヤーは現在のプレイヤー)の点数のリスト
  (let* ((player (car tree))  ; 手番のプレイヤー
         (ratings (get-ratings tree player))  ; 手番のプレイヤーにとっての点数
         (moves (caddr tree)))  ; 可能な指し手によるゲーム木のリスト
    ;; 現在のゲーム木で最も高い点数の指し手
    ;; **ratingの並びがゲーム木の並びと対応している前提なので、崩してはいけない**
    (cadr (nth (position (apply #'max ratings) ratings) moves))))
;}}}
