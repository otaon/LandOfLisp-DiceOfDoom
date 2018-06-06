;;; ==========================================================================
;;; ゲーム情報
;;; ==========================================================================

;;; ゲーム情報
(defparameter *num-players* 2)  ; プレイヤー数
(defparameter *max-dice* 3)     ; 1マスにおけるサイコロの最大数
(defparameter *board-size* 2)   ; 1辺のマスの数
(defparameter *board-hexnum* (* *board-size* *board-size*)) ; 盤上のマス数

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
;;; CLEAN FUNCTION
;;; ==========================================================================

;;; --------------------------------------------------------------------------
;;; ゲーム盤情報の型変換ユーティリティ
;;; --------------------------------------------------------------------------

(defun board-array (lst)
  "リストで表現されたゲーム盤を配列表現へと変える
   lst: ゲーム盤(リスト)
   ret: ゲーム盤(配列)"
  (make-array *board-hexnum* :initial-contents lst))


(defun player-letter (n)
  "ASCIIコードを該当する文字に変換する
   n: 1文字分のASCIIコード
   ret: 1文字"
  (code-char (+ 97 n)))


;;; --------------------------------------------------------------------------
;;; ゲームルールエンジン
;;; --------------------------------------------------------------------------

;;; ゲームツリーの生成 -------------------------------------------------------

(defun game-tree (board player spare-dice first-move)
  "与えられた初期条件から、全ての可能な指し手を表現する木構造を作る
   この関数はゲーム開始時に1度だけ呼ばれる
   board: 盤面の情報
   player: 現在手番のプレイヤーID
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数
   first-move: 今のプレイヤーが手番を得て最初の指し手かどうか
   ret: 全体のゲーム木"
  (list player
        board
        ;; 相手に手番を渡す指し手
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          ;; 攻撃の指し手
                          (attacking-moves board player spare-dice))))


(defun add-passing-move (board player spare-dice first-move moves)
  "指し手のリストに自分の手番を終了する動きを追加する
   board: 盤面の情報
   player: 現在手番のプレイヤーID
   spare-dice: 現在の手番でプレイヤーが獲得したサイコロの個数
   first-move: 今のプレイヤーが手番を得て最初の指し手かどうか
   moves: 現在までに集められた可能な指し手
   ret: プレイヤーが"
  ;; プレイヤーが手番を得てから最初の指し手の場合、
  (if first-move
      moves
      (cons (list
              ;; 指し手=自分の手番を終了する=nil
              nil
              ;; 手が指された後に起こりうる全てのパターンを持つゲーム木
              ;; プレイヤーはここで手番を終了するので、サイコロを補給する
              (game-tree (add-new-dice board player (1- spare-dice))  ; 盤面の情報
                         (mod (1+ player) *num-players*)  ; 手番を他プレイヤーに変更
                         0  ; プレイヤーに補給されるサイコロの個数
                         t))  ; 今のプレイヤーが手番を得て最初の指し手
            moves)))

;;; ==========================================================================
;;; DIRTY IMPERATIVE
;;; ==========================================================================

(defun gen-board ()
  "ゲーム盤をランダムに作る
   ret: ランダムで生成したゲーム盤(配列)"
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))


(defun draw-board (board)
  "ゲーム盤を画面に表示する
   board: ゲーム盤(配列)"
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
