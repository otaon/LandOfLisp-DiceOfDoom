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
; ((0 3) (0 3) (1 3) (1 1))
; [3][3]
; [3][1]


;;; ==========================================================================
;;; CLEAN FUNCTION
;;; ==========================================================================

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
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))
