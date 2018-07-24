;;; ------------------------------------------------------------------------------------------------
;;; dice of doom v3
;;; 
;;; webブラウザ版 dice of doom
;;; ------------------------------------------------------------------------------------------------

;;; カレントディレクトリを変更する
 ;{{{
#+clisp
(eval-when (compile load eval)
  (ext:cd "~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom"))

#+sbcl
(eval-when (compile load eval)
  (progn
    (sb-posix:chdir #P"~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom")
    (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/")))))
;}}}

;;; 下記をコンパイル、ロードする
;;; - dice of doom v2
;;; - web server ライブラリ
;;; - svg ライブラリ
(eval-when (compile load eval);{{{
  (progn
    (compile-file "dod3")
    (load "dod3")))
;}}}


;;; ------------------------------------------------------------------------------------------------
;;; ゲームプレイヤーとサイコロのを設定
;;; ------------------------------------------------------------------------------------------------
;{{{
(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63)     ; 赤
                             (63 63 255)     ; 青
                             (63 255 63)     ; 緑
                             (255 63 255)))  ; 紫
(defparameter *max-dice* 5)  ; サイコロの最大数
(defparameter *ai-level* 2)  ; AIが思考するゲーム木の深さ
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; 確率ノードを作成する
;;; ------------------------------------------------------------------------------------------------
(defun attacking-moves (board cur-player spare-dice);{{{
  "攻撃の手
   board: ゲーム盤
   cur-player: 現在のプレイヤーID
   spare-dice: 補給されるサイコロの個数
   ret: -"
  (labels ((player (pos)
             ;; 指定のマスのプレイヤーID
             (car (aref board pos)))
           (dice (pos)
             ;; 指定のマスのサイコロの個数
             (cadr (aref board pos))))
    (lazy-mapcan
      (lambda (src)
        (if (eq (player src) cur-player)
          (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) 1))
                  (make-lazy (list
                               (list
                                 (list src dst)
                                 (game-tree (board-attack board cur-player
                                                          src dst (dice src))
                                             cur-player
                                             (+ spare-dice (dice dst))
                                             nil)
                                 (game-tree (board-attack-fail board cur-player
                                                               src dst (dice src))
                                            cur-player
                                            (+ spare-dice (dice dst))
                                            nil))))
                  (lazy-nil)))
            (make-lazy (neighbors src)))
          (lazy-nil)))
      (make-lazy (loop for n below *board-hexnum*
                   collect n)))))
;}}}

(defun board-attack-fail (board player src dst dice);{{{
  "攻撃失敗した際のゲーム盤情報を返す
   board: 現在のゲーム木
   player: 現在のプレイヤーID
   src: 攻撃元のマス
   dst: 攻撃先のマス
   dice: サイコロの個数
   ret: 攻撃失敗時のゲーム盤情報"
  ;; 攻撃失敗したときの攻撃元のマスのサイコロを1個だけにする
  (board-array (loop for pos from 0
                     for hex across board
                     collect (if (eq pos src)
                                 (list player 1)
                                 hex))))
;}}}

(defun roll-dice (dice-num);{{{
  "サイコロを振る
   dice-num: 振るサイコロの個数
   ret: 降ったサイコロの合計値"
  ;; 降ったサイコロの合計値
  (let ((total (loop repeat dice-num
                     sum (1+ (random 6)))))
    ;; 降ったサイコロの個数と合計値をメッセージ表示
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))
;}}}


(defun roll-against (src-dice dst-dice);{{{
  "src-dice: 攻撃元のサイコロの個数
   dst-dice: 攻撃先のサイコロの個数
   ret: 攻撃成功の是非"
  (> (roll-dice src-dice) (roll-dice dst-dice)))
;}}}

(defun pick-chance-branch (board move);{{{
  "攻撃の指し手に対応するゲーム木を返す
   branch: 現在のゲーム木の枝
   move: 指し手
   ret: 攻撃の指し手に対応するゲーム木"
  (labels ((dice (pos)
             ;; 指定のマスにあるサイコロの個数を返す
             (cadr (aref board pos))))
    (let ((path (car move)))
      ;; 攻撃元と攻撃先のサイコロを振り、
      ;; 攻撃の成否を決める
      (if (or (null path)
              (roll-against (dice (car path))
                            (dice (cadr path))))
          ;; パスの場合、および、攻撃成功の場合は、
          ;; 手番成功用のゲーム木を返す
          (cadr move)
          ;; 攻撃失敗の場合は、
          ;; 手番失敗用のゲーム木を返す
          (caddr move)))))
;}}}

(defun web-handle-human (pos);{{{
  "人間の手番を処理する
   pos: 現在のマスの位置
   ret: -"
  (cond
    ;; マスを未選択:
    ;; 攻撃元のマス選択メッセージを表示
    ((not pos) (princ "Please choose a hex to move from:"))
    ;; パスを選択済み:
    ;; プレイヤーの補給が完了したとメッセージを表示
    ;; パラメータにnilを渡すcontinueリンクを表示
    ((eq pos 'pass) (setf *cur-game-tree*
                          (cadr (lazy-car (caddr *cur-game-tree*))))
                    (princ "Your reinforcements have been placed.")
                    (tag a (href (make-game-link nil))
                      (princ "continue")))
    ;; マスを選択済み & 攻撃元のタイルがセットされていない:
    ;; 今選ばれたマスを攻撃元としてセット
    ((not *from-tile*) (setf *from-tile* pos)
                       (princ "Now choose a destination:"))
    ;; 今選択したマスが攻撃元のタイルと同じ:
    ;; 攻撃元のタイルをリセット
    ((eq pos *from-tile*) (setf *from-tile* nil)
                       (princ "Move cancelled."))
    ;; 上記以外(=攻撃元と攻撃先を選択完了した):
    ;; 攻撃元と攻撃先に対応するゲーム木に遷移する
    ;; 次の手を指すかパスするかを選ばせる
    (t (setf *cur-game-tree*
             (pick-chance-branch
               (cadr *cur-game-tree*)
               (lazy-find-if (lambda (move)
                               (equal (car move)
                                      (list *from-tile* pos)))
                             (caddr *cur-game-tree*))))
       (setf *from-tile* nil)
       (princ "You may now ")
       (tag a (href (make-game-link 'pass))
         (princ "pass"))
       (princ " or make another move:"))))
;}}}

(defun handle-computer (tree);{{{
  "ゲームAIを操作する
   tree: 現在の遅延ゲーム木
   ret: ゲームAIの指し手に対応する遅延ゲーム木"
  ;; ratings: 現在のゲーム盤情報における、各指し手に対する点数のリスト
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    ;; 最高得点を得られる指し手を計算し、それに対応する遅延ゲーム木を返す
    (pick-chance-branch
      (cadr tree)
      (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))
;}}}

;;; サイコロを振ったときの勝率
;;; :各行:守備側のサイコロの個数(1〜5個)
;;; :各列:攻撃側のサイコロの個数(2〜5個)
(defparameter *dice-probability*;{{{
  #(#(0.84 0.97 1.0  1.0)
    #(0.44 0.78 0.94 0.99)
    #(0.15 0.45 0.74 0.91)
    #(0.04 0.19 0.46 0.72)
    #(0.01 0.06 0.22 0.46)))
;}}}

(defun get-ratings (tree player);{{{
  "現在の遅延ゲーム木における指定したプレイヤーが取りうる得点を全パターン返す
   tree: 遅延ゲーム木
   player: 得点を算出したいプレイヤーのID
   ret: 得点のリスト"
  (let ((board (cadr tree)))  ; ゲーム盤情報
    (labels
      ((dice (pos)
         ;; 指定のマスのサイコロの個数
         (cadr (aref board pos))))
      (take-all
        ;; 遅延リストをリストに変換する
        (lazy-mapcar
          ;; 
          (lambda (move)
            ;; 指定した指し手のゲーム木のある節のポイントを計算する
            ;; move: 指し手のリスト
            (let ((path (car move)))
              (if path
                  ;; 攻撃の指し手の場合、攻撃のポイントを返す
                  (let*
                    ;; 攻撃元のマス
                    ((src (car path))
                     ;; 攻撃先のマス
                     (dst (cadr path))
                     ;; 守備側と攻撃側のサイコロの個数に対応する勝率
                     (probability
                       (aref (aref *dice-probability*
                                   (1- (dice dst)))
                             (- (dice src) 2))))
                    ;; 攻撃成功と攻撃失敗を合わせたポイント
                    (+
                      ;; 攻撃の成功確率 * 攻撃元のポイント
                      (* probability (rate-position (cadr move) player))
                      ;; 攻撃の失敗確率 * 攻撃先のポイント
                      (* (- 1 probability) (rate-position (caddr move) player))))
                  ;; パスしたときの指し手の場合、パスのポイントを返す
                  (rate-position (cadr move) player))))
          ;; 可能な指し手のゲーム木
          (caddr tree))))))
;}}}

(defun limit-tree-depth (tree depth);{{{
  "探索の深さを制限する
   tree: ゲーム木
   depth: 探索の深さ
   ret: -"
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            ;; 探索の最大の深さになったら、空リストを返す
            (lazy-nil)
            ;; 探索可能な深さなら、ゲーム木の続きを返す
            (lazy-mapcar (lambda (move)
                           (cons (car move)
                                 (mapcar (lambda (x)
                                           (limit-tree-depth x (1- depth)))
                                         (cdr move))))
                         (caddr tree)))))
;}}}

(defun get-connected (board player pos);{{{
  "プレイヤーが所有する連続領域のリストを返す
   board: ゲーム盤
   player: プレイヤーID
   pos: 現在のマス
   ret: プレイヤーが所有する連続領域のリスト"
  (labels ((check-pos (pos visited)
             ;; 連続した領域のマスのリストvisitedを返す
             ;; pos: 注目中のマス
             ;; visited: 走査したマスのリスト
             (if (and (eq (car (aref board pos)) player)
                      (not (member pos visited)))
                 ;; 注目中のマスがプレイヤー所有のものであり、かつ、まだ走査していない:
                 ;; 注目中のマスを連続領域に追加し、更に全ての隣接マスを走査
                 (check-neighbors (neighbors pos) (cons pos visited))
                 ;; 現在のマスがプレイヤー所有ではない、または、走査済み:
                 ;; 走査したマスのリストを返す
                 visited))
           (check-neighbors (lst visited)
             ;; 隣接マス全てについてcheck-pos関数で走査させる
             ;; ;; check-neighbors関数は再帰で隣接マス全てに対してcheck-pos関数を呼ぶ
             ;; lst: 指定のマスのリスト
             ;; visited: 走査したマスのリスト
             (if lst
                 ;; 指定のマスがある:
                 ;; 指定のマスを走査して連続領域のリストを返す
                 (check-neighbors (cdr lst) (check-pos (car lst) visited))
                 ;; 指定のマスがない:
                 ;; 走査済みのマスのリストを返す
                 visited)))
    (check-pos pos '())))
;}}}

(defun largest-cluster-size (board player);{{{
  "指定したプレイヤーが持つ最大の連続領域のサイズを返す
   board: ゲーム盤情報
   player: プレイヤーID
   ret: 最大の連続領域のサイズ"
  (labels ((f (pos visited best)
             ;; ゲーム盤を0番目から最後まで再帰で走査して、一番大きかった領域を返す
             (if (< pos *board-hexnum*)
                 (if (and (eq (car (aref board pos)) player)
                          (not (member pos visited)))
                     ;; posがプレイヤーの領域で、まだ走査していなかったら、
                     ;; 連続した領域のリストをclusterに格納する
                     (let* ((cluster (get-connected board player pos))
                            (size (length cluster)))
                       (if (> size best)
                           ;; clusterのサイズを計って、今までで一番大きかったら、
                           ;; そのサイズをaccumlatorとしてfに渡して再帰的に走査する
                           ;; 今までで一番大きいものではなかったら、
                           ;; 今までのベストをaccumlatorとしてfに渡して再帰的に走査する
                           (f (1+ pos) (append cluster visited) size)
                           (f (1+ pos) (append cluster visited) best)))
                     ;; プレイヤーの領域ではなかったら、もしくは、走査済みだったら、
                     ;; 今までのペストをaccumlatorとしてfに渡して再帰的に走査する
                     (f (1+ pos) visited best))
                 ;; ゲーム盤を全て走査し終わったら、一番大きな領域のサイズを返す
                 best)))
    (f 0 '() 0)))
;}}}

(defun add-new-dice (board player spare-dice);{{{
  "各マスにサイコロを補給する
   board: ゲーム盤情報
   player: サイコロ補給対象のプレイヤーID
   spare-dice: 補給するサイコロの個数(未使用)"
  (labels ((f (lst n)
             ;; 
             ;; lst: ゲーム盤情報(リスト)
             ;; n: 補給するサイコロの個数
             (cond ((zerop n) lst)  ; 補給するサイコロがなくなったら現状のマスの情報のままを返す
                   ((null lst) nil) ; ゲーム盤を全て走査し終わったらnilを返す
                   (t (let ((cur-player (caar lst))  ; 走査中のマスを所有するプレイヤーID
                            (cur-dice (cadar lst)))  ; 走査中のマスにあるサイコロの個数
                        ;; 走査中のマスが、補給対象のプレイヤーで、かつ、まだ補給する空きがあるなら、サイコロを補給する
                        ;; それ以外は補給せずに次のマスを走査する
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            ;; サイコロを1個補給して、つぎのマスを再帰的に走査する
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            ;; サイコロを足さずに、つぎのマスを再帰的に走査する
                            (cons (car lst) (f (cdr lst) n))))))))
    ;; サイコロを補給した後のゲーム盤情報を計算する
    (board-array (f (coerce board 'list)
                   ;; 指定したプレイヤーが持つ最大の連続領域のサイズを計算する
                   (largest-cluster-size board player)))))
;}}}

