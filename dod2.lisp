;;; ---------------------------------------------------------------
;;; dice of doom v2
;;; 
;;; 遅延リスト版 dice of doom
;;; ---------------------------------------------------------------

;;; カレントディレクトリを変更する
#+clisp;{{{
(ext:cd "~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom")

#+sbcl
(progn
  (sb-posix:chdir #P"~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom")
  (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/"))))
;}}}

;;; dice of doom v1と遅延リストライブラリをコンパイル、ロードする
(compile-file "dod");{{{
(compile-file "lazy")
(load "dod")
(load "lazy")
;}}}

;;; dice of doom のゲーム盤サイズ定義
(defparameter *board-size* 5);{{{
(defparameter *board-hexnum* (* *board-size* *board-size*))
;}}}

;;; ゲームAIが先読みする遅延ゲーム木の深さ
(defparameter *ai-level* 4)

;;; ---------------------------------------------------------------
;;; ゲーム用関数群
;;; ---------------------------------------------------------------

;;; 相手に手番を渡す ---------------------------------------------------------
(defun add-passing-move (board player spare-dice first-move moves);{{{
  "相手に手番を渡すという指し手を遅延ゲーム木に加える
   board:      ゲーム盤
   player:     プレイヤーID
   spare-dice: 追加するサイコロの個数
   first-move: プレイヤー交代して最初の操作か
   moves:      可能な指し手
   ret: 相手に手番を渡すという指し手を加えた遅延ゲーム木"
  (if first-move
      ;; プレイヤー交代して最初の操作なら、可能な指し手を選ばせる
      moves
      ;; プレイヤー交代して2回目以降の操作なら、可能な指し手に加えプレイヤー交代も選ばせる
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player
                                                (1- spare-dice))
                                  (mod (1+ player) *num-players*)
                                  0
                                  t))
                 moves)))
;}}}

;;; 攻撃の手を計算する -------------------------------------------------------
(defun attacking-moves (board cur-player spare-dice);{{{
  "可能な攻撃の指し手を遅延ゲーム木に追加する
   board: ゲーム盤
   cur-player: 現在のプレイヤーID
   spare-dice: 追加するサイコロの個数
   ret: 可能な攻撃の指し手を追加した遅延ゲーム木"
  (labels ((player (pos)
             ;; あるマスのプレイヤーIDを取得する
             (car (aref board pos)))
           (dice (pos)
             ;; あるマスのサイコロの個数を取得する
             (cadr (aref board pos))))
    (lazy-mapcan
      (lambda (src)
        ;; 可能な攻撃の指し手を追加する
        ;; src: 攻撃元のマス
        (if (eq (player src) cur-player)
            (lazy-mapcan
              (lambda (dst)
                ;; 攻撃先のマスが相手プレイヤーのもの、
                ;; かつ、攻撃元のマスのサイコロ数が攻撃先のマスのサイコロ数より多い場合、
                ;; 攻撃の指し手として遅延ゲーム木に追加する
                (if (and (not (eq (player dst)
                                  cur-player))
                         (> (dice src) (dice dst)))
                    (make-lazy
                      (list (list (list src dst)
                                  (game-tree (board-attack board
                                                           cur-player
                                                           src
                                                           dst
                                                           (dice src))
                                             cur-player
                                             (+ spare-dice (dice dst))
                                             nil))))
                    (lazy-nil)))
              ;; 攻撃元のマスの隣のマスに対して、攻撃できるか調べる
              (make-lazy (neighbors src)))
            (lazy-nil)))
      ;; ゲーム盤の全てのマスについて、可能な攻撃の指し手を調べる
      (make-lazy (loop for n below *board-hexnum*
                       collect n)))))
;}}}

;;; 人間のプレイヤーからの入力を処理する -------------------------------------
(defun handle-human (tree);{{{
  "人間のプレイヤーに指し手を選んでもらう
   tree: ゲーム木
   ret: プレイヤーが選択した指し手に対応したゲーム木"
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       ;; 攻撃の指し手を表示
                       (format t "~a -> ~a" (car action) (cadr action))
                       ;; 手番を渡す指し手を表示
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    ;; プレイヤーが選んだ指し手に対応するゲーム木を返す
    (cadr (lazy-nth (1- (read)) moves))))
;}}}

;;; ゲームループ(対人用) -----------------------------------------------------
(defun play-vs-human (tree);{{{
  "対人専用ゲームループ
   tree: 現在のゲーム木
   ret: -"
  ;; ゲーム状態を表示
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      ;; 有向な指し手があれば次の指し手を促す
      (play-vs-human (handle-human tree))
      ;; 有向な指し手が無ければ勝者を表示して終了する
      (announce-winner (cadr tree))))
;}}}

(defun limit-tree-depth (tree depth);{{{
  "ゲーム木を指定の深さで刈り込む
   tree: 遅延ゲーム木
   depth: 何手先まで読むか(何手先で枝を刈るか)
   ret: 新しく作られる遅延ゲーム木のコピー"
  (list (car tree)  ; プレイヤーID
        (cadr tree) ; ゲーム盤情報
        ;; 刈り込む深さになったら、指し手の遅延リスト部分を空にする
        ;; 刈り込む深さでなかったら、可能な指し手に対応する遅延ゲーム木を取得する
        (if (zerop depth)
            (lazy-nil)  ; 空のゲーム木
            (lazy-mapcar (lambda (move)
                           ;; 指定された指し手に対応する遅延ゲーム木を取得する
                           ;; move: 指し手をキーに持つ遅延ゲーム木のalist
                           ;; ret: 指し手に対応する遅延ゲーム木
                           (list (car move)  ; 指し手
                                 (limit-tree-depth (cadr move) (1- depth)))) ; 指し手に対応するゲーム盤情報
                         ;; 指し手をキーに持つ遅延ゲーム木のalistのリスト
                         (caddr tree)))))
;}}}

;;; AIのプレイヤーからの入力を処理する ---------------------------------------
(defun handle-computer (tree);{{{
  "ゲームAIを操作する
   tree: 現在の遅延ゲーム木
   ret: ゲームAIの指し手に対応する遅延ゲーム木"
  ;; ratings: 現在のゲーム盤情報における、各指し手に対する点数のリスト
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    ;; 最高得点を得られる指し手を計算し、それに対応する遅延ゲーム木を返す
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))
;}}}

;;; ゲームループ(対AI用) -----------------------------------------------------
(defun play-vs-computer (tree);{{{
  "AIに指し手を選んでもらう
   tree: ゲーム木
   ret: AIが選択した指し手に対応したゲーム木"
  ;; ゲーム情報を表示する
  (print-info tree)
  ;; 指し手をキーとする遅延ゲーム木のalistが空なら、現在のゲーム盤情報から勝者を表示してゲーム終了
  ;; プレイヤーIDが0(人間の手番)なら、人間から指し手を要求してゲーム続行する
  ;; プレイヤーIDがゲームAIの手番なら、ゲームAIに指し手を計算させてゲーム続行する
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))
;}}}

(defun score-board (board player);{{{
  "指定のプレイヤーにとっての現在のゲーム盤情報のスコアを算出する
   board: ゲーム盤情報
   player: プレイヤーID
   ret: ゲーム盤情報のスコア"
  ;; ゲーム盤を走査しながら、各マスのスコアを合計する
  (loop for hex across board
        for pos from 0
        ;; 下記のルールで各マスのスコアを算出する
        ;; - プレイヤーが所有するマスで、より強い敵のマスが隣にない:2
        ;; - プレイヤーが所有するマスで、より強い敵のマスが隣にある:1
        ;; - 敵が所有するマス:-1
        sum (if (eq (car hex) player)
                (if (threatened pos board)
                    1
                    2)
                -1)))
;}}}

(defun threatened (pos board);{{{
  "隣のマスにより強い敵のマスがあるか判定する
   pos: ゲーム盤の位置
   board: ゲーム盤情報
   ret: t:隣により強い敵のマスがある nil:ない"
  (let* ((hex (aref board pos))  ; 引数posで指定したマス情報
         (player (car hex))      ; マスを所有するプレイヤーのID
         (dice (cadr hex)))      ; マスに置かれたサイコロの数
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n)) ; posの隣のマス情報
                    (nplayer (car nhex))  ; posの隣のマスを所有するプレイヤーのID
                    (ndice (cadr nhex)))  ; posの隣のマスに置かれたサイコロの数
               ;; posの隣のマスが、異なる所有者でより多くのサイコロを持っていたら、
               ;; 隣のマスにより強い敵のマスがあると評価する
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return n))))))
;}}}

(defun get-ratings (tree player);{{{
  "現在の遅延ゲーム木における指定したプレイヤーが取りうる得点を全パターン返す
   tree: 遅延ゲーム木
   player: 得点を算出したいプレイヤーのID
   ret: 得点のリスト"
  (take-all (lazy-mapcar (lambda (move)
                           ;; 指し手に対応するそのマスの得点を計算する
                           (rate-position (cadr move) player))
                         ;; 可能な全ての指し手
                         (caddr tree))))
;}}}

(defun rate-position (tree player);{{{
  "現在のゲーム木から指定プレイヤーの得点を算出する
   tree: 遅延ゲーム木
   player: プレイヤーID
   ret: 得点"
  (let ((moves (caddr tree)))  ; 可能な指し手
    ;; 現在のゲーム木に可能な指し手があれば、次に取りうる全てのゲーム木を見ていき、
    ;; ミニマックスアルゴリズムを適用したときの得点を返す
    ;; 現在のゲーム木に可能な指し手がなければ、現在のゲーム盤の得点を返す
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
          (get-ratings tree player))
        (score-board (cadr tree) player))))
;}}}

(defun ab-get-ratings-max (tree player upper-limit lower-limit);{{{
  "MAXノードにおいて、現在のゲーム盤で取りうるスコアの最大値を計算する
   tree: 現在の遅延ゲーム木
   player: プレイヤーID
   upper-limit: スコアの上限
   lower-limit: スコアの下限
   ret: スコアの最大値"
  (labels ((f (moves lower-limit)
             ;; 可能な指し手の中からスコアの最大値を求める
             ;; moves: 可能な指し手
             ;; lower-limit: 探索すべきスコアの下限
             ;; ret: スコアの最大値
             ;; 可能な指し手があれば、それらに対してスコアの最大値を計算する
             (unless (lazy-null moves)
               ;; x: 未探索の指し手のうち一番左側の指し手のスコアを計算する
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 ;; - xが上限以上なら、それ以上探索する必要はないので評価を打ち切る
                 ;; - xがそれ以外なら、残りの枝をさらに探索する必要がある
                 ;;   - xがそれまでのlower-limitより大きければxを新たなlower-limitとして採用する
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))

    ;; 可能な指し手と下限を指定して、スコアの最大値を計算する
    (f (caddr tree) lower-limit)))
;}}}

(defun ab-get-ratings-min (tree player upper-limit lower-limit);{{{
  "MINノードにおいて、現在のゲーム盤で取りうるスコアの最小値を計算する
   tree: 現在の遅延ゲーム木
   player: プレイヤーID
   upper-limit: スコアの上限
   lower-limit: スコアの下限
   ret: スコアの最大値"
  (labels ((f (moves upper-limit)
             ;; 可能な指し手の中からスコアの最大値を求める
             ;; moves: 可能な指し手
             ;; upper-limit: 探索すべきスコアの上限
             ;; ret: スコアの最大値
             ;; 可能な指し手があれば、それらに対してスコアの最大値を計算する
             (unless (lazy-null moves)
               ;; x: 未探索の指し手のうち一番左側の指し手のスコアを計算する
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 ;; - xが下限以下なら、それ以上探索する必要はないので評価を打ち切る
                 ;; - xがそれ以外なら、残りの枝をさらに探索する必要がある
                 ;;   - xがそれまでのupper-limitより大きければxを新たなupper-limitとして採用する
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))

    ;; 可能な指し手と上限を指定して、スコアの最小値を計算する
    (f (caddr tree) upper-limit)))
;}}}

(defun ab-rate-position (tree player upper-limit lower-limit);{{{
  "ゲーム木のある節での特定のプレイヤーの点数を計算する(アルファ・ベータアルゴリズム版)
   tree: ゲーム木
   player: プレイヤーID
   upper-limit: 探索を打ち切る上限値
   lower-limit: 探索を打ち切る下限値
   ret: ゲーム木のある節での特定のプレイヤーの点数"
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        ;; 可能な指し手があるなら、各指し手について、
        ;; それ以降の全ての指し手を考慮した点数を計算する
        (if (eq (car tree) player)
            ;; プレイヤーなら点数の最大値を返す
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
            ;; 敵なら点数の最小値を返す
            (apply #'min (ab-get-ratings-min tree
                                             player
                                             upper-limit
                                             lower-limit)))
        ;; ゲーム木のある節の点数を計算する
        (score-board (cadr tree) player))))
;}}}

(defun handle-computer (tree);{{{
  "tree: 現在のゲーム木
   ret: 現在のゲーム木で最も高い点数の指し手"
  ;; ratings: 現在のゲーム木(プレイヤーは現在のプレイヤー)の点数のリスト
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    ;; 現在のゲーム木で最も高い点数の指し手
    ;; **ratingの並びがゲーム木の並びと対応している前提なので、崩してはいけない**
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))
;}}}


