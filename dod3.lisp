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
    (compile-file "dod2")
    (compile-file "webserver")
    (compile-file "svg")
    (load "dod2")
    (load "webserver")
    (load "svg")
    (in-package "COMMON-LISP-USER")))
;}}}


;;; ------------------------------------------------------------------------------------------------
;;; ゲーム盤の大きさ定義情報
;;; ------------------------------------------------------------------------------------------------
;{{{
(defparameter *board-width* 900)   ; ゲーム盤の横幅(pixel)
(defparameter *board-height* 500)  ; ゲーム盤の高さ(pixel)
(defparameter *board-scale* 64)    ; 1つのマスの幅の半分の長さ(pixel)
(defparameter *top-offset* 3)      ; ゲーム盤の上にあける空白の大きさ(何マス分か)
(defparameter *dice-scale* 40)     ; 1つのサイコロの大きさ(pixel)
(defparameter *dot-size* 0.05)     ; サイコロの目の大きさ(サイコロ自体の何倍か)
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; サイコロを描画する
;;; ------------------------------------------------------------------------------------------------
(defun draw-die-svg (x y col);{{{
  "指定した座標にサイコロを1つ描画する
   x: サイコロを描画するx座標(pixel)
   y: サイコロを描画するy座標(pixel)
   col: サイコロの色(RGB値)
   ret: -"
  (labels ((calc-pt (pt)
             ;; 描画対象の座標を補正する
             ;; pt:  補正する前の座標コンスセル
             ;; ret: 補正した後の座標コンスセル
             (cons (+ x (* *dice-scale* (car pt)))
                   (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
             ;; 指定した頂点座標と色情報をもとにポリゴンを描画する
             ;; pol: ポリゴンの頂点座標
             ;; col: ポリゴンの色情報(RGB値)
             ;; ret: ポリゴンのsvg記述
             (polygon (mapcar #'calc-pt pol) col)))

    ;; サイコロの上面を描画する
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness col 40))
    ;; サイコロの左面を描画する
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       col)
    ;; サイコロの右面を描画する
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness col -40))
    ;; サイコロの目を描画する(サイコロ1つの3面分を一気に)
    (mapc (lambda (x y)
            (polygon (mapcar
                       (lambda (xx yy)
                         ;; サイコロの目を描画する
                         (calc-pt (cons (+ x (* xx *dot-size*))
                                        (+ y (* yy *dot-size*)))))
                       ;; サイコロの目のx座標とy座標
                       '(-1 -1 1 1)
                       '(-1 1 1 -1))
                     ;; サイコロの目の色(白)
                     '(255 255 255)))
          ;; サイコロの目のx座標とy座標
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))
;}}}


;;; ------------------------------------------------------------------------------------------------
;;; マスを描画する
;;; ------------------------------------------------------------------------------------------------
(defun draw-tile-svg (x y pos hex xx yy col chosen-tile);{{{
  "六角形のマスとその上に積み上がったサイコロを描く
   x: マスのx座標(マス目)
   y: マスのy座標(マス目)
   pos: 描画対象のマス
   hex: プレイヤーIDとサイコロ数のコンスセル
   xx: マスの描画用x座標(pixel)
   yy: マスの描画用y座標(pixel)
   col: マスとサイコロの色
   chosen-tile: 選択中のマスの番号
   ret: -"
  ;; マスを描く(厚みを持たせるため、縦をずらして2重に描く)
  (loop for z below 2
        do (polygon (mapcar (lambda (pt)
                              (cons (+ xx (* *board-scale* (car pt)))
                                    (+ yy (* *board-scale* (+ (cdr pt) (* (- 1 z) 0.1))))))
                            ;; 六角形のマスの座標(上から時計回り)
                            '((-1 . -0.2) (0 . -0.5) (1 . -0.2) (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                    ;; 選択中のマスを明るくする
                    (if (eql pos chosen-tile)
                        (brightness col 100)
                        col)))
  ;; サイコロを描く
  (loop for z below (second hex)
        do (draw-die-svg (+ xx
                            (* *dice-scale*
                               0.3
                               ;; サイコロを左右にブレさせる
                               (if (oddp (+ x y z))
                                   -0.3
                                   0.3)))
                         (- yy (* *dice-scale* z 0.8))
                         col)))
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; ゲーム盤を描く
;;; ------------------------------------------------------------------------------------------------
;; サイコロの色(赤と青)
(defparameter *die-colors* '((255 63 63) (63 63 255)));{{{
;}}}

(defun draw-board-svg (board chosen-tile legal-tiles);{{{
  "ゲーム盤をsvg記述する
   board: ゲーム盤情報
   chosen-tile: 選択中のマス
   legal-tiles: プレイヤーが次に選択可能なマスのリスト
   ret: -"
  ;; ゲーム盤の全マスを走査する
  (loop for y below *board-size*
        do (loop for x below *board-size*
                 ;; 現在のマスの番号
                 for pos = (+ x (* *board-size* y))
                 ;; 現在のマスの情報(プレイヤーIDとサイコロ数)
                 for hex = (aref board pos)
                 ;; 現在のマスの表示座標(x座標)
                 for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                 ;; 現在のマスの表示座標(y座標)
                 for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
                 ;; マスとサイコロの色(上の行ほど暗く補正する)
                 for col = (brightness (nth (first hex) *die-colors*)
                                       (* -15 (- *board-size* y)))
                 ;; 現在のマスが、プレイヤーが次に選択可能なマス、または、選択中のマスの場合、
                 ;; リンクで囲ってクリック可能にする
                 ;; 現在のマスが、それ以外の場合、そのまま選択される
                 do (if (or (member pos legal-tiles) (eql pos chosen-tile))
                        ;; リンクの場合は1マス分を<g>タグで囲んでグルーピングする
                        (tag g ()
                             (tag a ("xlink:href" (make-game-link pos))
                                  (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                        (draw-tile-svg x y pos hex xx yy col chosen-tile)))))
;}}}

(defun make-game-link (pos);{{{
  "リンクするURLを生成する
   pos: リンク対象のマスの番号
   ret: -"
  (format nil "/game.html?chosen=~a" pos))
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; リクエストハンドラ
;;; ------------------------------------------------------------------------------------------------
;; 現在のゲーム木
(defparameter *cur-game-tree* nil);{{{
(defparameter *from-tile* nil)
;}}}

(defun dod-request-handler (path header params);{{{
  "Webブラウザから来る全てのリクエストを処理する
   path: URL
   header: *未使用*
   params: URLのパラメータ
   ret: -"
  ;; アクセスされたURLがgame.htmlならゲーム処理する
  (if (equal path "game.html")
      ;; doctypeを指定して、html5だと認識させる
      (progn (princ "<!doctype html>")
             (tag center ()
                  (princ "Welcome to DICE OF DOOM!")
                  (tag br ())
                  (let ((chosen (assoc 'chosen params)))
                    ;; どのマスも選択されていないか、ゲーム木が空なら、
                    ;; ゲームを初期化する
                    (when (or (not *cur-game-tree*) (not chosen))
                      (setf chosen nil)
                      (web-initialize))
                    ;; ゲーム木における可能な手が空なら、ゲームを終了させる
                    ;; 人間のプレイヤーの手番なら、パラメータから指し手を取得し、htmlを組み立てる
                    ;; ゲームAIの手番なら、ゲームAIに指し手を選ばせ、htmlを組み立てる
                    (cond ((lazy-null (caddr *cur-game-tree*))
                           (web-announce-winner (cadr *cur-game-tree*)))
                          ((zerop (car *cur-game-tree*))
                           (web-handle-human
                             (when chosen
                               (read-from-string (cdr chosen)))))
                          (t (web-handle-computer))))
                  (tag br ())
                  ;; ゲーム盤を描く
                  (draw-dod-page *cur-game-tree* *from-tile*)))
      (princ "Sorry... I don't know that page.")))
;}}}

(defun web-initialize ();{{{
  "ゲームエンジンを初期化する
   ret: -"
  ;; ランダムなゲーム盤を作成して保持する
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))
;}}}

(defun web-announce-winner (board);{{{
  "勝者を表示する"
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (princ " play again")))
;}}}

(defun web-handle-human (pos);{{{
  "人間のプレイヤーを処理する
   pos: 選択したマスの番号"
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
             (cadr (lazy-find-if (lambda (move)
                                   (equal (car move)
                                          (list *from-tile* pos)))
                                 (caddr *cur-game-tree*))))
       (setf *from-tile* nil)
       (princ "You may now ")
       (tag a (href (make-game-link 'pass))
            (princ "pass"))
       (princ " or make another move:"))))
;}}}

(defun web-handle-computer ();{{{
  "ゲームAIプレイヤーを処理する"
  ;; ゲームAIにゲーム木を遷移させる
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  ;; webブラウザを5秒毎にリロードさせる
  ;; これによりリロードしたときにはコンピュータの手番とさせるために、chosen=NILとしている
  (tag script ()
       (princ "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))
;}}}

(defun draw-dod-page (tree selected-tile);{{{
  "HTMLの中にSVGゲーム盤を描く
   tree: ゲーム木
   selected-tile: タイルを選択中か"
  (svg *board-width*  ; ゲーム盤の幅
       *board-height* ; ゲーム盤の高さ
       (draw-board-svg (cadr tree)
                       selected-tile
                       ;; プレイヤーが選択可能なマスのリストを計算する
                       (take-all (if selected-tile
                                     ;; 攻撃元のタイルを選択中なら、
                                     ;; 有効な攻撃先を全て収集する
                                     (lazy-mapcar
                                       (lambda (move)
                                         (when (eql (caar move)
                                                    selected-tile)
                                           (cadar move)))
                                       (caddr tree))
                                     ;; 攻撃元のタイルを選択していなかったら、
                                     ;; 有効な攻撃から、攻撃元を収集する
                                     (lazy-mapcar #'caar (caddr tree)))))))
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; サーバ駆動関数ラッパ
;;; ------------------------------------------------------------------------------------------------
(defun serve (handler);{{{
  (princ "test1")
  (SBCL-SOCKET-SERVER:serve1 handler))
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; サーバテスト用ハンドラ
;;; ------------------------------------------------------------------------------------------------
(defun hello-request-handler (path header params);{{{
  "名前を問いかけて、得られたその名前を使って挨拶する
   CAUTION! リクエストパラメータをサニタイズしていないため、WANでの使用不可
   path: URLのパス部分
   header: HTTPヘッダフィールド
   params: URL末尾(GET用)とリクエストボディ(POST用)のリクエストパラメータ
   ret: レスポンスするHTMLドキュメント"
  (declare (ignore header))  ; 本関数ではHTTPヘッダフィールドは無視する
  ;; "/greeting"ページのみ提供する
  (if (equal path "greeting")
      ;; ページが"greeting"ならパラメータに合わせて表示処理を行う
      (let ((name (assoc 'name params)))
        (if (not name)
            ;; パラメータにnameが無ければ、もう一度名前を問いかける
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            ;; パラメータにnameがあれば、挨拶を表示する
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      ;; ページが"greeting"でなければ、要求されたページが無い旨を表示する
      (princ "Sorry... I don't know that page.")))
;}}}

