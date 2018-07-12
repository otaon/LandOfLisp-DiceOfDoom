;;; ------------------------------------------------------------------------------------------------
;;; dice of doom v3
;;; 
;;; webブラウザ版 dice of doom
;;; ------------------------------------------------------------------------------------------------

;;; カレントディレクトリを変更する
#+clisp;{{{
(ext:cd "~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom")

#+sbcl
(progn
  (sb-posix:chdir #P"~/github/Books/LandOfLisp/LandOfLisp-DiceOfDoom")
  (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/"))))
;}}}

;;; 下記をコンパイル、ロードする
;;; - dice of doom v2
;;; - web server ライブラリ
;;; - svg ライブラリ
(compile-file "dod2");{{{
(compile-file "webserver")
(compile-file "svg")
(load "dod2")
(load "webserver")
(load "svg")
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
   col: サイコロの色(RGB値)"
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
(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  "六角形のマスとその上に積み上がったサイコロを描く
   x: マスのx座標(マス目)
   y: マスのy座標(マス目)
   pos: 描画対象のマス
   hex: プレイヤーIDとサイコロ数のコンスセル
   xx: マスの描画用x座標(pixel)
   yy: マスの描画用y座標(pixel)
   col: マスとサイコロの色
   chosen-tile: 選択中のマスの番号
   ret: "
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


