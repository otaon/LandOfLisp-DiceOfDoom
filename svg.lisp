;;; ------------------------------------------------------------------------------------------------
;;; ユーティリティライブラリ
;;; ------------------------------------------------------------------------------------------------
(defmacro let1 (var val &body body);{{{
  ;; 1対のみの変数束縛を行う(括弧が減らせる)
  ;; var: 束縛する変数名
  ;; val: 束縛する値
  ;; body: 実行する式
  ;; ret: bodyの実行結果
  `(let ((,var ,val))
     ,@body))
;}}}

(defmacro split (val yes no);{{{
  ;; val: 頭と残りに分解したいリスト
  ;; yes: valが空ではない時に実行される式
  ;; no: valが空の時に実行される式
  ;; **マクロの仕様**
  ;; もしvalが分解可能なら、yesの式が実行される。
  ;; このとき、`split`マクロは自動的に2つのローカル変数、`head`と`tail`を作り、リストの頭と残りをそれに格納する。
  ;; これにより、関数の中で`car`と`cdr`を呼ぶ手間を省ける。
  ;; リストが空だったら、noの式が実行される。
  (let1 g (gensym)  ; マクロ展開時にgにシンボル名を代入
        ;; マクロ展開時には既にgはシンボル名に評価されている
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))
;}}}

(eval-when (compile load eval);{{{
  (defun pairs (lst)
    "リストから、要素2つずつのコンスセルのリストを返す
     lst: 2要素ずつコンスセルを作る対象となるリスト
     acc: 作ったコンスセルを格納するアキュムレータ
     ret: リストから作ったコンスセルのリスト"
    (labels ((f (lst acc)
               (split lst
                      (if tail
                          ;; lstが空でなく、かつ、残り部分も空でない場合、
                          ;; => ((head . tail) これまでに作ったコンスセル達)
                          (f (cdr tail) (cons (cons head (car tail)) acc))
                          ;; lstが空ではないが、残り部分が空の場合、
                          ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
                          (reverse acc))
                      ;; lstが空の場合、
                      ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
                      (reverse acc))))
      (f lst nil))))
;}}}

;;; ------------------------------------------------------------------------------------------------
;;; svgライブラリ
;;; ------------------------------------------------------------------------------------------------
(defun print-tag (name alst closingp);{{{
  "xmlフォーマットの開きタグ、または、閉じタグを出力する
   name: タグ名
   alst: 属性名と属性値のコンスセルのリスト
   closingp: 閉じタグか否か"
  (princ #\<)  ; タグの開き角括弧
  ;; 閉じタグならタグ名の頭に/をつける
  (when closingp
    (princ #\/))
  ;; タグ名を小文字に変換する
  (princ (string-downcase name))
  ;; 小文字の属性名と属性値を出力する
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))  ; タグの閉じ角括弧
;}}}

(defmacro tag (name atts &body body);{{{
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))
;}}}

(defmacro svg (width height &body body);{{{
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink"
                   "http://www.w3.org/1999/xlink"
                   height ,height width ,width)
        ,@body))
;}}}

(defun brightness (col amt);{{{
  "指定した色の明度を変更する
   col: 色 (RGB値のリスト)
   amt: 明度の変化量
   ret: 明度を変更した色"
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))
;}}}

(defun svg-style (color);{{{
  "表面の色と、枠線の色のスタイルを出力する
   スタイルは、枠線の色=表面の色-100"
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color (brightness color -100))))
;}}}

(defun polygon (points color);{{{
  "ポリゴンとなる頂点座標情報と色を指定したsvg記述を出力する
   points: 頂点座標のリスト
   color: 描画する色
   ret: ポリゴンのsvg記述"
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))
;}}}

