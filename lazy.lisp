;;; 遅延評価用の関数群
;;; リストを返す関数においては、遅延リストを返す

(defmacro lazy (&body body);{{{
  ;; 処理を関数に包んで返す
  ;; body: ラムダで包みたい処理
  ;; ret: 引数bobyで渡された処理をメモ化したlambda関数
  (let ((forced (gensym))
        (value  (gensym)))
    `(let ((,forced nil)  ;; bodyの処理が実行されたか判別するフラグ
           (,value nil))  ;; bodyの処理結果を保持する変数
       (lambda ()
         ;; bodyが未だ実行されていなければ実行して、結果をvalueに代入して返す
         ;; bodyが実行されていれば、保持しておいた結果を返す
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))
;}}}

(defun force (lazy-value);{{{
  "関数に包んだ処理を実行する
   lazy-value: 処理を包んでいる関数
   ret: 処理を包んでいる関数の実行結果"
  (funcall lazy-value))
;}}}

(defmacro lazy-cons (a b);{{{
  ;; 遅延評価版のcons
  ;; このマクロの戻り値で得られる関数をforceするとコンスセルが得られる
  ;; a: 作成したいコンスセルのcar部
  ;; b: 作成したいコンスセルのcdr部
  ;; ret: 遅延評価版コンスセル
  `(lazy (cons ,a ,b)))
;}}}

(defun lazy-car (x);{{{
  "遅延評価版のcar
   x: car部を取り出す対象となる遅延評価版コンスセル
   ret: 遅延評価版コンスセルのcar部"
  (car (force x)))
;}}}

(defun lazy-cdr (x);{{{
  "遅延評価版のcdr
   x: cdr部を取り出す対象となる遅延評価版コンスセル
   ret: 遅延評価版コンスセルのcdr部"
  (cdr (force x)))
;}}}

(defun lazy-nil ();{{{
  "遅延評価版のnil
   forceされるとnilを返す
   ret: 遅延評価版のnil"
  (lazy nil))
;}}}

(defun lazy-null (x);{{{
  "遅延評価版のnull
   遅延リストがnilならtを返す
   x: 処理が包まれている関数
   ret: t=処理結果がnil nil=処理結果がnilではない"
  (not (force x)))
;}}}

(defun make-lazy (lst);{{{
  "リストを遅延リストに変換する
   lst: 変換対象のリスト
   ret: 遅延リスト"
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))
;}}}

(defun take (n lst);{{{
  "遅延リストから指定した数の要素だけ取り出す
   n: 遅延リストから取り出したい要素の番号(0始まり)
   lst: 対象の遅延リスト
   ret: 取り出した要素"
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst)
          (take (1- n) (lazy-cdr lst)))))
;}}}

(defun take-all (lst);{{{
  "遅延リストをリストに変換する
   !!無限長の遅延リストには使用禁止!!
   lst: 変換対象遅延リスト
   ret: リスト"
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))
;}}}

(defun lazy-mapcar (fun lst);{{{
  "遅延評価版のmapcar
   fun: 遅延リストにmapcarする関数
   lst: 対象の遅延リスト
   ret: 関数適用済みの遅延リスト"
  (lazy (unless (lazy-null lst)  ; 遅延リストが空でないことを確認
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))
;}}}

(defun lazy-mapcan (fun lst);{{{
  "遅延評価版のmapcan
   # mapcan=各要素に対する計算結果をリストで得て、最後に繋ぎ合わせる
   fun: 遅延リストにmapcanする関数(遅延リストを返す)
   lst: 対象の遅延リスト
   ret: 関数適用済みの遅延リスト"
  (labels ((f (lst-cur)
             ;; 要素に対するfunの適用結果(遅延リスト)をlazy-mapcanの戻り値用の遅延リストに連結する
             ;; lst-cur: funの適用結果の遅延リスト
             (if (lazy-null lst-cur)
                 ;; lazy-curにfunを適用した結果が空だったら、lstのcdr部についてlazy-mapcanを実行する
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 ;; lazy-curにfunを適用した結果の各要素を、lazy-mapcanの戻り値用の遅延リストに連結する
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    ;; 遅延リストlstに対してfを適用する関数を返す
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))
;}}}

(defun lazy-find-if (fun lst);{{{
  "遅延リストから、条件に合う最初の要素を取り出す
   fun: 条件判定式
   lst: 対象の遅延リスト
   ret: 条件に合う最初の要素(無ければnil)"
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
        x
        (lazy-find-if fun (lazy-cdr lst))))))
;}}}

(defun lazy-nth (n lst);{{{
  "遅延リストのn番目の要素を取り出す
   n: 取り出したい要素の番号(0〜リスト長-1)
   lst: 対象の遅延リスト
   ret: n番目の要素"
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))
;}}}

