;;; ---------------------------------------------------------------
;;; dice of doom v3
;;; 
;;; webブラウザ版 dice of doom
;;; ---------------------------------------------------------------

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




