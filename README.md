# DICE OF DOOM

# HOW TO PLAY

1. serverを起動する

```lisp
> (load dod3.lisp)
> (serve #'dod-request-handler)
```

2. webブラウザで[ゲームページ](http://localhost:8080/game.html)にアクセスする
3. 攻撃元のマスをクリックして、その後に攻撃先のマスをクリックする
4. 攻撃したくなくなった、あるいは、攻撃できなくなったら、`pass`をクリックしてゲームAIに手番を渡す
5. ゲームAIは勝手に手番を進める
6. 自分の番が回ってきたら、再び攻撃をする
7. 上記の3から6を勝敗が決まるまで繰り返す

# プレイ画面
![screenshot](https://raw.githubusercontent.com/)
