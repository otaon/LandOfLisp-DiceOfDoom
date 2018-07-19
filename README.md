# DICE OF DOOM

## WHAT IS THIS
Land Of Lisp に載っているゲーム。  
ただし、CLISPとSBCLの両方で動作するように修正した。

## HOW TO PLAY

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

## プレイ画面
![screenshot](https://raw.githubusercontent.com/otaon/LandOfLisp-DiceOfDoom/master/screenshot.png)

## Dice of Doom のルール説明

- 2人のプレイヤーA,Bが、6角形のマス目のゲーム盤上でそれぞれ陣地を持っている。
- 各6角形には、その陣地を持つプレイヤーがサイコロをいくつか置いている。
- プレイヤーは自分の手番にいくつでも手を重ねて良いが、最低1回は手を刺さなければならない。
    - どちらかのプレイヤーに指す手がなくなったらゲーム終了となる。
- 指す手とは、敵の陣地となっている6角形を攻撃することである。
    - プレイヤーの陣地である6角形の中にあるサイコロの個数が、隣接する敵のサイコロよりも多ければ攻撃ができる。
    - *後のバージョンでは、攻撃の度にサイコロを振って戦いの成否を決めることになる。*
    - *現段階では、攻撃は常に成功するものとする。*
- 戦いに勝つと、敗者のサイコロは盤上から除かれ、勝者のマスにあったサイコロは1個を残して勝ち取ったマスに移る。
- 一方のプレイヤーが全ての動きを終えたら、そのプレイヤーの軍に補給が行われる。
    - その時点でプレイヤーが所有しているマスのうち左上のものから右に、そして次の行の左から右に、という具合にサイコロを1つずつ足していく。
    - 足せるサイコロの数は、その回でプレイヤーが相手から獲得したサイコロの総数から1つ減らした数。
- プレイヤーに打てる手がなくなったらゲーム終了となり、その時点でより多くのマスを所持しているプレイヤーが勝者となる。

```
# 初期状態
A3|A3
-----
B3|B1

# Aの攻撃
A3|A1
-----
B3|A2

# 補給
なし

# Bの攻撃
A3|A1
-----
B1|B2

# 補給
A3|A1
-----
B2|B2

# Aの攻撃
A1|A1
-----
A2|B2

# 補給
A2|A1
-----
A2|B2

# Bの攻撃
A2|B1
-----
A2|B1

# 補給
なし

# Aの攻撃
A1|A1
-----
A2|B1

# Aの攻撃
A1|A1
-----
A1|A1

# ゲーム終了(Aの勝利)
```

