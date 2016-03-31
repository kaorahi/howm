=begin

= ChangeLog Memo と howm

ChangeLog Memo 上でも howm が使えます.
(ChangeLog Memo 内で自己リンク) --- Case I

また, ChangeLog Memo と howm の併用もできます.
(ChangeLog Memo と howm memo との間で相互リンク) --- Case II

※ I と II の両立はできません.
インストール法なども違いますので,
どちらかを選んで該当する節をお読みください.

== 何がうれしい?

* ChangeLog Memo 上で howm のリンク機能が使える

* M-x occur, clgrep に加えて
  * @ を押せば表示形式をトグル (occur 風 ←→ clgrep 風)
  * occur 風表示では, RET を押さなくてもリアルタイムに内容を表示
  * さらにソートや多段の絞り込みも可能

* 例の浮沈式 todo list が使える

* メモ本体を汚さない
  * いつでもやめて, 素の ChangeLog に戻れる

= ■ (Case I) ChangeLog Memo 上で howm

ChangeLog Memo 内で自己リンクを張ります.

== できること

* 素の ChangeLog に加えて
  * goto link
    * 「>>> ほげ」の上で RET → 「ほげ」を検索
    * 「>>> ~/hoge.txt」の上で RET ×2 → ファイルを開く
    * 「>>> ~/hoge.pdf」の上で RET ×2 → 外部 viewer で開く
  * come-from link
    * 「* ほげ: ふがふが」という entry を書けば,
      メモ中の「ほげ」がすべてリンクに
    * 「ほげ」の上で RET → 「ほげ」の一覧
  * wiki link
    * [[ほげ]] の上で RET → entry 「ほげ」を追加
    * 以後は, [[ほげ]] の上で RET ×2 → entry 「ほげ」に飛ぶ

== インストール

* 想定例
  * ~/elisp/howm/ に一式を展開
  * ~/memo/clmemo.txt にメモをとる

* 手順例
  * メニューファイルをコピー
      cp ~/elisp/howm/ja/0000-00-00-000000.txt ~/.howm-menu
  * 以下を .emacs に
      ;; 設定に応じて
      (setq load-path (cons "~/elisp/howm" load-path))
      (setq howm-directory "~/memo")
      (setq howm-file-name-format "clmemo.txt")
      ;; 以下は決まり文句
      (setq howm-menu-lang 'ja)
      (setq howm-menu-file "~/.howm-menu")
      (require 'howm-mode)
      (howm-setup-change-log)

* アンインストール例
  * メモ本体は汚さないので, これだけで OK
      rm ~/.howm-*
      rm -rf ~/elisp/howm
      vi ~/.emacs
  
== 使い方

* ふつうに ChangeLog Memo をとってください
* entry に下線がつくので, その上で RET → 検索結果の一覧
* 一覧では
  * RET → jump
  * q → quit
* 詳しくは README 等を参照

= ■ (Case II) ChangeLog Memo と howm の併用

ChangeLog Memo と howm memo との間で相互リンクを張ります.

== できること

* ChangeLog Memo 上でも「<<< ほげ」「>>> ほげ」「[[ほげ]]」が機能
* <<< で指定したキーワードは, ChangeLog Memo 上でも下線 → ジャンプ

== インストール

* howm を普通にインストール
* .emacs に以下を追加
    (add-hook 'change-log-mode-hook 'howm-mode)
    (eval-after-load "howm-view"
      '(setq howm-view-title-regexp
             (concat howm-view-title-regexp
                     "\\|^[0-9-]+[ \t]+<.*>$")))

== 使い方

* ~/howm/ChangeLog に ChangeLog Memo をとってください.
* ChangeLog から howm へ
  * ChangeLog Memo 上で M-x howm-from-change-log
    → howm で新規メモを開いてタイトルをコピー
  * ChangeLog Memo 上で [[ほげ]] と書き, (カーソル戻して)その上で RET
    → <<< ほげ という howm memo を作る
* howm から ChangeLog へ
  * howm memo 上で M-x howm-to-change-log
    → ChangeLog Memo を開いてタイトルをコピー
* 相互に
  * どちらのメモからも, 日付上で RET
    → その日付のメモを両方一覧

== 補足

howm memo で RD 形式をお使いの方は,
((<URL:http://howm.sourceforge.jp/a/rd-mode-plus.el>))
も併用すると便利かもしれません.
上の howm-to-change-log のかわりに, 次の機能が使えます.

* M-x rd-to-change-log → ChangeLog Memo を開いて章・節のタイトルをコピー
  * 例
    * howm memo にこう書いてたら…
        = ほげ
        …
        == ふが
        …
        == ぴよ
        …
        ■ ← カーソル
    * こんな ChangeLog ができる
        2003-12-03  Foo Bar  <foo@bar.baz>
        	* ほげ
        	- ぴよ
  * 注
    * カーソルが属する章(=), 節(==)のタイトルを抽出します
    * 「ほげ」にカーソルを置いた場合, == のタイトルはコピーされません
    * C-u M-x rd-to-change-log なら, その章のすべての節のタイトルを抽出します
        2003-12-03  Foo Bar  <foo@bar.baz>
        	* ほげ
        	- ふが
        	- ぴよ


=end
