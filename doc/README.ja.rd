=begin

= howm (一人お手軽 Wiki もどき)
$Id: README.ja.rd,v 1.337 2012-12-29 00:59:48 hira Exp $

Emacs で断片的なメモをどんどんとるための環境です.
分類機能はあえてつけません.
かわりに, 全文検索とメモ間リンクが手軽にできるようにしました.
自由書式なので改宗も不要 :-)

== 目次

* ((<使い方>)) …
  ((<メモを書こう>))／((<メモを読もう>))／((<リマインダ>))
* ((<導入法>)) …
  ((<インストール>))／((<カスタマイズ>))／((<外部ツール>))
* ((<実装>)) …
  ((<実装について>))／((<動きませんよ?>))
* ((<備考>)) …
  ((<参考>))／((<更新記録>))／((<アドレス>))

== 使い方

* いろいろありすぎて何が何やら
  → 先に((<チュートリアル|URL:TUTORIAL.ja.rd>))をどうぞ
* 自由度ありすぎてどう使えばいいやら
  → ((<こんなふうに使えます|URL:index-j.html#hint>))

=== メモを書こう
(howm-mode)

* 手順
  * C-c , , (M-x howm-menu) でメニューを出し,
    [新規] にカーソル置いてリターン → 「今日のメモ」を開く
    * または, 直接 C-c , c (M-x howm-create)
  * こんなテンプレートが表示される
      = ■ ←(タイトル欄)
      [2002-09-16 20:08] >>> /home/hira/elisp/howm/howm-mode.el
      ↑(作成日 & その前に見てたファイル)
    * 見てたファイルが不要なら, undo (C-x u だか C-_ だか) で消す
    * テンプレート自体が不要なら, 続けてもう一度 undo
  * 好きなことを好きなように書く
  * そんだけ.

* タイトル欄 (1 ファイル複数メモ)
  * 次のように書くと, foo と bar がタイトル
      = foo
      (… ほげほげ …)
      = bar
      (… ふがふが …)
    * 連結表示では, (… ほげほげ …) や (… ふがふが …) がひとかたまり
  * 正確な書式は,
      (行頭)=(空白)(タイトル)(行末)
    * 書式は変更可 (→((<カスタマイズ>)))
  * タイトルなしで, 単にメモの区切りとして使うだけでも OK
      (… ほげほげ …)
      = 
      (… ふがふが …)
      = 
      (… へろへろ …)

* 次のように書くとリンク
  * goto リンク: ファイル(ディレクトリ)名 or 含まれる文字列
      >>> ~/.emacs
      >>> /usr/src
      >>> ほげほげ
    * 本文中に「ほげほげ」という文字列を含むメモへのリンク
  * come-from リンク: 他のメモで「ふがふが」という文字列が出てきたら,
    ぜんぶこのメモへのリンクに
      <<< ふがふが
    * 参考
      ((<"Jargon: COME-FROM"|URL:http://catb.org/~esr/jargon/html/C/COME-FROM.html>))
  * Wiki 風リンク: goto と同じ. ただし「<<< へろへろ」がもしなければ作る.
      [[へろへろ]]

* リンクには下線が引かれる. 下線にカーソル持ってってリターンキー!
  * 該当ファイルの一覧が表示される (→((<メモを読もう>)))
    * たとえば, 「>>> emacs」ならこんな一覧
        <<< emacs             ← ずばりの come-from 宣言をしたメモ
        <<< emacs lisp        ← 「emacs」を含む come-from を宣言したメモ
        <<< 自作 emacs lisp
        grep, ruby, emacs の regexp の違い ← 本文中に「emacs」を含むメモ
        emacs 用検索ツール?[2001-08-13]       (新しい順)
        …
    * 読みたいメモにカーソルあわせてリターンキー!
      → そのメモを開く
  * 裏技
    * come-from リンクの <<< 上でリターン → 「関連キーワード」へのリンク
      * 例
        * 「自作」「lisp」が come-from キーワードのとき
        * 「<<< 自作の lisp」の上でリターン
          → 「自作」「lisp」を含むキーワードがヒット
    * メモ中に「<<< foo <<< bar <<< baz」と書けば, 「alias」
      * foo, bar, baz のどれでリターンを叩いても
        「foo または bar または baz」の検索になります
      * Tips: 互いにたぐりたいけど alias で混ぜるのは嫌, という場合には…
        * どこかに「<<< foo」
        * 別のどこかに「foo <<< bar」
        * こうすれば, foo・bar どちらで検索しても「foo <<< bar」が上位に

* リンクの真相
  * 実は単に, 「grep ふがふが」のショートカットだったり
  * come-from リンクの効果
    * このキーワードが出てきたら, 自動的にリンク(= 検索)にしてくれ
    * そのキーワードを検索したときは, 先頭に表示してくれ
  * come-from リンクは…
    * なくてもよし
    * 1 つのメモ内に 2 個 3 個とあってもよし
    * 別のメモと同じキーワードがかぶってもよし
    * タイトルと兼ねるなら,
        = <<< ふがふが
  * come-from, goto とも, 大文字小文字を区別 (→((<カスタマイズ>)))
  * 書式は変更可 (→((<カスタマイズ>)))
  * 以下, come-from リンクのキーワードを単に「キーワード」と表記

* action-lock
  * 呪文の上でリターンキーたたくと魔法発動
  * { } と書くと「トグルスイッチ」.
    たたくたんびに { } → {*} → {-} → { } → …
  * {_} と書くと「未処理」.
    たたけば {_} → [2002-09-13 02:31]
  * http://… → ブラウザ起動
    * browse-url を使用. 必要なら適当に設定.
        (setq browse-url-browser-function 'browse-url-mozilla)
  * file://… → ファイルを開く
    * C-u RET なら窓を分割して開く
  * [2002-10-18] のような日付形式の上でリターン → minibuffer で…
    * そのままリターン → その日付を検索 (goto link)
    * 「+17」 → 17 日後の日付に書きかえ
    * 「20030921」 → [2003-09-21] に書きかえ
      * 年や月は省略可能
        * 「6」 → [2002-10-06]
        * 「803」 → [2002-08-03]
        * 「31103」 → [2003-11-03]
    * 「~20031030」 → その行の複製を [2003-10-30] 分まで挿入
      * 年や月は省略可能 (上と同様)
      * 「Every?」に対して
        * そのままリターン → 毎日
        * 3 → 3日ごと
        * w → 毎週
        * m → 毎月
        * y → 毎年
    * 「.」 → 今日の日付に書きかえ
    * ちなみに, メニューの [日↓] で日付形式を入力できます
  * リンクもこの呪文の一種
    * 他におもしろいアイデアあったら教えてください

* コマンド (★は howm-mode 以外でも常に有効)
  * C-c , , → メニューを開く ★
  * メニュー
    * キー
      * [space] と [backspace] → スクロール
      * TAB (M-TAB) → 次(前)の項目へ
      * [○○] や > の上でリターン → 実行 (ジャンプ)
      * ? → ヘルプ
      * q → 脱出
    * ボタン [○○] (コマンド)
      * 作成
        * [速記] (C-c , e) → ぱぱっとメモとり (C-c C-c で保存) ★
        * [新規] (C-c , c) → 新規メモ作成 (現リージョンがタイトル) ★
        * [複製] (C-c , D) → 現メモを複製 (住所録テンプレートなどの用途を想定)
      * 一覧
        * [一覧] (C-c , a) → 全メモの一覧 ★
        * [最近] (C-c , l) → 最近のメモの連結表示 ★
          * (C-u 20 C-c , l) → 最近 20 日分の一覧
        * [前後] (C-c , A) → 前後のメモ (見てたメモを中心に全メモの日付順一覧)
          * 対象ファイルを(編集モードで)開いた状態からメニューを呼ぶこと
        * [履歴] (C-c , h) → 検索履歴 ★
        * [予定] (C-c , y) → 予定表: ((<リマインダ>))参照 ★
        * [Todo] (C-c , t) → todo 一覧: ((<リマインダ>))参照 ★
        * [全バ] (C-c , b) → バッファ一覧 ★
        * [mark] (C-c , x) → バッファ内のマーク位置一覧 ★
      * 検索
        * [正規] (C-c , g) → 正規表現の検索 ★
          * 基本的には大文字小文字の区別なし
            * 「Wiki」のように明示的に大文字を指定したときは区別
        * [固定] (C-c , s) → キーワードを補完入力して固定文字列の検索 ★
          * C-u C-c , g や C-u C-c , m でも
        * [roma] (C-c , m) → ローマ字検索 (migemo) ★
        * [今日] (C-c , .) → 今日のメモ ★
          * (C-u 20 C-c , .) → 20 日前のメモ
        * [昨日] (C-c , :) → 昨日のメモ ★
          * (C-u 20 C-c , :) → 20 日前のメモ
        * [バ内] (C-c , o) → バッファ内を正規表現検索 ★
      * 編集: 対象ファイルを(編集モードで)開いた状態からメニューを呼ぶこと
        * [更新] (C-c , r) → 下線を引きなおす
        * [鍵↓] (C-c , i) → キーワードを補完入力して貼りつけ ★
          * Tips: M-v で候補一覧に移って migemo 検索すると楽
        * [日↓] (C-c , d) → 今日の日付 [yyyy-mm-dd] を貼りつけ ★
        * [時↓] (C-c , T) → 今日の日時 [yyyy-mm-dd HH:MM] を貼りつけ ★
        * [題↑] (C-c , K) → 現メモのタイトルを kill ring へ (C-y で貼りつけ) ★
          * タイトルがみつからなかったときはファイル名
        * [名↑] (C-u C-c , K) → ファイル名を kill ring へ ★
      * 特別
        * [menu 更新] (R) → メニューの予定表などを更新
        * [menu 編集] → メニューを編集
        * [全消] (C-c , Q) → howm-mode なバッファをすべて消す (未保存は除く) ★
        * [酔歩] (C-c , w) → ランダムにリンクをたどって自動閲覧. C-g で停止. ★
  * その他
    * [return] → リンク上なら該当ファイルを開く. さもなくば改行.
    * 移動
      * C-c , n → 次のリンクへ
      * C-c , p → 前のリンクへ
      * 一ファイル複数メモのとき…
        * C-c , N → 次のメモへ
        * C-c , P → 前のメモへ
        * C-c , H → 最初のメモへ
        * C-c , L → 最後のメモへ
    * 新規メモ
      * C-c , C → いま開いてるファイルに追加
        * メニューに [追加] と書くと, この動作のボタン.
          英語メニューなら [Add].
      * C-c , I → ファイル名を手動で (非推奨)
        * C-u C-c , I なら, カレントディレクトリに
    * narrow (1 ファイル複数メモのとき)
      * M-x howm-narrow-to-memo → 前後のメモを隠す. 戻すには M-x widen
      * M-x howm-toggle-narrow → 「隠す」「見せる」をトグル
    * C-c , SPC → howm なバッファと howm でないバッファとを切り替え ★
    * M-x howm-show-buffer-as-howm → 現バッファのコピーを howm-mode で表示 ★
      * 需要不明なので様子見[2003-09-29]

=== メモを読もう
(一覧モード)

* コマンド(再掲)
  * C-c , , (M-x howm-menu) → メニュー
  * C-c , a (M-x howm-list-all) → 全メモ一覧
  * C-c , g (M-x howm-list-grep) → 全メモ検索 (正規表現)
  * C-c , s (M-x howm-list-grep-fixed) → 全メモ検索 (固定キーワード)

* 検索やリンクジャンプをすると, 一覧モード
  * デフォルトは一覧表示
    * 一覧バッファ + 内容バッファ
    * カーソル位置のメモの内容が表示される
  * 連結表示もできる
    * @ で連結表示. もう一度 @ で一覧表示に戻る.
    * ヒットしたメモの内容をぜんぶつなげて表示
      * 断片的なメモをどんどん書く → つなげて読む
    * [tab] と [alt]-[tab] で次/前のメモへ
    * Tips: メモを探すとき, 検索である程度しぼりこんだら,
      連結表示して migemo 検索すると楽
  * 一覧表示で
    * 0 → 連結表示のトグル (@ と同じ)
    * 1 → 内容バッファを消す
    * 2 → 内容バッファを出す
    * v → 内容バッファをトグル
    * TAB, M-TAB → 次・前のファイルへ
    * T → タイトル表示をトグル
  * どちらの表示でも
    * n と p → 上下
    * [space] と [backspace] → スクロール
    * j と k → 一行スクロール
    * [return] → カーソル位置のメモを開く
      * C-u して [return] → メモを開いて一覧を消す
    * X → Dired-X を起動 (改名・削除などのファイル操作)
      * Dired-X の使い方は, info dired-x 等を参照
          v → 中身を見る (q → 戻る)
          d → 「消すぞ」マーク
          x → マークしたファイルたちを本当に消す
    * ? → ヘルプ
    * q → 脱出

* ソート
  * S → 何でソートするか聞いてくる (補完入力)
    * name: ファイル名
    * name-match: 指定したファイル名を上位に移す
    * date: 作成日
    * mtime: 更新時刻
    * summary: 一行表示の文字列
    * summary-match: 指定した正規表現を一行表示から検索して, 上位に移す
    * summary-match-string: 同上 + マッチした文字列順にソート
    * random: ランダムシャッフル
    * reminder: リマインダ順
    * numerical-name: ファイル名 (数字順. メールのソートを想定)
    * reverse: 現表示の逆順
  * C-u S ならデフォルトの逆順
  * R → reverse

* 絞りこみ (and 検索)
  * f → 何で絞りこむか聞いてくる (補完入力)
    * name: ファイル名
    * date: 作成日
    * mtime: 更新時刻
    * summary: 一行表示の文字列
    * contents: 内容
    * reminder: リマインダの日付範囲
    * Region: 領域
    * Around: カーソル位置の周辺
      * C-u 7 f → Around なら, 前後 7 つ
    * uniq: 同じファイル中で何箇所ヒットしても, 最初の一箇所だけ表示
  * C-u f なら, マッチしたものを取り除く
  * G → contents
  * u → uniq

* howm-mode と共通
  * l → 全メモの一覧
  * g → 検索 (grep)
    * C-u g → キーワードを補完入力して検索
  * m → ローマ字検索 (migemo)
    * C-u m → C-u g と同じ
  * c → 新規ファイル作成 (現リージョンがタイトル)
  * Q → howm-mode なバッファをすべて消す (未保存は除く)

* その他
  * 一覧表示で !  → shell でコマンド実行
    * メモを手っ取り早く捨てたければ, これで mv なり rm なりしてください
    * 2 回目からは小賢しい挙動をします :-)
  * >>> hoge.png なら外部 viewer で画像を開く
    * 設定は((<カスタマイズ>))参照

=== リマインダ
(予定表・todo)

* 機能
  * メモ中に
      [2002-10-20]+ ハイウェイ惑星 買おう
    のように書いておくと, 一覧で見ることができます
    * C-c , y → 予定表
      * . → 今日へ
    * C-c , t → todo 一覧
      * 一覧中の上下どの位置に表示されるかは, 日付と種類しだい
  * 「最近の予定」と「todo 冒頭」はメニューにも表示されます
    (ことあるごとにちらっと見えるのが重要かと)
    * メニューでは, 行頭の「>」上で RET を叩くとメモに飛びます
      (それ以外の位置でも, 下線がない所なら同様)
  * カレンダーソフト plan への export も可能 (→((<外部ツール>)))

* 書式
  * 覚書 (-)
      [2002-10-20]- ハイウェイ惑星 買おう
    * 指定日に浮きあがり, 以後は徐々に沈む
    * 指定日までは底に潜伏
    * 沈むのを遅くするには, 猶予日数で指定(デフォルト 1 日)
        [2002-10-20]-14 ハイウェイ惑星 買おう → 14 日間ぐらいは気にかけよう
  * todo (+)
      [2002-10-20]+ ハイウェイ惑星 買うべし
    * 指定日から, 徐々に浮きあがってくる
    * 指定日までは底に潜伏
    * 浮きあがる速さは, 猶予日数で指定(デフォルト 7 日)
        [2002-10-20]+14 ハイウェイ惑星 買うべし → 14 日間ぐらいのうちに
  * 〆切 (!)
      [2002-10-20]! ハイウェイ惑星 〆切
    * 指定日が近づくと, 浮きあがってくる
    * 指定日以降は, 一番上に浮きっぱなし
    * 何日前から浮きはじめるかは, 猶予日数で指定(デフォルト 7 日)
        [2002-10-20]!14 ハイウェイ惑星 〆切 → 14 日前ぐらいからぼちぼち
    * 予定表(後述)にも表示
  * 保留 (~)
      [2002-10-20]~ ハイウェイ惑星 買おうか
    * 指定日から, 浮き沈みをくりかえす
    * 指定日までは底に潜伏
    * 何日周期で浮き沈みするかは, 猶予日数で指定(デフォルト 30 日)
        [2002-10-20]!14 ハイウェイ惑星 買おうか → 14 日周期
  * 予定 (@)
      [2002-10-20]@ ハイウェイ惑星
    * todo 一覧ではなく, 予定表に表示
  * 済 (.)
      [2002-10-20]. ハイウェイ惑星
    * 常に底

* action-lock
  * 例
      [2002-10-20]+9 ほげほげ
    の「+9」にカーソル置いてリターンたたくと, ミニバッファにメニューが出て…
    * そのままリターン → 「済」
        [2002-10-20]. [2002-10-20]:+9 ほげほげ
    * x を入力 → 「cancel」
        [2002-10-20]. cancel [2002-10-20]:+9 ほげほげ
    * - を入力 → 種類を覚書に変更
        [2002-10-20]-9 ほげほげ
    * 14 を入力 → 猶予日数を 14 日に変更
        [2002-10-20]+14 ほげほげ
  * メニュー・予定表・todo 一覧からも直接叩けます

* Tips (私の使い方)
  * 「todo」や「〆切」は本当に必要なものだけ
    * それ以外は「覚書」で沈むにまかせる (どうせ全部はできません :p)
    * 後ろめたければ, 猶予日数の長い「覚書」に
        [2002-11-10]-10 ハイウェイ惑星
  * 緊急ではないが重要なこと
      [2002-11-10]-999 ●ハイウェイ惑星
  * 目立たせたいこと
      [2002-11-10]! ★★ハイウェイ惑星

== 導入法

=== インストール

==== MELPA でスナップショット版をインストールする場合

* パッケージ「howm」を ((<MELPA|URL:https://melpa.org/#/getting-started>)) でインストール

==== 自動インストールの場合

* インストール
  * ./configure して make して, root になって make install
    * *.el, *.elc は /usr/share/emacs/site-lisp/howm/ に
    * doc/, ext/ は /usr/local/share/howm/ に
  * xemacs の場合
      ./configure --with-xemacs
    * *.el, *.elc は /usr/lib/xemacs/site-lisp/howm/ に
  * インストール先の変更例
      ./configure --with-howmdir=$HOME/elisp --prefix=$HOME
    * *.el, *.elc は ~/elisp/ に
    * doc/, ext/ は ~/share/howm/ に
  * その他のオプションは ./configure --help を参照
* 設定
  * ~/.emacs (.emacs.el かも)に追加
    * case 1: emacs 起動時に読み込む
        (setq howm-menu-lang 'ja)
        (require 'howm)
    * case 2: はじめて C-c , , した時に読み込む
        (setq howm-menu-lang 'ja)
        (global-set-key "\C-c,," 'howm-menu)
        (autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
    * いずれも, もし「Cannot open load file」とかエラーが出るなら,
      上記の前にこれを追加
        (add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
  * ~/howm/ の作成やメニューファイルのコピーは不要です
    (メニュー起動時に自動作成)

==== 手動インストールの場合

* *.el を適当な場所に置く (例: ~/elisp/howm)
  * ~/.emacs (.emacs.el かも)に
    * 置き場に応じて, ↓のように記述
        (add-to-list 'load-path "~/elisp/howm/")
    * さらに, ((<自動インストールの場合>))と同様の記述を追加
  * お好みで, バイトコンパイル
      cd ~/elisp/howm
      \emacs -batch -q --no-site-file --eval '(progn (add-to-list (quote load-path) ".") (byte-recompile-directory "." 0))'

==== インストールの補足

* お好みで, ~/.emacs に設定を追加 (→((<カスタマイズ>)))
    ;; 設定例
    (define-key global-map [katakana] 'howm-menu) ; [カタカナ] キーでメニュー
    (setq howm-file-name-format "%Y/%m/%Y_%m_%d.txt") ; 1 日 1 ファイル
    (setq howm-keyword-case-fold-search t) ; <<< で大文字小文字を区別しない
    (setq howm-list-title nil) ; 一覧時にタイトルを表示しない
    (setq howm-menu-refresh-after-save nil) ; save 時にメニューを自動更新せず
    (setq howm-refresh-after-save nil) ; save 時に下線を引き直さない
    (setq howm-menu-expiry-hours 2) ; メニューを 2 時間キャッシュ

* なお, キーワード一覧は ~/.howm-keys に記録される
  * 万一壊れても, 再構築は簡単. 大文字小文字の区別に応じて…
    * 区別する場合
        find ~/howm -name '*.txt' -print | xargs ruby -ne '$_ =~ /<<<\s+(.+)$/ and puts $1.split(/\s*<<<\s*/).join "\t"' | sort -u > ~/.howm-keys
    * 区別しない場合
        find ~/howm -name '*.txt' -print | xargs ruby -ne '$_ =~ /<<<\s+(.+)$/ and puts $1.downcase.split(/\s*<<<\s*/).join "\t"' | sort -u > ~/.howm-keys

* 注意
  * GNU Emacs 以外の場合:
    私はよくわからないので, 検索してください
    * meadow:
      ((<設定済み Meadow|URL:http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=meadowmemo%20edition>))
      なら最初から使えます
      * cygwin + grep 使用のときは,
        メモディレクトリとコマンドをドライブレターから指定する.
        * ~/.emacs(.emacs.el かも) で↓のように
            (setq howm-directory "c:/cygwin/home/howm/")
        * cygwin から見た / と emacs から見た / が食い違うとかいう話.
    * xemacs:
      * font-lock のメッセージを抑制すると速くなるそう.
        thx > ((<笠原さん|URL:http://eron.info/k/diary/>))
          (setq font-lock-verbose nil)
    * Linux Zaurus:
      ((<Wiki|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      を参照ください.
      thx > ((<100|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))
  * SKK を使う場合
    * .emacs に以下を書いておかないと, Dired-X に C-x C-j を奪われます
        (setq dired-bind-jump nil)
  * viper-mode を使う場合
    * viper-mode より先に howm-mode をロードしておく
      * post-command-hook に悪さする??
  * コンソール (emacs -nw) の場合
    * 下線が表示されない端末なら
        (set-face-foreground 'action-lock-face "blue") ;; 下線のかわりに色つけ
  * ((<RD|URL:http://www.ruby-lang.org/ja/man/html/RD.html>))を使う場合
    * <<< が RD の include とかぶる
    * 対策例
      * include は使わない. 行のはじめに <<< を書かないよう注意する.
      * include は使わない. rd2 をかける前に howm2 -type=rd を通す.
      * リンク記号を変更する
          ;; 例: .emacs (howm ロードより前)に
          (setq howm-ref-header "==>") ; goto リンク
          (setq howm-keyword-header "<==") ; come-from リンク
      * ((<→ howm wiki の「併用ツール」|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))も参照

* 旧版からの移行 (必ずバックアップをとってから!) → ((<URL:OLD.rd>))
  * 新たに make install しても, 個人のメニューファイルを上書き更新はしません.
    必要なら, メニューを自分で編集するか,
    ja/0000-00-00-000000.txt を自分でコピーするかしてください.

=== カスタマイズ

基本的には M-x customize → [Applications] → [Howm] で.
ぴんとこない項目も, [Show] でありがちな既定値から選択可能.

そこにない設定については, ~/.emacs (~/.emacs.el かも)へ, 以下のように直接書く.
(もっと網羅的だが古い解説は, ((<URL:OLD.rd>))を参照)

* 色
  * howm 関連の全バッファに共通の色設定
      ;; 「ほげ」と「[ふが]」に着色
      ;; ・設定法の詳細は, 変数 font-lock-keywords のヘルプを参照
      ;; ・face の一覧は M-x list-faces-display
      (setq howm-user-font-lock-keywords
        '(
          ("ほげ" . (0 'highlight prepend))
          ("\\[ふが\\]" . (0 'font-lock-doc-face prepend))
          ))
    * todo や予定の色わけにでも使ってはいかがかと.
  * 内容バッファに rd-mode な色をつける
      ;; rd-mode.el が読み込まれているという前提で
      (setq howm-view-contents-font-lock-keywords rd-font-lock-keywords)

* 便利キー
  * 「カタカナ」でメニュー, 「Ctrl-カタカナ」で新規メモ
      (define-key global-map [katakana] 'howm-menu)
      (define-key global-map [(control katakana)] 'howm-create)
  * [tab]([alt]-[tab])で次(前)のリンクに移動
      (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
      (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)
    * 本来の tab は C-i で

* 保存場所
  * メモ置き場/年/年月日-時分秒.txt に
      (setq howm-file-name-format "%Y/%Y%m%d-%H%M%S.txt")
    * ファイル名自体に年月日が入っていないと, filter-by-date が機能しない
  * 1 日 1 ファイル (メモ置き場/年/月/年_月_日.txt に)
      (setq howm-file-name-format "%Y/%m/%Y_%m_%d.txt")
    * 不完全な点があります. 我慢できる人だけどうぞ
      * メモ単位であるべき処理の一部がファイル単位に
        (タイトル表示, 更新順一覧, 内容での絞りこみ, uniq)
  * キーワード一覧を ~/howm/.howm-keys に置く
      (setq howm-keyword-file "~/howm/.howm-keys") ;; デフォルトは ~/.howm-keys
    * こうしておけば, 違うマシンでも ~/howm/ 以下のコピーだけで済む.
    * すでに書いたメモがあるなら, mv ~/.howm-keys ~/howm/ をしておくか,
      再構築する(→((<インストール>))).
    * デメリット: 検索が遅くなる? (体感できるほどかは, やってみないと不明)

* 一覧
  * 一覧で「!」したときの初期コマンドを変更
      (setq howm-view-summary-shell-last-file "_FILE_")
      (setq howm-view-summary-shell-hist
        '("mv _FILE_ ~/gomi" "touch _FILE_" "ls -l _FILE_"))
    * 初期コマンドは「mv ファイル名 ~/gomi」
    * M-p 押していくと, 「touch ファイル名」や「ls -l ファイル名」
  * 一覧バッファの色つけ例
      (setq howm-view-summary-font-lock-keywords '(("^2003" . 'highlight)))

* メニュー
  * メニューの変更
    * メニューを開いて [menu 編集] 上でリターン → 自由に編集
    * よく開くメモへの goto リンクなどを書いておけば便利かと
  * メニューファイルに「%recent」や「%random」と書くと,
    「最近のメモ」や「ランダムに選んだメモ」のタイトル一覧
    * カスタマイズ
        (setq howm-menu-recent-num 20)  ;; 表示する個数
  * メニュー中に変数や関数の値を表示
    * メニュー中にこう書くと…
      * %here%foo     → foo の値を表示
      * %here%(foo 3) → (foo '3) の結果を表示
        * 例: %here%(howm-menu-search "ほげ") → 「ほげ」の検索結果を埋め込み
        * ただし, 登録した関数しか使えません (おっかないから)
            (setq howm-menu-allow
                  (append '(foo bar) howm-menu-allow)) ;; foo と bar を許可
  * メニューをメモ扱いしない (メモ一覧・検索の対象外に)
      ;; mv ~/howm/0000-00-00-000000.txt ~/hoge/fuga/menu.txt しといて…
      (setq howm-menu-file "~/hoge/fuga/menu.txt")
  * %reminder の仕切り例
      (setq howm-menu-reminder-separators
            '(
              (-1  . "━━━━━━━今日↓↑超過━━━━━━━")
              (0   . "━━━━━━━予定↓━━━━━━━")
              (3   . "━━━━━━━もっと先↓↑3日後まで━━━━━━━")
              (nil . "━━━━━━━todo↓━━━━━━━") ;予定とtodoの境
              ))

* もっと軽く (cf. ((<富豪的プログラミング|URL:http://pitecan.com/fugo.html>)))
  * 上述の M-x customize で [Howm Efficiency] を参照
  * 特に, 本気で使うには howm-view-use-grep の設定をおすすめします
    * grep 使用時の coding system 指定
        (setq howm-process-coding-system 'euc-japan-unix) ;; 読み書き共通
        (setq howm-process-coding-system '(utf-8-unix . sjis-unix)) ;; (読.書)
  * Tips: gc-cons-threshold の値を増やすと速くなる場合がある.
    ref > ((<220,234-236|URL:http://www.bookshelf.jp/2ch/unix/1077881095.html>))
      (setq gc-cons-threshold (* 4000 10000))
  * Tips: grep-2.5 では, 環境変数 LANG を C にしておくと,
    マルチバイト対応がオフになって速くなる
    ((<ref|URL:http://search.luky.org/vine-users.5/msg06363.html>))

* 検索
  * 対象ディレクトリの追加
    * 全文検索のとき, メモに加えて指定ディレクトリ以下も再帰的に探す
        (setq howm-search-path '("~/Mail" "~/News"))
        (setq howm-search-other-dir t) ;; 下記のトグルの初期値 (t か nil)
    * M-x howm-toggle-search-other-dir で,
      上記ディレクトリを検索対象にするかしないかトグル
      * キーバインドしたければ各自で (インターフェース模索中につき…)

* 未保存だろうと委細構わず, howm-mode なバッファをすべて強制削除するコマンド
  (おすすめしません. 使わないでください.)
  * C-u C-c , Q
  * メニューに書くなら [強制全消]
  * 物騒なので, ↓を書いとかないと無効
      (setq howm-kill-all-enable-force t)

* テンプレートの変更例
  * こんなふうに
      Subject: タイトルバーに時計を表示 ←直前のリージョンの内容
      Date: Thu, 12 Sep 2002 15:45:59 +0900
      In-Reply-To: </home/hira/sawfish/rich-title/rich-title.jl> ←直前ファイル
      
      ■ ← カーソル
    * ~/.emacs に
        (setq howm-template "Subject: %title\nDate: %date\n%file\n%cursor")
        (setq howm-template-date-format "%a, %d %b %Y %H:%M:%S %z")
        (setq howm-template-file-format "In-Reply-To: <%s>\n")
  * テンプレートを複数指定
      ;; C-u 2 C-c , c → 2 番目のテンプレートで新規メモ
      ;; メニューから C-u 2 c でも同様
      (setq howm-template
            '("= %title%cursor\n%date %file\n\n" "%date: %title%cursor"))
    * ついでに, howm-template の値が関数なら
      「universal-argument と直前のバッファを引数にしてそいつを呼ぶ」
      っていうのも仕込みました

* 書式の変更例 (howm-*.el の load より前に)
  * タイトル(メモ区切り) @@@ …
      (setq howm-view-title-header "@@@")
  * goto リンク ==>…, come-from リンク <==…
      (setq howm-ref-header "==>")
      (setq howm-keyword-header "<==")
  * goto リンク ((＜…＞)), come-from リンク ((：…：))
      ;; ＜＞：は半角に直してください
      (setq howm-ref-regexp "((＜\\([^＞\r\n]+\\)＞))")
      (setq howm-ref-regexp-pos 1)
      (setq howm-keyword-format "((：%s：))")
      (setq howm-keyword-regexp "\\(((：\\)\\([^：\r\n]+\\)：))")
      (setq howm-keyword-regexp-hilit-pos 1) ;; 「関連キーワード」用
      (setq howm-keyword-regexp-pos 2)
      (setq howm-keyword-regexp-format "%s") ;; M-x describe-variable 参照
    * 注: come-from キーワードの alias では,
      次のどちらかしか想定していません.
      * 「…から後」型: <<< foo <<< bar <<< baz
      * 「…から…まで」型: ((：foo：)) ((：bar：)) ((：baz：))
  * wiki 風リンク [[hoge]] の下線を「]]」だけに
    * 「<<< hoge」の作成後は, 「hoge」にも下線
        (setq howm-wiki-regexp "\\[\\[\\([^]\r\n]+\\)\\(\\]\\]\\)")
        (setq howm-wiki-regexp-hilit-pos 2)
        (setq howm-wiki-regexp-pos 1)

* こまごま
  * 日付入力(C-c , d または [日↓])で年や月を略したら, 「未来」と解釈
      (setq howm-insert-date-future t)
    * 新規入力時のみです. 「[2003-12-27]」上で RET したときの動作は従来どおり.
  * 「http://」でリターン押したら, URL を kill-ring へ
      (setq action-lock-no-browser t)

* 予定表・todo 一覧
  * リマインダ記号(!+-~@.)から RET 一発で「済」に
      (setq howm-action-lock-reminder-done-default "")
    * この場合, C-u RET で従来の動作 (キャンセル, 記号変更, …)
  * 予定表・todo 一覧からリマインダ記号上で直接 RET したとき,
    叩かれ先バッファを自動 save
      (setq howm-action-lock-forward-save-buffer t)
    * 「自動 save」に抵抗ない方だけどうぞ
    * 手動で C-x s (未保存バッファたちを save)なりする方が正道かと
  * 保留の浮沈範囲
      (setq howm-todo-priority-defer-init -14)  ;; 初期値 = 下限
      (setq howm-todo-priority-defer-peak 0) ;; 上限
  * !+-~. の旬度のカスタマイズ
    * 例: メニューで, 「潜伏中は非表示」「済は表示」
        (setq howm-menu-todo-priority -50000)
        (setq howm-todo-priority-done-bottom -44444)
    * howm-todo-priority-normal-bottom 等. ソース(howm-reminder.el)参照.
  * todo 一覧(M-x howm-list-todo)の仕切り例
      (setq howm-todo-separators
            '(
              (0  . "━━━━━━━↑超過━━━━━━━")
              (nil . "━━━━━━━潜伏中↓━━━━━━━")
              ))
    * 連結表示やソートをする場合にはじゃまかも…

* action-lock
  * { } (トグルスイッチ)の変更
      ;; howm の load 前に
      (setq action-lock-switch-default '("{ }" "{*}" "{-}")) ;; 何個でも
  * {_} (未処理)の変更
      (setq howm-dtime-format "[%a %b %d %H:%M:%S %Y]") ;; {_}
      (setq howm-template-date-format "[%Y-%m-%d %H:%M]") ;; テンプレート
  * 「file://…」や「http://…」の変更 (ましな設定募集)
    thx > ((<945|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))
      ;; howm (正確には action-lock.el) のロードより前に.
      ;; ・file://…
      (setq action-lock-open-regexp
            "\\<file://\\(localhost\\)?\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>")
      (setq action-lock-open-regexp-pos 2) ;; 2 個目の「\\(…\\)」がファイル名
      ;; ・http://…
      (setq action-lock-browse-regexp
            "\\<\\([htp]\\{3,5\\}s?\\|ftp\\)://\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>"
      (setq action-lock-browse-regexp-pos 0) ;; マッチした全体が URL
  * action-lock 追加例:
    「Message-ID: …」でリターン押したら, 該当メールを namazu で検索
      ;; howm を load した後に
      (defun my-howm-search-message-id (id)
        (message "Searching...")
        (let* ((query (format "+message-id:%s" id))
               (args `("-l" "-n" "1" ,query "/home/hoge/NMZ/Mail"))
               (found (car (howm-call-process "namazu" args))))
          (if found
              (progn
                (find-file found)
                (re-search-forward "^$" nil t)
                (message "Done."))
            (message "No match."))))
      (setq action-lock-default-rules
            (cons (action-lock-general 'my-howm-search-message-id
                                       "Message-[Ii][Dd]: \\(.*\\)$"
                                       1)
                  action-lock-default-rules))

* ((<RD|URL:http://www.ruby-lang.org/ja/man/html/RD.html>))を使う場合:
  行頭の * でエントリの開閉ができるように
  → ((<237-238|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))

* おまけ
    (setq howm-congrats-format
          '(
            "(´・ω・｀) %s"
            "(｀・ω・´) %s"
            ;; …以下略…
            ))
    (setq howm-congrats-command '("play" "~/sound/fanfare.wav"))

* もっといろいろいじるには, *.el 冒頭を参照

=== 外部ツール
(同梱ツールは ext/ に)

* HTML への変換: howm2 (同梱. 要 ruby)
  * 例
    * メモディレクトリ ~/howm/ を変換して ~/converted/ に吐く
        ./howm2 ~/howm/ ~/converted/
    * <<< の大文字小文字を無視
        ./howm2 -i ~/howm/ ~/converted/
    * リンク書式の指定
        ./howm2 -comefrom='<<<' -goto='>>>' ~/howm/ ~/converted/
    * 「ほげ」を含むファイルだけ HTML 化
        grep -rl 'ほげ' ~/howm/ | howm2 -list ~/converted/
  * 何も工夫してないので, 激遅かつメモリどか食い
  * alias の「再帰的な」展開は未サポート

* カレンダー & todo 一覧: hcal.rb (同梱. 要 ruby)
  * カレンダー(予定・〆切・済みの一覧)を出力
      hcal.rb -schedule_mark='○' -deadline_mark='●' -done_mark='／' ~/howm/*/*/*.txt
    * こんな感じでずらずら
        ----------------<6>---------------- 2003
        01 Sun 
        02 Mon ●田中先生に連絡 ○B4輪講 小林 ○工学基礎実験 12:40 <<<<##>>>>
        …
    * ●は〆切(@[2003-06-02]!), ○は予定(@[2003-06-02]@), ／は済(@[2003-06-02].)
    * <<<<# は「今日」, #>>>> は「毎年の同月同日」
      * こんな感じで alias しとくと便利
          alias hcal="hcal.rb -schedule_mark='○' -deadline_mark='●' -done_mark='／' ~/howm/*/*/*.txt | less '+/<<<<#'"
  * 「旬度順 todo 一覧」を出力
    (howm を使うなら不要. ChangeLog 派な人へのおまけです)
    * コマンドラインで
        hcal.rb -l memo.txt
    * emacs から M-x grep して
        Run grep (like this): hcal.rb -l ~/memo/*.txt

* 箇条書き支援:
  * ((<org-mode との併用|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?OrgMode>))

* 簡易小遣い帳
  * キーワードを決めて, 日記中に書いておく
      $食費$ 500円 ラーメン
  * 「<<< $食費$」なり「>>> $食費$」なりで一覧を表示.
    絞り込み・ソートして範囲指定.
  * M-x yen-region で, 「◯◯円」を合計
    → ((<yen.el|URL:http://howm.sourceforge.jp/a/yen.el>))

* ((<→ howm wiki の「併用ツール」|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))も参照

== 実装

=== 実装について

* ファイル開くたんびにスキャンっていう安易実装
  * ~/.howm-keys にキーワードの一覧
  * ファイルを開くときは…
    * .howm-keys の各キーワードについて, 出現の有無を検索
    * 出現キーワードを or でつないだ正規表現を作成
    * その正規表現を font-lock と action-lock に設定
  * ファイル保存時に内容をスキャンして, ~/.howm-keys を更新

* 検索
  * メモディレクトリ ~/howm/ 以下を再帰的に全検索.
    ファイル名も拡張子も ~/howm/ 以下のディレクトリ構成も, どうでもよい.
    * ファイル名の制約は, 
      * ファイル名に年月日が入っている (filter-by-date のため)
      * string<= でソートしたら日時順になる
  * 互換な検索関数を二本用意. 好きな方を使える.
    * real-grep (grep を呼ぶ)
    * fake-grep (elisp のみ)

* ファイル構成
  * howm 本体とは独立
    * bcomp.el
      * make 時に使うだけ
      * navi2ch-cvs-0.0.20031209 から借用
    * cheat-font-lock.el
      * font-lock-keywords を後から変更するための関数
      * font-lock.el の内部実装に依存
    * action-lock.el
      * action-lock-mode (minor-mode)
        * 呪文(正規表現)と魔法(関数)の組を登録
        * リターンキー叩いたら
          * 呪文の上 → 魔法が発動
          * それ以外 → 本来のリターンキー
    * riffle.el
      * riffle-{summary|contents}-mode
        * 一覧・内容のぱらぱら表示, 内容の連結表示
        * 一覧では, post-command-hook で移動検出 → 内容表示を更新
        * バッファローカル変数 riffle-item-list に項目を保持
      * gfunc.el を使用
    * gfunc.el
      * 安直 generic function
    * illusion.el
      * illusion-mode (minor-mode)
      * ふつうの「ファイル」でない対象を, 開いて編集して保存
      * 今のところ活用されていない
    * honest-report.el
      * バグレポートの生成
  * howm 本体
    * 主役
      * howm-backend.el
        * バックエンドの分離
        * 抽象化
          * ディレクトリ → folder
          * ファイル → page
          * マッチ箇所 → item
      * howm-view.el
        * howm-view-{summary|contents}-mode (major-mode)
          * riffle-{summary|contents}-mode から派生
          * 検索の実行
      * howm-mode.el (howm-mode-mode.el から改名[2004-07-14])
        * howm-mode (minor-mode)
          * 上述のスキャンなど
    * 脇役
      * howm-date.el
        * 日付入力の支援
      * howm-reminder.el
        * 浮沈式 todo
      * howm-menu.el
        * howm-menu-mode (major-mode)
    * 設定
      * howm-version.el
        * 定数 howm-version を設定するだけ
      * howm-vars.el
        * defvar, defcustom, 等
      * howm-lang-*.el
        * 言語依存の変数
      * howm-menu-*.el
        * 初期メニューファイルの内容を文字列定数として定義
      * howm-mkmenu.el
        * howm-menu-*.el を ja/0000-00-00-000000.txt 等から生成するスクリプト
        * 作者以外は使う必要ないはず
    * 雑
      * howm-cl.el
        * cl パッケージへの依存をまとめただけ
      * howm-common.el
        * howm-*.el で require
        * 特に, ファイルまたいで使うマクロはここへ (∵ byte-compile 対策)
      * howm-misc.el
        * 雑
      * howm.el (howm-mode.el から改名[2004-07-14])
        * メインファイル. require するだけ.

=== 動きませんよ?

(バグの指摘をくださる方へ)

* 以下のようにしていただくと, 調査しやすくなります
  * できるだけ make test をお願いします
      cd howm-○.○.○
      make test
  * win なら, test.bat をお願いします
    * test.bat 中の「HOWM_EMACS=…」を環境にあわせて修正
    * test.bat を実行
  * どちらも, emacs が立ちあがり, 質問票が表示されます
  * ((<なんでわざわざ? → バグレポートFAQ|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>))

* 補足: ガイシュツ上等
  * 「仕様か」「既知のバグか」のチェックって, おっくうですよねえ.
  * howm に関しては, このチェックは不要です.
    それよりも, 気軽にどんどん指摘していただく方がありがたいです.
  * ぜひ, 作者の目が届くところ(2ch か howm wiki)にたれ込んでください.
  * cf.
    ((<バグレポートFAQ|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>)),
    ((<YASWiki:オープンソースは下町気質|URL:http://web.archive.org/web/20041018232953/http://nnri.dip.jp/~yf/cgi-bin/yaswiki.cgi?name=%A5%AA%A1%BC%A5%D7%A5%F3%A5%BD%A1%BC%A5%B9%A4%CF%B2%BC%C4%AE%B5%A4%BC%C1>))

* 作者覚書
  * デバッグ用変数 howm-call-process-last-command
  * C-u M-x howm-bug-report で関連変数の一覧
  * M-x howm-elp で, プロファイラ elp の準備

== 備考

=== 参考

((<元ネタ|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?IdeaSource>))参照.
特に, Q-pocket・HashedWiki・ChangeLog メモからいっぱいまねしてます. 感謝.

* 広瀬雄二著「やさしい Emacs-Lisp 講座」(カットシステム, 1999)
  ISBN 4-906391-70-2
  → 
  ((<オンライン版 (抄?)|URL:http://www.gentei.org/~yuuji/elisp/>))
  * elisp はじめるなら圧倒的におすすめ
  * 6.4 章末問題の「サクサク dired」を参考にさせていただきました

=== 更新記録

thx > patch・改良案・指摘をくださった皆様

* リリース版 howm-1.4.4 [2016-12-31]
  * バイトコンパイル時の警告を回避
    * cl をやめて cl-lib を使う
    * howm-ime-fix (howm-1.1.1 以前の canna, egg, yc, anthy 個別対策) を廃止
    * その他こまごま
  * MELPA へのリンクを追加.
    thx > Yuki Inoue san (inouetakahiroki at gmail.com)
  * スナップショット版 2016-09-28 と同じ内容です

* リリース版 howm-1.4.3 [2015-12-31]
  * emacs 25.1.50.1 (2015-12-27 時点の git 先端 = de88375) で起動を確認
  * howm-1.4.3rc1 と同じ内容です
  * fix
    * git 先端 emacs でのエラー (void-function ed)
    * C-c , A (howm-list-around) で howm-list-title の設定が効いていなかった
      ((<thx|URL:http://peace.2ch.net/test/read.cgi/unix/1397477663/39>))
    * ((<howm-recentf|URL:http://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Recentf>))などで無駄にリモートファイルのパスワードを聞いてこないように.
      thx > 鯉江英隆さん (hide at koie.org)
    * ドキュメントのファイルモードの訂正

* リリース版 howm-1.4.2 [2013-12-31]
  * Note
    * emacs 24.3 に対応. 2013-12-25 時点の trunk (24.3.50.1) でも起動を確認.
    * howm-test130321 や howm-1.4.2rc1 と同じ内容です
  * 改良
    * C-c , a (howm-list-all) を高速化
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
  * fix
    * emacs 24.3.1 でバイトコンパイルせずに実行したときのエラー
      "Can't detect type of ..."
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * シンボリックリンク下で新規メモが howm-mode にならないバグ
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * バイトコンパイル時の警告

* リリース版 howm-1.4.1 [2012-12-27]
  * ~/.howm-keys が無かったら, 全メモをスキャンして再生成
    ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2012/000099.html>))
    > Albert-san (areiner at tph.tuwien.ac.at)
  * fix: *.txt と *.howm が混在しても一覧モードの表示がずれないように
    ((<thx|URL:http://toro.2ch.net/test/read.cgi/unix/1141892764/940>))

* リリース版 howm-1.4.0 [2012-08-16]
  * Note
    * ((*非互換変更*))に注意!
      * 1.3.* どおりの挙動を望むなら↓
          (setq howm-compatible-to-ver1dot3 t) ;; (require 'howm) より前に!
      * 個別に設定したければ, M-x customize-group RET howm-compatibility RET
    * 長いこと隠し機能だったものを公式機能にしました.
    * emacs-24 対応
    * howm-1.4.0rc2 と同内容です.
  * 変更
    * howm-file-name-format のデフォルトを *.howm から *.txt に変更
      * 拡張子のせいで他ツールとの連携に困っているらしい事例を見かけるので
    * 一ファイル複数メモのときも, 絞り込み等をファイル単位じゃなくメモ単位に.
      ただし, date での絞り込みはファイル単位のまま.
    * タイトルが空のときは本文の一行目をタイトルに.
  * こまごま改良
    * メニューに以下を書けば, タイムスタンプ順でなくファイル名順の上位を表示.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/890>))
        %here%(howm-menu-recent identity)
    * ディレクトリに対してもビューアを設定できるように.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/891>))
        ;; 「file://…/」や「>>> …/」は open コマンド(mac 用)で開く
        (setq howm-view-external-viewer-assoc '(("/$" . "open %s")))
    * 変数 howm-normalizer のありがちな設定ミスを察するように
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/865-870n>))
    * ドキュメントの古い箇所を手直し
    * rast まわりの試作を削除
  * 隠し機能の公式化
    * コマンド
      * C-c , e (howm-remember)
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/24-25n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/61>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/72-75n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/92-93n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/99>))
      * C-c , b (howm-list-buffers)
      * C-c , x (howm-list-mark-ring)
      * C-c , o (howm-occur)
    * リマインダ
      * メニューに「%reminder」と書くと, 予定と todo の統合一覧
        * 予定「@」は,
          howm-menu-schedule-days-before 日前から
          howm-menu-schedule-days 日後までを先頭に表示
          * [2004-12-03]@5 などと書くと, 「5 日間」の意
            (当日も含むので「12月3日から12月7日まで」).
            一覧から消えるのがそれだけ猶予される.
            ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/516>))
        * 〆切「!」も, 〆切日がその範囲までなら一緒に表示
        * それより下は従来どおり
        * 従来の %schedule + %todo とくらべると, スキャンが一回ですむぶん効率的
      * [2005-05-15 21:37]@ のような書式の予定は, 時刻順にソート
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/141>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/148>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/597>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/683>))
      * メニューからリマインダを直叩きしたときに,
        対応バッファの行数が多少ずれていても許す.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/519>))
          (setq howm-action-lock-forward-fuzziness 5) ;; 何行までずれても許すか
    * ((<カスタマイズ>))
      * 上記 %reminder や todo list 中の仕切り
      * grep 使用時の coding system 指定
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/784>))
      * howm 関連の全バッファに共通の色設定
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/42>))
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Idea>))
        > taku さん
      * todo を済ませたときに指定コマンドを実行 (howm-congrats-command)
  * fix: 2012-01-21 以降の emacs-24 でエラー (void-variable inhibit-first-line-modes-regexps)
    ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    thx > 佐々木 寛 さん (sasaki at fcc.ad.jp)

* 隠し機能 (experimental)
  * 1.1.1.* 以前から
    * ソースコードの読み書きも howm で
      * ((<GNU global|URL:http://www.tamacom.com/global-j.html>))
        (((<例|URL:http://www.tamacom.com/tour/lang/ruby/S/21.html>)))
        もどきの on the fly 版めざして
      * まだ開発中. 味見するには…
        * 変数 howm-configuration-for-major-mode を設定
          * major-mode に応じて, come-from リンク等の書式を変える
          * howm-misc.el のコメント参照
        * M-x howm-open-directory-independently して ~/elisp/howm などと入力
      * 正体は結局 grep なんだから, あまり賢い動作を期待してはいけない
        * elisp, tex では便利だけど, ruby じゃ使いものにならず.
          * ∵ elisp の関数名や tex のラベルは大域的に一意. ruby は否.
  * 1.2
    * 一覧時の内容バッファにファイル全体を表示させる
        (setq howm-view-preview-narrow nil)
      * 連結時は従来どおり(メモ区切りの範囲のみ)
      * howm-configuration-for-major-mode 以外で使う場面は, まあないでしょう
    * リマインダ
      * リマインダのカスタマイズ
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/296>))
          ;; 新しい型のリマインダ「*」を定義する例:
          ;; 「[2004-07-11]* ほげ」は, 指定日まで上昇し, そのあと下降.
          ;; (旬度 = - |遅れ / 猶予日数|. 猶予日数のデフォルトは 3)
          ;; 1. 旬度関数を準備 (遅れと猶予日数(とアイテム)を食って旬度を吐く)
          ;;    遅れ: 指定日から今日までの日数. まだならマイナス.
          ;;    猶予: 「[2004-07-11]*8」なら 8. 「[2004-07-11]*」だけなら nil.
          ;;    旬度: 大きいほど上. 「覚書」なら初日が 0 で毎日 1 ずつ減る.
          ;;    (アイテム: ふつうは使わないけどついでに. howm-backend.el 参照)
          (defun my-priority (late lazy item)
            (let ((r (howm-todo-relative-late late lazy 3)))
              ;; r = late / lazy. 無指定時は lazy = 3.
              (- (abs r))))
          ;; 2. face を準備
          (defface my-face '((t (:foreground "cyan"))) "my face")
          (setq my-face 'my-face)
          ;; 3. 記号, 旬度関数, face を登録.
          ;; 残りの引数二つは, 「予定表に表示するか」「todo リストに表示するか」.
          (howm-define-reminder "*" #'my-priority 'my-face nil t)
        * 参考: 既存の旬度関数のグラフが
          ((<UNIX USER 誌の記事|URL:http://howm.sourceforge.jp/uu/#label:11>))
          に出てます
        * バグ
          * 一部の記号はこけそう (正規表現 […] 内で特別な意味を持つ記号は×)
          * 「[2004-07-11]- ほげ」から「-」上で RET して「*」を入力するとエラー
        * とりあえず叩き台. こんなんでいいんでしょうか?
    * 日付形式
      * 日付上で RET×2 してから…
          -, + → 前日, 翌日
          (, ) → 前日, 翌日
          {, } → 前月, 翌月
          [, ] → 前年, 翌年
        * C-u 20 - → 20日前
        * ヒットしなかったらその先の日付を順に探す
            (setq howm-date-forward-ymd-limit 90)  ;; 90日先で give up
        * もっとましなキー設定ないかねえ
      * 日付入力「C-c , d」したときの動作をさらに小賢しく
          (setq howm-insert-date-pass-through t)
        * 日付コマンドについては元と同様
        * 日付コマンドじゃないときは, ただちに抜ける.
          C-c , d hoge とか C-c , d C-a とか試せばわかります.
        * しまった. 「[2004-05-21]+」とか入力しようとするととまどう.
          「+ RET」で「+を挿入」にはしてみたけど…
  * 1.2.1
    * Major
      * howm2 の作り直し? (ext/howmkara)
        * 必要にせまられてでっちあげ. 名前もてきとう.
          * 必要は満たされたから, また放置かも. 誰かどうにかしてくれれば…
        * 機能は退化. ソースは前よりはまし.
          * magic string がちらばってるのはけしからんけど…
        * 一メモ一ファイルに分割する ext/hsplit.rb も書いたけど,
          これはさらに手抜き
    * Minor
      * hcal.rb の「[2004-09-02]?」対応(自分専用そのばしのぎ)
        ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?TangledToDo>))
      * M-x howm-return-to-list → 一覧表示に戻る
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
        * とり急ぎ超雑でっちあげ. 反響に応じてまた考えよう.
        * 一覧表示にいちいち戻ることなく, 一覧の次項目を直接開く:
            (defun my-howm-next-hit (n)
              (interactive "p")
              (let ((buf (save-window-excursion
                           (howm-return-to-list)
                           (when (not (eq major-mode 'howm-view-summary-mode))
                             (error "Sorry. This case is not cared."))
                           (forward-line n)
                           (let ((howm-view-summary-keep-cursor nil))
                             (howm-view-summary-open))
                           (current-buffer))))
                (switch-to-buffer buf)))
            (defun my-howm-previous-hit (n)
              (interactive "p")
              (my-howm-next-hit (- n)))
  * 1.2.2
    * 特殊フォルダ
      * namazu folder 試作
        * コード雑すぎ
        * +from: などに未対応
        * 直接検索するには M-x howm-search-namazu
      * rot13 folder/page 試作
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/580>))
        * これ自体はお遊びだけど, 「ふつうでないページ」の練習として
        * rot13:xxx バッファは, C-c C-c で「rot13 して保存」
          * rot13 なファイルを開くには, M-x yarot13-find-file
      * howm-search-path に, 通常の「ディレクトリ」以外も書ける
          ;; namazu folder と rot13 folder を検索対象に追加
          ;; (M-x howm-toggle-search-other-dir で有効・無効を切りかえ)
          (let* ((nd "~/PATH/NMZ/Mail") ;; namazu インデックスのあるディレクトリ
                 (rd "~/g/r13") ;; このディレクトリ以下のファイルは rot13 される
                 (nf (howm-make-folder:namazu nd))
                 (rf (howm-make-folder:rot13dir rd)))
            (setq howm-search-path (list nf rf)))
          (howm-toggle-search-other-dir 1) ;; 0 なら初期状態は「無効」
    * [2004-12-13]_3 の猶予日数「3」の意味を 1 ずらした
      * いままでは, 省略と 0 と 1 が同じ意味になっていた
      * いずれ気が向いたら, もっとまじめに実装しなおすかも
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/522>))
    * ext/hcal.rb に iCalendar 出力を追加, …の最低限のとっかかりだけ
  * 1.3.1
    * 新しい「バグの指摘の手順」案
      * make test で emacs を起動
      * バグを発症させる
        * 発症しなければ, 自分の .emacs から関連しそうなところを
          sample/dot.emacs へコピーして, もう一度 make test
      * 発症したらすかさず M-x howm-bug-shot
        * バージョンやスクリーンショットなどが表示されます
      * コメントを加えて 2ch に貼る
  * 1.3.3
    * 新規メモ作成をすべて howm-remember にするには…
        ;; howm-create をすべて howm-remember にすりかえる
        (defadvice howm-create (around remember activate)
          (if (interactive-p)
              (howm-remember)
            ad-do-it))
        (setcdr (assoc "[新規]" howm-menu-command-table-ja)
                '(howm-remember current))  ;; [2006-05-15] 修正
      * メニュー上で c を押したとき, 「メニューの前に表示していたバッファ」
        を出す方が好みなら, 「current」を「previous」と直してください
    * カテゴリ別の todo list
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/885>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/890>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/909>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/919>))
      * 「分類」の要望はつっぱねてきたんだけど, 今日は気まぐれに気が向いたので
        お試し. 正式機能にしていくかどうかは未定.
      * メニューにこう書くと, 「foo」「bar」「baz」を含む todo を
        分類して表示
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz"))
        * ちなみに, %here% ではクオートは不要です
      * さらに, 各行の「foo」「bar」「baz」を消したければ
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz") nil t)
      * 「misc.」を非表示にしたければ
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz") nil nil t)
    * 一覧バッファのマッチ内容の左にタイトルを表示.
      ちなみに従来のは, 「マッチ内容のかわりにタイトルを表示」.
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000025.html>)) > Highfly さん
        (setq howm-view-list-title-type 2) ;; マッチ内容の左にタイトルを表示
        (setq howm-view-summary-format "") ;; ファイル名を消したければ
    * C-c , M で「ファイル名を指定してメモを開く」
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2005/000010.html>)) > Eduardo Ochs さん
  * 1.3.7
    * 一覧時にウィンドウ分割を壊さない設定.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/507>))
        (setq riffle-keep-window t)
        (setq riffle-window-initializer nil)
      * 内容バッファは表示されません.
      * とりあえず叩き台. 今後変更の可能性あり.
    * M-x howm-list-active-todo で, 現在有効な(＝潜伏中でない) todo のみを一覧.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/129-131n>))
      * ついでに, M-x howm-list-sleeping-todo で, 潜伏中の todo のみを一覧.
      * とりあえず関数だけ試作. インタフェースの案があればお聞かせください.
      * ちなみに, メニューから潜伏中 todo を消すには,
        M-x customize-variable RET howm-menu-todo-priority RET で
        「Hide sleeping reminders」を設定してください.
    * バッファ名を, ファイル名ではなくタイトルにする.
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000020.html>)) > Mielke-san (peter at exegenix.com),
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))
        ;; emacs 上でのバッファ名を, ファイル名ではなくタイトルに.
        ;; (ファイル名自体は変更しない)
        (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
        (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
      * タイトル「ほげ」のメモのバッファ名を「=ほげ」に
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/333>))
          ;; タイトルが AAA ならバッファ名を =AAA に.
          ;; 下の howm-mode-set-buffer-name を設定した上で…
          (setq howm-buffer-name-format "=%s")
      * 本当は howm と独立したツールにする方がいいけど,
        おっくうなのでひとまず.
  * 1.3.8
    * M-x howm-extend-deadlines で,
      指定日を過ぎた〆切(!)をすべて一定日数後へ延期.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/671>))
      * 仕様もインタフェースもまだ叩き台.
      * メモを勝手に書きかえる危険な操作なので, バックアップをとってから!
  * 1.3.9
    * メニューに「%here%(howm-menu-search "[断]片的" full t)」と書けば,
      「断片的」を含むメモの内容全体をメニューに埋め込み.
      ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2010/000097.html>))
      > Morgan Veyret さん (morgan.veyret at gmail.com).
      * 単に "断片的" としないのは, メニューファイル自身がヒットするのを避ける小細工
  * 1.4.2
    * 一覧バッファで「ファイル | マッチ行」のかわりにこんな表示に.
      このときタイトル先頭の「=」は表示しないように.
      (experimental)
      ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2012/000107.html>))
      ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2012/000111.html>))
      > Albert-san (areiner at tph.tuwien.ac.at)
        タイトル A|
        |マッチ行 A1
        |マッチ行 A2
        タイトル B|
        |マッチ行 B1
        |マッチ行 B2
      * 設定
          (setq howm-view-list-title-type 2) ;; Show title before summary.
          (setq howm-view-summary-format "") ;; If you want to delete file names.
          (setq howm-entitle-items-style2-max-length 50)
          (setq howm-entitle-items-style2-format "%-0s|%s") ;; for title and summary
          (setq howm-entitle-items-style2-title-line t) ;; independent title line?
      * さらに, M-x customize-variable RET howm-list-title RET も設定を
      * 制限: 一ファイル複数メモで C-c , a (howm-list-all) したときは
        対応するタイトル行に飛んでくれない
        * そもそも行指向で作っていたので実装が無理矢理
        * きちんと直すのはめんどう. 強い需要がなければ…
  * 1.4.4
    * 日付入力時の動作を拡張
      ((<thx|URL:https://osdn.jp/projects/howm/lists/archive/eng/2016/000118.html>))
      > Albert-san (areiner at tph.tuwien.ac.at)
        ;; 「[2003-12-27]」上で RET して年や月を略したら…
        (setq howm-action-lock-date-future t) ;; 2003-12-27 より未来と解釈
        ;(setq howm-action-lock-date-future 'closer) ;; 2003-12-27 に近い方で解釈
      * 新規入力時にだけそういう動作にしたければ
          ;; 日付入力(C-c , d または [日↓])で年や月を略したら…
          (setq howm-insert-date-future t) ;; 「未来」と解釈
          ;(setq howm-insert-date-future 'closer) ;; 今日に近い方で解釈

* …履歴抜粋… (((<URL:OLD.rd>)) 参照)
  * [2010-12-30] 1.3.9 微修正
  * [2009-12-31] 1.3.8 過ぎた〆切に着色
  * [2008-12-31] 1.3.7 内部コード整理 (副作用を分離).
    howm-list-normalizer から howm-normalizer へ.
  * [2008-05-31] 1.3.6 着色の不具合修正
  * [2007-12-09] 1.3.5 夏時間の不具合修正
  * [2006-12-16] 1.3.4 セキュリティ修正
  * [2005-08-02] 1.3.0 alias. M-x customize. タイトル表示.
  * [2005-05-02] 1.2.2 バックエンド切り離し. gfunc.el
  * [2004-08-24] 1.2 保留「~」の公式化. howm.el, riffle.el
  * [2004-05-06] 1.1.2 make test
  * [2004-02-27] ((<"2ch howm スレ 2"|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/>))
  * [2004-02-21] 1.1.1 「隠し機能」制度を導入
  * [2004-01-25] ((<"sf.jp"|URL:http://howm.sourceforge.jp/>)) へ移動
  * [2005-01-08] ((<"UNIX USER 2004.2"|URL:http://www.unixuser.jp/magazine/2004/200402.html>))
  * [2003-12-27] ((<howm wiki|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi>))
  * [2003-11-22] 1.1 デフォルトの変更
    (リンク・日付・リマインダの書式, 一メモ一ファイル, メニューもメモの一種)
  * [2003-10-27] 1.0.4.2 重くなるバグを修正. よくこんなので動いてたなあ…
  * [2003-10-02] 1.0.4 外部 viewer, メニューの過剰強化
  * [2003-09-23] 「テスト版」を導入
  * [2003-09-18] 1.0.2 HTML 化スクリプト howm2
  * [2003-09-17] ((<2ch howm スレ|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/>))
  * [2003-09-17] 1.0 メモディレクトリを階層化
  * [2003-09-16] 0.9.7.1 Wiki 風リンク [[ほげ]]
  * [2003-09-14] 0.9.4.1 grep 脱却
  * [2003-09-09] 0.9 ruby 脱却
  * [2003-08-31] 0.8.5 タイトル一覧
  * [2003-06-03] 0.8.4 安直カレンダー hcal.rb
  * [2002-11-03] 0.8 メニュー, 旬度順 todo @[2003/09/20]+
  * [2002-09-17] 0.7 1 日 1 ファイル, come-from リンク <<
  * [2002-09-14] 0.6 リンク廃止(すべては「検索」)
  * [2002-06-10] ((<"日本発の wiki クローンリスト"|URL:http://www1.neweb.ne.jp/wa/yamdas/column/technique/clonelist.html>))
  * [2002-05-29] 0.1 公開

=== アドレス

* 最新版: ((<URL:http://howm.sourceforge.jp/>))
* 連絡先: email アドレスはソースファイル冒頭を参照ください

=end
