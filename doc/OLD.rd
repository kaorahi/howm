=begin
$Id: OLD.rd,v 1.18 2012-08-16 09:52:06 hira Exp $

* 目次
  * ((<古い更新履歴>))
  * ((<古い告知>))
  * ((<旧版からの移行>))
  * ((<古いカスタマイズ法>))
  * ((<古い参考リンク>))

= 古い更新履歴

== 1.3.x

* リリース版 howm-1.3.9.2 [2011-12-28]
  * こまごま fix (howm-1.3.9.2rc4 と同内容です)
    * mac で grep 使用時にエラーが出ていた
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/787-790n>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/898-901n>))
      * grep のデフォルトオプションを設定する前に,
        --exclude-dir が通るか確認するようにしました.
    * 大きいフレームで一覧表示をしたときの余計なウィンドウ分割を修正
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-vars.elc ロード時の警告「old-style backqoute detected!」を回避.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))

* リリース版 howm-1.3.9.1 [2011-01-02]
  * fix: emacs-24.0.50 でのエラー
    (Symbol's function definition is void: make-local-hook).
    thx > 山本 宗宏 さん (munepi at vinelinux.org)

* リリース版 howm-1.3.9 [2010-12-30]
  * Note
    * ほとんど変更はありませんが, また一年ほどたったのでリリースしておきます.
    * howm-test100702 との違いは, ドキュメントの微修正や ext/tag2plan の削除だけ.
    * このリリースが済んだら, デフォルト設定を変えて
      隠し機能を公式化しただけのものを howm-1.4.0 としてリリースする予定です.
  * fix
    * メニューから y キー(または [予定] 上で RET)で予定表を開いたとき,
      内容バッファがカーソル位置のメモになっていなかった.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/826>))
    * ドキュメントの古い箇所を手直し
    * メンテされていないツール(ext/tag2plan)を削除

* リリース版 howm-1.3.8 [2009-12-31]
  * Note
    * 大きな変更はありませんが, 一年たったのでリリースしておきます.
    * howm-test090723 との違いは, howm-excluded-dirs に ".git" を
      追加しただけです.
  * 変更・改良
    * 過ぎた〆切に着色
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/738>))
    * _darcs/ などを検索対象外に (howm-excluded-dirs).
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/708n>))
      * いまどきの GNU grep を使っているなら↓もしておくと無駄な検索を省けます.
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs")
      * さらに, *.txt だけ検索するようにしたければ…
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs --include=*.txt")
    * ((<yagrep|URL:http://www.kt.rim.or.jp/~kbk/yagrep/index.html>)) との
      互換性のため, grep 呼び出し時にディレクトリ名末尾の / を削除.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/685-686n>))
    * ((<HidePrivateReminder|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))
      のために内部を少し掃除.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/731>))
  * fix
    * C-c , l (howm-list-recent)時に該当ファイルが多すぎるとエラー.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/733>))
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Windows>))
      * howm-view-use-grep を設定している場合に発症.
        meadow だと "Spawning child process: exec format error" になるらしい.
      * grep 呼び出し時のコマンドラインが howm-command-length-limit 以上に
        長いときは分割して呼び出すよう直しました.
    * (setq howm-list-title t) していたら,
      come-from リンク上で RET したときもタイトル一覧を表示するように.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/762>))
      * もし以前の動作がよければ,
        M-x customize-variable RET howm-list-title RET し,
        howm-keyword-search 以外をすべてセットしてください.
    * メモを保存したときにメニューが自動更新されなくなっていた.
      (howm-menu-expiry-hours を正に設定した場合のみ該当)

* リリース版 howm-1.3.7 [2008-12-31]
  * Note
    * 内部的なコード整理と, こまごま改良・修正
    * howm-1.3.6 (もしくは howm-test080531) 以前で
      変数 howm-list-normalizer を設定していた場合は,
      その設定を止め, 変数 howm-normalizer を設定してください
      * 自動読みかえも一応試みてはいますが…
    * howm-1.3.7rc4 とほぼ同じものです
      * 不本意に "Wrote ..." が表示されるバグを直しました
  * 変更・改良
    * 非互換な変更
      * 旧変数 howm-list-normalizer から新変数 howm-normalizer へ
        * 移行方法
          * M-x customize で設定していたなら,
            M-x customize-variable howm-list-normalizer RET で
            「Off」を設定し,
            M-x customize-variable howm-normalizer RET で改めて設定しなおす
          * .emacs 等で (setq howm-list-normalizer 'howm-view-sort-by-○○)
            と設定していたなら, 次のように書きかえる
              (setq howm-normalizer 'howm-sort-items-by-○○)
          * (setq howm-list-normalizer …それ以外の何か…)
            と設定していたなら,
            * lisp がわかる方:
              次の仕様変更にあわせて修正する
              * 旧変数: 「現在の一覧を並べかえて表示し直す関数」を指定
              * 新変数: 「与えられたリストに対し, その並べかえを返す関数」を指定
            * lisp がわからない方:
              ((<2ch UNIX 板 howm スレ|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/l50>))
              か
              ((<howm wiki の「なんでも」|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
              でご相談を
        * もし旧変数をセットしたままにしておくと…
          * 単純に読みかえられそうなら, 新変数に読みかえて新処理を実行
          * 読みかえられなかったら, 旧処理を実行 (非効率)
      * 「今日と明日の日付」は [YYYY-MM-DD] でなく YYYY-MM-DD を着色
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/691>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/275>))
        * 一覧やメニューのファイル名も, マッチすれば着色
        * 前のように戻したければ…
            ;; 今日と明日の日付は, [YYYY-MM-DD] の形式だけ着色
            (setq howm-highlight-date-regexp-format (regexp-quote "[%Y-%m-%d]"))
    * 一覧バッファ
      * 検索時の内部的な一覧バッファ再表示を抑制
      * 一覧バッファからの X (dired-x) 時に, カーソルを対応ファイル名へ置く
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
        > 797 さん
        * 1.3.2 の隠し機能を公式化 & デフォルト化.
          変数 howm-view-dired-keep-cursor は削除しました.
      * howm-view-summary-previous-section も「各ファイルの最初のヒット行」で
        止まるよう変更
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/360>))
      * 内容バッファで一アイテムだけ表示しているときは,
        区切り線「====>>> xxx.txt」を描かない.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/314>))
      * 一覧バッファのソート基準に summary-match-string を追加
        * 指定した正規表現にマッチした文字列の順にソート
          * 例: 「2006-06-..」を指定すれば, 2006年6月の項目を日付順に
        * ちなみに, summary-match は, マッチしたものを上位にもってくるだけ
          * マッチしたものどうしの上下比較はしない
    * メニュー
      * メニューの %recent や %random でもファイル名欄を桁そろえ.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000032.html>)) > Mielke-san (peter.mielke at gmail.com)
        * 変数 howm-menu-list-format は %recent および %random 用に
        * 新変数 howm-menu-reminder-format が %schedule および %todo 用
      * メニューの %random% で, 同じファイルからは一項目しか選ばれないように
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/563-566n>))
      * メニューの曜日表記をリストで指定するよう変更.
        英語表記のデフォルトも "Sun" 等に直した.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/147>))
          (setq howm-day-of-week-ja '("日" "月" "火" "水" "木" "金" "土"))
          (setq howm-day-of-week-en '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
        * っていうか, わざわざ独自に定義せずに
          (format-time-string "%a") 決め打ちでも構わない?
      * 初期メニューにボタンの説明を追加.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/144>))
    * いろいろ
      * howm-view-grep-option に複数のオプションを書けるように.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/458>))
          (setq howm-view-grep-option "-Hnr --include=*.txt") ;; *.txt のみ検索
        * 単純に split-string してるだけ.
          もっとまじめなのが必要ならお知らせください.
      * 単語の途中がたまたま come-from キーワードに一致しても下線を引かない設定.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000030.html>)) > Mielke-san (peter.mielke at gmail.com)
          ;; ASCII 文字のみのキーワードは, 単語途中にマッチしても下線を引かない
          (setq howm-check-word-break "^[[:ascii:]]+$")
      * 予定表, ToDo リストにも超過日数を表示.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000028.html>)) > Mielke-san (peter.mielke at gmail.com)
      * .howm-history まわりの挙動を改善.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/179>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/193-194n>))
        * バッファ一覧に表示しない
        * "Wrote ..." を表示しない
        * make test 時に ~/.howm-history を汚さない
  * fix
    * howm-menu-categorized-reminder で表示されない項目があった
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * (setq howm-view-list-title-type 2) のとき C-c , a でエラー
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/552>))
    * タイトルのないメモが C-c , a で表示されなかった
    * howmoney が使えなくなっていた.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/503>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/514>))
    * 予定や todo が一つもないときに予定表や todo リストを呼び出した場合.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/494>))
    * 予定表や todo リストで action-lock-mode が不本意にトグル.
    * howm2, howmkara の -exclude オプションに不具合.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > dareka さん
    * ((<HidePrivateReminder|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))で C-c , t が「No match」に
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>)) > taku さん
      * 互換性を修復
    * howm-occur で一覧バッファの検索語がハイライトされなくなっていた
    * 「＜＜＜ テスト ＜＜＜ Test」の「Test」上で RET を叩いても「テスト」が
      検索されなかった
      * howm-keyword-case-fold-search をセットしていたときの大文字小文字がらみ
    * C-c , l でいちいち日付を聞かないように
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/340>))
      * [2007-04-02] に作り込んだバグ
    * 検索結果の一覧で「＜＜＜ ほげ」が先頭にこない場合があった
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/328>))
      * (setq howm-list-title t) していると発症
      * remove-duplicates の仕様をよく知らなかったせい. 勉強になりました.
    * (setq howm-list-title t) だと一覧バッファに前回の内容が表示されるバグ
    * howm-view-contents-limit が効いていなかった
    * 日付での絞り込み結果が一日分多すぎた
    * narrowing 関連の不具合(widen 抜け)
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/160-161n>))
    * メニューの「%reminder」の底に, 過ぎた予定が表示されていた.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/149>))
    * メニュー中の「> 2006-07-26-015606 | …」の「7」上で RET を叩くとエラー.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > na さん
      * 変数 howm-menu-list-regexp の定義をちょっと直しただけ
    * 異なるディレクトリの同名ファイルが一覧表示で混同されていた.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-view-split-horizontally が t でも nil でもないときの特別な動作を廃止.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/591>))
      * howm-1.2 〜 1.3.7rc2 で壊れていたが, バグレポートなし.
        きっと誰も使っていない ^^;

* リリース版 howm-1.3.6 [2008-05-31]
  * fix: 2008-02-24 以降の CVS 先端 emacs で, 他バッファの着色が乱れる.
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/484-487n>))
    * minor mode が font-lock-keywords-only を直に触るのは行儀悪い?
    * howm-test20080514 からのバックポート
  * (howm-1.3.6rc1 と中身は同じです)

* リリース版 howm-1.3.5 [2007-12-09]
  * fix: 夏時間最終日に当日の予定がメニューに表示されなかった.
    ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000034.html>)) > Mielke-san (peter.mielke at gmail.com)
    * howm-test071108 からのバックポート
  * fix: 順不同でバイトコンパイルできるように
    * Wanderlust や Navi2ch を参考にして,
      巡回依存の扱い方(require の書き方)を修正
    * howm-test07-05-18 からのバックポート
  * automake を 1.6 から 1.10 に
    * howm-test07-05-05 からのバックポート
    * automake-1.10 の elisp-comp が使えるようになった
  * (howm-1.3.5rc1 と中身は同じです)

* リリース版 howm-1.3.4 [2006-12-16]
  * セキュリティ修正
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/207>))
    * 何が問題?
      * Emacs には, ファイルごとにローカル変数を自動設定する機能があります.
        これを悪用すると, howm 使用時に任意の命令を自動実行させることができます.
        ((<ref|URL:https://www.codeblog.org/blog/ueno/20060118.html>))
    * どう直した?
      * howm 関連の全シンボルに risky-local-variable 属性をセットし,
        上述の自動設定時にチェックが入るようにしました.
    * バージョンアップしたくない/できないのですが?
      * ソースの編集が可能なら,
        howm.el の末尾に以下のコードを加えるのが確実です.
        バイトコンパイルのしなおしもお忘れなく.
          ;; howm-1.2.2 以降用. howm 関連の全シンボルに risky-local-variable 属性.
          (mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                  (howm-symbols))
      * それが困難な場合は .emacs に以下を加えてください.
          (eval-after-load "howm"  ; ← autoload/load/require の記述にあわせて
            ;; howm-1.2.2 以降用. howm 関連の全シンボルに risky-local-variable 属性.
            '(mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                     (howm-symbols)))
      * どちらにせよ, 修正が反映されたことをご確認ください.
        * emacs を立ち上げ直し, howm を起動
        * 以下を *scratch* バッファに貼り, 閉じ括弧の後にカーソルを置いて C-j を
          押す
            (get 'howm-version 'risky-local-variable)
        * t と表示されれば OK
    * ローカル変数の自動設定をあえて使いたいときは?
      * 以下のように変数ごとに解禁してください.
          ;; 例: 変数 howm-auto-narrow はファイルごとの自動設定を許可
          (put 'howm-auto-narrow 'risky-local-variable nil)
    * howm に限らず, ローカル変数の自動設定を一切使えなくするには?
      * .emacs に以下を加えてください.
        ただし emacs のバージョンによっては不完全かもしれません.
        ((<ref|URL:http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html>))
          ;; ローカル変数の自動設定をオフ
          (setq enable-local-variables nil)
  * fix: CVS 先端 emacs でメニューなどに色がつかない
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/165-169n>))
    * 修正は, cheat-font-lock-20040624-format-p の定義中の = を >= に直すだけ
    * howm-test061015 からのバックポート

* リリース版 howm-1.3.3 [2006-06-05]
  * Note
    * 最新の Emacs 22.0.50 (CVS HEAD) にたぶん対応
      * Meadow 3.00-dev や Carbon Emacs もこれに相当します.
        これら「リリース前の開発版最先端 Emacs」
        を今後も追い続ける方は, howm もテスト版を覗いてみてください.
    * ファイル構成を少々変更
      * make install 以外の方法でインストールする場合はご確認ください.
        初期メニュー 0000-00-00-000000.howm の手動コピーは不要になりました.
    * メニューの todo 一覧では潜伏中の項目もデフォルトで表示
      * デフォルトは「安全側」に倒しておく方が良いでしょう.
        これまで通り隠す方法は下の「変更」を参照ください.
    * その他, grep の文字コードに関する修正や, 隠し機能など
      * 隠し機能では, メモとりをさらに手軽にする M-x howm-remember が
        好評のようです.
    * howm-1.3.3rc1 や howm-test060515 と中身は同じ
      * meadow3 でハマった人が多そうなのでリリースする気になりました.
        「不安定な開発版」という感じじゃなくもう一般に普及しているのかな…
  * 変更・改良
    * メニューの todo 一覧では潜伏中の項目もデフォルトで表示
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/75-77n>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/842-845n>))
      * 隠したければ M-x customize-variable howm-menu-todo-priority
    * grep 時の文字コード設定 howm-process-coding-system で,
      入力と出力に別の値を指定できるようにした
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/96>))
        ;; process (UTF-8)→ emacs
        ;; emacs →(SJIS) process
        (setq howm-process-coding-system '(utf-8-unix . sjis-unix))
    * メニューの「> …」で RET したとき, 「…」を検索するのでなく,
      対応ファイルを直接開く
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/823>))
    * ファイル構成やインストール手順について
      * 言語依存の変数を howm-lang-{en,ja}.el へ分離
      * configure の新オプション --with-howmdir.
        thx > 本庄さん
        * *.el と *.elc はここへインストールされる
        * lispdir のデフォルトは, …/site-lisp/howm から …/site-lisp に変更
      * 初期メニューテンプレートのインストール法を変更
        thx > 本庄さん, 銭谷さん
        ((<ref|URL:http://lists.sourceforge.jp/mailman/archives/macemacsjp-users/2005-November/000756.html>))
        ((<ref|URL:http://lists.sourceforge.jp/mailman/archives/macemacsjp-users/2005-November/000760.html>))
        * 従来は, /usr/local/share/howm/{en,ja}/0000-00-00-000000.howm
          に置いて, 定数 howm-{en,ja}-dir でその位置を指定
          * インストールし忘れや相対パスにより, トラブルが生じていた
          * インストールしない場合, 0000-00-00-000000.howm の手動コピーが必要
        * 今後は, howm-menu-{en,ja}.el
          * howm を初めて使うときのみ, 定数 howm-menu-{en,ja} を読み込みます
            * メモリにかかえ込むのがひんしゅくなら,
              「使用後に値を破棄」という小細工も考えられます.
              もし必要だったらお知らせください.
              (今どき数キロバイトなんて誤差範囲?)
          * 0000-00-00-000000.howm の手動コピーは完全に不要となったつもり
          * 結局こんな流れ
              ja/0000-00-00-000000.howm
                      ↓リリース時に作者が生成 (実際は howm-mkmenu.el で自動化)
              howm-menu-ja.el
                      ↓make install
              $lispdir/howm/howm-menu-ja.elc
                      ↓howm を初めて使ったときだけ読み込んで自動生成
              ~/howm/0000-00-00-000000.howm
  * fix
    * CVS 先端 emacs で make するとエラー
      "Font-lock trying to use keywords before setting them up".
      ((<thx|URL:http://tty0.exblog.jp/2944244>))
      ((<thx|URL:http://d.hatena.ne.jp/yoshk/20060102>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/867>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/873-874n>))
      ((<thx|URL:http://d.hatena.ne.jp/clock9/20060406/1144291193>))
      ((<thx|URL:http://d.hatena.ne.jp/AllStarMoves/20060425/p3>))
      ((<thx|URL:http://d.hatena.ne.jp/katase_n/20060519>))
      ((<thx|URL:http://d.hatena.ne.jp/AllStarMoves/20060602/p4>))
    * grep 時の howm-process-coding-system の処理タイミングにバグ
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/63-83n>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/94-95n>))
    * migemo-client のオプションを追加指定可能に
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/9>))
        (setq howm-migemo-client-option '("-H" "::1"))
      * howm-view-grep-option あたりとの不統一が気になるので,
        コマンド指定一般の拡張仕様案(とりあえず案だけ). おおげさすぎ?
          nil  ;; → デフォルト
          "コマンド名"
          ("コマンド名" "オプション" … "オプション")
          関数名  ;; → コマンドのかわりに elisp の関数を実行
    * howm-kill-all は .howm-keys バッファも消すべき
      ((<thx|URL:http://d.hatena.ne.jp/dasm/20060110>))
    * howm-mode-off-hook の定義がだぶっていた.
      thx > 竹村さん
    * ((<howmz.el|URL:http://noir.s7.xrea.com/archives/000136.html>))
      でエラーが出ていたそう.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      > (TxT) さん
    * emacs20 で M-x howm-bug-shot がエラーになっていた.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>))
      > 逃避さん

* リリース版 howm-1.3.2 [2005-11-04]
  * Note
    * 主に, 小さなバグ修正だけ
    * あとは隠し機能を少々
    * リリース予定版 1.3.2rc4 と同じものです
  * 変更
    * メニューの [今日] (C-c , , .) でもデフォルトでタイトルを表示.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
      > nobu さん
      * タイトル表示したくなければ…
        * M-x customize-variable RET howm-list-title RET
        * howm-action-lock-date-search のチェックをはずす
        * [Save for Future Sessions]
  * fix
    * xemacs だと, メニュー内で [2005-10-15] のような
      日付上での RET がエラー
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/797-801n>))
    * xemacs だと, 一覧バッファからの X (dired-x) がエラー
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
      > 797 さん
      * ついでに, 同じファイルが何度も表示されるのを修正
    * howm-view.el に (require 'riffle) を追加
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/782>))
    * Makefile 以外の手順でバイトコンパイルすると M-x howm-menu がエラー
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/789-791n>))
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BrokenMenu>))

* リリース版 howm-1.3.1 [2005-08-17]
  * xemacs でやけに遅くなっていたのを修正
    (xemacs のバージョンにもよるのかも)
  * 環境変数 LC_ALL, LC_CTYPE, LANG を設定しないとエラーが出ていたのを修正
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/780-781n>))
  * この README の 修正
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/779>))
    * my-howm-next-hit のコードを更新
    * howm-view-search-in-result-correctly を設定しても,
      date での絞り込みはファイル単位
      * 当面は仕様. 一日一ファイルまでなら問題ないはず.
      * 一月一ファイル・全メモ一ファイルなどだと問題.
      * 「指定月のメモ一覧」とかでっちあげようかとも思いましたが,
        自分の場合は長くなりすぎて役に立たなそうなので, やめました.
        ご意見があればお聞かせください.
  * 一覧で, 前と同じ名前もいちいち表示
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/779>))
      (setq howm-view-summary-omit-same-name nil)
  * あとは隠し機能を少々
  * リリース予定版 1.3.1rc1 と同じものです

* リリース版 howm-1.3.0 [2005-08-02]
  * Note
    * 目玉
      * come-from キーワードの alias
      * M-x customize 対応 ([Applications] → [Howm])
        * この README の((<カスタマイズ>))はがさがさ削りました.
      * 検索履歴
      * 一覧時のタイトル表示
      * 一覧に同じファイル名をくり返し表示しない
      * 自動酔歩
      * メニューに最近のメモ一覧・ランダム選択一覧
      * メニューに [履歴] [酔歩] [設定] [時↓] を追加
        * すでに howm を使っていた場合,
          make install しても勝手には追加されません.
          メニューを自分で編集するか,
          ja/0000-00-00-000000.howm を自分でコピーするかしてください.
    * デフォルトを変更しました. 戻したければ .emacs などに↓を書いてください.
        ;; タイトル表示は常時オフ
        (setq howm-list-title nil)
        ;; 検索履歴
        (setq howm-history-limit 0) ;; 検索履歴を記録しない
        (setq howm-history-unique nil)  ;; 検索履歴から重複を取り除かない
        ;; grep -E/-F でなく egrep/fgrep
        (setq howm-view-grep-command "egrep")
        (setq howm-view-fgrep-command "fgrep")
        (setq howm-view-grep-extended-option nil)
        (setq howm-view-grep-fixed-option nil)
        (setq howm-view-grep-file-stdin-option nil)  ;; パターンは引数で渡す
        ;; howm-template が関数だったときは, universal-argument を
        ;; 引数にしてそいつを呼ぶ
        (setq howm-template-receive-buffer nil)
        ;; 一覧から RET で開くとき, 内容バッファのカーソル位置を保たない
        (setq howm-view-summary-keep-cursor nil)
    * テストにご協力くださる方は, ↓を試していただけると助かります.
        ;; (隠し機能)
        ;; 一ファイル複数メモのときも, 絞り込み等を
        ;; ファイル単位じゃなくメモ単位に.
        ;; ただし, date での絞り込みはファイル単位のまま.
        (setq howm-view-search-in-result-correctly t)
    * 内部実装の変更 (riffle.el)
    * リリース予定版 howm-1.3.0rc5 と中身は同じです.
  * 仕様変更
    * howm-template の値が関数だったときは,
      「universal-argument と((*直前のバッファ*))」を引数にしてそいつを呼ぶ
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
      * 使用例
          ;; snap.el でのリンクを入れる
          (setq howm-template #'my-howm-template)
          (defun my-howm-template (which buf) ;; C-u 3 C-c , c なら which = 3
            (let ((snap (with-current-buffer buf
                          (or (snap-record-string) ""))))
              (format "= %%title%%cursor\n%%date\n%s\n\n" snap)))
      * 従来と互換に戻したければ…
          ;; howm-template が関数だったときは, universal-argument を
          ;; 引数にしてそいつを呼ぶ
          (setq howm-template-receive-buffer nil)
    * メニュー等からのリマインダ直叩き時, 叩き先の自動保存について…
      * たとえ howm-action-lock-forward-save-buffer が non-nil でも,
        叩き前からすでに「該当バッファが modified」だったときは
        保存しない
    * howm-todo-menu-types のデフォルトに "." も追加
    * デフォルト設定の変更
      * egrep/fgrep でなく grep -E/-F の方をデフォルトに.
        変数 howm-view-fgrep-command は将来廃止するかも.
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/670>))
      * grep へは標準入力でパターンを渡す
      * {全|最近|前後}メモ一覧にデフォルトでタイトル表示
  * 隠し機能の公式化 (▲ は「おすすめ」)
    * 1.1.1.* 以前から
      * howm-view-before-open-hook
      * メニューの todo に旬度を表示可
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/505>))
          (setq howm-menu-todo-priority-format "(%8.1f)")
      * デバッグ用変数 howm-call-process-last-command
      * (setq howm-message-time t) すれば, 検索等に要した時間を表示
    * 1.2 から
      * come-from キーワードの alias ▲ → ((<メモを書こう>))
        ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?CompoundComeFrom>))
      * メニュー
        * メニューに「%recent」や「%random」 ▲
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/242>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/273>))
        * メニュー中に変数や関数の値を表示
      * 一覧
        * ソート法に「random」を追加
          ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?OldMemo>))
        * 一覧表示窓の行数設定
          ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/724>))
            (setq howm-view-summary-window-size 10)
        * 「タイトル」の正規表現を, 機能ごとに変更可能
            (setq howm-list-title-regexp "^[*=] [^ ]")  ;; 一覧表示
            (setq howm-menu-recent-regexp "^[*=] [^ ]")  ;; メニュー中の %recent
        * 一覧から RET で開くとき, 内容バッファのカーソル位置を保つ
            (setq howm-view-summary-keep-cursor t) ;; ← デフォルトにしました
          * ちょっと自信なし. 不具合出たら教えてください.
        * 単語の途中にマッチしたものは後まわし
            (setq howm-list-prefer-word t)
          * 「euc」を検索したら, 「euclid」や「takeuchi」よりも,
            単語「euc」にマッチしたものを上に表示
          * ただし, 「<<< euclid」はあいかわらずてっぺんへ
      * 新規メモ
        * いま開いてるファイルに新規メモを追加: M-x howm-create-here
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/293>))
        * 新規メモのファイル名を手動でつける: M-x howm-create-interactively
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/367>))
          * こんな感じでしょうか? > 367 さん
      * M-x howm-narrow-to-memo, M-x howm-toggle-narrow
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/293>))
        * ついでに, M-x howm-toggle-narrow で, 隠す・見せるをトグル
        * メモを開いたとき自動的に narrow に
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/301>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/542>))
            (add-hook 'howm-view-open-hook 'howm-auto-narrow)
            (add-hook 'howm-create-hook 'howm-auto-narrow) ;; 追加[2005-01-07]
      * リマインダの「cancel」を違う言葉にカスタマイズ
        (thx > NARA Shinsuke さん)
          (setq howm-reminder-cancel-string "give up")
    * 1.2.1 から
      * M-x howm-history で検索履歴. 各履歴から RET で飛べる. ▲
        * ((<RandomNote|URL:http://ninjinix.x0.com/rn/index.rb?AboutPage.txt>))
          や
          ((<namapo|URL:http://tiki.is.os-omicron.org/tiki.cgi?c=v&p=namapo>))
          に触発されて…
        * 記録は, 「固定文字列検索」「絞り込み検索」「リンク」のみにしてみた
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/496>))
        * 例によって「メニューに表示」もしたいけど,
          「メニューをキャッシュ」との兼ね合いが.
        * 1.2.2 からは最大記録数を設定可
        * 1.3.0 からは重複を除去
      * 一ファイル複数メモのとき, 前・後・最初・最後のメモへ移動
        (narrowing も考慮)
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
        * M-x howm-previous-memo
        * M-x howm-next-memo
        * M-x howm-first-memo
        * M-x howm-last-memo
      * ○○のときだけ自動 narrow.
        ただし, 「>>> foo.howm」で foo.howm に飛んだときは narrow にしない.
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
          ;; ↓デフォルトの動作に不満がなければ設定不要
          (setq howm-auto-narrow
                ;; ↓に書いたコマンドでだけ narrow
                '(howm-list-all howm-list-recent
                  howm-list-grep howm-list-grep-fixed howm-list-migemo
                  howm-list-related howm-list-around
                  howm-keyword-search)) ;; これは come-from リンク・goto リンク
      * C-c , T (howm-insert-dtime) → [2004-09-01 23:26] とか記入
        ((<ref|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/elisp/get-date.el>))
        ((<ref|URL:http://www.gentei.org/~yuuji/software/euc/instamp.el>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/399>))
    * 1.2.2 から
      * 自動酔歩 ▲
      * メニューに「%here%(howm-menu-search "ほげ")」と書けば,
        「ほげ」の検索結果を埋め込み ▲
        (thx > Konstantin Levinski (kostya@pmail.ntu.edu.sg))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/605>))
      * 新規メモまわり
        * 一ファイル複数メモのとき, 新しいメモは先頭に追加とする設定 ▲
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/535>))
            (setq howm-prepend t)
        * 「ほげ」を検索して一覧した状態から新規メモを作ると, タイトルを「ほげ」に
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/573>))
            (setq howm-title-from-search t)
        * テキストを選択してから「新規メモ」 → そのテキストを自動挿入
          ((<thx|URL:http://hpcgi1.nifty.com/spen/index.cgi?ZaurusSL-C3000%2F%BD%E9%B4%FC%C0%DF%C4%EA%2Femacs%A4%BD%A4%CE%A3%B4#i0>))
            ;; transient-mark-mode でないと, この設定は無視される
            (setq howm-content-from-region t)
        * howm-create-here で, 有無を言わさず「現カーソル行に」新規メモ作成
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/542>))
            (setq howm-create-here-just t)
      * 一覧からのソート基準に numerical-name を追加
      * 開発用
        * C-u M-x howm-bug-report で関連変数の一覧
        * M-x howm-elp で, プロファイラ elp の準備
  * その他の改良
    * M-x customize に対応 ([Applications] → [Howm])
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?RoadMap>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/668>))
    * 一覧バッファで, 同じファイル名をくり返し表示しない
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/754>))
      * ついでに色もつけてみた. どなたかもっとましな配色をください.
        (M-x customize-group RET howm-faces RET して,
        howm-view-name-face と howm-view-empty-face)
      * (参考) 関連する既存機能
        * TAB・ALT-TAB → 次・前のファイルへ
        * u → 一つのメモは一行だけに
        * @ → 連結表示すれば同じメモは一つにまとまる
    * 一覧で T → タイトル表示を「トグル」
      * 1.2.1 の隠し機能から改良
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/427>))
    * メニュー内の一覧では, 行頭でなくても RET でジャンプ
    * リマインダ直叩きで「臨時に開いたバッファ」を自動で閉じる
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/705>))
        ;; 叩き先を自動で閉じる. undo できなくなるからおすすめしません.
        (setq howm-action-lock-forward-kill-buffer t)
    * howm-menu-lang のデフォルトは locale を見て決める
    * ext/howm2 で「come-from キーワードの alias」をサポート
      * あいかわらずやっつけ仕事.
        …というか, 元がやっつけ仕事すぎて, もう解読できず.
  * 内部実装
    * cl パッケージからの関数を howm-cl.el に分離.
      いつか気合がはいれば追放しよう…
    * make 時の警告「… not known to be defined」を抑制
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1111816102/485>))
    * riffle.el の仕様を変更(gfunc.el を使う). ユーザーには影響ないつもり.
      ((<howmoney|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?howmoney>))
      もだいじょうぶと思うんだけど…
  * バグ修正
    * 白黒機でエラー
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/706>))
      * カラーディスプレイでないときは, 下線のかわりに反転表示して,
        それ以外の飾りはなし
    * %reminder の区切り線がずれる
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/703>))
    * 一覧から T でタイトル表示したとき, 無タイトル分が多重表示されていた
      * make test して C-c , s top [RET] T で発症
    * win で「…\.foo\…」などを検索対象としないように
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/746>))
    * howm-message-time をセットしても「No match」メッセージを隠さないように
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/751>))
    * メニューのリマインダ内で come-from キーワードに
      下線がつかなくなっていた.
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/732>))

== 1.2.x

* [2005-06-10] v1.2.4
  * bug fix
    * howm-search-path 内の予定・todo がメニューに表示されなかった
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/685>))
    * メニューに表示される予定の範囲が一日短かかった
    * xemacs で, ファイルが少ないとメニューの %recent がエラーに
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * メニュー上で C-c , r したときは「メニュー更新」
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/696>))
    * howm-menu-list-face のデフォルトを空に
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/697>))
    * (require 'howm-mode) したら cl 未ロードでエラー
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/698>))
      * howm-1.2 からは (require 'howm) が正式.
        「howm-mode」を require や autoload していたら,
        「howm」に直してください.
    * ((<howmz|URL:http://noir.s7.xrea.com/archives/000136.html>))
      でエラー (関数 howm-view-sort-items がない)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      > (TxT) さん
    * リマインダ直叩きでメニューが崩れる
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/705>))
      * こんな設定で発症
          (setq howm-menu-expiry-hours 2)
          (setq howm-action-lock-forward-save-buffer t)
    * POBox の RET (変換確定)を奪ってしまう
      * 実は POBox に限らず不本意な動作をしていた
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/711>))
    * howm-message-time が真のときの所要時間表示は 1 秒未満も計測
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/709>))
      * [2005-03-09] にコメントアウトされていた(理由失念)のを戻した
  * リリース予定版 howm-1.2.4rc7 との違いは…
    * ext/howmkara, ext/hsplit.rb, doc/README.html を配布ファイルに追加

* [2005-05-07] v1.2.3
  * bug fix
    * C-c , s RET のように空文字列を検索したときは, 全メモ一覧
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/668>))
    * make でなく手動 byte-compile したら(?), %reminder でエラー
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/669>))
  * リリース予定版 howm-1.2.3rc1 [2005-05-06] とほとんど同じです

* [2005-05-02] v1.2.2
  * Note
    * 表面上ほぼ変化なし. 内部に手をいれて拡張性を高めた(バックエンド切り離し).
    * 後述の隠し機能は, やっぱりまだ隠し
      * 詰めが甘くても使ってくださるなら, このへんがおすすめ
        * come-from キーワードの alias
        * メニューに「%reminder」「%recent」「%random」
          「%here%(howm-menu-search "ほげ")」
        * M-x howm-list-buffers, M-x howm-list-mark-ring
        * M-x howm-random-walk
    * 気になる指摘もあるけど, どうもはっきりせず, 見切り.
      再現できた方はお知らせください.
      ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/582>))
    * 途中から ChangeLog をつけはじめたので, 細かい直しはそちらも参照
    * リリース予定版 howm-1.2.2rc6 [2005-04-29]と同じものです
  * 仕様変更 (たいがい影響なさそう)
    * 設定 howm-menu-top と howm-menu-file の優先順を逆に
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/648>))
      * 旧: howm-menu-top が指定されているなら, howm-menu-file は無視
      * 新: howm-menu-file が指定されているなら, howm-menu-top は無視
      * なお, デフォルトでは,
        howm-menu-top が指定済みで howm-menu-file が無指定
    * トップメニュー「<<< %menu%」を探す際は howm-search-path を無視.
      howm-directory だけ検索.
  * 改良
    * egrep, fgrep がないけど GNU grep はある, という環境(リナザウ?)に対応
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/621-625n>))
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
    * contents での絞り込みも C-u に対応 (マッチしたメモを排除)
      → ((<応用例|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/593>))
    * todo リストやメニューの %reminder でも, 旬度が同じなら文字列順ソート
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/571>))
    * こまごま
      * 一覧から「T」でタイトル表示したとき, 無タイトル分は最後にまわす
      * 下線以外で RET 叩いたときの処理を若干丁寧に
      * ext/howmkara で, 自身へのリンクを抑制.
        ついでに Home へのリンクをフッタに追加.
      * ext/hcal.rb の [2004-12-25]@3 記法対応
      * 英語メニューを充実. 英文 index.html に高速化オプションの説明.
      * 表示用バッファには (buffer-disable-undo)
    * 開発者向け
      * バックエンドの切り離しに着手(howm-backend.el)
        * おかげで, 特殊フォルダや特殊一覧が可能になった
        * 不完全だけど, 使いながらこけた所を直していくっていう泥縄で
        * gfunc.el (安直 generic function)
        * howm-view-call-process → howm-call-process 等
      * cheat-font-lock.el を大掃除
      * win 用に, 「make test」相当の test.bat
      * make test は --no-site-file に
        * xemacs だと -no-site-file みたいだけど, 放置
      * howm-configuration-for-major-mode の例(howm-misc.el のコメント)を微修正
        (defalias 用)
  * bug fix
    * 検索がメモの最終行にマッチしたとき, 連結表示で二重表示になっていた
    * howm-view-use-grep が non-nil のときエラー
      (howm-view-grep-file-stdin-option が nil だと発症)
    * howm-view-use-grep が nil のとき, howm-excluded-file-regexp に
      該当するファイルはスキャンしないように
      ((<thx|URL:http://www.bookshelf.jp/pukiwiki/pukiwiki.php?%BC%C1%CC%E4%BD%B8%2F42>))
      * …というか, もっと根元から対象外ファイルのチェックを見直し
      * ついでに, howm-excluded-file-regexp のデフォルトに
        (image-file-name-regexp) 相当を追加
    * 0123-45-6789 みたな電話番号に下線を引かない (日付とみなす条件を厳しく)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * 「%」を含む message のエラー
    * emacs20 でメニューを隠しバッファにすると色がつかない
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/541>))
    * 一覧・内容バッファからメモを開く際の, narrowing 解除の判定ミス
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/532>))
    * come-from 記法を変更した場合の手当て
      (thx > taku さん)
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
  * テスト版・リリース予定版の bug fix (抄)
    * >>> ~/hoge.ps や file://~/hoge.ps を RET で開こうとするとエラー
      (thx > Konstantin Levinski (kostya@pmail.ntu.edu.sg))
      * たぶん [2005-01-07] にやらかしたバグ
    * emacs21 と 20 とで, font-lock-fontify-{block|buffer} を使いわけ
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/416>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/533>))
      * 理解してなくて, インチキ手当です
      * 一ファイルが長くて着色に時間のかかる方は,
        emacs20 でなく emacs21 をおすすめします
    * howm-list-all-title が non-nil のとき C-c , a の並び順が逆
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/599-600n>))
    * メモを開くのが異常に遅く
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/635-636n>))
      … 直ったのか未確認
    * Meadow 2.10 で make がエラーに
      (thx > taku さん, 「も」さん)
      ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/638>))
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?MaxSpecpdlSize>))
    * Meadow-1.15 だと, image-file-name-regexps が未定義でエラー
      (thx > 逃避さん)
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * howm-directory が ~/.howm のようなドットディレクトリだと何も検索されず
      (thx > taku さん)
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))

* [2004-12-11] v1.2.1
  * Note
    * 表向きは, ほとんど bug fix のみ
    * 隠し機能は, まだ隠し
  * 改良・変更
    * 予定表で, 同じ日の予定は記述内容でソート
      * 「[2004-11-04]@ 07:30 ほげほげ」みたいに書いとけば, 時刻順に並ぶはず
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/398>))
      * 「同じ日のは重要度順でソート」って要望も, これ使ってなんとかなる?
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/433>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/442>))
        ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/445>))
      * tag2plan は直していません.
    * howm-template のデフォルトは howm-view-title-header に応じる
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/498>))
    * howm-view-contents-persistent のデフォルト値を nil から t に変更
      * 内容バッファ *howmC* から RET で開いても, *howmS* や *howmC* は消えない
    * 変数 howm-list-title の設定値に「howm-keyword-search」を追加
  * Fix
    * メニューの予定表で,
      「howm-menu-schedule-days-before 日前から howm-menu-schedule-days 日後まで」
      のはずが「…日前から…日間」になっていた
    * タイトル表示時に, タイトル欄「= 」がないメモを救出
      * タイトル表示オンをデフォルトにするための布石
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/490-493n>))
        ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/503>))
    * ((<奥山さん版 auto-save-buffers|URL:http://homepage3.nifty.com/oatu/emacs/misc.html#asb>))
      との併用で,
      「.howm-keys has changed since visited or saved. Save anyway?」
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/507-509n>))
    * [2004-09-01 23:26]@ とかの当日分がメニューに表示されなかった
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/411>))
      * 表示されるってだけ. おすすめしません.
        この書式を本気でサポートするか未定なので.
    * meadow で, [2004-08-08]! の「!」で RET 叩くとエラー 
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/384>))
      * 再現できなかったけど, 何にせよこれで直るんじゃないかと
    * メモを開いたときに narrow だと, 隠れてる部分の初期化ぬけ
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
  * 内部整理
    * howm-list-migemo が直接 howm-view-search を叩かないよう
    * howm-set-mode-p を howm-set-mode から分離
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/388>))
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SetMode>))
    * リマインダの action-lock を整理していじりやすく. 動作は変化ないはず.
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?TangledToDo>))
      * 副作用で, howm-reminder-regexp-XXX-pos のデフォルト値がひとつずれた
  * その他
    * 一覧バッファと内容バッファを「o」で行き来
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/424>))
      …は, 書いてみたけど消しました. @ or 0 があれば不要ですよね?

* [2004-08-24] v1.2
  * Note
    * バグ修正 (narrowing, CVS 先端 emacs での着色)
    * 1.1.1〜1.1.2 の隠し機能を公式化 (todo 直叩き, 保留「~」)
    * ファイル構成の一部変更 (howm.el, riffle.el) → ((<インストール>))
    * その他こまごま改良 (file:// でも外部 viewer)
    * 隠し機能追加 (メニューに最近＆ランダム一覧)
    * ほんとの目玉は come-from の alias なんだけど, まだしばらくは隠し機能
    * リリース予定版 howm-1.2rc6 [2004-08-16]と同じものです
  * 隠し機能の公式化
    * 目玉
      * todo の新型: 保留「[2004-01-09]~100 ほげ」
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/497>))
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Defer>))
      * メニュー・予定表・todo 一覧から, -+!@~. 上で RET を直接叩ける
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/506>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/568>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/580>))
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?RepeatedToDo>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/698>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/732>))
    * 小改造
      * 一覧
        * 表示切りかえ (0,1,2,v キー)
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/826-827n>))
        * T でタイトル表示, TAB・M-TAB で次・前のファイルへ
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/61>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/892>))
        * リマインダの日付によるソート・絞り込み
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/694>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/726>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/742>))
          * もうちょっと吟味して一般化したい気も (赤字箇所でソート, とか)
        * 絞り込み基準に Region, Around を追加
        * ((<カスタマイズ>))参照
          * 一覧バッファへの色つけ
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/675>))
          * ○○のときだけタイトル表示
            * howm-list-recent-title, howm-list-all-title はそのうち廃止の予定
            * howm-list-title には関数も指定可能
              ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/61>))
          * C-x 1 後は勝手にフレームを分割しない
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/747>))
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/766>))
      * 検索
        * foo を検索したら [[foo]] を上位に
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/835>))
          * デフォルトにしました. いやなら (setq howm-list-prefer-wiki nil)
          * [[…]] をカスタマイズしたら, howm-wiki-format も要設定
              (setq howm-wiki-format "((＜%s＞))")  ;; ((＜…＞)) に変更した例
        * howm で foo を検索した後は, C-s C-s も foo の検索に
            (setq howm-view-update-search-ring t)
        * 検索に使う関数を((<カスタマイズ>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/166-167n>))
      * メニュー
        * [今日] [昨日] を拡張
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/37>))
          * 一メモ一ファイルのときは, その日付文字列を検索.
            一日一ファイルのときは, その日付のファイルを開く.
          * デフォルトのメニューに追加しました
          * もっと便利そうな
            ((<別解|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/39>))
            がすでに :-)
        * キー設定に C-i と M-C-i を追加
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
      * action-lock の((<カスタマイズ>))
        * 「file://…」や「http://…」
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/945>))
        * { } や {_} を変更しやすく
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/828>))
          * {_} はテンプレート書式と連動した方がいいかと思って, 小細工してました
            * howm の load 前に howm-dtime-format だけ設定すると,
              howm-template-date-format もそれになる
            * {_} の書式は howm 側で上書きするから, ↓は無効
                (setq action-lock-date-default '("{_}" "[%Y-%m-%d %H:%M]")) ;; 叩き前後
            * howm-action-lock-done-date-format なんて
              README に書いてたのはウソ. 作ってませんでした ^^;
      * リマインダ
        * !+-~. の旬度の((<カスタマイズ>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/84>))
  * こまごま改良
    * file:// でも画像等は外部 viewer 起動
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/309>))
    * 日付形式の直叩き (メニューや todo 一覧から)
    * メニューの %schedule, %todo, %recent, %random の action-lock を統一
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/264>))
      * 「>」で RET 叩くと「|」以降を検索
      * カスタマイズ
        * 色の変更
            (set-face-foreground 'howm-menu-list-face "cyan")
        * 書式の変更: 変数をいじる →
          howm-menu-list-format, howm-menu-list-regexp,
          howm-menu-list-regexp-key-pos, howm-menu-list-regexp-action-pos,
          howm-menu-list-regexp-face-pos
        * 激しい変更(日曜は赤, など): 関数をいじる →
          howm-menu-list-format, howm-menu-list-rules,
          howm-menu-font-lock-rules
    * 「日付上で RET」「リマインダ上で RET」したときの動作を小賢しく
      * 一文字コマンド → ただちに発動 (その後の RET は不要)
      * 多文字コマンド → 続けてミニバッファから入力
    * 一覧から C-u f したときには「Reject」と表示
    * 予定表・todo 一覧に曜日を表示
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * 文書の更新
      * インストール法の説明は, make install の方をメインに
      * index-j.html に画像と UNIX USER 記事へのリンクを追加
    * 実装・開発
      * 懸案のファイル名変更を決行
        * howm-mode-mode.el → howm-mode.el
        * howm-mode.el → howm.el
        * 小細工で, 従来の (require 'howm-mode) でも動くようにはなってる
      * ぱらぱら一覧 riffle.el を, howm-view.el から分離
        * 仕様変更[2004-07-20]
          * riffle-controller の insert-item → contents-item
            * 内容を自分で書き出すんじゃなく, 内容を文字列で返す
          * 「section」を廃止
      * howm-menu.el 整理
        * 内部仕様の変更 (howm-menu-display-rules まわり)
        * 変数・関数の並べかえ
      * howm-bug-report 改良
        * emacs バージョン表示を詳しく
        * make しなくてもバージョンだけは表示
        * byte-compile, make, make test の有無も表示
      * howm2 から [[○○]] の処理を削除
        * 対応する <<< ○○ があるはずなので, そっちにまかせる
      * hcal.rb, tag2plan の ruby オプション記法なおし
        ((<thx|URL:http://arika.org/debian/howm_1.1.2.1-0+0.diff.gz>))
  * Fix
    * CVS 先端 emacs で色がつかない
      ((<thx|URL:http://www.unix-fan.com/celt/blog/20040612.html>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/302>))
    * メモが narrow されてると, 検索されない・閲覧できない・開いても見えない
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/311>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/329>))
    * 「>>> ファイル名」のときはタイトル表示を off
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/309>))
    * >>> から画像を開いたあと, 元と違うバッファが表示されたりしてた
    * 「<<<」上で RET (関連キーワード一覧)を正しく. 激遅に ;_;
      * emacs と grep の正規表現の違いがまたもネック
      * ちなみに仕様は,
        「そのキーワードに含まれるキーワードを含むキーワードの一覧」
    * 存在しないディレクトリを howm-directory に指定すると,
      初回の C-c , , でエラー (make install した場合)
    * howm-from-change-log でタイトルがコピーされなくなってた
      (thx > 'UCONNのポスドク'さん)
    * howm-view-open-hook が二度呼ばれてた
    * 設定によって, 全メモ一覧で「タイトル抽出」が二回呼ばれてたかも
    * README 訂正 (howm-title-header → howm-view-title-header)
    * Meadow 用の「ドライブ名の大文字小文字」対策を
      微修正(howm-normalize-file-name). 挙動は変化ないはず.
      ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?DriveLetter>))
    * テスト版のバグ(抄)
      * 「メニューに %recent」で対象外ファイルのチェックを忘れてた
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/246>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/247-248n>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/255-257n>))
      * ((<インストール>))の「はじめて C-c , , した時に読み込む」の設定抜け
      * テスト版のバージョン表示が, ユーザが ./configure した日になってた.
      * emacs -nw なとき, [2004-08-08]! の「!」で RET 叩くとエラー
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/354>))
      * 各ファイル個別に byte-compile したときの不具合
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
        * マクロがマクロ扱いされてなかった
          → ファイルまたいで使うマクロは howm-common.el に置き,
          各 howm-*.el が明示的に require

== 1.1.2.x

* [2004-05-09] v1.1.2.1 
  * fix: make test がエラー
    * パッケージングのミスで, ~/sample/ 以下のファイルに抜け

* [2004-05-06] v1.1.2
  * Note
    * 中途半端だけど, せっかくしばらく変更ないからリリースしとこうかと.
      * 1.2 はまだ先. でも 1.1.1.x はもうメンテしたくない. ってことでつなぎに.
    * 公式機能は 1.1.1.3 からたいして変更なし.
    * 1.1.2rc1 から全く変更なし.
  * 隠し機能
    * alias 試作, その他小改造
  * 微改造
    * 「一覧からの絞りこみ検索」のキー変更 (s → G)
      * s は C-c , s の省略形とかぶってた
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/133-134n>))
    * 「make test」で「素の設定でのテスト」
    * メニュー中での「%|」による不可視トグルを公式機能に
    * howm-view-real-grep は「パターンのリスト」も受けつける
        (setq howm-view-grep-file-stdin-option "-f -")
      * 特に linux 以外で grep 使用な方は, この設定をして不都合が出ないか,
        試していただけると助かります.
        見た目の動作は何も変わらないつもりですが…
      * 「come-from キーワードの alias」に向けた布石です
        ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?CompoundComeFrom>))
  * fix
    * 「<<< foo {_}」の「<<<」で RET 叩いても no match
      * grep 使用時のみ? ({}が特殊文字)
      * やっつけ修正. 根本修正は「複数パターン grep」ができてから.
    * come-from リンク「foo」上で RET しても,
      「<<< foo bar」の方が上に表示されたり
      * come-from 書式を変更して「行末まで」じゃなくしたときは,
        変数 howm-keyword-regexp-format も設定してください
    * howm-keyword-case-fold-search が真でも,
      正規表現を downcase しないよう修正 (\W とか意味変わってしまうから)
    * M-x howm-mode で howm を off にしてから保存しようとするとエラー
    * hcal.rb で保留記号が古いままだった (* → ~)

== 1.1.1.x

* [2004-04-29] v1.1.1.3
  * fix: C-c , d RET で今日の日付を入力したとき, 勝手に一覧へ飛んでた
    (howm-insert-date-future が nil のとき)
    * ついでに, howm-insert-date-future を設定して
      おせっかいが発動したときは, メッセージを出すようにした
  * v1.1.1.3rc1 [2004-04-14] と内容同じ

* [2004-04-01] v1.1.1.2
  * Note
    * 1.1.1.1 からの bug fix 一点のみ
    * 1.1.1.2rc1 からの変更はありません
  * fix: C-c , g foo で Foo がヒットしない (grep 不使用時)
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/88>))
    * メニューの予定・todo 一覧の「>」で RET → no match
      というバグも出ていた模様
      (howm-keyword-case-fold-search 設定時)
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/96>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/108>))
      thx > NARA Shinsuke さん

* [2004-03-16] v1.1.1.1
  * Note
    * v1.1.1 からの bug fix 版です
    * 「隠し機能」も少々追加
  * 微改良
    * migemo-server を不要に
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/899>))
      * grep 使用ならまだ必要
        (→((<カスタマイズ>)))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
    * howm-menu-mode-map をふつうにキー定義可能に (buffer local じゃなくした)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
    * action-lock-set-rules は勝手に remove-duplicates
  * fix
    * (setq howm-list-title t) しても C-c , l でタイトルが出ない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/924>))
    * 日付入力で 01230 が 20001230 じゃなく 1230 と解釈されてた
    * hcal.rb でも ~ は指定日まで潜伏するよう
    * 内容バッファの C-t, M-C-t → C-i, M-C-i
    * 「<<< [foo」で「[foo」を叩くとエラー (regexp-quote し忘れ)
      * [2004-02-23] の修正はまちがってたので再修正
    * emacs20, meadow1.x(?) で http:// に下線がつかない
      (拡張正規表現 "[htp]\\{3,5\\}")
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * grep 不使用時, S → foo で「Foo」がひっかからない
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/51-54n>))
    * README の修正
      * todo 表示件数のカスタマイズ法を追記
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/69>))
      * {_} のカスタマイズ法の説明を修正
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/79-82n>))

* [2004-02-21] v1.1.1
  * Note
    * 大量のこまごま改良
    * 目玉改造(保留, todo 直叩き, ソースコード閲覧)は,
      まだ「隠し機能」ってことで
    * v1.1.0.* から, そのまま移行できるつもり. 書式変更などなし.
    * v1.1.1rc3 と内容同じ
  * 新設定 (→((<カスタマイズ>)))
    * todo を RET 一発で済に (howm-action-lock-reminder-done-default)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/698>))
    * 内容バッファに色つけ (howm-view-contents-font-lock-keywords)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/665>))
      * ほんとはファイル冒頭と拡張子から自動判定すべきなんだけど, ひとまず.
    * 一覧のソート基準 (howm-list-normalizer)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/503>))
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SortBy>))
    * メニューの予定表の表示範囲 (howm-menu-schedule-days-before)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * 日付の新規入力時に年や月を略したら未来と解釈 (howm-insert-date-future)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/425>))
    * howm-template を選択可能に
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SwitchTemplate>))
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * (setq howm-list-title t) すれば, 一覧では常にタイトルを表示
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/288>))
      * howm-list-recent-title, howm-list-all-title は undocumented に.
        そのうち廃止のつもり.
    * 短縮ファイル名の設定を howm-abbreviate-file-name に一本化.
      ついでに howm-keyword-to-kill-ring も短縮ファイル名に.
    * キーワード一覧の置き場所 (howm-keyword-file)
    * howm-create-file-hook を追加
    * ChangeLog メモとの併用 (→CL-HOWM.ja.rd)
      (thx > 'UCONNのポスドク'さん)
  * 新コマンド
    * C-c , s で固定文字列検索 howm-list-grep-fixed (C-u C-c , g と同じ)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?KeyBinding>))
    * C-c , SPC で howm なバッファと howm でないバッファとを切り替え.
      howm なバッファがなければメニューを開く.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Idea>))
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ImplementedIdea>))
    * 一覧表示・内容表示では n, p でもカーソル上下
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/473>))
    * 「file://…」上で C-u RET → find-file-other-window
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/449>))
    * [今日] [昨日] まわり (一日一ファイルを想定)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/452>))
      * howm-open-today は, 新規作成時にテンプレート挿入
      * M-x howm-open-past で昨日を,
        C-u 3 M-x howm-open-past で 3 日前を開く 
  * 変更
    * 一覧バッファ・内容バッファ
      * 一組しか作らない(不評なら戻します). ((<カスタマイズ>))も参照.
      * q したら, フレーム分割を強制解除
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/359>))
        * 「元の状態に復帰」は give up です.
          ごめんなさい. 私の腕と気合では, 根本解決は難しそう…
          ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/194>))
          ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/415-416n>))
      * sort-by-date を「新しいものが上」に変更
      * 一覧表示から @ で連結表示に切りかえたら, window の分割を解除
      * 連結表示で, 各メモの最後の空行を省く
      * 「>>> ファイル名」で RET×2 したら, window 分割状態を復元
    * メニュー
      * > で RET → 該当行へ直ジャンプ
      * 「済み」は表示しない
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
      * 潜伏中のリマインダは表示しない (howm-menu-todo-priority)
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/620>))
      * メニューのキャッシュを設定しない限り, メモ保存時のメニュー更新はオフ
      * schedule, todo 欄に表題・書式説明を追加
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/428-430n>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/432>))
        * %sdays (予定表の表示日数), %tnum (todo の表示件数)も
    * howm2
      * メモ一覧にタイトルを表示
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/317>))
      * howm2 -r でファイル一覧を逆順に
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/334>))
    * autoconf, automake に着手
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/304-307n>))
      * navi2ch から恥知らずにコピー. よくわからないままいじる.
        * doc/releng.txt よりメモ: tar.gz を作るには (開発者用)
            aclocal && autoconf && automake && make Makefiles && make dist elcdist
      * 参考: ((<URL:http://shimaki-hp.hp.infoseek.co.jp/autoconf/book1.html>))
      * デフォルトのインストール先が /usr/share/emacs/site-lisp/howm
        になるように
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/324>))
      * make install で doc, ext, en, ja もインストール
        * 自分用メモ
            make maintainer-clean; aclocal && automake && autoconf && ./configure --prefix=/tmp/hoge --with-lispdir=/tmp/hoge/el && make && make install
      * 初回起動時に howm-directory やメニューファイルがなければ自動で作る
        (make install したときのみ)
    * その他
      * auto-save-buffers が呼ばれたら save 時の自動処理を off に
      * 未保存の編集も, 検索や内容表示に反映
        (howm-view-use-grep が nil のときに限る)
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/482>))
      * Wiki 風リンク [[hoge]] は hoge に下線 (「<<< hoge」の有無にかかわらず)
        (→((<カスタマイズ>)))
        ((<thx|URL:http://tiki.is.os-omicron.org/tiki.cgi?c=v&p=howm>))
        ((<thx|URL:http://pc2.2ch.net/test/read.cgi/win/1067394259/341-347n>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/672-673n>))
        * 従来は「]]」だけに下線.
          「作成済みかどうかを下線のつき方で区別できる」っていう
          とんちだったんだけど, はまった方が複数.
      * ttp:// の類も http:// に直してブラウザへ
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/676>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/680>))
      * howm-toggle-search-other-dir で, 予定表や todo リストの検索範囲も拡大
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/623>))
      * ((<インストール>))の説明にバイトコンパイルを追加
      * howm-reminder-tomorrow-face の背景を茶色からピンクに
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/805-807n>))
    * ユーザーには関係なし
      * ファイル構成の変更 (→((<実装について>)))
      * howm-menu-action の引数仕様を変更
      * copy-list → copy-sequence (cl パッケージ → built-in)
  * fix
    * URL の抽出をちょっとだけましに
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/672-673n>))
    * 一覧バッファのむだな書きなおしを抑制
      * howm-list-normalize からむだに何度も howm-view-summary が呼ばれて…
      * いちばんひどいとこだけ修正. まだむだは残ってる.
    * meadow 向けに, ドライブレターの大文字小文字を無視
      (howm-normalize-file-name)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?DriveLetter>))
    * 英語メニューの [String] と [Regexp] が逆
    * xemacs canna 対策
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?CannaReturnKey>))
    * split-horizontally が t なら C-x 1 後も横並べに
      (((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/634>))
      > ヽ(´ー`)ノさん)
    * howm-menu-{schedule|todo} のメッセージ,
      howm-view-call-process の戻り値チェック
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/626>))
    * grep 不使用時の migemo 検索 (howm-list-migemo)
      (thx > やまだあきらさん akira@arika.org)
    * meadow 1.15 で他バッファの font-lock が注釈・文字列のみに
      * font-lock-defaults の大域値を変えてしまってた
    * 一覧で ! (shell) が xemacs や emacs20 でエラー
      (replace-regexp-in-string や (buffer-size buf) がない)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Usage>))
    * xemacs 関連:
      thx > ((<笠原さん|URL:http://www.nc.kyushu-u.ac.jp/~kasahara/diary/2004/01b.html>))
      …こっそり言及返し :p
      * 「(setq font-lock-verbose nil) で速くなるよ」 (→((<インストール>)))
        * howm なバッファでは自動でこれを…と試みたけど, できてなさげ
      * xemacs での byte-compile
        * defvar を make-variable-buffer-local より前に
          * action-locl.el:
            action-lock-rules,
            action-lock-original-font-lock-keywords,
            action-lock-original-return
          * howm-view.el:
            howm-view-name,
            howm-view-item-list,
            howm-view-summary-last-line,
            howm-view-contents-end
        * ついでに凡ミスも多数露呈
          (reference to free variable / bound but not referenced)
    * 「>>>」だけの行から次の行にかけて下線が引かれてた
    * howm-menu.el を iso-2022-7bit に (for Meadow)
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
    * .gz, .jpg, .gif, .png も検索対象から除外
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ImageCapture>))
    * 最終行に改行がないと, 内容表示でその行が出ない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/471>))
    * 一ファイル複数メモで (setq howm-list-title t) なとき,
      一覧で「S → date」をくり返すとソート順が入れかわる
      (thx > NAKANO Yasuhiro さん <nkyasu@yahoo.co.jp>)
    * 一部環境で, global-font-lock が効かなくなる
      (emacs-20.7.2 on Vine Linux 2.6, Meadow1.15 on WindowsXP Professional).
      (thx > NAKANO Yasuhiro さん <nkyasu@yahoo.co.jp>)
    * 一覧表示のファイル名欄のデフォルト幅
      (thx > Jun Kuriyama さん <kuriyama@imgsrc.co.jp>)
    * Jargon のリンク切れ
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/420>))
    * コード添削
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/421>))
      * skip-one-link → action-lock-skip-one-link
      * *.el の先頭・末尾に定型コメント
    * ((<mcomplete.el|URL:http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el>))
      との併用でエラー
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/365>))
    * 「<<<」で RET したときにも howm-keyword-case-fold-search を反映
    * (setq howm-list-title t) すると, 一覧と内容の上下が逆転
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/348>))
    * バイトコンパイル時の警告をほぼ退治
    * howm-keyword-regexp-header-pos → howm-keyword-regexp-hilit-pos
    * howm2 のデフォルトで, ファイル #foo.bar や foo.bar~ も対象外に
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/316-317n>))
    * ruby 1.8 系で howm2 がエラー
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/308-310n>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/317-318n>))
    * 明背景むけの配色
    * カーソルがタイトル上にあると
      C-c , K (howm-keyword-to-kill-ring) に失敗
    * CVS 先端 emacs だと on-the-fly で色がつかない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/268>))
    * 一覧で ! による shell command の出力がすぐ消えてしまう
      * ついでに, 出力が空のときは出力バッファを出さないようにした
      * ついでに, これも短縮ファイル名に
    * 連結表示で TAB を叩いてから RET 叩くとエラー
    * CVS 先端 emacs で, 一覧・内容バッファに色がつかない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/259>))
    * howm-search-other-dir の説明を追加
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/260>))
  * テスト版からの変更 (抄)
    * fix: (setq howm-insert-date-future t) の状態で,
      [2004-01-23]に C-c , d して「215」 → 2005 年に
      (howm-datestr-expand)
      * テスト版 2003-12-27 〜 2004-01-21 のバグ
      * テスト版 2005-01-23 で修正
    * yc 対策を柔軟に ← 結局廃止
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
      * eval-after-load だと (load "~/elisp/yc.el") がマッチしないので,
        defadvice に変更
      * ほんとは, 同様のことを egg, anthy にもやるべき
        (patch 募集. howm-misc.el 参照) ← やっぱやめ[2004-01-18]
    * メニューでは [2004-01-10]! 等に下線をつけない ← 「直叩き」として復活
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/506>))
    * fix: メニューに旬度を表示したら > から飛べなくなってた
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * fix: anthy との併用 ← 結局廃止
      (thx > Jun Kuriyama さん <kuriyama@imgsrc.co.jp>)
    * fix: index-j.html からのリンクずれ
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/361>))
    * fix: ドキュメントのインストール先
      /usr/local/share/doc/howm → /usr/local/share/howm/doc
    * fix: README のミス. 全メモ一覧は C-c, l じゃなく C-c , a
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/347>))
    * fix: ロード後に howm-keyword-file をセットした場合でも,
      初回起動時に自動作成
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/270>))
    * fix: ((<yc.el|URL:http://www.ceres.dti.ne.jp/~knak/yc.html>))
      で「確定」の RET キーを howm が食う
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/274>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/283-284n>))
    * C-c , d → d で日時を挿入 ← 廃止
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/595>))
      ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/597-598n>))
      * ついでに prompt に曜日
    * ((<カスタマイズ>)) の記述ミス: howm-directory → howm-keyword-file
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * configure.in に Meadow.exe を追加
      (((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?RoadMap>))
      > Meadow2使いさん)
  * 廃止
    * canna, egg, yc, anthy の個別対策
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
      * かわりに, action-lock-mode の優先度を最弱に.
      * 以前の対策に戻すには,
          (setq action-lock-bury-minor-mode-p nil)
          (setq howm-ime-fix t)
    * C-u C-c , c (新規メモに「>>> 元ファイル」を入れない)と
      howm-template-use-file
    * howm-view-quit-to-nonhowm (一覧で q → howm 以外の buffer へ)
    * howm-template-file-abbrev, howm-view-header-abbrev
      (howm-abbreviate-file-name に一本化)

== 1.1.0.x

* [2004-02-08] v1.1.0.4
  テスト版からの backport (thx > 皆様 …テスト版 README 参照)
  * 潜伏中の項目はメニューの todo リストに表示しない
  * C-c , s で固定文字列検索 howm-list-grep-fixed (C-u C-c , g と同じ)
  * canna, egg, yc, anthy の個別対策を廃止. かわりに抜本対策.
  * メニューのキャッシュを設定しない限り, メモ保存時のメニュー更新はオフ
  * auto-save-buffers が呼ばれたら save 時の自動処理を off に
  * .jpg, .gif, .png も検索対象から除外
  * 「xemacs では, (setq font-lock-verbose nil) で速くなるよ」 (→((<インストール>)))
  * fix
    * xemacs canna 対策
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?CannaReturnKey>))
    * 英語メニューの [String] と [Regexp] が逆
    * howm-view-call-process の戻り値チェック
    * meadow 1.15 で他バッファの font-lock が注釈・文字列のみに
    * 一覧で ! (shell) が xemacs, emacs20 でエラー
    * xemacs での byte-compile
    * 「>>>」だけの行から次の行にかけて下線が引かれてた
    * howm-menu.el を iso-2022-7bit に (for Meadow)
    * CVS 先端 emacs だと on-the-fly で色がつかない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/268>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/501>))
    * 最終行に改行がない場合, その行が内容表示に出ない
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/482>))
* [2003-12-31] v1.1.0.3
  * fix: 一部環境で, global-font-lock が効かなくなる
    (emacs-20.7.2 on Vine Linux 2.6, Meadow1.15 on WindowsXP Professional).
    (thx > NAKANO Yasuhiro さん <nkyasu@yahoo.co.jp>)
  * fix: 一覧表示のファイル名欄のデフォルト幅
    (thx > Jun Kuriyama さん <kuriyama@imgsrc.co.jp>)
* [2003-12-29] v1.1.0.2
  * fix: anthy との併用
    (thx > Jun Kuriyama さん <kuriyama@imgsrc.co.jp>)
  * メニューに schedule・todo の表題と説明を追加
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/428-432n>))
* [2003-12-27] v1.1.0.1
  * fix: 連結表示で TAB を叩いてから RET 叩くとエラー
  * fix: 明背景むけの配色
  * fix: egg, yc, mcomplete との併用
  * fix: Jargon のリンク切れ
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/420>))
* [2003-11-22] v1.1
  * Note
    * 非互換な変更をまとめてやってしまおう版
    * オプションで, 従来どおりにも使えるようにしたつもり
    * 「((<インストール>))」の移行例を参照
  * デフォルト書式の変更
    * リンク書式を <<, >> から <<<, >>> に変更
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/173>))
      * 対応して, hcal.rb の「今日マーク」を <<<<##>>>> に変更
    * 1 メモ 1 ファイルをデフォルトに
    * リマインダの書式変更 (→((<リマインダ>)))
    * 日付書式を [2003/10/21] から [2003-10-21] に変更
      → ((<参考|URL:http://www.kanzaki.com/docs/html/dtf.html>))
    * メニューをメモと統合
      * 「%○○%」を検索したら, 「<<< %○○%」を menu-mode で開く
      * C-c , , で「<<< %menu%」を開く (menu.howm ではなく)
  * 改良
    * <<< での大文字小文字の区別 (→((<カスタマイズ>)))
    * <<< などに色つけ
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/133>))
    * デフォルトのメニューにショートカットキーを追加
    * 日付形式「2003-10-30」の action-lock に「くり返し」を追加
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/224>))
    * XEmacs, CVS 先端 Emacs, Carbon Emacs に対応(?)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/209>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/229>))
      * 'や'さんの patch を取り込みました
    * howm2
      * <<< と >>> がからむ場合を修正. 激遅になった ;_;
      * 対象ファイルを選別可能に (-list)
      * オプション追加 (-goto, -comefrom, -i)
    * ((<ChangeLog Memo との併用|URL:CL-HOWM.ja.rd>))
  * 小変更
    * C-c , d と C-c , D を入れかえ
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
    * ソースファイル分割 (howm-font-lock.el, howm-menu.el)
    * デフォルトの major-mode を text-mode に
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/181>))
    * [前後] にキー割りあて (C-c , A)
    * 内容表示とテンプレートのファイル名は /home/hoge/… じゃなく ~/… に
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/228-229n>))
    * 新規メモ作成直後に undo すれば「>>> 元ファイル」を消せる
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/230-235n>))
  * 微変更
    * face を変数で指定
    * howm-menu-lang を設定しなければ, おせっかいに推測
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/121-123n>))
    * howm-menu-refresh-after-save, howm-refresh-after-save を起動後も変更可に
    * メニューの "...done" メッセージやっぱ復活 (schedule, todo のスキャン時)
    * howm2 の index.html で, 数字じゃないファイル名を先に
    * メニュー脱出時の bury-buffer を廃止.
    * howm-ignore-comefrom-same を廃止
    * 隠し機能: howm-view-{summary|contents}-persistent に関数も設定可能
        ;; 例: 予定表・todo リストや全メモ一覧等だけ persistent
        (setq howm-view-summary-persistent
              (lambda () (member howm-view-name '("{schedule}" "{todo}" ""))))
      * 気がかわる可能性あり
    * 変数名 howm-reminder-regexp-date-pos → …-day-pos
    * howm-template-{file|date}-format のデフォルト値は
      howm-ref-header, howm-date-format を参照する
    * *.elc も検索対象外
    * ドキュメント
      * 「((<動きませんよ?>))」にチェックリストを追加
      * 「((<インストール>))」に設定例を追加
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
      * メニューは ~/howm/ja/*.howm じゃなく ~/howm/*.howm
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/122>))
      * スクリーンショットをカラー版に
      * goto, come-from の書式変更例を RD 風に
        (→((<カスタマイズ>)))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
      * メニューバッファ隠しの別法を紹介 (→((<カスタマイズ>)))
      * 実はコンソールでも使えてた
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/167>))
      * <<< が RD の include とかぶってる旨の注意
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/198-205n>))
        (→((<インストール>)))
      * カスタマイズ例におまけを追加
      * 「インストール」の移行例に, GNU touch にがない場合のローテク版を追加
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/190>))
      * ((<カスタマイズ>))に「メニューをメモ扱いしない」設定を明記
      * yen.el の紹介 (→((<外部ツール>)))
      * tag2plan の説明を修正 (-date_sep, ~/howm/*/*/*.howm)
      * 「参考」に簑系・超簑を追加
  * fix
    * grep 使用時は「-hoge」が検索できなかった
    * 内容バッファの font-lock (仮修正)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/128-136n>))
      * v1.0.4.1 のバグ: 検索文字列の色
      * [2003-10-05] のバグ: ファイル区切りの色
    * 一覧アからさらに一覧イを表示した後で q したら, 一覧アに戻る
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/149-150n>))
      * 「フレーム分割を復元」との折りあいに自信なし.
        もし不具合が出たら, ↓で従来動作(アの前のバッファに戻る)に.
          (setq howm-view-quit-to-nonhowm t)
    * [前後] の実行直後の内容バッファ
    * filter-by-date 等で「2003 年 9 月 * 日まで」と入力したら,
        「2003 年 9 月 31 日 (= 10 月 2 日)まで」と解釈されてた
      * これ以外にもバグまみれだった[2003-10-26]
    * 一覧バッファと内容バッファの位置が入れかわる症状
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/193>))
      * 再現法: 内容バッファの方にカーソル置いて, g で検索 → q
    * 二日以上起動し続けると, メニュー(予定・todo)の当日と翌日の色つけが変
    * mailcap.el がないときの >>> /etc
    * >>> の後に何も書いてないときは無視すべし
    * 正規表現 [^…] を [^\r\n…] にひととおり直し
  * テスト版からの変更・fix
    * font-lock がどんどん重くなってた. よくこんなので動いてたなあ…
    * fix: xemacs で font-lock が固まる
      ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/248-249n>))
    * やっぱりデフォルトは「タイトル表示なし」
    * 移行例の旧キー設定が, howm ロード前でも後でも反映されるように
    * メニューが開かなかった (.howm-keys に「%menu%」追加)
    * メニューが隠しバッファのとき, 下線がつかなかった
    * メニューの footer がデフォルトでは出なくなってた
    * メニューの %eval% や %call% でバッファ強制切りかえ
    * 変数 howm-menu-persistent を廃止
    * howm-reminder-today-face が未定義だった
    * {_} で RET したときも, 設定した日付書式に
    * 「<<< http」があっても http://… はブラウザを起動すべし
    * howm2 で URL をリンクにするのを忘れてた
      * howm2 -type=rd で URL をリンクにするのを忘れてた
    * [更新] ボタンは元のバッファに戻る
    * 「>>> /foo/bar/」の一覧見出しが空になってた
    * xemacs で mailcap.el がないときのエラー回避
      * howm-try-require の実装をすっきり
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/219>))
    * ~/howm/CVS/* が検索対象になっていた
    * howm-keyword-case-fold-search を設定したら,
      キーワード補完でも大文字小文字を無視
    * デフォルトのファイル名を変更 (ごめんなさい)
      * 20031030-202259.howm → 2003-10-30-202259.howm
      * 変換手順
        * メモディレクトリに cd して,
            find . -name '*.howm' -print | ruby -ne '$_.chop!; d = File::dirname $_; f = File::basename($_).sub(/^(\d{4})(\d{2})(\d{2})/){|s| "#$1-#$2-#$3"}; puts %~mv #$_ #{File::expand_path f, d}~' > ~/howm_kuzu
        * ~/howm_kuzu の内容を確認し, 問題なければ
            cat ~/howm_kuzu | /bin/sh
        * 必要なら, 移行例のようにタイムスタンプをでっちあげ
    * C-u C-c , c なら, 新規メモに「>>> 元ファイル」を入れず, タイトルも空欄
      (変数 howm-template-use-file でカスタマイズ)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/230>))
      … undocumented. そのうち廃止予定.
    * こまごま fix
      * >>> まわり
        * 外部 viewer が常に off になってた
        * howm-keyword-case-fold-search を設定したときの >>> /etc/X11
      * 日付形式「2003-10-30」の action-lock からの search 対象ファイル
      * <<< や >>> で空白を含む文字列が検索できなくなってた
      * タイトルが常に空欄になってた
      * howm-keyword-case-fold-search を設定したら,
        >>> foo でも「<<< FOO」は先頭にくるべき
      * C-c , c でエラー
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/241-242n>))

== 1.0.x

* [2003-10-27] v1.0.4.2
  * fix: font-lock がどんどん重くなってた. よくこんなので動いてたなあ…
* [2003-10-04] v1.0.4.1
  * fix: 「最近」がサブディレクトリをチェックしなくなってた
  * fix: sort-by-date のデフォルトを「新しい順」に (以前はそうだった気が?)
  * ショートカットの多重定義には警告表示
    * ついでにメニューの "...done" メッセージを廃止
  * TUTORIAL.rd に「日付を入れる」追加
* [2003-10-02] v1.0.4
  * >> で外部 viewer (→((<メモを読もう>)),((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/54-57>))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/107-112n>))
    * ディレクトリも対応. 「>> /usr/src」とか.
  * キー割りあて微修正
    * (一覧モード) 一行スクロール: n,p → j,k
    * (メニュー) スクロール: SPC, BS
    * タイトルを kill-ring へ: C-c , k → C-c , K
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/67>))
    * fix: contents-mode で C-h が未設定だった
  * howm-mode なバッファの save 時に, 下線を引き直し, メニューも更新
    (→((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/98>))
  * 検索対象ディレクトリの追加 (→((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/69-72n>))
  * ドットファイルは検索しない
    (>> や howm-search-path で明示的に指定すれば探す)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/74>))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/86>))
  * メニューを過剰に強化
    (→((<URL:ja/menu_edit.howm>)))
    * HyperCard や Um4 に刺激されました. ほんとは別ツールにすべき.
  * 現バッファのコピーを howm-mode で表示(M-x howm-show-buffer-as-howm)
    ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/71>))
    * 需要不明なので様子見
  * action-lock の "...Done" メッセージを廃止
  * fix
    * 「>> ファイル名」が howm-excluded-file-regexp に関わらず効くように
    * call-process で STDERR は捨てる
    * action-lock の説明に file://… を追加
    * 寸前の window 分割状況に関わらず, 一覧を上, 内容を下に表示
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/90>))
    * sort や filter にファイル名を使う際は, ディレクトリ部分を除去
    * howm-menu-refresh を M-x やキー割りあてから呼べるように
    * テスト版のバグ
      * 一覧表示した直後は内容が表示されない
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/59-64n>))
      * Wiki 風 link [[○○]] がエラー
      * [メニュー更新] がエラー
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/95>))
      * >> で該当ファイルが先頭に出なくなっていた
  * 「参考」を整理
  * 以下も実装済みだけど, v1.1 まで寝かすつもり
    * リマインダの書式変更
    * メニューとメモの統合
* [2003-09-23] 反省してテスト版を分離
  * 二系統メンテする能力はないので…
    * リリース版は放置
    * テスト版は遠慮なく更新
    * テスト版の更新が落ち着いたら,
      その状態にバージョン番号つけて, リリース版ってことにする
* [2003/09/23] v1.0.3.4
  * 日付形式 [2003/09/21] の action-lock を拡張 (→((<メモを書こう>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/49-51n>))
  * fix: howm2 のリンク一覧を相対パスに
  * fix: template まわり
    * 前のメモとの間に空行を入れる
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/53>))
    * スペルミス cursol → cursor
    * 新メモ template の %xxx 置きかえが, 前のメモにも及んでいた
  * fix: 「次(前)のリンクへ」を修正
  * fix: canna 対策を, ロード順序に関わらず有効に
* [2003/09/21] v1.0.2.3
  * fix: 初めて起動したときに ~/.howm-keys を作成
  * fix: canna 対策
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/45>))
* [2003/09/20] v1.0.2.2
  * HTML 化スクリプト howm2 でっちあがり (→((<外部ツール>)))
  * fix: 「!」するとバッファ名が変
* [2003/09/18] v1.0.2
  * HTML 化スクリプト howm2 の試作 (→((<外部ツール>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/35>))
  * 未保存だろうと委細構わず, howm-mode なバッファをすべて強制削除するコマンド
    (→((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/36>))
  * 「やさしい Emacs-Lisp 講座」の URL を修正.
    いま howm があるのはこの本のおかげです.
* [2003/09/17] v1.0.1
  * 一覧モードで「!」 → shell でコマンド実行 (→((<メモを読もう>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/501>))
    * Dired-X でうまく逃げたつもりだったんだけど, 不評なので
    * lisper たる者, 要求仕様の上を行かないと :-p
* [2003/09/17] v1.0.0.1
  * Dired-X について最低限の説明
  * fix: Dired-X の対象バッファを訂正
* [2003/09/17] v1.0
  * メモディレクトリを階層化: ~/howm/年/月/年_月_日.howm
    * こんなふうにディレクトリも指定できるようにした
        (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm")
    * 移行については, ((<インストール>))の「注意」

== 0.9.x

* [2003/09/16] v0.9.9
  * メニュー (→((<メモを書こう>)))
    * 前後のメモの一覧
    * 説明を補足
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/494>))
  * 一覧モードで「X」 → Dired-X を起動してファイル操作
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/496>))
  * howm-mode なバッファをすべて消す
    * fix: window の状態を復元
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/497>))
    * どんなモードでも C-c , Q が効く
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/498>))
* [2003/09/16] v0.9.8.3
  * fix: タイトル一覧でエラー
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/490>))
    * pure elisp 版のとき
    * タイトルが一つもなかったとき
  * menu.howm を一覧に出さない方法 (→((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/491>))
* [2003/09/16] v0.9.8.2
  * メニューをファイルにして編集を推奨 (→((<カスタマイズ>)))
    * 同梱の ja/menu.howm を ~/howm/ にコピーしてください
  * ((<URL:TUTORIAL.ja.rd>))
  * デフォルトの変更 (→((<カスタマイズ>)))
    * grep 使わない (バグ出し期待)
    * メニューのキャッシュはオフ
    * ファイルを開く際, 一覧を残す
  * 楽しい連休でした :-)
* [2003/09/16] v0.9.7.1
  * Wiki 風リンク [[ほげ]] (→((<メモを書こう>)))
  * 「<<」上でリターン叩くと「関連キーワード」へのリンク (→((<メモを書こう>)))
  * 古い更新記録と移行例を移動 (→((<URL:OLD.rd>)))
  * howm-ignore-comefrom-same を undocumented に. そのうち廃止?
  * fix: 曜日表示にも言語設定を反映
* [2003/09/15] v0.9.6
  * 「n」と「p」で一行単位のスクロール
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/485>))
* [2003/09/15] v0.9.5
  * メニューに [全消] (howm-mode なバッファをすべて消す)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/481>))
* [2003/09/14] v0.9.4.2
  * fix: タイトル一覧が裏に隠れてしまってた
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/475>))
  * fix: 「カーソルを対応カラムへ」の微修正(マッチしなければ行頭へ)
* [2003/09/14] v0.9.4.1
  * grep 脱却の試み (→((<インストール>)))
    * メモが大量だとやっぱりちょっと遅い ;_;
  * fix: 「Wrong type argument: window-configuration-p, nil」
  * 一覧モードからファイルを開いたとき, カーソルを対応カラムへ:
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/472>))に向けて
* [2003/09/13] v0.9.3
  * 一覧モードから抜ける際, window の状態を復元
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/450>))
  * C-u RET で一覧を残す (→((<メモを読もう>)))
    * howm-view-summary-persistent (→((<カスタマイズ>)))
  * howm-view-kill-buffer, howm-view-delete-other-windows を廃止
* [2003/09/12] v0.9.2.1
  * fix: come-from リンクを最長一致に
  * fix: ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/447>))
    * howm-view-kill-buffer の説明逆だった
    * タイトル一覧表示の際, タイトルなし一覧が裏に残ってた
  * howm-ignore-comefrom-same (→((<カスタマイズ>)))
  * なんか不評なので, マニュアルを HTML 化
* [2003/09/10] v0.9.1
  * デフォルトを英語に
    * .emacs に以下を書けばメニューが日本語に
        (setq howm-menu-lang 'ja)
    * hcal.rb もデフォルト記号は ASCII 文字に
* [2003/09/09] v0.9
  * ruby, find 脱却. elisp がこんなに速かったとは…
  * howm-view-kill-buffer の説明 (→((<カスタマイズ>)))
  * ext/easy-mmode.el の同梱を廃止 (意味なさげ)

== 0.8.x

* [2003/08/31] v0.8.5
  * タイトルの一覧表示 (→((<カスタマイズ>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/398>))
  * 一覧モードやメニューモードで「?」を押すとヘルプ
  * 「参考」に vim 版を追加
* [2003/06/03] v0.8.4
  * 安直カレンダー hcal.rb (htodo.rb を改名・拡張)
  * 予定表に済項目も表示 (→((<カスタマイズ>)))
  * 「参考」に wikilog を追加
* [2003/05/11] v0.8.3
  * fix: howm-excluded-file-regexp が menu, schedule, todo で効いてなかった
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/126>))
  * README
    * maxima についての注意
    * M-x howm-create → M-x howm-menu
* [2003/03/19] v0.8.2
  * 外部ツールに htodo.rb 追加
  * fix: todo 一覧で, 済項目(@[2003/03/19]. など)も日付順に並ぶよう
* [2003/02/20] v0.8.1.1
  * fix: 「@[2003/02/14] 501室」の laziness が 501 になってたのを修正
  * fix: バックアップファイル「.#foobar.baz」を検索対象外に
* [2002/12/14] v0.8.1
  * howm-congrats-hook
  * ドットファイルも検索対象に (howm-excluded-file-regexp から削除)
    * 「>> ファイル名」関連をまじめに修正するまでの暫定処置
* [2002/11/03] v0.8
  * メニュー
  * リマインダ
    * 従来の「@[2002/10/21] !!」などは廃止
  * ローマ字検索 (migemo)
    * 付随して, grep コマンドを egrep に変更
  * その他いろいろ
    * action-lock の追加例 (namazu を使い, Message-ID からメールを検索)
    * action-lock の「未処理」を {___} から {_} に変更
    * [2002/10/18] のような日付形式もリンクに
    * howm-view-summary の一行表示で, 先頭空白は削除
    * howm-view-split-horizontally
    * howm-insert-date
    * 変数 howm-view-summary-format の仕様変更
      (縦分割なら todo 一覧にファイル名表示なし, のやっつけ実装のため)
    * 一覧でのタイトル表示を一旦廃止 (複雑な割に役たたず)
    * 一部キーバインド変更
  * fix: 「今日のメモ」上で C-c , c したときは「前に見てたファイル」は略

== 0.7.x

* [2002/09/30] v0.7.6.1
  * howm-view-kill-buffer に関係なく, タグ一覧バッファだけは残す(やっつけ)
  * fix: case-fold-search を設定しているときの, grep / grep -i の判定
* [2002/09/26] v0.7.5
  * 検索で大文字小文字を区別しないよう変更
* [2002/09/24] v0.7.4
  * fix: 検索で no match になったとき, 前バッファの色などが変に
    (font-lock-keywords-only の大域値を誤って変更)
* [2002/09/20] v0.7.3
  * action-lock-no-browser
  * fix: @ で一覧表示と連結表示とを切りかえたときカーソル位置がずれぬよう
  * fix: マークを設定せず howm-create してもエラー出ぬよう
* [2002/09/19] v0.7.2
  * 一覧で summry 未スキャンの部分には「type 'p'」と表示
  * fix: .howm-keys への重複登録チェックでは大文字小文字を区別するように
  * fix: ディレクトリ名は一覧対象外に (例: CVS/)
* [2002/09/18] v0.7.1
  * http:// で web browser 起動
  * fix: 「キーワードを kill-ring へ」→「タイトルを kill-ring へ」(迷い中)
* [2002/09/17] v0.7
  * 1 日 1 ファイルに
  * タイトル = と come-from リンク << とを分離
  * 最近のメモの一覧 (キーバインドも変更)
  * ファイル名を kill-ring へ (C-u C-c , k)
  * 「>> ファイル名」 (file://… と ((<URL:…>)) も残してはある)
  * 用語変更: 明示・暗示 → goto・come-from
  * 全メモ一覧にもヘッダ
  * fix: 古い ruby-mode.el を使うと全メモ一覧に変な色がつくのをやっつけ修正

== 0.6.x

* [2002/09/14] v0.6 全面書きなおし
  * 「リンク」の廃止. すべては検索である.
  * 検索結果のソート・絞りこみ・連結表示
  * キーワード一覧ファイル(~/.howm-keys)をしぶしぶ導入
    * 暗示リンクの書式を可変にするため
      * emacs, grep, ruby の正規表現の違いに悩みたくないから,
        暗示リンク宣言のスキャン(正規表現検索!)は emacs 内で完結させたい
      * 全ファイルの暗示リンク宣言を emacs でスキャンするのは
        さすがに遅そう
    * 逆手にとって, 何か検索するたんびに
      その検索語を登録してしまおうかとも
  * shell を使わない (…にしたつもり)
  * パラグラフ指向は廃止
    * かわりに, 各パラグラフを 1 ファイルにして, 連結表示
  * migemo 検索が未実装
  * ファイル名は有無を言わさず勝手につける

== 0.5.x 以前

* [2002/06/26] v0.5.3 (リリースせず[2002/09/12])
  * パラグラフ一覧の日時をハイライト
* [2002/06/25] v0.5.2
  * migemo 検索でもマッチをハイライト
  * describe-mode にパラグラフ関連のキー設定も記述
  * rd-memo との併用設定例
* [2002/06/24] v0.5.1
  * カーソルがパラグラフの先頭にあったときの挙動を修正
  * パラグラフを新規タイトルに移動したとき howm-create-title が効くよう
* [2002/06/23] v0.5
  * パラグラフ指向と時系列メモ (試用中)
  * howm-default-directory
  * howm-menu を廃止し, 普通に keymap を使用
  * 現題名を kill ring へ
  * popview-mode からファイルを開くと, 見ていた箇所にカーソル移動
  * grep では題名でなくマッチ行を表示
  * その他, 微修正
    * migemo-client --type=egrep を明示
    * popview-mode の post-command-hook は buffer-local に
    * popview-mode の update 要不要チェックは, キーワードじゃなく位置で
    * popview-mode は 'popview でなく 'popview-mode を provide
    * howm-create-file に補完
* [2002/06/06] v0.4.2
  * migemo
  * grep → egrep
  * 題名の補完入力
  * tag2plan で「@[2002/06/06] foo」が登録されなかったのを修正
* [2002/06/05] v0.4.1
  * grep に -i
  * 空白行は題名とみなさない
  * howm コマンドも題名書式のデフォルトを「一行目」に
* [2002/06/03] v0.4 題名書式を変更 (「= 題名」→ 一行目)
* [2002/06/03] v0.3.2 デバッグ(1ファイル複数タグ)
* [2002/06/02] v0.3.1
  * 各一覧のバッファを別に
  * tag 一覧は今日の日付にカーソル移動
  * 新規作成時, 題名が空なら題名書式自体("= ")を省略
  * howm -pipe
  * w3mmenu
* [2002/06/02] v0.3
  * コマンド追加 (次/前のリンクへ, tag, 絞りこみ検索, 新規, 複製)
  * tag2plan
  * popview-mode に最低限ドキュメント
  * その他, 微修正
* [2002/05/30] v0.2 コマンド追加 (refresh, 参照先/元一覧, 全題名一覧, 検索)
* [2002/05/29] v0.1 公開

= 古い告知

* 夏時間のバグ 2007-11-09
  * howm-1.3.5 の更新記録を参照

* セキュリティ修正 2006-12-16
  * howm-1.3.4 の更新記録を参照

* テスト版にバグ:
  [2003-12-27]〜[2004-01-21]のテスト版で
  (setq howm-insert-date-future t)
  を設定していた場合,
  「未来の日付を入力したのに, それをもう一段未来へ送ってしまう」
  というバグがありました. すみません.
  * 予定・todo の一覧で, 年月にまちがいがないか確かめてください.
    (特に, 「2004 年のつもりが 2005 年に」という症状をご注意ください)
  * リリース版か、[2004-01-22]以降のテスト版に update してください.

= 旧版からの移行
(必ずバックアップをとってから!)

== v1.0.x からの移行例

* .emacs の修正 → ((<インストール>))
  * 「<<< で大文字小文字を区別しない」を設定した場合は,
    キーワード一覧を作り直して重複を消すとよいでしょう
* 新体制に移行する場合
  * リマインダの書式変更
      @[2003/09/25]! → [2003/09/25]!
      @[2003/09/25]  → [2003/09/25]-
      [2003/09/25]!  → [2003/09/25]:!
      [2003/09/25]   → [2003/09/25]
    * メモディレクトリに cd して,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.gsub!(%r~(@?)(\[[0-9]+/[0-9]+/[0-9]+\])([-+@!.]?)~){|s| if ($1 == %~~ && $3 == %~~); s; else; $2 + ($1 == %~@~ ? %~~ : %~:~) + ($3 == %~~ ? %~-~ : $3); end}'
    * 確認後, *.bak を捨てる
  * 日付の書式変更
      [2003/10/21] → [2003-10-21]
    * メモディレクトリに cd して,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.gsub!(%r!(\D)(\d{4}/\d{2}/\d{2})(\D)!){|s| $1 + ($2.tr "/", "-") + $3}'
    * 確認後, *.bak を捨てる
  * リンクの書式変更 (<<, >> を <<<, >>> に)
    * メモディレクトリに cd して,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.sub!(/(<<|>>).*/){|s| $1[0,1] + s}'
    * 確認後, *.bak を捨てる
  * やりたければ, メモを改名してもよい
      2003_10_18.howm → 2003-10-18-000000.howm
    * メモディレクトリに cd して,
        find . -name '*.howm' -print | ruby -ne '$_.chop!; d = File::dirname $_; f = File::basename($_).tr("_", "-").sub(/[.][^.]+$/){|s| "-000000" + s}; puts %~mv #$_ #{File::expand_path f, d}~' > ~/howm_kuzu
    * ~/howm_kuzu の内容を確認し, 問題なければ
        cat ~/howm_kuzu | /bin/sh
  * 更新順と名前順が一致するよう, タイムスタンプをでっちあげ
    * メモディレクトリに cd して,
      * GNU touch の場合
          find . -name '*.howm' -print | sort -r | ruby -ne 'puts %~touch -d "#{ARGF.lineno} min ago" #$_~' > ~/howm_kuzu
      * それ以外の場合
          find . -name '*.howm' -print | sort | ruby -ne '$_.chop!; puts %~sleep 1; touch #$_~' > ~/howm_kuzu
    * ~/howm_kuzu の内容を確認し, 問題なければ
        cat ~/howm_kuzu | /bin/sh
  * メニューファイルのさしかえ (ja/* のコピー) → ((<インストール>))
    * 旧メニューファイルは捨てる
* 旧体制を貫く場合: .emacs (howm ロードより前)に
    (setq howm-ref-header ">>") ; goto リンク
    (setq howm-keyword-header "<<") ; come-from リンク
    (setq howm-reminder-old-format t) ; リマインダの書式変更をしない
    (setq howm-date-separator "/") ; 日付は 2003/10/21 の形式
    (setq howm-menu-keyword-regexp nil) ; 「%○○%」を menu-mode にしない
    (setq howm-menu-top nil) ; C-c , , で「<< %menu%」を開かない
    (setq howm-menu-file "menu.howm") ; C-c , , で menu.howm を開く
    (setq howm-use-color nil) ; << などに色をつけない
    (setq howm-menu-name-format " *howmM:%s*") ; メニューは隠し buffer
    (setq howm-abbreviate-file-name nil) ; ファイル名表記は full path
    ;; 旧デフォルト設定
    (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm") ; 1 日 1 ファイル
    (add-to-list 'auto-mode-alist '("\\.howm$" . fundamental-mode))
    (setq howm-default-key-table
      '(
        ;; ("key"  func list-mode-p global-p)
        ("r" howm-initialize-buffer)         
        ("l" howm-list-recent t t)
        ("a" howm-list-all t t)
        ("g" howm-list-grep t t)
        ("m" howm-list-migemo t t)
        ("t" howm-list-todo t t)
        ("y" howm-list-schedule t t)
        ("c" howm-create t t)
        ("," howm-menu t t)
        ("d" howm-dup)                       
        ("i" howm-insert-keyword)              
        ("D" howm-insert-date)                       
        ("K" howm-keyword-to-kill-ring t t)        
        ("n" action-lock-goto-next-link)            
        ("p" action-lock-goto-previous-link)        
        ("Q" howm-kill-all t t)
        ))
    (when (featurep 'howm-mode) (howm-set-keymap))
  * 同梱ツールの使用時は, 次のオプションをつける
    * howm2 -comefrom='<<' -goto='>>'
    * hcal.rb -format=old -date_sep=/
    * tag2plan -format=old -date_sep=/

== v0.8.x, v0.9.x からの移行例

* メモディレクトリの階層化に応じて…
  * 新体制に移行
    * 何もしなくてよい.
    * やりたければ, ~/howm/2003/09/ などを作って旧メモを移動してもよい
  * 旧体制を貫く: .emacs に
      (setq howm-file-name-format "%Y_%m_%d.howm")

== v0.7.x からの移行例

* リマインダの書式変更に伴い, 次のスクリプトでメモを変換
  * 変換スクリプト (~/kuzu.rb とする)
      $d = '@\\[[0-9 :/]+\\]'
      $_.gsub!(/(#$d) *!!!/){|s| "#$1! ★★"}
      $_.gsub!(/(#$d) *!!/){|s| "#$1! ★"}
      $_.gsub!(/(#$d) *!/){|s| "#$1!"}
      $_.gsub!(/(#$d) *@@@/){|s| "#$1@ ★★"}
      $_.gsub!(/(#$d) *@@/){|s| "#$1@ ★"}
      $_.gsub!(/(#$d) *@/){|s| "#$1@"}
      $_.gsub!(/(#$d) *~~~/){|s| "#$1+ ★★"}
      $_.gsub!(/(#$d) *~~/){|s| "#$1+ ★"}
      $_.gsub!(/(#$d) *~/){|s| "#$1+"}
      END {
        STDERR.print '.'
        sleep 1
      }
  * 変換手順
      cd ~/howm
      ls *.howm | xargs -n 1 ruby -p -i.bak ~/kuzu.rb
      (確認後, *.bak を捨てる)

== v0.6 以前からの移行例

* come-from の書式変更に伴い, 以下のようにメモを変換
    cd ~/howm
    ruby -p -i.bak -e '$_.sub! /^= /, "= << "; $_.sub! /^= << link$/, "= link"' *.howm
    (確認後, *.bak を捨てる)
  * rd-memo との互換性のために, 「= link」は変換しない
* さらに, ファイル名も変更
  * 新方式に移す場合
    * 次のようにして, 「cat ◯◯ ◯◯ > ◯◯」というコマンド列を作成
        cd ~/howm
        ruby -e 'h=Hash::new []; ARGV.each{|f| f=~/^([0-9]+)-[0-9]+.*/ and h[$1] = h[$1] + [f]}; h.keys.sort.each{|k| puts "cat #{h[k].sort.join %! !} > #{k =~ /(....)(..)(..)/; %!#$1_#$2_#$3.howm!} && sleep 1"}' *.howm > howm_kuzu
      * sleep 1 は, ファイル更新時刻順でちゃんと並ぶように
    * howm_kuzu の内容を確認し, 問題なければ
        cat howm_kuzu | /bin/sh
        (確認後, 200*-*.howm を捨てる)
  * 旧方式を貫く場合: ~/.emacs に
      (setq howm-file-name-format "%Y%m%d-%H%M%S.howm") ;; 1 メモ 1 ファイル
      (setq howm-file-name-format "%Y%m%d-000000.howm") ;; 1 日 1 ファイル

= 古いカスタマイズ法

== v1.2.3 のカスタマイズ [2005-05-08]

~/.emacs (~/.emacs.el かも)に, 以下のように書く

* 色
  * <<< などに色をつけない
      (setq howm-use-color nil)
  * 色かえ: howm のロードより後に
      (set-face-foreground 'howm-mode-keyword-face "red") ;; <<<
      (set-face-foreground 'howm-mode-ref-face "magenta") ;; >>>
      (set-face-background 'howm-mode-wiki-face "green") ;; [[]] の背景
      (set-face-background 'howm-mode-title-face "yellow") ;; = の背景
      (set-face-foreground 'action-lock-face "blue") ;; 下線文字
      (set-face-underline 'action-lock-face t) ;; 下線は文字と同色 (Emacs 21)
      (set-face-underline 'action-lock-face "dark cyan") ;; 下線 (Emacs 21)
      (set-face-foreground 'howm-menu-key-face "orange") ;; メニューの shortcut
    * 色名は M-x list-colors-display 参照
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
  * メモを ~/memo/ に置く
      (setq howm-directory "~/memo/")
  * メモ置き場/年/年月日-時分秒.howm に
      (setq howm-file-name-format "%Y/%Y%m%d-%H%M%S.howm")
    * ファイル名自体に年月日が入っていないと, filter-by-date が機能しない
  * 1 日 1 ファイル (メモ置き場/年/月/年_月_日.howm に)
      (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm")
    * 不完全な点があります. 我慢できる人だけどうぞ
      * メモ単位であるべき処理の一部がファイル単位に
        (タイトル表示, 更新順一覧, 内容での絞りこみ, uniq)
    * メニューに以下のボタンを書いておくと便利
      * [今日] → 今日のメモ
      * [昨日] → 昨日のメモ
  * キーワード一覧を ~/howm/.howm-keys に置く
      (setq howm-keyword-file "~/howm/.howm-keys") ;; デフォルトは ~/.howm-keys
    * こうしておけば, 違うマシンでも ~/howm/ 以下のコピーだけで済む.
    * すでに書いたメモがあるなら, mv ~/.howm-keys ~/howm/ をしておくか,
      再構築する(→((<インストール>))).
    * デメリット: 検索が遅くなる? (体感できるほどかは, やってみないと不明)

* 一覧
  * タイトル一覧を表示
      (setq howm-list-title t)
    * タイトルを書かないときでも, タイトル欄「= 」は残しておく方が安全でしょう
      (対策はしたつもりですが…)
    * ○○のときだけタイトル表示
        (setq howm-list-title
              ;; ↓に書いたコマンドでだけタイトル表示
              '(howm-list-all howm-list-recent
                howm-list-grep howm-list-grep-fixed howm-list-migemo
                howm-list-related howm-list-around
                howm-keyword-search)) ;; これは come-from リンク・goto リンク
    * 関数を指定することも可能
        (setq howm-list-title (lambda () …))
  * 一覧バッファと内容バッファを横に並べる
      (setq howm-view-split-horizontally t)
    * 横に並べるけど, 一旦 C-x 1 した後は縦に並ぶ
        (setq howm-view-split-horizontally 'hoge) ;; t, nil 以外を指定
      * 誰も使っていないのでこの機能は削除 [2008-10-07]
  * [return] でファイルを開く際, 一覧バッファを消す.
    C-u して [return] だと, 一覧を残す.
      (setq howm-view-summary-persistent nil)
  * 一覧で「!」したときの初期コマンドを変更
      (setq howm-view-summary-shell-last-file "_FILE_")
      (setq howm-view-summary-shell-hist
        '("mv _FILE_ ~/gomi" "touch _FILE_" "ls -l _FILE_"))
    * 初期コマンドは「mv ファイル名 ~/gomi」
    * M-p 押していくと, 「touch ファイル名」や「ls -l ファイル名」
  * 一覧バッファ・内容バッファを検索ごとに個別に作る
      (setq howm-view-summary-name "*howmS:%s*")
      (setq howm-view-contents-name "*howmC:%s*")
  * 一覧をデフォルトで日付順に
      (setq howm-list-normalizer 'howm-view-sort-by-reverse-date)
  * C-x 1 後は勝手にフレームを分割しない (SPC で再分割)
      (setq howm-view-keep-one-window t)
  * 一覧バッファの色つけ例
      (setq howm-view-summary-font-lock-keywords '(("^2003" . 'highlight)))
  * foo を検索しても [[foo]] を上位にしない
      (setq howm-list-prefer-wiki nil)

* メニュー
  * メニューの変更
    * メニューを開いて [menu 編集] 上でリターン → 自由に編集
    * よく開くメモへの goto リンクなどを書いておけば便利かと
  * メニューの末尾に「R[menu 更新] E[menu 編集]」をつける
      (setq howm-menu-footer "\n-- \n%\"R\"[menu 更新] %\"E\"[menu 編集]")
  * メニューは隠しバッファに
      (setq howm-menu-name-format " *howmM:%s*")
    * ※ Emacs 豆知識
      * 空白で始まるバッファ名は, C-x b や C-x C-b で出てこない
      * そんなバッファを見るには, C-x b C-q SPC SPC
  * メニューをメモ扱いしない (メモ一覧・検索の対象外に)
      ;; mv ~/howm/0000-00-00-000000.howm ~/hoge/fuga/menu.howm しといて…
      (setq howm-menu-file "~/hoge/fuga/menu.howm")
  * 予定表の表示範囲
      (setq howm-menu-schedule-days-before 2)  ;; ○日前から
      (setq howm-menu-schedule-days 7)  ;; ○日後まで
  * todo の表示件数
      (setq howm-menu-todo-num 50)

* もっと軽く (cf. ((<富豪的プログラミング|URL:http://pitecan.com/fugo.html>)))
  * メニューファイルを直接指定
      (setq howm-menu-file "0000-00-00-000000.howm")
  * メニューを 2 時間キャッシュ
      (setq howm-menu-expiry-hours 2)
    * かわりに, メモ保存時にメニューを自動更新するようになる(かなり重い).
      それも止めるなら
        (setq howm-menu-refresh-after-save nil)
  * セーブ後の下線引き直しをしない
      (setq howm-refresh-after-save nil)
  * 一覧でのタイトル表示を抑制
      ;; 「最近の」または「前後の」メモ一覧時に限る
      (setq howm-list-title '(howm-list-recent howm-list-around))
      ;; 一切表示せず
      ;(setq howm-list-title 'nil)
  * 検索に grep を使う
      (setq howm-view-use-grep t)
    * この場合, ローマ字検索を使うには migemo-server を動かしておく必要あり
    * GNU grep じゃないとだめかも. -H とか -r とか -e とか…
    * grep コマンド名を変更するなら…
        (setq howm-view-grep-command "egrep")
        (setq howm-view-fgrep-command "fgrep")
    * egrep, fgrep がないけど GNU grep はある, という環境(リナザウ?)なら…
        (setq howm-view-grep-command "grep")
        (setq howm-view-fgrep-command "grep")
        (setq howm-view-grep-extended-option "-E")
        (setq howm-view-grep-fixed-option "-F")

* 外部 viewer
  * ファイル名(正規表現)と viewer の対応を指定 (~/.mailcap より優先します)
      (setq howm-view-external-viewer-assoc
            '(
              ("[.]\\(jpg\\|gif\\|png\\)$" . "display %s")
              ("[.]dvi$" . "xdvi %s")
             ))
  * 「外部 viewer を使わない mime-type」の指定 (正規表現)
      (setq howm-view-open-by-myself '("text/.*" "application/emacs-lisp"))
  * ~/.mime-types や ~/.mailcap を参照しない
      (setq howm-view-use-mailcap nil)
    * この設定をしてなくても, ライブラリの load に失敗した場合は参照しません
      * emacs-21.2.1 付属の gnus/mailcap に依存
      * 古い FLIM は conflict?

* 検索
  * ドットファイルも探す
      (setq howm-excluded-file-regexp
            "^[.][.]\\|[~#]$\\|\\.bak$\\|/CVS/")
  * 対象ディレクトリの追加
    * 全文検索のとき, メモに加えて指定ディレクトリ以下も再帰的に探す
        (setq howm-search-path '("~/Mail" "~/News"))
        (setq howm-search-other-dir t) ;; 下記のトグルの初期値 (t か nil)
    * M-x howm-toggle-search-other-dir で,
      上記ディレクトリを検索対象にするかしないかトグル
      * キーバインドしたければ各自で (インターフェース模索中につき…)
  * 検索に使う関数をすりかえ
      ;; t なら grep コマンド, nil ならデフォルトの elisp 関数を使用.
      ;; これ以外なら, 指定した関数を使用.
      (setq howm-view-use-grep #'my-grep-function)
      (defun my-grep-function (str file-list
                                   &optional fixed-p force-case-fold)
        "STR を FILE-LIST (ファイル名のリスト)から検索.
      戻り値は, ((ファイル名 行番号 行内容) (ファイル名 行番号 行内容) …)
      というリスト.
      FIXED-P が真なら文字列 STR を, 偽なら正規表現 STR を検索する.
      STR が大文字を含まないときや, 含んでも FORCE-CASE-FOLD が真のときは,
      大文字小文字の違いを無視する."
        …)
  * howm で foo を検索した後は, C-s C-s も foo の検索に
      (setq howm-view-update-search-ring t)
    * 正規表現は C-u C-s C-s なので注意

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
      「universal-argument を引数にしてそいつを呼ぶ」っていうのも仕込みました

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
  * wiki 風リンク [[hoge]] の下線を「]]」だけに
    * 「<<< hoge」の作成後は, 「hoge」にも下線
        (setq howm-wiki-regexp "\\[\\[\\([^]\r\n]+\\)\\(\\]\\]\\)")
        (setq howm-wiki-regexp-hilit-pos 2)
        (setq howm-wiki-regexp-pos 1)

* こまごま
  * <<< で大文字小文字を区別しない
      (setq howm-keyword-case-fold-search t)
  * 日付入力(C-c , d または [日↓])で年や月を略したら, 「未来」と解釈
      (setq howm-insert-date-future t)
    * 新規入力時のみです. 「[2003-12-27]」上で RET したときの動作は従来どおり.
  * 「http://」でリターン押したら, URL を kill-ring へ
      (setq action-lock-no-browser t)

* 予定表・todo 一覧
  * 済項目を表示しない
      (setq howm-schedule-types "[!@]")  ;; 予定表
      (setq howm-todo-types "[-+~!]")  ;; todo 一覧
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

* action-lock
  * { } (トグルスイッチ)の変更
      ;; howm の load 前に
      (setq action-lock-switch-default '("{ }" "{*}" "{-}")) ;; 何個でも
  * {_} (未処理)の変更
      (setq howm-dtime-format "[%a %b %d %H:%M:%S %Y]") ;; {_}
      (setq howm-template-date-format "[%Y-%m-%d %H:%M]") ;; テンプレート
  * 「file://…」や「http://…」の変更 (ましな設定募集)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/945>))
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

* メニューを更新するたびに, カレンダーへの export も更新 (→((<外部ツール>)))
    (defun my-howm-menu-hook ()
      (shell-command "tag2plan ~/howm/*/*/*.howm > ~/.dayplan_tag &")
      (switch-to-buffer howm-menu-name))
    (add-hook 'howm-menu-hook 'my-howm-menu-hook)

* ((<RD|URL:http://www2.pos.to/~tosh/ruby/rdtool/ja/>))を使う場合:
  ((<"行頭の * でエントリの開閉ができるように"|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/237-238n>))

* おまけ
    (setq howm-congrats-format
          '(
            "%sキタ━━━━━(゜∀゜)━━━━━!!!!"
            "(・∀・) %s!"
            "（°Д°)%s？"
            "（　´_ゝ`）＜　%s"
            ;; …以下略…
            ))

* もっといろいろいじるには, *.el 冒頭を参照

= 古い参考リンク

* ((<Wiki|URL:http://c2.com/cgi/wiki>)):
  web で誰でも編集＋お手軽リンク＋お手軽フォーマット
  * ((<WikiModeDiscussion|URL:http://www.emacswiki.org/cgi-bin/wiki.pl/WikiModeDiscussion>))
    (EmacsWiki): Emacs での Wiki
  * ((<RWiki-mode|URL:http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=rwiki-mode>))
    (RWiki): RWiki を Emacs から使う案
  * ((<QP-Wiki|URL:http://pitecan.com/UnixMagazine/>))
    (増井俊之さん): PDA で Wiki
  * ((<HashedWiki|URL:http://www.google.com/search?q=hashedwiki>))
    (SHIMADA Keiki さん): パラグラフ指向 Wiki
  * ((<ishinao さんの各種ツール|URL:http://ishinao.net/>)):
    Wiki にとらわれないアイデア満載
  * ((<「日本発の wiki クローンリスト」|URL:http://www.yamdas.org/column/technique/clonelist.html>))
    ((<「2」|URL:http://www.yamdas.org/column/technique/clonelist2.html>))
    (yomoyomo さん)
* HyperCard: card 型 database 的 visual script 言語環境???
  * ((<「HyperCard」|URL:http://www.hyuki.com/yukiwiki/wiki.cgi?HyperCard>))
    (YukiWiki)
  * ((<「HyperCardのリアルタイム性」|URL:http://web.archive.org/web/20040111061953/http://mwave.sppd.ne.jp/wiki/pukiwiki.php?%5b%5bHyperCard%A4%CE%A5%EA%A5%A2%A5%EB%A5%BF%A5%A4%A5%E0%C0%AD%5d%5d>))
    (SsPukiWiki)
  * ((<「ハイパーカードでつくるオフィスシステム」|URL:http://www.kanzaki.com/hc/MacUser.html>))
    (神崎正英さん)
* メモとり環境
  * 分類せず, 時間順と全文検索で管理
    * ((<Q-Pocket|URL:http://pitecan.com/UnixMagazine/>))
      (増井俊之さん):
      PDA 版も
    * ChangeLog メモ
      * ((<「Unixのメモ技術」|URL:http://namazu.org/~satoru/unimag/1/>))
        (高林哲さん)
      * ((<「私の ChangeLog メモ活用法」|URL:http://nais.to/~yto/doc/zb/0016.html>))
        (山下達雄さん)
      * ((<「ChangeLog メモを試してみよう」|URL:http://pop-club.hp.infoseek.co.jp/emacs/changelog.html>))
        (安宅正之さん)
  * ((<簑系・超簑|URL:http://www.google.com/search?q=%E2%C0%8Cn+%92%B4%96%AA>))
    (syo さん): ChangeLog + 目次・並べかえ・hyper link って感じ?
  * スクラップブック
    * ((<紙 2001|URL:http://www.vector.co.jp/soft/win95/writing/se120325.html>))
      (洛西一周さん): 定番
    * ((<WeBoX|URL:http://webox.sakura.ne.jp/software/webox/>))
      (中村聡史さん): すごくいいらしい
  * その他の Emacs 用ツール
    * ((<notes-mode|URL:http://www.isi.edu/~johnh/SOFTWARE/NOTES_MODE/>))
      (John Heidemann さん):
      link の便利さを知りました
      * ((<notes-mode と memo-mode の比較論|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/others/compare-notes-and-memo-mode.html>))
        (西本孝志さん)
    * ((<memoma|URL:http://web.archive.org/web/20040803170229/http://www.jaist.ac.jp/~tetsu/memoma/memoma.html>))
      (原田哲治さん): MH 形式 → メールリーダでも読める
    * ((<Um4|URL:http://www.d4.dion.ne.jp/~usuda/emacs/index.html>))
      (臼田拓史さん): いろいろ保存メニュー
    * rd-memo
      (拙作. 開発終了 → ((<tar.gz|URL:http://howm.sourceforge.jp/a/rd-memo.tar.gz>)))
      * ((<「コンピュータ環境でのメモ」|URL:http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=%A5%B3%A5%F3%A5%D4%A5%E5%A1%BC%A5%BF%B4%C4%B6%AD%A4%C7%A4%CE%A5%E1%A5%E2>))
        (Toshさん): Wiki に注目したきっかけ
  * howm 関連
    * 移植
      * ((<howm-mode.vim|URL:http://sworddancer.funkyboy.jp/howm_vim/>))
        (七島功一さん)
        ((<＋α|URL:http://www.google.com/search?q=vim+howm+%82%AD%82%D3%82%F1>)): vim 版
      * ((<howm-wrap|URL:http://homepage3.nifty.com/~ko-ji/>))
        (kimura さん)と
        ((<howm-helper|URL:http://www.geocities.co.jp/Milano-Cat/2067/howm-init.html>))
        (deecay さん): xyzzy 版
      * ((<howm.mac|URL:http://mrm.seesaa.net/category/789739.html>))
        (Mr.M さん)
        ((<＋α|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HideMaru>)): 秀丸版
    * 浮沈式 todo リスト
      * ((<wikilog|URL:http://web.archive.org/web/20040308005605/http://koten.hypermart.net/wikilog_rc01.l>))
        (Gonza さん): xyzzy エディタ用の, Wiki + ChangeLog メモ
        → ((<経緯|URL:http://pc2.2ch.net/test/read.cgi/win/1053880433/n29-36>))
      * ((<howm式TODO管理WEBアプリ|URL:http://web.archive.org/web/20060128122538/http://www.lyricfathom.com/pukiwiki/pukiwiki.php?howm%BC%B0TODO%B4%C9%CD%FDWEB%A5%A2%A5%D7%A5%EA>))
        (鮎川さん): PHP での実装
      * ((<wema|URL:http://wema.sourceforge.jp/>))
        (ふしはらかんさん): 付箋ベースの Wiki 的なもの.
        付箋自体が上下に移動. 脱帽.
      * ((<LesserWiki|URL:http://lesserwiki.org/>))
        (yatsuさん): Ajax な Wiki
      * ((<Whem|URL:http://www.n314.com/whem/?action=ExecLogin&mail=guest>))
        (Nishimuraさん): Web用マルチユーザメモツール. goto/come-from リンクあり.
* お気にいり
  * ((<memo-mode|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/>))
    (OSHIRO Naoki さん): 箇条書き支援. べたぼれ.
  * ((<get-date|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/>))
    (OSHIRO Naoki さん): 今日の日付を反射的に入力. べたぼれ.
  * ((<migemo|URL:http://migemo.namazu.org/>))
    (高林哲さん): ローマ字を入れるだけで日本語も検索. 愛用.
  * ((<rdtool|URL:http://www.google.com/search?q=rdtool>))
    (Toshさん): この README で使ってるドキュメントフォーマット. 愛用.
  * ((<elscreen|URL:http://www.morishima.net/~naoto/j/software/elscreen/>))
    (Naoto Morishimaさん): GNU screen の Emacs 版. 愛用.

=end
