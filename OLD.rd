=begin

* �ܼ�
  * ((<�Ť���������>))
  * ((<�Ť�����>))
  * ((<���Ǥ���ΰܹ�>))
  * ((<�Ť��������ޥ���ˡ>))
  * ((<GNU Emacs �ʳ��ξ��>))
  * ((<�Ť����ͥ��>))

= �Ť���������

== 1.4.x

* ��꡼���� howm-1.4.6 [2019-12-31]
  * fix: C-c , : (howm-find-yesterday) ������Ƥ���
    ((<thx|URL:https://twitter.com/U5948U5e7eU4e43/status/1162732269347336193>))
    ((<ref|URL:http://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=b06917a4912a60402025286d07d4a195749245c4>))

* ��꡼���� howm-1.4.5 [2018-12-31]
  * migemo-client �Τ����� cmigemo ���б�
      (setq howm-migemo-client '((type . cmigemo) (command . "cmigemo")))
      (setq howm-migemo-client-option
            '("-q" "-d" "/usr/share/cmigemo/utf-8/migemo-dict"))
  * �ե�󥹸��˥塼���ɲ�
    ((<thx|URL:https://osdn.net/projects/howm/lists/archive/eng/2018/000136.html>))
    ((<thx|URL:http://piratepad.net/ZNywUOscRL>))
    ((<thx|URL:https://osdn.net/projects/howm/lists/archive/eng/2018/000139.html>))
    > Yassine san (yacinechaouche at yahoo.com),
    Xavier Maillard san (xavier at maillard.im)
  * autotools �ޤ����ݽ�
  * howm-1.4.5-snapshot3 ��Ʊ�����ƤǤ�

* ��꡼���� howm-1.4.4 [2016-12-31]
  * �Х��ȥ���ѥ�����ηٹ�����
    * cl ����� cl-lib ��Ȥ�
    * howm-ime-fix (howm-1.1.1 ������ canna, egg, yc, anthy �����к�) ���ѻ�
    * ����¾���ޤ���
  * MELPA �ؤΥ�󥯤��ɲ�.
    thx > Yuki Inoue san (inouetakahiroki at gmail.com)
  * ���ʥåץ���å��� 2016-09-28 ��Ʊ�����ƤǤ�

* ��꡼���� howm-1.4.3 [2015-12-31]
  * emacs 25.1.50.1 (2015-12-27 ������ git ��ü = de88375) �ǵ�ư���ǧ
  * howm-1.4.3rc1 ��Ʊ�����ƤǤ�
  * fix
    * git ��ü emacs �ǤΥ��顼 (void-function ed)
    * C-c , A (howm-list-around) �� howm-list-title �����꤬�����Ƥ��ʤ��ä�
      ((<thx|URL:http://peace.2ch.net/test/read.cgi/unix/1397477663/39>))
    * ((<howm-recentf|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Recentf>))�ʤɤ�̵�̤˥�⡼�ȥե�����Υѥ���ɤ�ʹ���Ƥ��ʤ��褦��.
      thx > �񹾱�δ���� (hide at koie.org)
    * �ɥ�����ȤΥե�����⡼�ɤ�����

* ��꡼���� howm-1.4.2 [2013-12-31]
  * Note
    * emacs 24.3 ���б�. 2013-12-25 ������ trunk (24.3.50.1) �Ǥⵯư���ǧ.
    * howm-test130321 �� howm-1.4.2rc1 ��Ʊ�����ƤǤ�
  * ����
    * C-c , a (howm-list-all) ���®��
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
  * fix
    * emacs 24.3.1 �ǥХ��ȥ���ѥ��뤻���˼¹Ԥ����Ȥ��Υ��顼
      "Can't detect type of ..."
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * ����ܥ�å���󥯲��ǿ�����⤬ howm-mode �ˤʤ�ʤ��Х�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * �Х��ȥ���ѥ�����ηٹ�

* ��꡼���� howm-1.4.1 [2012-12-27]
  * ~/.howm-keys ��̵���ä���, �����򥹥���󤷤ƺ�����
    ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2012/000099.html>))
    > Albert-san (areiner at tph.tuwien.ac.at)
  * fix: *.txt �� *.howm �����ߤ��Ƥ�����⡼�ɤ�ɽ��������ʤ��褦��
    ((<thx|URL:http://toro.2ch.net/test/read.cgi/unix/1141892764/940>))

* ��꡼���� howm-1.4.0 [2012-08-16]
  * Note
    * ((*��ߴ��ѹ�*))�����!
      * 1.3.* �ɤ���ε�ư��˾��ʤ颭
          (setq howm-compatible-to-ver1dot3 t) ;; (require 'howm) �������!
      * ���̤����ꤷ�������, M-x customize-group RET howm-compatibility RET
    * Ĺ�����ȱ�����ǽ���ä���Τ������ǽ�ˤ��ޤ���.
    * emacs-24 �б�
    * howm-1.4.0rc2 ��Ʊ���ƤǤ�.
  * �ѹ�
    * howm-file-name-format �Υǥե���Ȥ� *.howm ���� *.txt ���ѹ�
      * ��ĥ�ҤΤ�����¾�ġ���Ȥ�Ϣ�Ȥ˺��äƤ���餷������򸫤�����Τ�
    * ��ե�����ʣ�����ΤȤ���, �ʤ��������ե�����ñ�̤���ʤ����ñ�̤�.
      ������, date �Ǥιʤ���ߤϥե�����ñ�̤Τޤ�.
    * �����ȥ뤬���ΤȤ�����ʸ�ΰ���ܤ򥿥��ȥ��.
  * ���ޤ��޲���
    * ��˥塼�˰ʲ���񤱤�, �����ॹ����׽�Ǥʤ��ե�����̾��ξ�̤�ɽ��.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/890>))
        %here%(howm-menu-recent identity)
    * �ǥ��쥯�ȥ���Ф��Ƥ�ӥ塼��������Ǥ���褦��.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/891>))
        ;; ��file://��/�פ��>>> ��/�פ� open ���ޥ��(mac ��)�ǳ���
        (setq howm-view-external-viewer-assoc '(("/$" . "open %s")))
    * �ѿ� howm-normalizer �Τ��꤬��������ߥ��򻡤���褦��
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/865-870n>))
    * �ɥ�����ȤθŤ��ս���ľ��
    * rast �ޤ��λ�����
  * ������ǽ�θ�����
    * ���ޥ��
      * C-c , e (howm-remember)
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/24-25n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/61>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/72-75n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/92-93n>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/99>))
      * C-c , b (howm-list-buffers)
      * C-c , x (howm-list-mark-ring)
      * C-c , o (howm-occur)
    * ��ޥ����
      * ��˥塼�ˡ�%reminder�פȽ񤯤�, ͽ��� todo ���������
        * ͽ���@�פ�,
          howm-menu-schedule-days-before ��������
          howm-menu-schedule-days ����ޤǤ���Ƭ��ɽ��
          * [2004-12-03]@5 �ʤɤȽ񤯤�, ��5 ���֡פΰ�
            (������ޤ�Τǡ�12��3������12��7���ޤǡ�).
            ��������ä���Τ��������ͱͽ�����.
            ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/516>))
        * ���ڡ�!�פ�, �������������ϰϤޤǤʤ����ɽ��
        * �����겼�Ͻ���ɤ���
        * ����� %schedule + %todo �Ȥ���٤��, ������󤬰��Ǥ���֤��ΨŪ
      * [2005-05-15 21:37]@ �Τ褦�ʽ񼰤�ͽ���, �����˥�����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/141>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/148>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/597>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/683>))
      * ��˥塼�����ޥ������ľá�������Ȥ���,
        �б��Хåե��ιԿ���¿������Ƥ��Ƥ����.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/519>))
          (setq howm-action-lock-forward-fuzziness 5) ;; ���ԤޤǤ���Ƥ������
    * ((<�������ޥ���>))
      * �嵭 %reminder �� todo list ��λ��ڤ�
      * grep ���ѻ��� coding system ����
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/784>))
      * howm ��Ϣ�����Хåե��˶��̤ο�����
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/42>))
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Idea>))
        > taku ����
      * todo ��Ѥޤ����Ȥ��˻��ꥳ�ޥ�ɤ�¹� (howm-congrats-command)
  * fix: 2012-01-21 �ʹߤ� emacs-24 �ǥ��顼 (void-variable inhibit-first-line-modes-regexps)
    ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    thx > ������ �� ���� (sasaki at fcc.ad.jp)

== 1.3.x

* ��꡼���� howm-1.3.9.2 [2011-12-28]
  * ���ޤ��� fix (howm-1.3.9.2rc4 ��Ʊ���ƤǤ�)
    * mac �� grep ���ѻ��˥��顼���ФƤ���
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/787-790n>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/898-901n>))
      * grep �Υǥե���ȥ��ץ��������ꤹ������,
        --exclude-dir ���̤뤫��ǧ����褦�ˤ��ޤ���.
    * �礭���ե졼��ǰ���ɽ���򤷤��Ȥ���;�פʥ�����ɥ�ʬ�����
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-vars.elc ���ɻ��ηٹ��old-style backqoute detected!�פ����.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))

* ��꡼���� howm-1.3.9.1 [2011-01-02]
  * fix: emacs-24.0.50 �ǤΥ��顼
    (Symbol's function definition is void: make-local-hook).
    thx > ���� ���� ���� (munepi at vinelinux.org)

* ��꡼���� howm-1.3.9 [2010-12-30]
  * Note
    * �ۤȤ���ѹ��Ϥ���ޤ���, �ޤ���ǯ�ۤɤ��ä��Τǥ�꡼�����Ƥ����ޤ�.
    * howm-test100702 �Ȥΰ㤤��, �ɥ�����Ȥ��������� ext/tag2plan �κ������.
    * ���Υ�꡼�����Ѥ����, �ǥե����������Ѥ���
      ������ǽ����������������Τ�Τ� howm-1.4.0 �Ȥ��ƥ�꡼������ͽ��Ǥ�.
  * fix
    * ��˥塼���� y ����(�ޤ��� [ͽ��] ��� RET)��ͽ��ɽ�򳫤����Ȥ�,
      ���ƥХåե�������������֤Υ��ˤʤäƤ��ʤ��ä�.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/826>))
    * �ɥ�����ȤθŤ��ս���ľ��
    * ���Ƥ���Ƥ��ʤ��ġ���(ext/tag2plan)����

* ��꡼���� howm-1.3.8 [2009-12-31]
  * Note
    * �礭���ѹ��Ϥ���ޤ���, ��ǯ���ä��Τǥ�꡼�����Ƥ����ޤ�.
    * howm-test090723 �Ȥΰ㤤��, howm-excluded-dirs �� ".git" ��
      �ɲä��������Ǥ�.
  * �ѹ�������
    * �᤮�����ڤ��忧
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/738>))
    * _darcs/ �ʤɤ򸡺��оݳ��� (howm-excluded-dirs).
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/708n>))
      * ���ޤɤ��� GNU grep ��ȤäƤ���ʤ颭�⤷�Ƥ�����̵�̤ʸ�����ʤ��ޤ�.
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs")
      * �����, *.txt ������������褦�ˤ�������С�
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs --include=*.txt")
    * ((<yagrep|URL:http://www.kt.rim.or.jp/~kbk/yagrep/index.html>)) �Ȥ�
      �ߴ����Τ���, grep �ƤӽФ����˥ǥ��쥯�ȥ�̾������ / ����.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/685-686n>))
    * ((<HidePrivateReminder|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))
      �Τ���������򾯤��ݽ�.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/731>))
  * fix
    * C-c , l (howm-list-recent)���˳����ե����뤬¿������ȥ��顼.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/733>))
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Windows>))
      * howm-view-use-grep �����ꤷ�Ƥ������ȯ��.
        meadow ���� "Spawning child process: exec format error" �ˤʤ�餷��.
      * grep �ƤӽФ����Υ��ޥ�ɥ饤�� howm-command-length-limit �ʾ��
        Ĺ���Ȥ���ʬ�䤷�ƸƤӽФ��褦ľ���ޤ���.
    * (setq howm-list-title t) ���Ƥ�����,
      come-from ��󥯾�� RET �����Ȥ��⥿���ȥ������ɽ������褦��.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/762>))
      * �⤷������ư��褱���,
        M-x customize-variable RET howm-list-title RET ��,
        howm-keyword-search �ʳ��򤹤٤ƥ��åȤ��Ƥ�������.
    * ������¸�����Ȥ��˥�˥塼����ư��������ʤ��ʤäƤ���.
      (howm-menu-expiry-hours ���������ꤷ�����Τ߳���)

* ��꡼���� howm-1.3.7 [2008-12-31]
  * Note
    * ����Ū�ʥ�����������, ���ޤ��޲��ɡ�����
    * howm-1.3.6 (�⤷���� howm-test080531) ������
      �ѿ� howm-list-normalizer �����ꤷ�Ƥ�������,
      ���������ߤ�, �ѿ� howm-normalizer �����ꤷ�Ƥ�������
      * ��ư�ɤߤ���������ߤƤϤ��ޤ�����
    * howm-1.3.7rc4 �Ȥۤ�Ʊ����ΤǤ�
      * ���ܰդ� "Wrote ..." ��ɽ�������Х���ľ���ޤ���
  * �ѹ�������
    * ��ߴ����ѹ�
      * ���ѿ� howm-list-normalizer ���鿷�ѿ� howm-normalizer ��
        * �ܹ���ˡ
          * M-x customize �����ꤷ�Ƥ����ʤ�,
            M-x customize-variable howm-list-normalizer RET ��
            ��Off�פ����ꤷ,
            M-x customize-variable howm-normalizer RET �ǲ�������ꤷ�ʤ���
          * .emacs ���� (setq howm-list-normalizer 'howm-view-sort-by-����)
            �����ꤷ�Ƥ����ʤ�, ���Τ褦�˽񤭤�����
              (setq howm-normalizer 'howm-sort-items-by-����)
          * (setq howm-list-normalizer �Ĥ���ʳ��β�����)
            �����ꤷ�Ƥ����ʤ�,
            * lisp ���狼����:
              ���λ����ѹ��ˤ��碌�ƽ�������
              * ���ѿ�: �ָ��ߤΰ������¤٤�����ɽ����ľ���ؿ��פ����
              * ���ѿ�: ��Ϳ����줿�ꥹ�Ȥ��Ф�, �����¤٤������֤��ؿ��פ����
            * lisp ���狼��ʤ���:
              ((<2ch UNIX �� howm ����|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/l50>))
              ��
              ((<howm wiki �Ρ֤ʤ�Ǥ��|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
              �Ǥ����̤�
        * �⤷���ѿ��򥻥åȤ����ޤޤˤ��Ƥ����ȡ�
          * ñ����ɤߤ�����줽���ʤ�, ���ѿ����ɤߤ����ƿ�������¹�
          * �ɤߤ������ʤ��ä���, �������¹� (���Ψ)
      * �ֺ��������������աפ� [YYYY-MM-DD] �Ǥʤ� YYYY-MM-DD ���忧
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/691>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/275>))
        * �������˥塼�Υե�����̾��, �ޥå�������忧
        * ���Τ褦���ᤷ������С�
            ;; ���������������դ�, [YYYY-MM-DD] �η��������忧
            (setq howm-highlight-date-regexp-format (regexp-quote "[%Y-%m-%d]"))
    * �����Хåե�
      * ������������Ū�ʰ����Хåե���ɽ��������
      * �����Хåե������ X (dired-x) ����, ����������б��ե�����̾���֤�
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
        > 797 ����
        * 1.3.2 �α�����ǽ������� & �ǥե���Ȳ�.
          �ѿ� howm-view-dired-keep-cursor �Ϻ�����ޤ���.
      * howm-view-summary-previous-section ��ֳƥե�����κǽ�Υҥåȹԡפ�
        �ߤޤ�褦�ѹ�
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/360>))
      * ���ƥХåե��ǰ쥢���ƥ����ɽ�����Ƥ���Ȥ���,
        ���ڤ�����====>>> xxx.txt�פ������ʤ�.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/314>))
      * �����Хåե��Υ����ȴ��� summary-match-string ���ɲ�
        * ���ꤷ������ɽ���˥ޥå�����ʸ����ν�˥�����
          * ��: ��2006-06-..�פ���ꤹ���, 2006ǯ6��ι��ܤ����ս��
        * ���ʤߤ�, summary-match ��, �ޥå�������Τ��̤ˤ�äƤ������
          * �ޥå�������Τɤ����ξ岼��ӤϤ��ʤ�
    * ��˥塼
      * ��˥塼�� %recent �� %random �Ǥ�ե�����̾���夽��.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000032.html>)) > Mielke-san (peter.mielke at gmail.com)
        * �ѿ� howm-menu-list-format �� %recent ����� %random �Ѥ�
        * ���ѿ� howm-menu-reminder-format �� %schedule ����� %todo ��
      * ��˥塼�� %random% ��, Ʊ���ե����뤫��ϰ���ܤ������Ф�ʤ��褦��
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/563-566n>))
      * ��˥塼������ɽ����ꥹ�Ȥǻ��ꤹ��褦�ѹ�.
        �Ѹ�ɽ���Υǥե���Ȥ� "Sun" ����ľ����.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/147>))
          (setq howm-day-of-week-ja '("��" "��" "��" "��" "��" "��" "��"))
          (setq howm-day-of-week-en '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
        * �äƤ�����, �虜�虜�ȼ������������
          (format-time-string "%a") ����Ǥ��Ǥ⹽��ʤ�?
      * �����˥塼�˥ܥ�����������ɲ�.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/144>))
    * ������
      * howm-view-grep-option ��ʣ���Υ��ץ�����񤱤�褦��.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/458>))
          (setq howm-view-grep-option "-Hnr --include=*.txt") ;; *.txt �Τ߸���
        * ñ��� split-string ���Ƥ����.
          ��äȤޤ���ʤΤ�ɬ�פʤ餪�Τ餻��������.
      * ñ������椬���ޤ��� come-from ������ɤ˰��פ��ƤⲼ��������ʤ�����.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000030.html>)) > Mielke-san (peter.mielke at gmail.com)
          ;; ASCII ʸ���ΤߤΥ�����ɤ�, ñ������˥ޥå����ƤⲼ��������ʤ�
          (setq howm-check-word-break "^[[:ascii:]]+$")
      * ͽ��ɽ, ToDo �ꥹ�Ȥˤ�Ķ��������ɽ��.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000028.html>)) > Mielke-san (peter.mielke at gmail.com)
      * .howm-history �ޤ��ε�ư�����.
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/179>))
        ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/193-194n>))
        * �Хåե�������ɽ�����ʤ�
        * "Wrote ..." ��ɽ�����ʤ�
        * make test ���� ~/.howm-history ������ʤ�
  * fix
    * howm-menu-categorized-reminder ��ɽ������ʤ����ܤ����ä�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * (setq howm-view-list-title-type 2) �ΤȤ� C-c , a �ǥ��顼
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/552>))
    * �����ȥ�Τʤ���⤬ C-c , a ��ɽ������ʤ��ä�
    * howmoney ���Ȥ��ʤ��ʤäƤ���.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/503>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/514>))
    * ͽ��� todo ����Ĥ�ʤ��Ȥ���ͽ��ɽ�� todo �ꥹ�Ȥ�ƤӽФ������.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/494>))
    * ͽ��ɽ�� todo �ꥹ�Ȥ� action-lock-mode �����ܰդ˥ȥ���.
    * howm2, howmkara �� -exclude ���ץ������Զ��.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > dareka ����
    * ((<HidePrivateReminder|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))�� C-c , t ����No match�פ�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>)) > taku ����
      * �ߴ�������
    * howm-occur �ǰ����Хåե��θ����줬�ϥ��饤�Ȥ���ʤ��ʤäƤ���
    * �֡��� �ƥ��� ���� Test�פΡ�Test�׾�� RET ��á���Ƥ�֥ƥ��ȡפ�
      ��������ʤ��ä�
      * howm-keyword-case-fold-search �򥻥åȤ��Ƥ����Ȥ�����ʸ����ʸ�������
    * C-c , l �Ǥ����������դ�ʹ���ʤ��褦��
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/340>))
      * [2007-04-02] �˺�������Х�
    * ������̤ΰ����ǡ֡��� �ۤ��פ���Ƭ�ˤ��ʤ���礬���ä�
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/328>))
      * (setq howm-list-title t) ���Ƥ����ȯ��
      * remove-duplicates �λ��ͤ�褯�Τ�ʤ��ä�����. �ٶ��ˤʤ�ޤ���.
    * (setq howm-list-title t) ���Ȱ����Хåե�����������Ƥ�ɽ�������Х�
    * howm-view-contents-limit �������Ƥ��ʤ��ä�
    * ���դǤιʤ���߷�̤�����ʬ¿������
    * narrowing ��Ϣ���Զ��(widen ȴ��)
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/160-161n>))
    * ��˥塼�Ρ�%reminder�פ����, �᤮��ͽ�꤬ɽ������Ƥ���.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/149>))
    * ��˥塼��Ρ�> 2006-07-26-015606 | �ġפΡ�7�׾�� RET ��á���ȥ��顼.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > na ����
      * �ѿ� howm-menu-list-regexp ����������ä�ľ��������
    * �ۤʤ�ǥ��쥯�ȥ��Ʊ̾�ե����뤬����ɽ���Ǻ�Ʊ����Ƥ���.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-view-split-horizontally �� t �Ǥ� nil �Ǥ�ʤ��Ȥ������̤�ư����ѻ�.
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/591>))
      * howm-1.2 �� 1.3.7rc2 �ǲ���Ƥ�����, �Х���ݡ��Ȥʤ�.
        ���ä�ï��ȤäƤ��ʤ� ^^;

* ��꡼���� howm-1.3.6 [2008-05-31]
  * fix: 2008-02-24 �ʹߤ� CVS ��ü emacs ��, ¾�Хåե����忧������.
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/484-487n>))
    * minor mode �� font-lock-keywords-only ��ľ�˿���ΤϹԵ�����?
    * howm-test20080514 ����ΥХå��ݡ���
  * (howm-1.3.6rc1 ����Ȥ�Ʊ���Ǥ�)

* ��꡼���� howm-1.3.5 [2007-12-09]
  * fix: �ƻ��ֺǽ�����������ͽ�꤬��˥塼��ɽ������ʤ��ä�.
    ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000034.html>)) > Mielke-san (peter.mielke at gmail.com)
    * howm-test071108 ����ΥХå��ݡ���
  * fix: ����Ʊ�ǥХ��ȥ���ѥ���Ǥ���褦��
    * Wanderlust �� Navi2ch �򻲹ͤˤ���,
      ����¸�ΰ�����(require �ν���)����
    * howm-test07-05-18 ����ΥХå��ݡ���
  * automake �� 1.6 ���� 1.10 ��
    * howm-test07-05-05 ����ΥХå��ݡ���
    * automake-1.10 �� elisp-comp ���Ȥ���褦�ˤʤä�
  * (howm-1.3.5rc1 ����Ȥ�Ʊ���Ǥ�)

* ��꡼���� howm-1.3.4 [2006-12-16]
  * �������ƥ�����
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/207>))
    * ��������?
      * Emacs �ˤ�, �ե����뤴�Ȥ˥������ѿ���ư���ꤹ�뵡ǽ������ޤ�.
        ������Ѥ����, howm ���ѻ���Ǥ�դ�̿���ư�¹Ԥ����뤳�Ȥ��Ǥ��ޤ�.
        ((<ref|URL:https://www.codeblog.org/blog/ueno/20060118.html>))
    * �ɤ�ľ����?
      * howm ��Ϣ��������ܥ�� risky-local-variable °���򥻥åȤ�,
        ��Ҥμ�ư������˥����å�������褦�ˤ��ޤ���.
    * �С�����󥢥åפ������ʤ�/�Ǥ��ʤ��ΤǤ���?
      * ���������Խ�����ǽ�ʤ�,
        howm.el �������˰ʲ��Υ����ɤ�ä���Τ��μ¤Ǥ�.
        �Х��ȥ���ѥ���Τ��ʤ����⤪˺��ʤ�.
          ;; howm-1.2.2 �ʹ���. howm ��Ϣ��������ܥ�� risky-local-variable °��.
          (mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                  (howm-symbols))
      * ���줬����ʾ��� .emacs �˰ʲ���ä��Ƥ�������.
          (eval-after-load "howm"  ; �� autoload/load/require �ε��Ҥˤ��碌��
            ;; howm-1.2.2 �ʹ���. howm ��Ϣ��������ܥ�� risky-local-variable °��.
            '(mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                     (howm-symbols)))
      * �ɤ���ˤ���, ������ȿ�Ǥ��줿���Ȥ򤴳�ǧ��������.
        * emacs ��Ω���夲ľ��, howm ��ư
        * �ʲ��� *scratch* �Хåե���Ž��, �Ĥ���̤θ�˥���������֤��� C-j ��
          ����
            (get 'howm-version 'risky-local-variable)
        * t ��ɽ�������� OK
    * �������ѿ��μ�ư����򤢤��ƻȤ������Ȥ���?
      * �ʲ��Τ褦���ѿ����Ȥ˲�ؤ��Ƥ�������.
          ;; ��: �ѿ� howm-auto-narrow �ϥե����뤴�Ȥμ�ư��������
          (put 'howm-auto-narrow 'risky-local-variable nil)
    * howm �˸¤餺, �������ѿ��μ�ư�������ڻȤ��ʤ�����ˤ�?
      * .emacs �˰ʲ���ä��Ƥ�������.
        ������ emacs �ΥС������ˤ�äƤ��Դ������⤷��ޤ���.
        ((<ref|URL:http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html>))
          ;; �������ѿ��μ�ư����򥪥�
          (setq enable-local-variables nil)
  * fix: CVS ��ü emacs �ǥ�˥塼�ʤɤ˿����Ĥ��ʤ�
    ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/165-169n>))
    * ������, cheat-font-lock-20040624-format-p �������� = �� >= ��ľ������
    * howm-test061015 ����ΥХå��ݡ���

* ��꡼���� howm-1.3.3 [2006-06-05]
  * Note
    * �ǿ��� Emacs 22.0.50 (CVS HEAD) �ˤ��֤��б�
      * Meadow 3.00-dev �� Carbon Emacs �⤳����������ޤ�.
        �����֥�꡼�����γ�ȯ�Ǻ���ü Emacs��
        �򺣸���ɤ�³��������, howm ��ƥ����Ǥ������ƤߤƤ�������.
    * �ե����빽���򾯡��ѹ�
      * make install �ʳ�����ˡ�ǥ��󥹥ȡ��뤹����Ϥ���ǧ��������.
        �����˥塼 0000-00-00-000000.howm �μ�ư���ԡ������פˤʤ�ޤ���.
    * ��˥塼�� todo �����Ǥ�������ι��ܤ�ǥե���Ȥ�ɽ��
      * �ǥե���Ȥϡְ���¦�פ��ݤ��Ƥ��������ɤ��Ǥ��礦.
        ����ޤ��̤걣����ˡ�ϲ��Ρ��ѹ��פ򻲾Ȥ�������.
    * ����¾, grep ��ʸ�������ɤ˴ؤ��뽤����, ������ǽ�ʤ�
      * ������ǽ�Ǥ�, ���Ȥ�򤵤�˼�ڤˤ��� M-x howm-remember ��
        ��ɾ�Τ褦�Ǥ�.
    * howm-1.3.3rc1 �� howm-test060515 ����Ȥ�Ʊ��
      * meadow3 �ǥϥޤä��ͤ�¿�����ʤΤǥ�꡼�����뵤�ˤʤ�ޤ���.
        ���԰���ʳ�ȯ�ǡפȤ�����������ʤ��⤦���̤���ڤ��Ƥ���Τ��ʡ�
  * �ѹ�������
    * ��˥塼�� todo �����Ǥ�������ι��ܤ�ǥե���Ȥ�ɽ��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/75-77n>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/842-845n>))
      * ����������� M-x customize-variable howm-menu-todo-priority
    * grep ����ʸ������������ howm-process-coding-system ��,
      ���ϤȽ��Ϥ��̤��ͤ����Ǥ���褦�ˤ���
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/96>))
        ;; process (UTF-8)�� emacs
        ;; emacs ��(SJIS) process
        (setq howm-process-coding-system '(utf-8-unix . sjis-unix))
    * ��˥塼�Ρ�> �ġפ� RET �����Ȥ�, �֡ġפ򸡺�����ΤǤʤ�,
      �б��ե������ľ�ܳ���
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/823>))
    * �ե����빽���䥤�󥹥ȡ�����ˤĤ���
      * �����¸���ѿ��� howm-lang-{en,ja}.el ��ʬΥ
      * configure �ο����ץ���� --with-howmdir.
        thx > �ܾ�����
        * *.el �� *.elc �Ϥ����إ��󥹥ȡ��뤵���
        * lispdir �Υǥե���Ȥ�, ��/site-lisp/howm ���� ��/site-lisp ���ѹ�
      * �����˥塼�ƥ�ץ졼�ȤΥ��󥹥ȡ���ˡ���ѹ�
        thx > �ܾ�����, ��ë����
        ((<ref|URL:http://lists.sourceforge.jp/mailman/archives/macemacsjp-users/2005-November/000756.html>))
        ((<ref|URL:http://lists.sourceforge.jp/mailman/archives/macemacsjp-users/2005-November/000760.html>))
        * �����, /usr/local/share/howm/{en,ja}/0000-00-00-000000.howm
          ���֤���, ��� howm-{en,ja}-dir �Ǥ��ΰ��֤����
          * ���󥹥ȡ��뤷˺������Хѥ��ˤ��, �ȥ�֥뤬�����Ƥ���
          * ���󥹥ȡ��뤷�ʤ����, 0000-00-00-000000.howm �μ�ư���ԡ���ɬ��
        * �����, howm-menu-{en,ja}.el
          * howm ����ƻȤ��Ȥ��Τ�, ��� howm-menu-{en,ja} ���ɤ߹��ߤޤ�
            * ����ˤ���������Τ��Ҥ󤷤夯�ʤ�,
              �ֻ��Ѹ���ͤ��˴��פȤ������ٹ���ͤ����ޤ�.
              �⤷ɬ�פ��ä��餪�Τ餻��������.
              (���ɤ�������Х��Ȥʤ�Ƹ��ϰ�?)
          * 0000-00-00-000000.howm �μ�ư���ԡ��ϴ��������פȤʤä��Ĥ��
          * ��ɤ����ή��
              ja/0000-00-00-000000.howm
                      ����꡼�����˺�Ԥ����� (�ºݤ� howm-mkmenu.el �Ǽ�ư��)
              howm-menu-ja.el
                      ��make install
              $lispdir/howm/howm-menu-ja.elc
                      ��howm ����ƻȤä��Ȥ������ɤ߹���Ǽ�ư����
              ~/howm/0000-00-00-000000.howm
  * fix
    * CVS ��ü emacs �� make ����ȥ��顼
      "Font-lock trying to use keywords before setting them up".
      ((<thx|URL:http://tty0.exblog.jp/2944244>))
      ((<thx|URL:http://d.hatena.ne.jp/yoshk/20060102>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/867>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/873-874n>))
      ((<thx|URL:http://d.hatena.ne.jp/clock9/20060406/1144291193>))
      ((<thx|URL:http://d.hatena.ne.jp/AllStarMoves/20060425/p3>))
      ((<thx|URL:http://d.hatena.ne.jp/katase_n/20060519>))
      ((<thx|URL:http://d.hatena.ne.jp/AllStarMoves/20060602/p4>))
    * grep ���� howm-process-coding-system �ν��������ߥ󥰤˥Х�
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/63-83n>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/94-95n>))
    * migemo-client �Υ��ץ������ɲû����ǽ��
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/9>))
        (setq howm-migemo-client-option '("-H" "::1"))
      * howm-view-grep-option ������Ȥ������줬���ˤʤ�Τ�,
        ���ޥ�ɻ�����̤γ�ĥ���Ͱ�(�Ȥꤢ�����Ƥ���). ������������?
          nil  ;; �� �ǥե����
          "���ޥ��̾"
          ("���ޥ��̾" "���ץ����" �� "���ץ����")
          �ؿ�̾  ;; �� ���ޥ�ɤΤ����� elisp �δؿ���¹�
    * howm-kill-all �� .howm-keys �Хåե���ä��٤�
      ((<thx|URL:http://d.hatena.ne.jp/dasm/20060110>))
    * howm-mode-off-hook ����������֤äƤ���.
      thx > ��¼����
    * ((<howmz.el|URL:http://noir.s7.xrea.com/archives/000136.html>))
      �ǥ��顼���ФƤ�������.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      > (TxT) ����
    * emacs20 �� M-x howm-bug-shot �����顼�ˤʤäƤ���.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>))
      > ƨ�򤵤�

* ��꡼���� howm-1.3.2 [2005-11-04]
  * Note
    * ���, �����ʥХ���������
    * ���Ȥϱ�����ǽ�򾯡�
    * ��꡼��ͽ���� 1.3.2rc4 ��Ʊ����ΤǤ�
  * �ѹ�
    * ��˥塼�� [����] (C-c , , .) �Ǥ�ǥե���Ȥǥ����ȥ��ɽ��.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
      > nobu ����
      * �����ȥ�ɽ���������ʤ���С�
        * M-x customize-variable RET howm-list-title RET
        * howm-action-lock-date-search �Υ����å���Ϥ���
        * [Save for Future Sessions]
  * fix
    * xemacs ����, ��˥塼��� [2005-10-15] �Τ褦��
      ���վ�Ǥ� RET �����顼
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/797-801n>))
    * xemacs ����, �����Хåե������ X (dired-x) �����顼
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
      > 797 ����
      * �Ĥ��Ǥ�, Ʊ���ե����뤬���٤�ɽ�������Τ���
    * howm-view.el �� (require 'riffle) ���ɲ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/782>))
    * Makefile �ʳ��μ��ǥХ��ȥ���ѥ��뤹��� M-x howm-menu �����顼
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/789-791n>))
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BrokenMenu>))

* ��꡼���� howm-1.3.1 [2005-08-17]
  * xemacs �Ǥ䤱���٤��ʤäƤ����Τ���
    (xemacs �ΥС������ˤ���Τ���)
  * �Ķ��ѿ� LC_ALL, LC_CTYPE, LANG �����ꤷ�ʤ��ȥ��顼���ФƤ����Τ���
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/780-781n>))
  * ���� README �� ����
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/779>))
    * my-howm-next-hit �Υ����ɤ򹹿�
    * howm-view-search-in-result-correctly �����ꤷ�Ƥ�,
      date �Ǥιʤ���ߤϥե�����ñ��
      * ���̤ϻ���. ������ե�����ޤǤʤ�����ʤ��Ϥ�.
      * ����ե����롦������ե�����ʤɤ�������.
      * �ֻ����Υ������פȤ��Ǥä������褦���Ȥ�פ��ޤ�����,
        ��ʬ�ξ���Ĺ���ʤꤹ�������Ω���ʤ����ʤΤ�, ���ޤ���.
        ���ո�������Ф�ʹ������������.
  * ������, ����Ʊ��̾���⤤������ɽ��
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/779>))
      (setq howm-view-summary-omit-same-name nil)
  * ���Ȥϱ�����ǽ�򾯡�
  * ��꡼��ͽ���� 1.3.1rc1 ��Ʊ����ΤǤ�

* ��꡼���� howm-1.3.0 [2005-08-02]
  * Note
    * �ܶ�
      * come-from ������ɤ� alias
      * M-x customize �б� ([Applications] �� [Howm])
        * ���� README ��((<�������ޥ���>))�Ϥ����������ޤ���.
      * ��������
      * �������Υ����ȥ�ɽ��
      * ������Ʊ���ե�����̾�򤯤��֤�ɽ�����ʤ�
      * ��ư����
      * ��˥塼�˺Ƕ�Υ��������������������
      * ��˥塼�� [����] [����] [����] [����] ���ɲ�
        * ���Ǥ� howm ��ȤäƤ������,
          make install ���Ƥ⾡��ˤ��ɲä���ޤ���.
          ��˥塼��ʬ���Խ����뤫,
          ja/0000-00-00-000000.howm ��ʬ�ǥ��ԡ����뤫���Ƥ�������.
    * �ǥե���Ȥ��ѹ����ޤ���. �ᤷ������� .emacs �ʤɤˢ���񤤤Ƥ�������.
        ;; �����ȥ�ɽ���Ͼ������
        (setq howm-list-title nil)
        ;; ��������
        (setq howm-history-limit 0) ;; ���������Ͽ���ʤ�
        (setq howm-history-unique nil)  ;; �������򤫤��ʣ��������ʤ�
        ;; grep -E/-F �Ǥʤ� egrep/fgrep
        (setq howm-view-grep-command "egrep")
        (setq howm-view-fgrep-command "fgrep")
        (setq howm-view-grep-extended-option nil)
        (setq howm-view-grep-fixed-option nil)
        (setq howm-view-grep-file-stdin-option nil)  ;; �ѥ�����ϰ������Ϥ�
        ;; howm-template ���ؿ����ä��Ȥ���, universal-argument ��
        ;; �����ˤ��Ƥ����Ĥ�Ƥ�
        (setq howm-template-receive-buffer nil)
        ;; �������� RET �ǳ����Ȥ�, ���ƥХåե��Υ���������֤��ݤ��ʤ�
        (setq howm-view-summary-keep-cursor nil)
    * �ƥ��Ȥˤ����Ϥ�����������, �����Ƥ���������Ƚ�����ޤ�.
        ;; (������ǽ)
        ;; ��ե�����ʣ�����ΤȤ���, �ʤ��������
        ;; �ե�����ñ�̤���ʤ����ñ�̤�.
        ;; ������, date �Ǥιʤ���ߤϥե�����ñ�̤Τޤ�.
        (setq howm-view-search-in-result-correctly t)
    * �����������ѹ� (riffle.el)
    * ��꡼��ͽ���� howm-1.3.0rc5 ����Ȥ�Ʊ���Ǥ�.
  * �����ѹ�
    * howm-template ���ͤ��ؿ����ä��Ȥ���,
      ��universal-argument ��((*ľ���ΥХåե�*))�פ�����ˤ��Ƥ����Ĥ�Ƥ�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
      * ������
          ;; snap.el �ǤΥ�󥯤������
          (setq howm-template #'my-howm-template)
          (defun my-howm-template (which buf) ;; C-u 3 C-c , c �ʤ� which = 3
            (let ((snap (with-current-buffer buf
                          (or (snap-record-string) ""))))
              (format "= %%title%%cursor\n%%date\n%s\n\n" snap)))
      * ����ȸߴ����ᤷ������С�
          ;; howm-template ���ؿ����ä��Ȥ���, universal-argument ��
          ;; �����ˤ��Ƥ����Ĥ�Ƥ�
          (setq howm-template-receive-buffer nil)
    * ��˥塼������Υ�ޥ����ľá����, á����μ�ư��¸�ˤĤ��ơ�
      * ���Ȥ� howm-action-lock-forward-save-buffer �� non-nil �Ǥ�,
        á�������餹�Ǥˡֳ����Хåե��� modified�פ��ä��Ȥ���
        ��¸���ʤ�
    * howm-todo-menu-types �Υǥե���Ȥ� "." ���ɲ�
    * �ǥե����������ѹ�
      * egrep/fgrep �Ǥʤ� grep -E/-F ������ǥե���Ȥ�.
        �ѿ� howm-view-fgrep-command �Ͼ����ѻߤ��뤫��.
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/670>))
      * grep �ؤ�ɸ�����Ϥǥѥ�������Ϥ�
      * {��|�Ƕ�|����}�������˥ǥե���Ȥǥ����ȥ�ɽ��
  * ������ǽ�θ����� (�� �ϡ֤��������)
    * 1.1.1.* ��������
      * howm-view-before-open-hook
      * ��˥塼�� todo �˽��٤�ɽ����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/505>))
          (setq howm-menu-todo-priority-format "(%8.1f)")
      * �ǥХå����ѿ� howm-call-process-last-command
      * (setq howm-message-time t) �����, ���������פ������֤�ɽ��
    * 1.2 ����
      * come-from ������ɤ� alias �� �� ((<����񤳤�>))
        ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?CompoundComeFrom>))
      * ��˥塼
        * ��˥塼�ˡ�%recent�פ��%random�� ��
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/242>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/273>))
        * ��˥塼����ѿ���ؿ����ͤ�ɽ��
      * ����
        * ������ˡ�ˡ�random�פ��ɲ�
          ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?OldMemo>))
        * ����ɽ����ιԿ�����
          ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/724>))
            (setq howm-view-summary-window-size 10)
        * �֥����ȥ�פ�����ɽ����, ��ǽ���Ȥ��ѹ���ǽ
            (setq howm-list-title-regexp "^[*=] [^ ]")  ;; ����ɽ��
            (setq howm-menu-recent-regexp "^[*=] [^ ]")  ;; ��˥塼��� %recent
        * �������� RET �ǳ����Ȥ�, ���ƥХåե��Υ���������֤��ݤ�
            (setq howm-view-summary-keep-cursor t) ;; �� �ǥե���Ȥˤ��ޤ���
          * ����äȼ����ʤ�. �Զ��Ф��鶵���Ƥ�������.
        * ñ�������˥ޥå�������Τϸ�ޤ路
            (setq howm-list-prefer-word t)
          * ��euc�פ򸡺�������, ��euclid�פ��takeuchi�פ���,
            ñ���euc�פ˥ޥå�������Τ���ɽ��
          * ������, ��<<< euclid�פϤ�������餺�Ƥäڤ��
      * �������
        * ���޳����Ƥ�ե�����˿��������ɲ�: M-x howm-create-here
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/293>))
        * �������Υե�����̾���ư�ǤĤ���: M-x howm-create-interactively
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/367>))
          * ����ʴ����Ǥ��礦��? > 367 ����
      * M-x howm-narrow-to-memo, M-x howm-toggle-narrow
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/293>))
        * �Ĥ��Ǥ�, M-x howm-toggle-narrow ��, �������������ȥ���
        * ���򳫤����Ȥ���ưŪ�� narrow ��
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/301>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/542>))
            (add-hook 'howm-view-open-hook 'howm-auto-narrow)
            (add-hook 'howm-create-hook 'howm-auto-narrow) ;; �ɲ�[2005-01-07]
      * ��ޥ�����Ρ�cancel�פ�㤦���դ˥������ޥ���
        (thx > NARA Shinsuke ����)
          (setq howm-reminder-cancel-string "give up")
    * 1.2.1 ����
      * M-x howm-history �Ǹ�������. �����򤫤� RET �����٤�. ��
        * ((<RandomNote|URL:http://ninjinix.x0.com/rn/index.rb?AboutPage.txt>))
          ��
          ((<namapo|URL:http://tiki.is.os-omicron.org/tiki.cgi?c=v&p=namapo>))
          �˿�ȯ����ơ�
        * ��Ͽ��, �ָ���ʸ���󸡺��סֹʤ���߸����ס֥�󥯡פΤߤˤ��Ƥߤ�
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/496>))
        * ��ˤ�äơ֥�˥塼��ɽ���פ⤷��������,
          �֥�˥塼�򥭥�å���פȤη�͹礤��.
        * 1.2.2 ����Ϻ��絭Ͽ���������
        * 1.3.0 ����Ͻ�ʣ�����
      * ��ե�����ʣ�����ΤȤ�, �����塦�ǽ顦�Ǹ�Υ��ذ�ư
        (narrowing ���θ)
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
        * M-x howm-previous-memo
        * M-x howm-next-memo
        * M-x howm-first-memo
        * M-x howm-last-memo
      * �����ΤȤ�������ư narrow.
        ������, ��>>> foo.howm�פ� foo.howm ��������Ȥ��� narrow �ˤ��ʤ�.
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
          ;; ���ǥե���Ȥ�ư����������ʤ������������
          (setq howm-auto-narrow
                ;; ���˽񤤤����ޥ�ɤǤ��� narrow
                '(howm-list-all howm-list-recent
                  howm-list-grep howm-list-grep-fixed howm-list-migemo
                  howm-list-related howm-list-around
                  howm-keyword-search)) ;; ����� come-from ��󥯡�goto ���
      * C-c , T (howm-insert-dtime) �� [2004-09-01 23:26] �Ȥ�����
        ((<ref|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/elisp/get-date.el>))
        ((<ref|URL:http://www.gentei.org/~yuuji/software/euc/instamp.el>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/399>))
    * 1.2.2 ����
      * ��ư���� ��
      * ��˥塼�ˡ�%here%(howm-menu-search "�ۤ�")�פȽ񤱤�,
        �֤ۤ��פθ�����̤������� ��
        (thx > Konstantin Levinski (kostya@pmail.ntu.edu.sg))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/605>))
      * �������ޤ��
        * ��ե�����ʣ�����ΤȤ�, ������������Ƭ���ɲäȤ������� ��
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/535>))
            (setq howm-prepend t)
        * �֤ۤ��פ򸡺����ư����������֤��鿷���������, �����ȥ��֤ۤ��פ�
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/573>))
            (setq howm-title-from-search t)
        * �ƥ����Ȥ����򤷤Ƥ���ֿ������� �� ���Υƥ����Ȥ�ư����
          ((<thx|URL:http://hpcgi1.nifty.com/spen/index.cgi?ZaurusSL-C3000%2F%BD%E9%B4%FC%C0%DF%C4%EA%2Femacs%A4%BD%A4%CE%A3%B4#i0>))
            ;; transient-mark-mode �Ǥʤ���, ���������̵�뤵���
            (setq howm-content-from-region t)
        * howm-create-here ��, ̵ͭ����蘆���ָ���������Ԥˡ׿���������
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/542>))
            (setq howm-create-here-just t)
      * ��������Υ����ȴ��� numerical-name ���ɲ�
      * ��ȯ��
        * C-u M-x howm-bug-report �Ǵ�Ϣ�ѿ��ΰ���
        * M-x howm-elp ��, �ץ�ե����� elp �ν���
  * ����¾�β���
    * M-x customize ���б� ([Applications] �� [Howm])
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?RoadMap>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/668>))
    * �����Хåե���, Ʊ���ե�����̾�򤯤��֤�ɽ�����ʤ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/754>))
      * �Ĥ��Ǥ˿���Ĥ��Ƥߤ�. �ɤʤ�����äȤޤ����ۿ��򤯤�����.
        (M-x customize-group RET howm-faces RET ����,
        howm-view-name-face �� howm-view-empty-face)
      * (����) ��Ϣ�����¸��ǽ
        * TAB��ALT-TAB �� �������Υե������
        * u �� ��ĤΥ��ϰ�Ԥ�����
        * @ �� Ϣ��ɽ�������Ʊ�����ϰ�ĤˤޤȤޤ�
    * ������ T �� �����ȥ�ɽ����֥ȥ����
      * 1.2.1 �α�����ǽ�������
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/427>))
    * ��˥塼��ΰ����Ǥ�, ��Ƭ�Ǥʤ��Ƥ� RET �ǥ�����
    * ��ޥ����ľá���ǡ��׻��˳������Хåե��פ�ư���Ĥ���
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/705>))
        ;; á�����ư���Ĥ���. undo �Ǥ��ʤ��ʤ뤫�餪�����ᤷ�ޤ���.
        (setq howm-action-lock-forward-kill-buffer t)
    * howm-menu-lang �Υǥե���Ȥ� locale �򸫤Ʒ���
    * ext/howm2 �ǡ�come-from ������ɤ� alias�פ򥵥ݡ���
      * ��������餺��äĤ��Ż�.
        �ĤȤ�����, ������äĤ��Ż�������, �⤦���ɤǤ���.
  * ��������
    * cl �ѥå���������δؿ��� howm-cl.el ��ʬΥ.
      ���Ĥ����礬�Ϥ�����������褦��
    * make ���ηٹ�֡� not known to be defined�פ�����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1111816102/485>))
    * riffle.el �λ��ͤ��ѹ�(gfunc.el ��Ȥ�). �桼�����ˤϱƶ��ʤ��Ĥ��.
      ((<howmoney|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?howmoney>))
      ��������礦�֤Ȼפ�������ɡ�
  * �Х�����
    * ������ǥ��顼
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/706>))
      * ���顼�ǥ����ץ쥤�Ǥʤ��Ȥ���, �����Τ�����ȿžɽ������,
        ����ʳ��ξ���Ϥʤ�
    * %reminder �ζ��ڤ����������
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/703>))
    * �������� T �ǥ����ȥ�ɽ�������Ȥ�, ̵�����ȥ�ʬ��¿��ɽ������Ƥ���
      * make test ���� C-c , s top [RET] T ��ȯ��
    * win �ǡ֡�\.foo\�ġפʤɤ򸡺��оݤȤ��ʤ��褦��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/746>))
    * howm-message-time �򥻥åȤ��Ƥ��No match�ץ�å������򱣤��ʤ��褦��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/751>))
    * ��˥塼�Υ�ޥ������� come-from ������ɤ�
      �������Ĥ��ʤ��ʤäƤ���.
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/732>))

== 1.2.x

* [2005-06-10] v1.2.4
  * bug fix
    * howm-search-path ���ͽ�ꡦtodo ����˥塼��ɽ������ʤ��ä�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/685>))
    * ��˥塼��ɽ�������ͽ����ϰϤ�����û�����ä�
    * xemacs ��, �ե����뤬���ʤ��ȥ�˥塼�� %recent �����顼��
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * ��˥塼��� C-c , r �����Ȥ��ϡ֥�˥塼������
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/696>))
    * howm-menu-list-face �Υǥե���Ȥ����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/697>))
    * (require 'howm-mode) ������ cl ̤���ɤǥ��顼
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/698>))
      * howm-1.2 ����� (require 'howm) ������.
        ��howm-mode�פ� require �� autoload ���Ƥ�����,
        ��howm�פ�ľ���Ƥ�������.
    * ((<howmz|URL:http://noir.s7.xrea.com/archives/000136.html>))
      �ǥ��顼 (�ؿ� howm-view-sort-items ���ʤ�)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      > (TxT) ����
    * ��ޥ����ľá���ǥ�˥塼�������
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/705>))
      * ����������ȯ��
          (setq howm-menu-expiry-hours 2)
          (setq howm-action-lock-forward-save-buffer t)
    * POBox �� RET (�Ѵ�����)��å�äƤ��ޤ�
      * �¤� POBox �˸¤餺���ܰդ�ư��򤷤Ƥ���
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/711>))
    * howm-message-time �����ΤȤ��ν��׻���ɽ���� 1 ��̤�����¬
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/709>))
      * [2005-03-09] �˥����ȥ����Ȥ���Ƥ���(��ͳ��ǰ)�Τ��ᤷ��
  * ��꡼��ͽ���� howm-1.2.4rc7 �Ȥΰ㤤�ϡ�
    * ext/howmkara, ext/hsplit.rb, doc/README.html �����ۥե�������ɲ�

* [2005-05-07] v1.2.3
  * bug fix
    * C-c , s RET �Τ褦�˶�ʸ����򸡺������Ȥ���, ��������
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/668>))
    * make �Ǥʤ���ư byte-compile ������(?), %reminder �ǥ��顼
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/669>))
  * ��꡼��ͽ���� howm-1.2.3rc1 [2005-05-06] �ȤۤȤ��Ʊ���Ǥ�

* [2005-05-02] v1.2.2
  * Note
    * ɽ�̾�ۤ��Ѳ��ʤ�. �����˼�򤤤�Ƴ�ĥ�����᤿(�Хå�������ڤ�Υ��).
    * ��Ҥα�����ǽ��, ��äѤ�ޤ�����
      * �ͤ᤬�Ť��Ƥ�ȤäƤ�������ʤ�, ���Τؤ󤬤�������
        * come-from ������ɤ� alias
        * ��˥塼�ˡ�%reminder�ס�%recent�ס�%random��
          ��%here%(howm-menu-search "�ۤ�")��
        * M-x howm-list-buffers, M-x howm-list-mark-ring
        * M-x howm-random-walk
    * ���ˤʤ��Ŧ�⤢�뤱��, �ɤ���Ϥä��ꤻ��, ���ڤ�.
      �Ƹ��Ǥ������Ϥ��Τ餻��������.
      ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/582>))
    * ���椫�� ChangeLog ��Ĥ��Ϥ��᤿�Τ�, �٤���ľ���Ϥ�����⻲��
    * ��꡼��ͽ���� howm-1.2.2rc6 [2005-04-29]��Ʊ����ΤǤ�
  * �����ѹ� (���������ƶ��ʤ�����)
    * ���� howm-menu-top �� howm-menu-file ��ͥ����դ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/648>))
      * ��: howm-menu-top �����ꤵ��Ƥ���ʤ�, howm-menu-file ��̵��
      * ��: howm-menu-file �����ꤵ��Ƥ���ʤ�, howm-menu-top ��̵��
      * �ʤ�, �ǥե���ȤǤ�,
        howm-menu-top ������Ѥߤ� howm-menu-file ��̵����
    * �ȥåץ�˥塼��<<< %menu%�פ�õ���ݤ� howm-search-path ��̵��.
      howm-directory ��������.
  * ����
    * egrep, fgrep ���ʤ����� GNU grep �Ϥ���, �Ȥ����Ķ�(��ʥ���?)���б�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/621-625n>))
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
    * contents �Ǥιʤ���ߤ� C-u ���б� (�ޥå����������ӽ�)
      �� ((<������|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/593>))
    * todo �ꥹ�Ȥ��˥塼�� %reminder �Ǥ�, ���٤�Ʊ���ʤ�ʸ����祽����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/571>))
    * ���ޤ���
      * ���������T�פǥ����ȥ�ɽ�������Ȥ�, ̵�����ȥ�ʬ�ϺǸ�ˤޤ魯
      * �����ʳ��� RET á�����Ȥ��ν�����㴳��ǫ��
      * ext/howmkara ��, ���ȤؤΥ�󥯤�����.
        �Ĥ��Ǥ� Home �ؤΥ�󥯤�եå����ɲ�.
      * ext/hcal.rb �� [2004-12-25]@3 ��ˡ�б�
      * �Ѹ��˥塼�򽼼�. ��ʸ index.html �˹�®�����ץ���������.
      * ɽ���ѥХåե��ˤ� (buffer-disable-undo)
    * ��ȯ�Ը���
      * �Хå�����ɤ��ڤ�Υ�������(howm-backend.el)
        * ��������, �ü�ե�������ü��������ǽ�ˤʤä�
        * �Դ���������, �Ȥ��ʤ��餳�������ľ���Ƥ����äƤ���ť���
        * gfunc.el (��ľ generic function)
        * howm-view-call-process �� howm-call-process ��
      * cheat-font-lock.el �����ݽ�
      * win �Ѥ�, ��make test�������� test.bat
      * make test �� --no-site-file ��
        * xemacs ���� -no-site-file �ߤ���������, ����
      * howm-configuration-for-major-mode ����(howm-misc.el �Υ�����)��������
        (defalias ��)
  * bug fix
    * ���������κǽ��Ԥ˥ޥå������Ȥ�, Ϣ��ɽ�������ɽ���ˤʤäƤ���
    * howm-view-use-grep �� non-nil �ΤȤ����顼
      (howm-view-grep-file-stdin-option �� nil ����ȯ��)
    * howm-view-use-grep �� nil �ΤȤ�, howm-excluded-file-regexp ��
      ��������ե�����ϥ�����󤷤ʤ��褦��
      ((<thx|URL:http://www.bookshelf.jp/pukiwiki/pukiwiki.php?%BC%C1%CC%E4%BD%B8%2F42>))
      * �ĤȤ�����, ��äȺ��������оݳ��ե�����Υ����å���ľ��
      * �Ĥ��Ǥ�, howm-excluded-file-regexp �Υǥե���Ȥ�
        (image-file-name-regexp) �������ɲ�
    * 0123-45-6789 �ߤ��������ֹ�˲���������ʤ� (���դȤߤʤ����򸷤���)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * ��%�פ�ޤ� message �Υ��顼
    * emacs20 �ǥ�˥塼�򱣤��Хåե��ˤ���ȿ����Ĥ��ʤ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/541>))
    * ���������ƥХåե�������򳫤��ݤ�, narrowing �����Ƚ��ߥ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/532>))
    * come-from ��ˡ���ѹ��������μ�����
      (thx > taku ����)
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
  * �ƥ����ǡ���꡼��ͽ���Ǥ� bug fix (��)
    * >>> ~/hoge.ps �� file://~/hoge.ps �� RET �ǳ������Ȥ���ȥ��顼
      (thx > Konstantin Levinski (kostya@pmail.ntu.edu.sg))
      * ���֤� [2005-01-07] �ˤ�餫�����Х�
    * emacs21 �� 20 �Ȥ�, font-lock-fontify-{block|buffer} ��Ȥ��櫓
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/416>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/533>))
      * ���򤷤Ƥʤ���, ������������Ǥ�
      * ��ե����뤬Ĺ�����忧�˻��֤Τ���������,
        emacs20 �Ǥʤ� emacs21 �򤪤����ᤷ�ޤ�
    * howm-list-all-title �� non-nil �ΤȤ� C-c , a ���¤ӽ礬��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/599-600n>))
    * ���򳫤��Τ��۾���٤�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/635-636n>))
      �� ľ�ä��Τ�̤��ǧ
    * Meadow 2.10 �� make �����顼��
      (thx > taku ����, �֤�פ���)
      ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/638>))
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?MaxSpecpdlSize>))
    * Meadow-1.15 ����, image-file-name-regexps ��̤����ǥ��顼
      (thx > ƨ�򤵤�)
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * howm-directory �� ~/.howm �Τ褦�ʥɥåȥǥ��쥯�ȥ���Ȳ��⸡�����줺
      (thx > taku ����)
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))

* [2004-12-11] v1.2.1
  * Note
    * ɽ������, �ۤȤ�� bug fix �Τ�
    * ������ǽ��, �ޤ�����
  * ���ɡ��ѹ�
    * ͽ��ɽ��, Ʊ������ͽ��ϵ������Ƥǥ�����
      * ��[2004-11-04]@ 07:30 �ۤ��ۤ��פߤ����˽񤤤Ȥ���, �������¤֤Ϥ�
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/398>))
      * ��Ʊ�����ΤϽ����ٽ�ǥ����ȡפä���˾��, ����ȤäƤʤ�Ȥ��ʤ�?
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/433>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/442>))
        ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/445>))
      * tag2plan ��ľ���Ƥ��ޤ���.
    * howm-template �Υǥե���Ȥ� howm-view-title-header �˱�����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/498>))
    * howm-view-contents-persistent �Υǥե�����ͤ� nil ���� t ���ѹ�
      * ���ƥХåե� *howmC* ���� RET �ǳ����Ƥ�, *howmS* �� *howmC* �Ͼä��ʤ�
    * �ѿ� howm-list-title �������ͤˡ�howm-keyword-search�פ��ɲ�
  * Fix
    * ��˥塼��ͽ��ɽ��,
      ��howm-menu-schedule-days-before �������� howm-menu-schedule-days ����ޤǡ�
      �ΤϤ����֡�������������֡פˤʤäƤ���
    * �����ȥ�ɽ������, �����ȥ����= �פ��ʤ�����߽�
      * �����ȥ�ɽ�������ǥե���Ȥˤ��뤿�������
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/490-493n>))
        ((<ref|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/503>))
    * ((<���������� auto-save-buffers|URL:http://homepage3.nifty.com/oatu/emacs/misc.html#asb>))
      �Ȥ�ʻ�Ѥ�,
      ��.howm-keys has changed since visited or saved. Save anyway?��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/507-509n>))
    * [2004-09-01 23:26]@ �Ȥ�������ʬ����˥塼��ɽ������ʤ��ä�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/411>))
      * ɽ�������äƤ���. �������ᤷ�ޤ���.
        ���ν񼰤��ܵ��ǥ��ݡ��Ȥ��뤫̤��ʤΤ�.
    * meadow ��, [2004-08-08]! �Ρ�!�פ� RET á���ȥ��顼 
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/384>))
      * �Ƹ��Ǥ��ʤ��ä�����, ���ˤ��褳���ľ��󤸤�ʤ�����
    * ���򳫤����Ȥ��� narrow ����, ����Ƥ���ʬ�ν�����̤�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
  * ��������
    * howm-list-migemo ��ľ�� howm-view-search ��á���ʤ��褦
    * howm-set-mode-p �� howm-set-mode ����ʬΥ
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/388>))
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?SetMode>))
    * ��ޥ������ action-lock ���������Ƥ�����䤹��. ư����Ѳ��ʤ��Ϥ�.
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?TangledToDo>))
      * �����Ѥ�, howm-reminder-regexp-XXX-pos �Υǥե�����ͤ��ҤȤĤ��줿
  * ����¾
    * �����Хåե������ƥХåե����o�פǹԤ���
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/424>))
      �Ĥ�, �񤤤Ƥߤ����ɾä��ޤ���. @ or 0 ����������פǤ����?

* [2004-08-24] v1.2
  * Note
    * �Х����� (narrowing, CVS ��ü emacs �Ǥ��忧)
    * 1.1.1��1.1.2 �α�����ǽ������� (todo ľá��, ��α��~��)
    * �ե����빽���ΰ����ѹ� (howm.el, riffle.el) �� ((<���󥹥ȡ���>))
    * ����¾���ޤ��޲��� (file:// �Ǥ⳰�� viewer)
    * ������ǽ�ɲ� (��˥塼�˺Ƕ�����������)
    * �ۤ�Ȥ��̤ܶ� come-from �� alias �ʤ������, �ޤ����Ф餯�ϱ�����ǽ
    * ��꡼��ͽ���� howm-1.2rc6 [2004-08-16]��Ʊ����ΤǤ�
  * ������ǽ�θ�����
    * �ܶ�
      * todo �ο���: ��α��[2004-01-09]~100 �ۤ���
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/497>))
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Defer>))
      * ��˥塼��ͽ��ɽ��todo ��������, -+!@~. ��� RET ��ľ��á����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/506>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/568>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/580>))
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?RepeatedToDo>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/698>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/732>))
    * ����¤
      * ����
        * ɽ���ڤ꤫�� (0,1,2,v ����)
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/826-827n>))
        * T �ǥ����ȥ�ɽ��, TAB��M-TAB �Ǽ������Υե������
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/61>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/892>))
        * ��ޥ���������դˤ�륽���ȡ��ʤ����
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/694>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/726>))
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/742>))
          * �⤦����äȶ�̣���ư��̲����������� (�ֻ��ս�ǥ�����, �Ȥ�)
        * �ʤ���ߴ��� Region, Around ���ɲ�
        * ((<�������ޥ���>))����
          * �����Хåե��ؤο��Ĥ�
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/675>))
          * �����ΤȤ����������ȥ�ɽ��
            * howm-list-recent-title, howm-list-all-title �Ϥ��Τ����ѻߤ�ͽ��
            * howm-list-title �ˤϴؿ�������ǽ
              ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/61>))
          * C-x 1 ��Ͼ���˥ե졼���ʬ�䤷�ʤ�
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/747>))
            ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/766>))
      * ����
        * foo �򸡺������� [[foo]] ���̤�
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/835>))
          * �ǥե���Ȥˤ��ޤ���. ����ʤ� (setq howm-list-prefer-wiki nil)
          * [[��]] �򥫥����ޥ���������, howm-wiki-format ��������
              (setq howm-wiki-format "((��%s��))")  ;; ((��ġ�)) ���ѹ�������
        * howm �� foo �򸡺��������, C-s C-s �� foo �θ�����
            (setq howm-view-update-search-ring t)
        * �����˻Ȥ��ؿ���((<�������ޥ���>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/166-167n>))
      * ��˥塼
        * [����] [����] ���ĥ
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/37>))
          * �����ե�����ΤȤ���, ��������ʸ����򸡺�.
            ������ե�����ΤȤ���, �������դΥե�����򳫤�.
          * �ǥե���ȤΥ�˥塼���ɲä��ޤ���
          * ��ä�����������
            ((<�̲�|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/39>))
            �����Ǥ� :-)
        * ��������� C-i �� M-C-i ���ɲ�
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
      * action-lock ��((<�������ޥ���>))
        * ��file://�ġפ��http://�ġ�
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/945>))
        * { } �� {_} ���ѹ����䤹��
          ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/828>))
          * {_} �ϥƥ�ץ졼�Ƚ񼰤�Ϣư���������������Ȼפä�, ���ٹ����Ƥޤ���
            * howm �� load ���� howm-dtime-format �������ꤹ���,
              howm-template-date-format �⤽��ˤʤ�
            * {_} �ν񼰤� howm ¦�Ǿ�񤭤��뤫��, ����̵��
                (setq action-lock-date-default '("{_}" "[%Y-%m-%d %H:%M]")) ;; á������
            * howm-action-lock-done-date-format �ʤ��
              README �˽񤤤Ƥ��Τϥ���. ��äƤޤ���Ǥ��� ^^;
      * ��ޥ����
        * !+-~. �ν��٤�((<�������ޥ���>))
          ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/84>))
  * ���ޤ��޲���
    * file:// �Ǥ�������ϳ��� viewer ��ư
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/309>))
    * ���շ�����ľá�� (��˥塼�� todo ��������)
    * ��˥塼�� %schedule, %todo, %recent, %random �� action-lock ������
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/264>))
      * ��>�פ� RET á���ȡ�|�װʹߤ򸡺�
      * �������ޥ���
        * �����ѹ�
            (set-face-foreground 'howm-menu-list-face "cyan")
        * �񼰤��ѹ�: �ѿ��򤤤��� ��
          howm-menu-list-format, howm-menu-list-regexp,
          howm-menu-list-regexp-key-pos, howm-menu-list-regexp-action-pos,
          howm-menu-list-regexp-face-pos
        * �㤷���ѹ�(���ˤ���, �ʤ�): �ؿ��򤤤��� ��
          howm-menu-list-format, howm-menu-list-rules,
          howm-menu-font-lock-rules
    * �����վ�� RET�ס֥�ޥ������� RET�פ����Ȥ���ư��򾮸�����
      * ��ʸ�����ޥ�� �� ��������ȯư (���θ�� RET ������)
      * ¿ʸ�����ޥ�� �� ³���ƥߥ˥Хåե���������
    * �������� C-u f �����Ȥ��ˤϡ�Reject�פ�ɽ��
    * ͽ��ɽ��todo ������������ɽ��
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * ʸ��ι���
      * ���󥹥ȡ���ˡ��������, make install ������ᥤ���
      * index-j.html �˲����� UNIX USER �����ؤΥ�󥯤��ɲ�
    * ��������ȯ
      * ���ƤΥե�����̾�ѹ�����
        * howm-mode-mode.el �� howm-mode.el
        * howm-mode.el �� howm.el
        * ���ٹ���, ����� (require 'howm-mode) �Ǥ�ư���褦�ˤϤʤäƤ�
      * �Ѥ�Ѥ���� riffle.el ��, howm-view.el ����ʬΥ
        * �����ѹ�[2004-07-20]
          * riffle-controller �� insert-item �� contents-item
            * ���Ƥ�ʬ�ǽ񤭽Ф��󤸤�ʤ�, ���Ƥ�ʸ������֤�
          * ��section�פ��ѻ�
      * howm-menu.el ����
        * �������ͤ��ѹ� (howm-menu-display-rules �ޤ��)
        * �ѿ����ؿ����¤٤���
      * howm-bug-report ����
        * emacs �С������ɽ����ܤ���
        * make ���ʤ��Ƥ�С�����������ɽ��
        * byte-compile, make, make test ��̵ͭ��ɽ��
      * howm2 ���� [[����]] �ν�������
        * �б����� <<< ���� ������Ϥ��ʤΤ�, ���ä��ˤޤ�����
      * hcal.rb, tag2plan �� ruby ���ץ����ˡ�ʤ���
        ((<thx|URL:http://arika.org/debian/howm_1.1.2.1-0+0.diff.gz>))
  * Fix
    * CVS ��ü emacs �ǿ����Ĥ��ʤ�
      ((<thx|URL:http://www.unix-fan.com/celt/blog/20040612.html>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/302>))
    * ��⤬ narrow ����Ƥ��, ��������ʤ��������Ǥ��ʤ��������Ƥ⸫���ʤ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/311>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/329>))
    * ��>>> �ե�����̾�פΤȤ��ϥ����ȥ�ɽ���� off
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/309>))
    * >>> ��������򳫤�������, ���Ȱ㤦�Хåե���ɽ�����줿�ꤷ�Ƥ�
    * ��<<<�׾�� RET (��Ϣ������ɰ���)��������. ���٤� ;_;
      * emacs �� grep ������ɽ���ΰ㤤���ޤ���ͥå�
      * ���ʤߤ˻��ͤ�,
        �֤��Υ�����ɤ˴ޤޤ�륭����ɤ�ޤ७����ɤΰ�����
    * ¸�ߤ��ʤ��ǥ��쥯�ȥ�� howm-directory �˻��ꤹ���,
      ���� C-c , , �ǥ��顼 (make install �������)
    * howm-from-change-log �ǥ����ȥ뤬���ԡ�����ʤ��ʤäƤ�
      (thx > 'UCONN�Υݥ��ɥ�'����)
    * howm-view-open-hook �����ٸƤФ�Ƥ�
    * ����ˤ�ä�, ���������ǡ֥����ȥ���Сפ����ƤФ�Ƥ�����
    * README ���� (howm-title-header �� howm-view-title-header)
    * Meadow �ѤΡ֥ɥ饤��̾����ʸ����ʸ�����к���
      ������(howm-normalize-file-name). ��ư���Ѳ��ʤ��Ϥ�.
      ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?DriveLetter>))
    * �ƥ����ǤΥХ�(��)
      * �֥�˥塼�� %recent�פ��оݳ��ե�����Υ����å���˺��Ƥ�
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/246>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/247-248n>))
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/255-257n>))
      * ((<���󥹥ȡ���>))�Ρ֤Ϥ���� C-c , , ���������ɤ߹���פ�����ȴ��
      * �ƥ����ǤΥС������ɽ����, �桼���� ./configure �������ˤʤäƤ�.
      * emacs -nw �ʤȤ�, [2004-08-08]! �Ρ�!�פ� RET á���ȥ��顼
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/354>))
      * �ƥե�������̤� byte-compile �����Ȥ����Զ��
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
        * �ޥ����ޥ���������Ƥʤ��ä�
          �� �ե�����ޤ����ǻȤ��ޥ���� howm-common.el ���֤�,
          �� howm-*.el ������Ū�� require

== 1.1.2.x

* [2004-05-09] v1.1.2.1 
  * fix: make test �����顼
    * �ѥå������󥰤Υߥ���, ~/sample/ �ʲ��Υե������ȴ��

* [2004-05-06] v1.1.2
  * Note
    * ����Ⱦü������, ���ä������Ф餯�ѹ��ʤ������꡼�����Ȥ�������.
      * 1.2 �Ϥޤ���. �Ǥ� 1.1.1.x �Ϥ⤦���Ƥ������ʤ�. �äƤ��ȤǤĤʤ���.
    * ������ǽ�� 1.1.1.3 ���餿�������ѹ��ʤ�.
    * 1.1.2rc1 ���������ѹ��ʤ�.
  * ������ǽ
    * alias ���, ����¾����¤
  * ����¤
    * �ְ�������ιʤꤳ�߸����פΥ����ѹ� (s �� G)
      * s �� C-c , s �ξ�ά���Ȥ��֤äƤ�
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/133-134n>))
    * ��make test�פǡ��Ǥ�����ǤΥƥ��ȡ�
    * ��˥塼��ǤΡ�%|�פˤ���ԲĻ�ȥ���������ǽ��
    * howm-view-real-grep �ϡ֥ѥ�����Υꥹ�ȡפ�����Ĥ���
        (setq howm-view-grep-file-stdin-option "-f -")
      * �ä� linux �ʳ��� grep ���Ѥ�����, ��������򤷤����Թ礬�Фʤ���,
        ��Ƥ���������Ƚ�����ޤ�.
        �����ܤ�ư��ϲ����Ѥ��ʤ��Ĥ��Ǥ�����
      * ��come-from ������ɤ� alias�פ˸��������ФǤ�
        ((<ref|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?CompoundComeFrom>))
  * fix
    * ��<<< foo {_}�פΡ�<<<�פ� RET á���Ƥ� no match
      * grep ���ѻ��Τ�? ({}���ü�ʸ��)
      * ��äĤ�����. ���ܽ����ϡ�ʣ���ѥ����� grep�פ��Ǥ��Ƥ���.
    * come-from ��󥯡�foo�׾�� RET ���Ƥ�,
      ��<<< foo bar�פ��������ɽ�����줿��
      * come-from �񼰤��ѹ����ơֹ����ޤǡפ���ʤ������Ȥ���,
        �ѿ� howm-keyword-regexp-format �����ꤷ�Ƥ�������
    * howm-keyword-case-fold-search �����Ǥ�,
      ����ɽ���� downcase ���ʤ��褦���� (\W �Ȥ���̣�Ѥ�äƤ��ޤ�����)
    * M-x howm-mode �� howm �� off �ˤ��Ƥ�����¸���褦�Ȥ���ȥ��顼
    * hcal.rb ����α���椬�Ť��ޤޤ��ä� (* �� ~)

== 1.1.1.x

* [2004-04-29] v1.1.1.3
  * fix: C-c , d RET �Ǻ��������դ����Ϥ����Ȥ�, ����˰���������Ǥ�
    (howm-insert-date-future �� nil �ΤȤ�)
    * �Ĥ��Ǥ�, howm-insert-date-future �����ꤷ��
      �����ä�����ȯư�����Ȥ���, ��å�������Ф��褦�ˤ���
  * v1.1.1.3rc1 [2004-04-14] ������Ʊ��

* [2004-04-01] v1.1.1.2
  * Note
    * 1.1.1.1 ����� bug fix �����Τ�
    * 1.1.1.2rc1 ������ѹ��Ϥ���ޤ���
  * fix: C-c , g foo �� Foo ���ҥåȤ��ʤ� (grep �Ի��ѻ�)
    ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/88>))
    * ��˥塼��ͽ�ꡦtodo �����Ρ�>�פ� RET �� no match
      �Ȥ����Х���ФƤ�������
      (howm-keyword-case-fold-search �����)
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/96>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/108>))
      thx > NARA Shinsuke ����

* [2004-03-16] v1.1.1.1
  * Note
    * v1.1.1 ����� bug fix �ǤǤ�
    * �ֱ�����ǽ�פ⾯���ɲ�
  * ������
    * migemo-server �����פ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/899>))
      * grep ���Ѥʤ�ޤ�ɬ��
        (��((<�������ޥ���>)))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
    * howm-menu-mode-map ��դĤ��˥��������ǽ�� (buffer local ����ʤ�����)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/909>))
    * action-lock-set-rules �Ͼ���� remove-duplicates
  * fix
    * (setq howm-list-title t) ���Ƥ� C-c , l �ǥ����ȥ뤬�Фʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/924>))
    * �������Ϥ� 01230 �� 20001230 ����ʤ� 1230 �Ȳ�ᤵ��Ƥ�
    * hcal.rb �Ǥ� ~ �ϻ������ޤ���������褦
    * ���ƥХåե��� C-t, M-C-t �� C-i, M-C-i
    * ��<<< [foo�פǡ�[foo�פ�á���ȥ��顼 (regexp-quote ��˺��)
      * [2004-02-23] �ν����Ϥޤ����äƤ��ΤǺƽ���
    * emacs20, meadow1.x(?) �� http:// �˲������Ĥ��ʤ�
      (��ĥ����ɽ�� "[htp]\\{3,5\\}")
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * grep �Ի��ѻ�, S �� foo �ǡ�Foo�פ��Ҥä�����ʤ�
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/51-54n>))
    * README �ν���
      * todo ɽ������Υ������ޥ���ˡ���ɵ�
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/69>))
      * {_} �Υ������ޥ���ˡ����������
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/79-82n>))

* [2004-02-21] v1.1.1
  * Note
    * ���̤Τ��ޤ��޲���
    * �̲ܶ�¤(��α, todo ľá��, �����������ɱ���)��,
      �ޤ��ֱ�����ǽ�פäƤ��Ȥ�
    * v1.1.0.* ����, ���ΤޤްܹԤǤ���Ĥ��. ���ѹ��ʤɤʤ�.
    * v1.1.1rc3 ������Ʊ��
  * ������ (��((<�������ޥ���>)))
    * todo �� RET ��ȯ�ǺѤ� (howm-action-lock-reminder-done-default)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/698>))
    * ���ƥХåե��˿��Ĥ� (howm-view-contents-font-lock-keywords)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/665>))
      * �ۤ�Ȥϥե�������Ƭ�ȳ�ĥ�Ҥ��鼫ưȽ�ꤹ�٤��ʤ������, �ҤȤޤ�.
    * �����Υ����ȴ�� (howm-list-normalizer)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/503>))
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?SortBy>))
    * ��˥塼��ͽ��ɽ��ɽ���ϰ� (howm-menu-schedule-days-before)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * ���դο������ϻ���ǯ����ά������̤��Ȳ�� (howm-insert-date-future)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/425>))
    * howm-template �������ǽ��
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?SwitchTemplate>))
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
    * (setq howm-list-title t) �����, �����ǤϾ�˥����ȥ��ɽ��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/288>))
      * howm-list-recent-title, howm-list-all-title �� undocumented ��.
        ���Τ����ѻߤΤĤ��.
    * û�̥ե�����̾������� howm-abbreviate-file-name �˰��ܲ�.
      �Ĥ��Ǥ� howm-keyword-to-kill-ring ��û�̥ե�����̾��.
    * ������ɰ������֤���� (howm-keyword-file)
    * howm-create-file-hook ���ɲ�
    * ChangeLog ���Ȥ�ʻ�� (��CL-HOWM.ja.rd)
      (thx > 'UCONN�Υݥ��ɥ�'����)
  * �����ޥ��
    * C-c , s �Ǹ���ʸ���󸡺� howm-list-grep-fixed (C-u C-c , g ��Ʊ��)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?KeyBinding>))
    * C-c , SPC �� howm �ʥХåե��� howm �Ǥʤ��Хåե��Ȥ��ڤ��ؤ�.
      howm �ʥХåե����ʤ���Х�˥塼�򳫤�.
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Idea>))
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?ImplementedIdea>))
    * ����ɽ��������ɽ���Ǥ� n, p �Ǥ⥫������岼
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/473>))
    * ��file://�ġ׾�� C-u RET �� find-file-other-window
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/449>))
    * [����] [����] �ޤ�� (������ե����������)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/452>))
      * howm-open-today ��, �����������˥ƥ�ץ졼������
      * M-x howm-open-past �Ǻ�����,
        C-u 3 M-x howm-open-past �� 3 �����򳫤� 
  * �ѹ�
    * �����Хåե������ƥХåե�
      * ���Ȥ������ʤ�(��ɾ�ʤ��ᤷ�ޤ�). ((<�������ޥ���>))�⻲��.
      * q ������, �ե졼��ʬ��������
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/359>))
        * �ָ��ξ��֤������פ� give up �Ǥ�.
          �����ʤ���. ����Ӥȵ���Ǥ�, ���ܲ����񤷤�����
          ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/194>))
          ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/415-416n>))
      * sort-by-date ��ֿ�������Τ���פ��ѹ�
      * ����ɽ������ @ ��Ϣ��ɽ�����ڤ꤫������, window ��ʬ�����
      * Ϣ��ɽ����, �ƥ��κǸ�ζ��Ԥ�ʤ�
      * ��>>> �ե�����̾�פ� RET��2 ������, window ʬ����֤�����
    * ��˥塼
      * > �� RET �� �����Ԥ�ľ������
      * �ֺѤߡפ�ɽ�����ʤ�
        ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
      * ������Υ�ޥ������ɽ�����ʤ� (howm-menu-todo-priority)
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/620>))
      * ��˥塼�Υ���å�������ꤷ�ʤ��¤�, �����¸���Υ�˥塼�����ϥ���
      * schedule, todo ���ɽ�ꡦ���������ɲ�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/428-430n>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/432>))
        * %sdays (ͽ��ɽ��ɽ������), %tnum (todo ��ɽ�����)��
    * howm2
      * �������˥����ȥ��ɽ��
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/317>))
      * howm2 -r �ǥե����������ս��
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/334>))
    * autoconf, automake �����
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/304-307n>))
      * navi2ch �������Τ餺�˥��ԡ�. �褯�狼��ʤ��ޤޤ�����.
        * doc/releng.txt �����: tar.gz ����ˤ� (��ȯ����)
            aclocal && autoconf && automake && make Makefiles && make dist elcdist
      * ����: ((<URL:http://shimaki-hp.hp.infoseek.co.jp/autoconf/book1.html>))
      * �ǥե���ȤΥ��󥹥ȡ����褬 /usr/share/emacs/site-lisp/howm
        �ˤʤ�褦��
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/324>))
      * make install �� doc, ext, en, ja �⥤�󥹥ȡ���
        * ��ʬ�ѥ��
            make maintainer-clean; aclocal && automake && autoconf && ./configure --prefix=/tmp/hoge --with-lispdir=/tmp/hoge/el && make && make install
      * ���ư���� howm-directory ���˥塼�ե����뤬�ʤ���м�ư�Ǻ��
        (make install �����Ȥ��Τ�)
    * ����¾
      * auto-save-buffers ���ƤФ줿�� save ���μ�ư������ off ��
      * ̤��¸���Խ���, ����������ɽ����ȿ��
        (howm-view-use-grep �� nil �ΤȤ��˸¤�)
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/482>))
      * Wiki ����� [[hoge]] �� hoge �˲��� (��<<< hoge�פ�̵ͭ�ˤ�����餺)
        (��((<�������ޥ���>)))
        ((<thx|URL:http://tiki.is.os-omicron.org/tiki.cgi?c=v&p=howm>))
        ((<thx|URL:http://pc2.2ch.net/test/read.cgi/win/1067394259/341-347n>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/672-673n>))
        * ����ϡ�]]�פ����˲���.
          �ֺ����Ѥߤ��ɤ��������ΤĤ����Ƕ��̤Ǥ���פäƤ���
          �Ȥ�����ä��������, �Ϥޤä�����ʣ��.
      * ttp:// ����� http:// ��ľ���ƥ֥饦����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/676>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/680>))
      * howm-toggle-search-other-dir ��, ͽ��ɽ�� todo �ꥹ�Ȥθ����ϰϤ����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/623>))
      * ((<���󥹥ȡ���>))�������˥Х��ȥ���ѥ�����ɲ�
      * howm-reminder-tomorrow-face ���طʤ��㿧����ԥ󥯤�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/805-807n>))
    * �桼�����ˤϴط��ʤ�
      * �ե����빽�����ѹ� (��((<�����ˤĤ���>)))
      * howm-menu-action �ΰ������ͤ��ѹ�
      * copy-list �� copy-sequence (cl �ѥå����� �� built-in)
  * fix
    * URL ����Ф����äȤ����ޤ���
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/672-673n>))
    * �����Хåե��Τ���ʽ񤭤ʤ���������
      * howm-list-normalize �������˲��٤� howm-view-summary ���ƤФ�ơ�
      * �����Ф�Ҥɤ��Ȥ���������. �ޤ�����ϻĤäƤ�.
    * meadow ������, �ɥ饤�֥쥿������ʸ����ʸ����̵��
      (howm-normalize-file-name)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?DriveLetter>))
    * �Ѹ��˥塼�� [String] �� [Regexp] ����
    * xemacs canna �к�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?CannaReturnKey>))
    * split-horizontally �� t �ʤ� C-x 1 ��ⲣ�¤٤�
      (((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/634>))
      > ��(����`)�Τ���)
    * howm-menu-{schedule|todo} �Υ�å�����,
      howm-view-call-process ������ͥ����å�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/626>))
    * grep �Ի��ѻ��� migemo ���� (howm-list-migemo)
      (thx > ��ޤ������餵�� akira@arika.org)
    * meadow 1.15 ��¾�Хåե��� font-lock ����ᡦʸ����Τߤ�
      * font-lock-defaults ������ͤ��Ѥ��Ƥ��ޤäƤ�
    * ������ ! (shell) �� xemacs �� emacs20 �ǥ��顼
      (replace-regexp-in-string �� (buffer-size buf) ���ʤ�)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Usage>))
    * xemacs ��Ϣ:
      thx > ((<�޸�����|URL:http://www.nc.kyushu-u.ac.jp/~kasahara/diary/2004/01b.html>))
      �Ĥ��ä�������֤� :p
      * ��(setq font-lock-verbose nil) ��®���ʤ��� (��((<���󥹥ȡ���>)))
        * howm �ʥХåե��Ǥϼ�ư�Ǥ����ĤȻ�ߤ�����, �Ǥ��Ƥʤ���
      * xemacs �Ǥ� byte-compile
        * defvar �� make-variable-buffer-local �������
          * action-locl.el:
            action-lock-rules,
            action-lock-original-font-lock-keywords,
            action-lock-original-return
          * howm-view.el:
            howm-view-name,
            howm-view-item-list,
            howm-view-summary-last-line,
            howm-view-contents-end
        * �Ĥ��Ǥ��ޥߥ���¿��Ϫ��
          (reference to free variable / bound but not referenced)
    * ��>>>�פ����ιԤ��鼡�ιԤˤ����Ʋ�����������Ƥ�
    * howm-menu.el �� iso-2022-7bit �� (for Meadow)
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
    * .gz, .jpg, .gif, .png �⸡���оݤ������
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?ImageCapture>))
    * �ǽ��Ԥ˲��Ԥ��ʤ���, ����ɽ���Ǥ��ιԤ��Фʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/471>))
    * ��ե�����ʣ������ (setq howm-list-title t) �ʤȤ�,
      �����ǡ�S �� date�פ򤯤��֤��ȥ����Ƚ礬���줫���
      (thx > NAKANO Yasuhiro ���� <nkyasu@yahoo.co.jp>)
    * �����Ķ���, global-font-lock �������ʤ��ʤ�
      (emacs-20.7.2 on Vine Linux 2.6, Meadow1.15 on WindowsXP Professional).
      (thx > NAKANO Yasuhiro ���� <nkyasu@yahoo.co.jp>)
    * ����ɽ���Υե�����̾��Υǥե������
      (thx > Jun Kuriyama ���� <kuriyama@imgsrc.co.jp>)
    * Jargon �Υ���ڤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/420>))
    * ������ź��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/421>))
      * skip-one-link �� action-lock-skip-one-link
      * *.el ����Ƭ���������귿������
    * ((<mcomplete.el|URL:http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el>))
      �Ȥ�ʻ�Ѥǥ��顼
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/365>))
    * ��<<<�פ� RET �����Ȥ��ˤ� howm-keyword-case-fold-search ��ȿ��
    * (setq howm-list-title t) �����, ���������Ƥξ岼����ž
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/348>))
    * �Х��ȥ���ѥ�����ηٹ��ۤ��༣
    * howm-keyword-regexp-header-pos �� howm-keyword-regexp-hilit-pos
    * howm2 �Υǥե���Ȥ�, �ե����� #foo.bar �� foo.bar~ ���оݳ���
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/316-317n>))
    * ruby 1.8 �Ϥ� howm2 �����顼
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/308-310n>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/317-318n>))
    * ���طʤऱ���ۿ�
    * �������뤬�����ȥ��ˤ����
      C-c , K (howm-keyword-to-kill-ring) �˼���
    * CVS ��ü emacs ���� on-the-fly �ǿ����Ĥ��ʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/268>))
    * ������ ! �ˤ�� shell command �ν��Ϥ������ä��Ƥ��ޤ�
      * �Ĥ��Ǥ�, ���Ϥ����ΤȤ��Ͻ��ϥХåե���Ф��ʤ��褦�ˤ���
      * �Ĥ��Ǥ�, �����û�̥ե�����̾��
    * Ϣ��ɽ���� TAB ��á���Ƥ��� RET á���ȥ��顼
    * CVS ��ü emacs ��, ���������ƥХåե��˿����Ĥ��ʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/259>))
    * howm-search-other-dir ���������ɲ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/260>))
  * �ƥ����Ǥ�����ѹ� (��)
    * fix: (setq howm-insert-date-future t) �ξ��֤�,
      [2004-01-23]�� C-c , d ���ơ�215�� �� 2005 ǯ��
      (howm-datestr-expand)
      * �ƥ����� 2003-12-27 �� 2004-01-21 �ΥХ�
      * �ƥ����� 2005-01-23 �ǽ���
    * yc �к������� �� ����ѻ�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
      * eval-after-load ���� (load "~/elisp/yc.el") ���ޥå����ʤ��Τ�,
        defadvice ���ѹ�
      * �ۤ�Ȥ�, Ʊ�ͤΤ��Ȥ� egg, anthy �ˤ���٤�
        (patch �罸. howm-misc.el ����) �� ��äѤ��[2004-01-18]
    * ��˥塼�Ǥ� [2004-01-10]! ���˲�����Ĥ��ʤ� �� ��ľá���פȤ�������
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/506>))
    * fix: ��˥塼�˽��٤�ɽ�������� > �������٤ʤ��ʤäƤ�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * fix: anthy �Ȥ�ʻ�� �� ����ѻ�
      (thx > Jun Kuriyama ���� <kuriyama@imgsrc.co.jp>)
    * fix: index-j.html ����Υ�󥯤���
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/361>))
    * fix: �ɥ�����ȤΥ��󥹥ȡ�����
      /usr/local/share/doc/howm �� /usr/local/share/howm/doc
    * fix: README �Υߥ�. ���������� C-c, l ����ʤ� C-c , a
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/347>))
    * fix: ���ɸ�� howm-keyword-file �򥻥åȤ������Ǥ�,
      ���ư���˼�ư����
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/270>))
    * fix: ((<yc.el|URL:http://www.ceres.dti.ne.jp/~knak/yc.html>))
      �ǡֳ���פ� RET ������ howm ������
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/274>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/283-284n>))
    * C-c , d �� d ������������ �� �ѻ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/595>))
      ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/597-598n>))
      * �Ĥ��Ǥ� prompt ������
    * ((<�������ޥ���>)) �ε��ҥߥ�: howm-directory �� howm-keyword-file
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?Comment>))
    * configure.in �� Meadow.exe ���ɲ�
      (((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?RoadMap>))
      > Meadow2�Ȥ�����)
  * �ѻ�
    * canna, egg, yc, anthy �θ����к�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?WorkOn>))
      * ������, action-lock-mode ��ͥ���٤�Ǽ��.
      * �������к����᤹�ˤ�,
          (setq action-lock-bury-minor-mode-p nil)
          (setq howm-ime-fix t)
    * C-u C-c , c (�������ˡ�>>> ���ե�����פ�����ʤ�)��
      howm-template-use-file
    * howm-view-quit-to-nonhowm (������ q �� howm �ʳ��� buffer ��)
    * howm-template-file-abbrev, howm-view-header-abbrev
      (howm-abbreviate-file-name �˰��ܲ�)

== 1.1.0.x

* [2004-02-08] v1.1.0.4
  �ƥ����Ǥ���� backport (thx > ���� �ĥƥ����� README ����)
  * ������ι��ܤϥ�˥塼�� todo �ꥹ�Ȥ�ɽ�����ʤ�
  * C-c , s �Ǹ���ʸ���󸡺� howm-list-grep-fixed (C-u C-c , g ��Ʊ��)
  * canna, egg, yc, anthy �θ����к����ѻ�. ������ȴ���к�.
  * ��˥塼�Υ���å�������ꤷ�ʤ��¤�, �����¸���Υ�˥塼�����ϥ���
  * auto-save-buffers ���ƤФ줿�� save ���μ�ư������ off ��
  * .jpg, .gif, .png �⸡���оݤ������
  * ��xemacs �Ǥ�, (setq font-lock-verbose nil) ��®���ʤ��� (��((<���󥹥ȡ���>)))
  * fix
    * xemacs canna �к�
      ((<thx|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?CannaReturnKey>))
    * �Ѹ��˥塼�� [String] �� [Regexp] ����
    * howm-view-call-process ������ͥ����å�
    * meadow 1.15 ��¾�Хåե��� font-lock ����ᡦʸ����Τߤ�
    * ������ ! (shell) �� xemacs, emacs20 �ǥ��顼
    * xemacs �Ǥ� byte-compile
    * ��>>>�פ����ιԤ��鼡�ιԤˤ����Ʋ�����������Ƥ�
    * howm-menu.el �� iso-2022-7bit �� (for Meadow)
    * CVS ��ü emacs ���� on-the-fly �ǿ����Ĥ��ʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/268>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/501>))
    * �ǽ��Ԥ˲��Ԥ��ʤ����, ���ιԤ�����ɽ���˽Фʤ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/482>))
* [2003-12-31] v1.1.0.3
  * fix: �����Ķ���, global-font-lock �������ʤ��ʤ�
    (emacs-20.7.2 on Vine Linux 2.6, Meadow1.15 on WindowsXP Professional).
    (thx > NAKANO Yasuhiro ���� <nkyasu@yahoo.co.jp>)
  * fix: ����ɽ���Υե�����̾��Υǥե������
    (thx > Jun Kuriyama ���� <kuriyama@imgsrc.co.jp>)
* [2003-12-29] v1.1.0.2
  * fix: anthy �Ȥ�ʻ��
    (thx > Jun Kuriyama ���� <kuriyama@imgsrc.co.jp>)
  * ��˥塼�� schedule��todo ��ɽ����������ɲ�
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/428-432n>))
* [2003-12-27] v1.1.0.1
  * fix: Ϣ��ɽ���� TAB ��á���Ƥ��� RET á���ȥ��顼
  * fix: ���طʤऱ���ۿ�
  * fix: egg, yc, mcomplete �Ȥ�ʻ��
  * fix: Jargon �Υ���ڤ�
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/420>))
* [2003-11-22] v1.1
  * Note
    * ��ߴ����ѹ���ޤȤ�Ƥ�äƤ��ޤ�����
    * ���ץ�����, ����ɤ���ˤ�Ȥ���褦�ˤ����Ĥ��
    * ��((<���󥹥ȡ���>))�פΰܹ���򻲾�
  * �ǥե���Ƚ񼰤��ѹ�
    * ��󥯽񼰤� <<, >> ���� <<<, >>> ���ѹ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/173>))
      * �б�����, hcal.rb �Ρֺ����ޡ����פ� <<<<##>>>> ���ѹ�
    * 1 ��� 1 �ե������ǥե���Ȥ�
    * ��ޥ�����ν��ѹ� (��((<��ޥ����>)))
    * ���ս񼰤� [2003/10/21] ���� [2003-10-21] ���ѹ�
      �� ((<����|URL:http://www.kanzaki.com/docs/html/dtf.html>))
    * ��˥塼���������
      * ��%����%�פ򸡺�������, ��<<< %����%�פ� menu-mode �ǳ���
      * C-c , , �ǡ�<<< %menu%�פ򳫤� (menu.howm �ǤϤʤ�)
  * ����
    * <<< �Ǥ���ʸ����ʸ���ζ��� (��((<�������ޥ���>)))
    * <<< �ʤɤ˿��Ĥ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/133>))
    * �ǥե���ȤΥ�˥塼�˥��硼�ȥ��åȥ������ɲ�
    * ���շ�����2003-10-30�פ� action-lock �ˡ֤����֤��פ��ɲ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/224>))
    * XEmacs, CVS ��ü Emacs, Carbon Emacs ���б�(?)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/209>))
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/229>))
      * '��'����� patch ������ߤޤ���
    * howm2
      * <<< �� >>> ������������. ���٤ˤʤä� ;_;
      * �оݥե���������̲�ǽ�� (-list)
      * ���ץ�����ɲ� (-goto, -comefrom, -i)
    * ((<ChangeLog Memo �Ȥ�ʻ��|URL:CL-HOWM.ja.rd>))
  * ���ѹ�
    * C-c , d �� C-c , D �����줫��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
    * �������ե�����ʬ�� (howm-font-lock.el, howm-menu.el)
    * �ǥե���Ȥ� major-mode �� text-mode ��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/181>))
    * [����] �˥�����ꤢ�� (C-c , A)
    * ����ɽ���ȥƥ�ץ졼�ȤΥե�����̾�� /home/hoge/�� ����ʤ� ~/�� ��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/228-229n>))
    * ����������ľ��� undo ����С�>>> ���ե�����פ�ä���
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/230-235n>))
  * ���ѹ�
    * face ���ѿ��ǻ���
    * howm-menu-lang �����ꤷ�ʤ����, �����ä����˿�¬
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/121-123n>))
    * howm-menu-refresh-after-save, howm-refresh-after-save ��ư����ѹ��Ĥ�
    * ��˥塼�� "...done" ��å�������ä����� (schedule, todo �Υ�������)
    * howm2 �� index.html ��, ��������ʤ��ե�����̾�����
    * ��˥塼æ�л��� bury-buffer ���ѻ�.
    * howm-ignore-comefrom-same ���ѻ�
    * ������ǽ: howm-view-{summary|contents}-persistent �˴ؿ��������ǽ
        ;; ��: ͽ��ɽ��todo �ꥹ�Ȥ��������������� persistent
        (setq howm-view-summary-persistent
              (lambda () (member howm-view-name '("{schedule}" "{todo}" ""))))
      * ����������ǽ������
    * �ѿ�̾ howm-reminder-regexp-date-pos �� ��-day-pos
    * howm-template-{file|date}-format �Υǥե�����ͤ�
      howm-ref-header, howm-date-format �򻲾Ȥ���
    * *.elc �⸡���оݳ�
    * �ɥ������
      * ��((<ư���ޤ����?>))�פ˥����å��ꥹ�Ȥ��ɲ�
      * ��((<���󥹥ȡ���>))�פ���������ɲ�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
      * ��˥塼�� ~/howm/ja/*.howm ����ʤ� ~/howm/*.howm
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/122>))
      * �����꡼�󥷥�åȤ򥫥顼�Ǥ�
      * goto, come-from �ν��ѹ���� RD ����
        (��((<�������ޥ���>)))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/174>))
      * ��˥塼�Хåե���������ˡ��Ҳ� (��((<�������ޥ���>)))
      * �¤ϥ��󥽡���Ǥ�Ȥ��Ƥ�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/167>))
      * <<< �� RD �� include �Ȥ��֤äƤ�ݤ����
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/198-205n>))
        (��((<���󥹥ȡ���>)))
      * �������ޥ�����ˤ��ޤ����ɲ�
      * �֥��󥹥ȡ���פΰܹ����, GNU touch �ˤ��ʤ����Υ��ƥ��Ǥ��ɲ�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/190>))
      * ((<�������ޥ���>))�ˡ֥�˥塼���ⰷ�����ʤ������������
      * yen.el �ξҲ� (��((<�����ġ���>)))
      * tag2plan ���������� (-date_sep, ~/howm/*/*/*.howm)
      * �ֻ��͡פ��·ϡ�Ķ�¤��ɲ�
  * fix
    * grep ���ѻ��ϡ�-hoge�פ������Ǥ��ʤ��ä�
    * ���ƥХåե��� font-lock (������)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/128-136n>))
      * v1.0.4.1 �ΥХ�: ����ʸ����ο�
      * [2003-10-05] �ΥХ�: �ե�������ڤ�ο�
    * ���������餵��˰�������ɽ��������� q ������, �����������
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/149-150n>))
      * �֥ե졼��ʬ��������פȤ��ޤꤢ���˼����ʤ�.
        �⤷�Զ�礬�Ф���, ���ǽ���ư��(�������ΥХåե������)��.
          (setq howm-view-quit-to-nonhowm t)
    * [����] �μ¹�ľ������ƥХåե�
    * filter-by-date ���ǡ�2003 ǯ 9 �� * ���ޤǡפ����Ϥ�����,
        ��2003 ǯ 9 �� 31 �� (= 10 �� 2 ��)�ޤǡפȲ�ᤵ��Ƥ�
      * ����ʳ��ˤ�Х��ޤߤ���ä�[2003-10-26]
    * �����Хåե������ƥХåե��ΰ��֤����줫���ɾ�
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/193>))
      * �Ƹ�ˡ: ���ƥХåե������˥��������֤���, g �Ǹ��� �� q
    * �����ʾ嵯ư��³�����, ��˥塼(ͽ�ꡦtodo)�������������ο��Ĥ�����
    * mailcap.el ���ʤ��Ȥ��� >>> /etc
    * >>> �θ�˲���񤤤Ƥʤ��Ȥ���̵�뤹�٤�
    * ����ɽ�� [^��] �� [^\r\n��] �ˤҤȤȤ���ľ��
  * �ƥ����Ǥ�����ѹ���fix
    * font-lock ���ɤ�ɤ�Ť��ʤäƤ�. �褯����ʤΤ�ư���Ƥ��ʤ���
    * fix: xemacs �� font-lock ���Ǥޤ�
      ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/248-249n>))
    * ��äѤ�ǥե���Ȥϡ֥����ȥ�ɽ���ʤ���
    * �ܹ���ε쥭�����꤬, howm �������Ǥ��Ǥ�ȿ�Ǥ����褦��
    * ��˥塼�������ʤ��ä� (.howm-keys �ˡ�%menu%���ɲ�)
    * ��˥塼�������Хåե��ΤȤ�, �������Ĥ��ʤ��ä�
    * ��˥塼�� footer ���ǥե���ȤǤϽФʤ��ʤäƤ�
    * ��˥塼�� %eval% �� %call% �ǥХåե������ڤ꤫��
    * �ѿ� howm-menu-persistent ���ѻ�
    * howm-reminder-today-face ��̤������ä�
    * {_} �� RET �����Ȥ���, ���ꤷ�����ս񼰤�
    * ��<<< http�פ����äƤ� http://�� �ϥ֥饦����ư���٤�
    * howm2 �� URL ���󥯤ˤ���Τ�˺��Ƥ�
      * howm2 -type=rd �� URL ���󥯤ˤ���Τ�˺��Ƥ�
    * [����] �ܥ���ϸ��ΥХåե������
    * ��>>> /foo/bar/�פΰ������Ф������ˤʤäƤ�
    * xemacs �� mailcap.el ���ʤ��Ȥ��Υ��顼����
      * howm-try-require �μ����򤹤ä���
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/219>))
    * ~/howm/CVS/* �������оݤˤʤäƤ���
    * howm-keyword-case-fold-search �����ꤷ����,
      ��������䴰�Ǥ���ʸ����ʸ����̵��
    * �ǥե���ȤΥե�����̾���ѹ� (�����ʤ���)
      * 20031030-202259.howm �� 2003-10-30-202259.howm
      * �Ѵ����
        * ���ǥ��쥯�ȥ�� cd ����,
            find . -name '*.howm' -print | ruby -ne '$_.chop!; d = File::dirname $_; f = File::basename($_).sub(/^(\d{4})(\d{2})(\d{2})/){|s| "#$1-#$2-#$3"}; puts %~mv #$_ #{File::expand_path f, d}~' > ~/howm_kuzu
        * ~/howm_kuzu �����Ƥ��ǧ��, ����ʤ����
            cat ~/howm_kuzu | /bin/sh
        * ɬ�פʤ�, �ܹ���Τ褦�˥����ॹ����פ�Ǥä�����
    * C-u C-c , c �ʤ�, �������ˡ�>>> ���ե�����פ����줺, �����ȥ�����
      (�ѿ� howm-template-use-file �ǥ������ޥ���)
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/230>))
      �� undocumented. ���Τ����ѻ�ͽ��.
    * ���ޤ��� fix
      * >>> �ޤ��
        * ���� viewer ����� off �ˤʤäƤ�
        * howm-keyword-case-fold-search �����ꤷ���Ȥ��� >>> /etc/X11
      * ���շ�����2003-10-30�פ� action-lock ����� search �оݥե�����
      * <<< �� >>> �Ƕ����ޤ�ʸ���󤬸����Ǥ��ʤ��ʤäƤ�
      * �����ȥ뤬��˶���ˤʤäƤ�
      * howm-keyword-case-fold-search �����ꤷ����,
        >>> foo �Ǥ��<<< FOO�פ���Ƭ�ˤ���٤�
      * C-c , c �ǥ��顼
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/241-242n>))

== 1.0.x

* [2003-10-27] v1.0.4.2
  * fix: font-lock ���ɤ�ɤ�Ť��ʤäƤ�. �褯����ʤΤ�ư���Ƥ��ʤ���
* [2003-10-04] v1.0.4.1
  * fix: �ֺǶ�פ����֥ǥ��쥯�ȥ������å����ʤ��ʤäƤ�
  * fix: sort-by-date �Υǥե���Ȥ�ֿ�������פ� (�����Ϥ������ä�����?)
  * ���硼�ȥ��åȤ�¿������ˤϷٹ�ɽ��
    * �Ĥ��Ǥ˥�˥塼�� "...done" ��å��������ѻ�
  * TUTORIAL.rd �ˡ����դ��������ɲ�
* [2003-10-02] v1.0.4
  * >> �ǳ��� viewer (��((<�����ɤ⤦>)),((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/54-57>))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/107-112n>))
    * �ǥ��쥯�ȥ���б�. ��>> /usr/src�פȤ�.
  * ������ꤢ��������
    * (�����⡼��) ��ԥ�������: n,p �� j,k
    * (��˥塼) ��������: SPC, BS
    * �����ȥ�� kill-ring ��: C-c , k �� C-c , K
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/67>))
    * fix: contents-mode �� C-h ��̤������ä�
  * howm-mode �ʥХåե��� save ����, ���������ľ��, ��˥塼�⹹��
    (��((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/98>))
  * �����оݥǥ��쥯�ȥ���ɲ� (��((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/69-72n>))
  * �ɥåȥե�����ϸ������ʤ�
    (>> �� howm-search-path ������Ū�˻��ꤹ���õ��)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/74>))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/86>))
  * ��˥塼����˶���
    (��((<URL:ja/menu_edit.howm>)))
    * HyperCard �� Um4 �˻ɷ㤵��ޤ���. �ۤ�Ȥ��̥ġ���ˤ��٤�.
  * ���Хåե��Υ��ԡ��� howm-mode ��ɽ��(M-x howm-show-buffer-as-howm)
    ((<ref|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/71>))
    * ���������ʤΤ��ͻҸ�
  * action-lock �� "...Done" ��å��������ѻ�
  * fix
    * ��>> �ե�����̾�פ� howm-excluded-file-regexp �˴ؤ�餺�����褦��
    * call-process �� STDERR �ϼΤƤ�
    * action-lock �������� file://�� ���ɲ�
    * ������ window ʬ������˴ؤ�餺, �������, ���Ƥ򲼤�ɽ��
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/90>))
    * sort �� filter �˥ե�����̾��Ȥ��ݤ�, �ǥ��쥯�ȥ���ʬ�����
    * howm-menu-refresh �� M-x �䥭����ꤢ�Ƥ���Ƥ٤�褦��
    * �ƥ����ǤΥХ�
      * ����ɽ������ľ������Ƥ�ɽ������ʤ�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/59-64n>))
      * Wiki �� link [[����]] �����顼
      * [��˥塼����] �����顼
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/95>))
      * >> �ǳ����ե����뤬��Ƭ�˽Фʤ��ʤäƤ���
  * �ֻ��͡פ�����
  * �ʲ�������Ѥߤ�����, v1.1 �ޤǿ������Ĥ��
    * ��ޥ�����ν��ѹ�
    * ��˥塼�ȥ�������
* [2003-09-23] ȿ�ʤ��ƥƥ����Ǥ�ʬΥ
  * ��������Ƥ���ǽ�ϤϤʤ��Τǡ�
    * ��꡼���Ǥ�����
    * �ƥ����Ǥϱ�θ�ʤ�����
    * �ƥ����Ǥι���������夤����,
      ���ξ��֤˥С�������ֹ�Ĥ���, ��꡼���ǤäƤ��Ȥˤ���
* [2003/09/23] v1.0.3.4
  * ���շ��� [2003/09/21] �� action-lock ���ĥ (��((<����񤳤�>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/49-51n>))
  * fix: howm2 �Υ�󥯰��������Хѥ���
  * fix: template �ޤ��
    * ���Υ��Ȥδ֤˶��Ԥ������
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/53>))
    * ���ڥ�ߥ� cursol �� cursor
    * ����� template �� %xxx �֤�������, ���Υ��ˤ�ڤ�Ǥ���
  * fix: �ּ�(��)�Υ�󥯤ءפ���
  * fix: canna �к���, ���ɽ���˴ؤ�餺ͭ����
* [2003/09/21] v1.0.2.3
  * fix: ���Ƶ�ư�����Ȥ��� ~/.howm-keys �����
  * fix: canna �к�
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/45>))
* [2003/09/20] v1.0.2.2
  * HTML ��������ץ� howm2 �Ǥä������� (��((<�����ġ���>)))
  * fix: ��!�פ���ȥХåե�̾����
* [2003/09/18] v1.0.2
  * HTML ��������ץ� howm2 �λ�� (��((<�����ġ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/35>))
  * ̤��¸�����ȰѺٹ��鷺, howm-mode �ʥХåե��򤹤٤ƶ���������륳�ޥ��
    (��((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/36>))
  * �֤䤵���� Emacs-Lisp �ֺ¡פ� URL ����.
    ���� howm ������ΤϤ����ܤΤ������Ǥ�.
* [2003/09/17] v1.0.1
  * �����⡼�ɤǡ�!�� �� shell �ǥ��ޥ�ɼ¹� (��((<�����ɤ⤦>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/501>))
    * Dired-X �Ǥ��ޤ�ƨ�����Ĥ����ä��������, ��ɾ�ʤΤ�
    * lisper �����, �׵���ͤξ��Ԥ��ʤ��� :-p
* [2003/09/17] v1.0.0.1
  * Dired-X �ˤĤ��ƺ���¤�����
  * fix: Dired-X ���оݥХåե�������
* [2003/09/17] v1.0
  * ���ǥ��쥯�ȥ���ز�: ~/howm/ǯ/��/ǯ_��_��.howm
    * ����ʤդ��˥ǥ��쥯�ȥ�����Ǥ���褦�ˤ���
        (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm")
    * �ܹԤˤĤ��Ƥ�, ((<���󥹥ȡ���>))�Ρ���ա�

== 0.9.x

* [2003/09/16] v0.9.9
  * ��˥塼 (��((<����񤳤�>)))
    * ����Υ��ΰ���
    * ��������­
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/494>))
  * �����⡼�ɤǡ�X�� �� Dired-X ��ư���ƥե��������
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/496>))
  * howm-mode �ʥХåե��򤹤٤ƾä�
    * fix: window �ξ��֤�����
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/497>))
    * �ɤ�ʥ⡼�ɤǤ� C-c , Q ������
      ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/498>))
* [2003/09/16] v0.9.8.3
  * fix: �����ȥ�����ǥ��顼
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/490>))
    * pure elisp �ǤΤȤ�
    * �����ȥ뤬��Ĥ�ʤ��ä��Ȥ�
  * menu.howm ������˽Ф��ʤ���ˡ (��((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/491>))
* [2003/09/16] v0.9.8.2
  * ��˥塼��ե�����ˤ����Խ���侩 (��((<�������ޥ���>)))
    * Ʊ���� ja/menu.howm �� ~/howm/ �˥��ԡ����Ƥ�������
  * ((<URL:TUTORIAL.ja.rd>))
  * �ǥե���Ȥ��ѹ� (��((<�������ޥ���>)))
    * grep �Ȥ�ʤ� (�Х��Ф�����)
    * ��˥塼�Υ���å���ϥ���
    * �ե�����򳫤���, ������Ĥ�
  * �ڤ���Ϣ�٤Ǥ��� :-)
* [2003/09/16] v0.9.7.1
  * Wiki ����� [[�ۤ�]] (��((<����񤳤�>)))
  * ��<<�׾�ǥ꥿����á���ȡִ�Ϣ������ɡפؤΥ�� (��((<����񤳤�>)))
  * �Ť�������Ͽ�Ȱܹ�����ư (��((<URL:OLD.rd>)))
  * howm-ignore-comefrom-same �� undocumented ��. ���Τ����ѻ�?
  * fix: ����ɽ���ˤ���������ȿ��
* [2003/09/15] v0.9.6
  * ��n�פȡ�p�פǰ��ñ�̤Υ�������
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/485>))
* [2003/09/15] v0.9.5
  * ��˥塼�� [����] (howm-mode �ʥХåե��򤹤٤ƾä�)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/481>))
* [2003/09/14] v0.9.4.2
  * fix: �����ȥ������΢�˱���Ƥ��ޤäƤ�
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/475>))
  * fix: �֥���������б������ءפ�������(�ޥå����ʤ���й�Ƭ��)
* [2003/09/14] v0.9.4.1
  * grep æ�Ѥλ�� (��((<���󥹥ȡ���>)))
    * ��⤬���̤��Ȥ�äѤ����ä��٤� ;_;
  * fix: ��Wrong type argument: window-configuration-p, nil��
  * �����⡼�ɤ���ե�����򳫤����Ȥ�, ����������б�������:
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/472>))�˸�����
* [2003/09/13] v0.9.3
  * �����⡼�ɤ���ȴ�����, window �ξ��֤�����
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/450>))
  * C-u RET �ǰ�����Ĥ� (��((<�����ɤ⤦>)))
    * howm-view-summary-persistent (��((<�������ޥ���>)))
  * howm-view-kill-buffer, howm-view-delete-other-windows ���ѻ�
* [2003/09/12] v0.9.2.1
  * fix: come-from ��󥯤��Ĺ���פ�
  * fix: ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/447>))
    * howm-view-kill-buffer �������դ��ä�
    * �����ȥ����ɽ���κ�, �����ȥ�ʤ�������΢�˻ĤäƤ�
  * howm-ignore-comefrom-same (��((<�������ޥ���>)))
  * �ʤ���ɾ�ʤΤ�, �ޥ˥奢��� HTML ��
* [2003/09/10] v0.9.1
  * �ǥե���Ȥ�Ѹ��
    * .emacs �˰ʲ���񤱤Х�˥塼�����ܸ��
        (setq howm-menu-lang 'ja)
    * hcal.rb ��ǥե���ȵ���� ASCII ʸ����
* [2003/09/09] v0.9
  * ruby, find æ��. elisp ������ʤ�®���ä��Ȥϡ�
  * howm-view-kill-buffer ������ (��((<�������ޥ���>)))
  * ext/easy-mmode.el ��Ʊ�����ѻ� (��̣�ʤ���)

== 0.8.x

* [2003/08/31] v0.8.5
  * �����ȥ�ΰ���ɽ�� (��((<�������ޥ���>)))
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/398>))
  * �����⡼�ɤ��˥塼�⡼�ɤǡ�?�פ򲡤��ȥإ��
  * �ֻ��͡פ� vim �Ǥ��ɲ�
* [2003/06/03] v0.8.4
  * ��ľ�������� hcal.rb (htodo.rb ���̾����ĥ)
  * ͽ��ɽ�˺ѹ��ܤ�ɽ�� (��((<�������ޥ���>)))
  * �ֻ��͡פ� wikilog ���ɲ�
* [2003/05/11] v0.8.3
  * fix: howm-excluded-file-regexp �� menu, schedule, todo �Ǹ����Ƥʤ��ä�
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1050963280/126>))
  * README
    * maxima �ˤĤ��Ƥ����
    * M-x howm-create �� M-x howm-menu
* [2003/03/19] v0.8.2
  * �����ġ���� htodo.rb �ɲ�
  * fix: todo ������, �ѹ���(@[2003/03/19]. �ʤ�)�����ս���¤֤褦
* [2003/02/20] v0.8.1.1
  * fix: ��@[2003/02/14] 501���פ� laziness �� 501 �ˤʤäƤ��Τ���
  * fix: �Хå����åץե������.#foobar.baz�פ򸡺��оݳ���
* [2002/12/14] v0.8.1
  * howm-congrats-hook
  * �ɥåȥե�����⸡���оݤ� (howm-excluded-file-regexp ������)
    * ��>> �ե�����̾�״�Ϣ��ޤ���˽�������ޤǤλ������
* [2002/11/03] v0.8
  * ��˥塼
  * ��ޥ����
    * ����Ρ�@[2002/10/21] !!�פʤɤ��ѻ�
  * ���޻����� (migemo)
    * �տ路��, grep ���ޥ�ɤ� egrep ���ѹ�
  * ����¾������
    * action-lock ���ɲ��� (namazu ��Ȥ�, Message-ID ����᡼��򸡺�)
    * action-lock �Ρ�̤�����פ� {___} ���� {_} ���ѹ�
    * [2002/10/18] �Τ褦�����շ������󥯤�
    * howm-view-summary �ΰ��ɽ����, ��Ƭ����Ϻ��
    * howm-view-split-horizontally
    * howm-insert-date
    * �ѿ� howm-view-summary-format �λ����ѹ�
      (��ʬ��ʤ� todo �����˥ե�����̾ɽ���ʤ�, �Τ�äĤ������Τ���)
    * �����ǤΥ����ȥ�ɽ�����ö�ѻ� (ʣ���ʳ���򤿤���)
    * ���������Х�����ѹ�
  * fix: �ֺ����Υ��׾�� C-c , c �����Ȥ��ϡ����˸��Ƥ��ե�����פ�ά

== 0.7.x

* [2002/09/30] v0.7.6.1
  * howm-view-kill-buffer �˴ط��ʤ�, ���������Хåե������ϻĤ�(��äĤ�)
  * fix: case-fold-search �����ꤷ�Ƥ���Ȥ���, grep / grep -i ��Ƚ��
* [2002/09/26] v0.7.5
  * ��������ʸ����ʸ������̤��ʤ��褦�ѹ�
* [2002/09/24] v0.7.4
  * fix: ������ no match �ˤʤä��Ȥ�, ���Хåե��ο��ʤɤ��Ѥ�
    (font-lock-keywords-only ������ͤ��ä��ѹ�)
* [2002/09/20] v0.7.3
  * action-lock-no-browser
  * fix: @ �ǰ���ɽ����Ϣ��ɽ���Ȥ��ڤ꤫�����Ȥ�����������֤�����̤褦
  * fix: �ޡ��������ꤻ�� howm-create ���Ƥ⥨�顼�Ф̤褦
* [2002/09/19] v0.7.2
  * ������ summry ̤����������ʬ�ˤϡ�type 'p'�פ�ɽ��
  * fix: .howm-keys �ؤν�ʣ��Ͽ�����å��Ǥ���ʸ����ʸ������̤���褦��
  * fix: �ǥ��쥯�ȥ�̾�ϰ����оݳ��� (��: CVS/)
* [2002/09/18] v0.7.1
  * http:// �� web browser ��ư
  * fix: �֥�����ɤ� kill-ring �ءע��֥����ȥ�� kill-ring �ء�(�¤���)
* [2002/09/17] v0.7
  * 1 �� 1 �ե������
  * �����ȥ� = �� come-from ��� << �Ȥ�ʬΥ
  * �Ƕ�Υ��ΰ��� (�����Х���ɤ��ѹ�)
  * �ե�����̾�� kill-ring �� (C-u C-c , k)
  * ��>> �ե�����̾�� (file://�� �� ((<URL:��>)) ��Ĥ��ƤϤ���)
  * �Ѹ��ѹ�: �������ż� �� goto��come-from
  * ���������ˤ�إå�
  * fix: �Ť� ruby-mode.el ��Ȥ��������������Ѥʿ����Ĥ��Τ��äĤ�����

== 0.6.x

* [2002/09/14] v0.6 ���̽񤭤ʤ���
  * �֥�󥯡פ��ѻ�. ���٤Ƥϸ����Ǥ���.
  * ������̤Υ����ȡ��ʤꤳ�ߡ�Ϣ��ɽ��
  * ������ɰ����ե�����(~/.howm-keys)�򤷤֤���Ƴ��
    * �ż���󥯤ν񼰤���Ѥˤ��뤿��
      * emacs, grep, ruby ������ɽ���ΰ㤤��Ǻ�ߤ����ʤ�����,
        �ż��������Υ������(����ɽ������!)�� emacs ��Ǵ��뤵������
      * ���ե�����ΰż��������� emacs �ǥ�����󤹤�Τ�
        ���������٤���
    * �ռ�ˤȤä�, �����������뤿��Ӥ�
      ���θ��������Ͽ���Ƥ��ޤ������Ȥ�
  * shell ��Ȥ�ʤ� (�Ĥˤ����Ĥ��)
  * �ѥ饰��ջظ����ѻ�
    * ������, �ƥѥ饰��դ� 1 �ե�����ˤ���, Ϣ��ɽ��
  * migemo ������̤����
  * �ե�����̾��̵ͭ����蘆������ˤĤ���

== 0.5.x ����

* [2002/06/26] v0.5.3 (��꡼������[2002/09/12])
  * �ѥ饰��հ�����������ϥ��饤��
* [2002/06/25] v0.5.2
  * migemo �����Ǥ�ޥå���ϥ��饤��
  * describe-mode �˥ѥ饰��մ�Ϣ�Υ�������⵭��
  * rd-memo �Ȥ�ʻ��������
* [2002/06/24] v0.5.1
  * �������뤬�ѥ饰��դ���Ƭ�ˤ��ä��Ȥ��ε�ư����
  * �ѥ饰��դ򿷵������ȥ�˰�ư�����Ȥ� howm-create-title �������褦
* [2002/06/23] v0.5
  * �ѥ饰��ջظ��Ȼ������� (������)
  * howm-default-directory
  * howm-menu ���ѻߤ�, ���̤� keymap �����
  * ����̾�� kill ring ��
  * popview-mode ����ե�����򳫤���, ���Ƥ����ս�˥��������ư
  * grep �Ǥ���̾�Ǥʤ��ޥå��Ԥ�ɽ��
  * ����¾, ������
    * migemo-client --type=egrep ������
    * popview-mode �� post-command-hook �� buffer-local ��
    * popview-mode �� update �����ץ����å���, ������ɤ���ʤ����֤�
    * popview-mode �� 'popview �Ǥʤ� 'popview-mode �� provide
    * howm-create-file ���䴰
* [2002/06/06] v0.4.2
  * migemo
  * grep �� egrep
  * ��̾���䴰����
  * tag2plan �ǡ�@[2002/06/06] foo�פ���Ͽ����ʤ��ä��Τ���
* [2002/06/05] v0.4.1
  * grep �� -i
  * ����Ԥ���̾�Ȥߤʤ��ʤ�
  * howm ���ޥ�ɤ���̾�񼰤Υǥե���Ȥ�ְ���ܡפ�
* [2002/06/03] v0.4 ��̾�񼰤��ѹ� (��= ��̾�ע� �����)
* [2002/06/03] v0.3.2 �ǥХå�(1�ե�����ʣ������)
* [2002/06/02] v0.3.1
  * �ư����ΥХåե����̤�
  * tag �����Ϻ��������դ˥��������ư
  * ����������, ��̾�����ʤ���̾�񼰼���("= ")���ά
  * howm -pipe
  * w3mmenu
* [2002/06/02] v0.3
  * ���ޥ���ɲ� (��/���Υ�󥯤�, tag, �ʤꤳ�߸���, ����, ʣ��)
  * tag2plan
  * popview-mode �˺���¥ɥ������
  * ����¾, ������
* [2002/05/30] v0.2 ���ޥ���ɲ� (refresh, ������/������, ����̾����, ����)
* [2002/05/29] v0.1 ����

= �Ť�����

* �ƻ��֤ΥХ� 2007-11-09
  * howm-1.3.5 �ι�����Ͽ�򻲾�

* �������ƥ����� 2006-12-16
  * howm-1.3.4 �ι�����Ͽ�򻲾�

* �ƥ����Ǥ˥Х�:
  [2003-12-27]��[2004-01-21]�Υƥ����Ǥ�
  (setq howm-insert-date-future t)
  �����ꤷ�Ƥ������,
  ��̤������դ����Ϥ����Τ�, �����⤦����̤������äƤ��ޤ���
  �Ȥ����Х�������ޤ���. ���ߤޤ���.
  * ͽ�ꡦtodo �ΰ�����, ǯ��ˤޤ��������ʤ����Τ���Ƥ�������.
    (�ä�, ��2004 ǯ�ΤĤ�꤬ 2005 ǯ�ˡפȤ����ɾ�����դ�������)
  * ��꡼���Ǥ���[2004-01-22]�ʹߤΥƥ����Ǥ� update ���Ƥ�������.

= ���Ǥ���ΰܹ�
(ɬ���Хå����åפ�ȤäƤ���!)

== v1.0.x ����ΰܹ���

* .emacs �ν��� �� ((<���󥹥ȡ���>))
  * ��<<< ����ʸ����ʸ������̤��ʤ��פ����ꤷ������,
    ������ɰ�������ľ���ƽ�ʣ��ä��Ȥ褤�Ǥ��礦
* �������˰ܹԤ�����
  * ��ޥ�����ν��ѹ�
      @[2003/09/25]! �� [2003/09/25]!
      @[2003/09/25]  �� [2003/09/25]-
      [2003/09/25]!  �� [2003/09/25]:!
      [2003/09/25]   �� [2003/09/25]
    * ���ǥ��쥯�ȥ�� cd ����,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.gsub!(%r~(@?)(\[[0-9]+/[0-9]+/[0-9]+\])([-+@!.]?)~){|s| if ($1 == %~~ && $3 == %~~); s; else; $2 + ($1 == %~@~ ? %~~ : %~:~) + ($3 == %~~ ? %~-~ : $3); end}'
    * ��ǧ��, *.bak ��ΤƤ�
  * ���դν��ѹ�
      [2003/10/21] �� [2003-10-21]
    * ���ǥ��쥯�ȥ�� cd ����,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.gsub!(%r!(\D)(\d{4}/\d{2}/\d{2})(\D)!){|s| $1 + ($2.tr "/", "-") + $3}'
    * ��ǧ��, *.bak ��ΤƤ�
  * ��󥯤ν��ѹ� (<<, >> �� <<<, >>> ��)
    * ���ǥ��쥯�ȥ�� cd ����,
        find . -name '*.howm' -print | xargs -n 1 ruby -p -i.bak -e '$_.sub!(/(<<|>>).*/){|s| $1[0,1] + s}'
    * ��ǧ��, *.bak ��ΤƤ�
  * ��ꤿ�����, �����̾���Ƥ�褤
      2003_10_18.howm �� 2003-10-18-000000.howm
    * ���ǥ��쥯�ȥ�� cd ����,
        find . -name '*.howm' -print | ruby -ne '$_.chop!; d = File::dirname $_; f = File::basename($_).tr("_", "-").sub(/[.][^.]+$/){|s| "-000000" + s}; puts %~mv #$_ #{File::expand_path f, d}~' > ~/howm_kuzu
    * ~/howm_kuzu �����Ƥ��ǧ��, ����ʤ����
        cat ~/howm_kuzu | /bin/sh
  * �������̾���礬���פ���褦, �����ॹ����פ�Ǥä�����
    * ���ǥ��쥯�ȥ�� cd ����,
      * GNU touch �ξ��
          find . -name '*.howm' -print | sort -r | ruby -ne 'puts %~touch -d "#{ARGF.lineno} min ago" #$_~' > ~/howm_kuzu
      * ����ʳ��ξ��
          find . -name '*.howm' -print | sort | ruby -ne '$_.chop!; puts %~sleep 1; touch #$_~' > ~/howm_kuzu
    * ~/howm_kuzu �����Ƥ��ǧ��, ����ʤ����
        cat ~/howm_kuzu | /bin/sh
  * ��˥塼�ե�����Τ������� (ja/* �Υ��ԡ�) �� ((<���󥹥ȡ���>))
    * ���˥塼�ե�����ϼΤƤ�
* ��������Ӥ����: .emacs (howm ���ɤ����)��
    (setq howm-ref-header ">>") ; goto ���
    (setq howm-keyword-header "<<") ; come-from ���
    (setq howm-reminder-old-format t) ; ��ޥ�����ν��ѹ��򤷤ʤ�
    (setq howm-date-separator "/") ; ���դ� 2003/10/21 �η���
    (setq howm-menu-keyword-regexp nil) ; ��%����%�פ� menu-mode �ˤ��ʤ�
    (setq howm-menu-top nil) ; C-c , , �ǡ�<< %menu%�פ򳫤��ʤ�
    (setq howm-menu-file "menu.howm") ; C-c , , �� menu.howm �򳫤�
    (setq howm-use-color nil) ; << �ʤɤ˿���Ĥ��ʤ�
    (setq howm-menu-name-format " *howmM:%s*") ; ��˥塼�ϱ��� buffer
    (setq howm-abbreviate-file-name nil) ; �ե�����̾ɽ���� full path
    ;; ��ǥե��������
    (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm") ; 1 �� 1 �ե�����
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
  * Ʊ���ġ���λ��ѻ���, ���Υ��ץ�����Ĥ���
    * howm2 -comefrom='<<' -goto='>>'
    * hcal.rb -format=old -date_sep=/
    * tag2plan -format=old -date_sep=/

== v0.8.x, v0.9.x ����ΰܹ���

* ���ǥ��쥯�ȥ�γ��ز��˱����ơ�
  * �������˰ܹ�
    * ���⤷�ʤ��Ƥ褤.
    * ��ꤿ�����, ~/howm/2003/09/ �ʤɤ��äƵ�����ư���Ƥ�褤
  * ��������Ӥ�: .emacs ��
      (setq howm-file-name-format "%Y_%m_%d.howm")

== v0.7.x ����ΰܹ���

* ��ޥ�����ν��ѹ���ȼ��, ���Υ�����ץȤǥ����Ѵ�
  * �Ѵ�������ץ� (~/kuzu.rb �Ȥ���)
      $d = '@\\[[0-9 :/]+\\]'
      $_.gsub!(/(#$d) *!!!/){|s| "#$1! ����"}
      $_.gsub!(/(#$d) *!!/){|s| "#$1! ��"}
      $_.gsub!(/(#$d) *!/){|s| "#$1!"}
      $_.gsub!(/(#$d) *@@@/){|s| "#$1@ ����"}
      $_.gsub!(/(#$d) *@@/){|s| "#$1@ ��"}
      $_.gsub!(/(#$d) *@/){|s| "#$1@"}
      $_.gsub!(/(#$d) *~~~/){|s| "#$1+ ����"}
      $_.gsub!(/(#$d) *~~/){|s| "#$1+ ��"}
      $_.gsub!(/(#$d) *~/){|s| "#$1+"}
      END {
        STDERR.print '.'
        sleep 1
      }
  * �Ѵ����
      cd ~/howm
      ls *.howm | xargs -n 1 ruby -p -i.bak ~/kuzu.rb
      (��ǧ��, *.bak ��ΤƤ�)

== v0.6 ��������ΰܹ���

* come-from �ν��ѹ���ȼ��, �ʲ��Τ褦�˥����Ѵ�
    cd ~/howm
    ruby -p -i.bak -e '$_.sub! /^= /, "= << "; $_.sub! /^= << link$/, "= link"' *.howm
    (��ǧ��, *.bak ��ΤƤ�)
  * rd-memo �Ȥθߴ����Τ����, ��= link�פ��Ѵ����ʤ�
* �����, �ե�����̾���ѹ�
  * �������˰ܤ����
    * ���Τ褦�ˤ���, ��cat ���� ���� > �����פȤ������ޥ��������
        cd ~/howm
        ruby -e 'h=Hash::new []; ARGV.each{|f| f=~/^([0-9]+)-[0-9]+.*/ and h[$1] = h[$1] + [f]}; h.keys.sort.each{|k| puts "cat #{h[k].sort.join %! !} > #{k =~ /(....)(..)(..)/; %!#$1_#$2_#$3.howm!} && sleep 1"}' *.howm > howm_kuzu
      * sleep 1 ��, �ե����빹�������Ǥ������¤֤褦��
    * howm_kuzu �����Ƥ��ǧ��, ����ʤ����
        cat howm_kuzu | /bin/sh
        (��ǧ��, 200*-*.howm ��ΤƤ�)
  * ��������Ӥ����: ~/.emacs ��
      (setq howm-file-name-format "%Y%m%d-%H%M%S.howm") ;; 1 ��� 1 �ե�����
      (setq howm-file-name-format "%Y%m%d-000000.howm") ;; 1 �� 1 �ե�����

= �Ť��������ޥ���ˡ

== v1.2.3 �Υ������ޥ��� [2005-05-08]

~/.emacs (~/.emacs.el ����)��, �ʲ��Τ褦�˽�

* ��
  * <<< �ʤɤ˿���Ĥ��ʤ�
      (setq howm-use-color nil)
  * ������: howm �Υ��ɤ����
      (set-face-foreground 'howm-mode-keyword-face "red") ;; <<<
      (set-face-foreground 'howm-mode-ref-face "magenta") ;; >>>
      (set-face-background 'howm-mode-wiki-face "green") ;; [[]] ���ط�
      (set-face-background 'howm-mode-title-face "yellow") ;; = ���ط�
      (set-face-foreground 'action-lock-face "blue") ;; ����ʸ��
      (set-face-underline 'action-lock-face t) ;; ������ʸ����Ʊ�� (Emacs 21)
      (set-face-underline 'action-lock-face "dark cyan") ;; ���� (Emacs 21)
      (set-face-foreground 'howm-menu-key-face "orange") ;; ��˥塼�� shortcut
    * ��̾�� M-x list-colors-display ����
  * ���ƥХåե��� rd-mode �ʿ���Ĥ���
      ;; rd-mode.el ���ɤ߹��ޤ�Ƥ���Ȥ��������
      (setq howm-view-contents-font-lock-keywords rd-font-lock-keywords)

* ��������
  * �֥������ʡפǥ�˥塼, ��Ctrl-�������ʡפǿ������
      (define-key global-map [katakana] 'howm-menu)
      (define-key global-map [(control katakana)] 'howm-create)
  * [tab]([alt]-[tab])�Ǽ�(��)�Υ�󥯤˰�ư
      (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
      (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)
    * ����� tab �� C-i ��

* ��¸���
  * ���� ~/memo/ ���֤�
      (setq howm-directory "~/memo/")
  * ����֤���/ǯ/ǯ����-��ʬ��.howm ��
      (setq howm-file-name-format "%Y/%Y%m%d-%H%M%S.howm")
    * �ե�����̾���Τ�ǯ���������äƤ��ʤ���, filter-by-date ����ǽ���ʤ�
  * 1 �� 1 �ե����� (����֤���/ǯ/��/ǯ_��_��.howm ��)
      (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm")
    * �Դ�������������ޤ�. �����Ǥ���ͤ����ɤ���
      * ���ñ�̤Ǥ���٤������ΰ������ե�����ñ�̤�
        (�����ȥ�ɽ��, ���������, ���ƤǤιʤꤳ��, uniq)
    * ��˥塼�˰ʲ��Υܥ����񤤤Ƥ���������
      * [����] �� �����Υ��
      * [����] �� �����Υ��
  * ������ɰ����� ~/howm/.howm-keys ���֤�
      (setq howm-keyword-file "~/howm/.howm-keys") ;; �ǥե���Ȥ� ~/.howm-keys
    * �������Ƥ�����, �㤦�ޥ���Ǥ� ~/howm/ �ʲ��Υ��ԡ������ǺѤ�.
    * ���Ǥ˽񤤤���⤬����ʤ�, mv ~/.howm-keys ~/howm/ �򤷤Ƥ�����,
      �ƹ��ۤ���(��((<���󥹥ȡ���>))).
    * �ǥ��å�: �������٤��ʤ�? (�δ��Ǥ���ۤɤ���, ��äƤߤʤ�������)

* ����
  * �����ȥ������ɽ��
      (setq howm-list-title t)
    * �����ȥ��񤫤ʤ��Ȥ��Ǥ�, �����ȥ����= �פϻĤ��Ƥ������������Ǥ��礦
      (�к��Ϥ����Ĥ��Ǥ�����)
    * �����ΤȤ����������ȥ�ɽ��
        (setq howm-list-title
              ;; ���˽񤤤����ޥ�ɤǤ��������ȥ�ɽ��
              '(howm-list-all howm-list-recent
                howm-list-grep howm-list-grep-fixed howm-list-migemo
                howm-list-related howm-list-around
                howm-keyword-search)) ;; ����� come-from ��󥯡�goto ���
    * �ؿ�����ꤹ�뤳�Ȥ��ǽ
        (setq howm-list-title (lambda () ��))
  * �����Хåե������ƥХåե��򲣤��¤٤�
      (setq howm-view-split-horizontally t)
    * �����¤٤뤱��, ��ö C-x 1 ������ϽĤ��¤�
        (setq howm-view-split-horizontally 'hoge) ;; t, nil �ʳ������
      * ï��ȤäƤ��ʤ��ΤǤ��ε�ǽ�Ϻ�� [2008-10-07]
  * [return] �ǥե�����򳫤���, �����Хåե���ä�.
    C-u ���� [return] ����, ������Ĥ�.
      (setq howm-view-summary-persistent nil)
  * �����ǡ�!�פ����Ȥ��ν�����ޥ�ɤ��ѹ�
      (setq howm-view-summary-shell-last-file "_FILE_")
      (setq howm-view-summary-shell-hist
        '("mv _FILE_ ~/gomi" "touch _FILE_" "ls -l _FILE_"))
    * ������ޥ�ɤϡ�mv �ե�����̾ ~/gomi��
    * M-p �����Ƥ�����, ��touch �ե�����̾�פ��ls -l �ե�����̾��
  * �����Хåե������ƥХåե��򸡺����Ȥ˸��̤˺��
      (setq howm-view-summary-name "*howmS:%s*")
      (setq howm-view-contents-name "*howmC:%s*")
  * ������ǥե���Ȥ����ս��
      (setq howm-list-normalizer 'howm-view-sort-by-reverse-date)
  * C-x 1 ��Ͼ���˥ե졼���ʬ�䤷�ʤ� (SPC �Ǻ�ʬ��)
      (setq howm-view-keep-one-window t)
  * �����Хåե��ο��Ĥ���
      (setq howm-view-summary-font-lock-keywords '(("^2003" . 'highlight)))
  * foo �򸡺����Ƥ� [[foo]] ���̤ˤ��ʤ�
      (setq howm-list-prefer-wiki nil)

* ��˥塼
  * ��˥塼���ѹ�
    * ��˥塼�򳫤��� [menu �Խ�] ��ǥ꥿���� �� ��ͳ���Խ�
    * �褯�������ؤ� goto ��󥯤ʤɤ�񤤤Ƥ�������������
  * ��˥塼�������ˡ�R[menu ����] E[menu �Խ�]�פ�Ĥ���
      (setq howm-menu-footer "\n-- \n%\"R\"[menu ����] %\"E\"[menu �Խ�]")
  * ��˥塼�ϱ����Хåե���
      (setq howm-menu-name-format " *howmM:%s*")
    * �� Emacs Ʀ�μ�
      * ����ǻϤޤ�Хåե�̾��, C-x b �� C-x C-b �ǽФƤ��ʤ�
      * ����ʥХåե��򸫤�ˤ�, C-x b C-q SPC SPC
  * ��˥塼���ⰷ�����ʤ� (���������������оݳ���)
      ;; mv ~/howm/0000-00-00-000000.howm ~/hoge/fuga/menu.howm ���Ȥ��ơ�
      (setq howm-menu-file "~/hoge/fuga/menu.howm")
  * ͽ��ɽ��ɽ���ϰ�
      (setq howm-menu-schedule-days-before 2)  ;; ����������
      (setq howm-menu-schedule-days 7)  ;; ������ޤ�
  * todo ��ɽ�����
      (setq howm-menu-todo-num 50)

* ��äȷڤ� (cf. ((<�ٹ�Ū�ץ���ߥ�|URL:http://pitecan.com/fugo.html>)))
  * ��˥塼�ե������ľ�ܻ���
      (setq howm-menu-file "0000-00-00-000000.howm")
  * ��˥塼�� 2 ���֥���å���
      (setq howm-menu-expiry-hours 2)
    * ������, �����¸���˥�˥塼��ư��������褦�ˤʤ�(���ʤ�Ť�).
      �����ߤ��ʤ�
        (setq howm-menu-refresh-after-save nil)
  * �����ָ�β�������ľ���򤷤ʤ�
      (setq howm-refresh-after-save nil)
  * �����ǤΥ����ȥ�ɽ��������
      ;; �ֺǶ�Ρפޤ��ϡ�����Ρץ��������˸¤�
      (setq howm-list-title '(howm-list-recent howm-list-around))
      ;; ����ɽ������
      ;(setq howm-list-title 'nil)
  * ������ grep ��Ȥ�
      (setq howm-view-use-grep t)
    * ���ξ��, ���޻�������Ȥ��ˤ� migemo-server ��ư�����Ƥ���ɬ�פ���
    * GNU grep ����ʤ��Ȥ��ᤫ��. -H �Ȥ� -r �Ȥ� -e �Ȥ���
    * grep ���ޥ��̾���ѹ�����ʤ��
        (setq howm-view-grep-command "egrep")
        (setq howm-view-fgrep-command "fgrep")
    * egrep, fgrep ���ʤ����� GNU grep �Ϥ���, �Ȥ����Ķ�(��ʥ���?)�ʤ��
        (setq howm-view-grep-command "grep")
        (setq howm-view-fgrep-command "grep")
        (setq howm-view-grep-extended-option "-E")
        (setq howm-view-grep-fixed-option "-F")

* ���� viewer
  * �ե�����̾(����ɽ��)�� viewer ���б������ (~/.mailcap ���ͥ�褷�ޤ�)
      (setq howm-view-external-viewer-assoc
            '(
              ("[.]\\(jpg\\|gif\\|png\\)$" . "display %s")
              ("[.]dvi$" . "xdvi %s")
             ))
  * �ֳ��� viewer ��Ȥ�ʤ� mime-type�פλ��� (����ɽ��)
      (setq howm-view-open-by-myself '("text/.*" "application/emacs-lisp"))
  * ~/.mime-types �� ~/.mailcap �򻲾Ȥ��ʤ�
      (setq howm-view-use-mailcap nil)
    * ��������򤷤Ƥʤ��Ƥ�, �饤�֥��� load �˼��Ԥ������ϻ��Ȥ��ޤ���
      * emacs-21.2.1 ��°�� gnus/mailcap �˰�¸
      * �Ť� FLIM �� conflict?

* ����
  * �ɥåȥե������õ��
      (setq howm-excluded-file-regexp
            "^[.][.]\\|[~#]$\\|\\.bak$\\|/CVS/")
  * �оݥǥ��쥯�ȥ���ɲ�
    * ��ʸ�����ΤȤ�, ���˲ä��ƻ���ǥ��쥯�ȥ�ʲ���Ƶ�Ū��õ��
        (setq howm-search-path '("~/Mail" "~/News"))
        (setq howm-search-other-dir t) ;; �����Υȥ���ν���� (t �� nil)
    * M-x howm-toggle-search-other-dir ��,
      �嵭�ǥ��쥯�ȥ�򸡺��оݤˤ��뤫���ʤ����ȥ���
      * �����Х���ɤ�������гƼ��� (���󥿡��ե������Ϻ���ˤĤ���)
  * �����˻Ȥ��ؿ��򤹤꤫��
      ;; t �ʤ� grep ���ޥ��, nil �ʤ�ǥե���Ȥ� elisp �ؿ������.
      ;; ����ʳ��ʤ�, ���ꤷ���ؿ������.
      (setq howm-view-use-grep #'my-grep-function)
      (defun my-grep-function (str file-list
                                   &optional fixed-p force-case-fold)
        "STR �� FILE-LIST (�ե�����̾�Υꥹ��)���鸡��.
      ����ͤ�, ((�ե�����̾ ���ֹ� ������) (�ե�����̾ ���ֹ� ������) ��)
      �Ȥ����ꥹ��.
      FIXED-P �����ʤ�ʸ���� STR ��, ���ʤ�����ɽ�� STR �򸡺�����.
      STR ����ʸ����ޤޤʤ��Ȥ���, �ޤ�Ǥ� FORCE-CASE-FOLD �����ΤȤ���,
      ��ʸ����ʸ���ΰ㤤��̵�뤹��."
        ��)
  * howm �� foo �򸡺��������, C-s C-s �� foo �θ�����
      (setq howm-view-update-search-ring t)
    * ����ɽ���� C-u C-s C-s �ʤΤ����

* ̤��¸�����ȰѺٹ��鷺, howm-mode �ʥХåե��򤹤٤ƶ���������륳�ޥ��
  (�������ᤷ�ޤ���. �Ȥ�ʤ��Ǥ�������.)
  * C-u C-c , Q
  * ��˥塼�˽񤯤ʤ� [��������]
  * ʪ���ʤΤ�, ����񤤤Ȥ��ʤ���̵��
      (setq howm-kill-all-enable-force t)

* �ƥ�ץ졼�Ȥ��ѹ���
  * ����ʤդ���
      Subject: �����ȥ�С��˻��פ�ɽ�� ��ľ���Υ꡼����������
      Date: Thu, 12 Sep 2002 15:45:59 +0900
      In-Reply-To: </home/hira/sawfish/rich-title/rich-title.jl> ��ľ���ե�����
      
      �� �� ��������
    * ~/.emacs ��
        (setq howm-template "Subject: %title\nDate: %date\n%file\n%cursor")
        (setq howm-template-date-format "%a, %d %b %Y %H:%M:%S %z")
        (setq howm-template-file-format "In-Reply-To: <%s>\n")
  * �ƥ�ץ졼�Ȥ�ʣ������
      ;; C-u 2 C-c , c �� 2 ���ܤΥƥ�ץ졼�Ȥǿ������
      ;; ��˥塼���� C-u 2 c �Ǥ�Ʊ��
      (setq howm-template
            '("= %title%cursor\n%date %file\n\n" "%date: %title%cursor"))
    * �Ĥ��Ǥ�, howm-template ���ͤ��ؿ��ʤ�
      ��universal-argument ������ˤ��Ƥ����Ĥ�Ƥ֡פäƤ����Τ�Ź��ߤޤ���

* �񼰤��ѹ��� (howm-*.el �� load �������)
  * �����ȥ�(�����ڤ�) @@@ ��
      (setq howm-view-title-header "@@@")
  * goto ��� ==>��, come-from ��� <==��
      (setq howm-ref-header "==>")
      (setq howm-keyword-header "<==")
  * goto ��� ((��ġ�)), come-from ��� ((���ġ�))
      ;; ��䡧��Ⱦ�Ѥ�ľ���Ƥ�������
      (setq howm-ref-regexp "((��\\([^��\r\n]+\\)��))")
      (setq howm-ref-regexp-pos 1)
      (setq howm-keyword-format "((��%s��))")
      (setq howm-keyword-regexp "\\(((��\\)\\([^��\r\n]+\\)��))")
      (setq howm-keyword-regexp-hilit-pos 1) ;; �ִ�Ϣ������ɡ���
      (setq howm-keyword-regexp-pos 2)
      (setq howm-keyword-regexp-format "%s") ;; M-x describe-variable ����
  * wiki ����� [[hoge]] �β������]]�פ�����
    * ��<<< hoge�פκ������, ��hoge�פˤⲼ��
        (setq howm-wiki-regexp "\\[\\[\\([^]\r\n]+\\)\\(\\]\\]\\)")
        (setq howm-wiki-regexp-hilit-pos 2)
        (setq howm-wiki-regexp-pos 1)

* ���ޤ���
  * <<< ����ʸ����ʸ������̤��ʤ�
      (setq howm-keyword-case-fold-search t)
  * ��������(C-c , d �ޤ��� [����])��ǯ����ά������, ��̤��פȲ��
      (setq howm-insert-date-future t)
    * �������ϻ��ΤߤǤ�. ��[2003-12-27]�׾�� RET �����Ȥ���ư��Ͻ���ɤ���.
  * ��http://�פǥ꥿���󲡤�����, URL �� kill-ring ��
      (setq action-lock-no-browser t)

* ͽ��ɽ��todo ����
  * �ѹ��ܤ�ɽ�����ʤ�
      (setq howm-schedule-types "[!@]")  ;; ͽ��ɽ
      (setq howm-todo-types "[-+~!]")  ;; todo ����
  * ��ޥ��������(!+-~@.)���� RET ��ȯ�ǡֺѡפ�
      (setq howm-action-lock-reminder-done-default "")
    * ���ξ��, C-u RET �ǽ����ư�� (����󥻥�, �����ѹ�, ��)
  * ͽ��ɽ��todo ���������ޥ����������ľ�� RET �����Ȥ�,
    á������Хåե���ư save
      (setq howm-action-lock-forward-save-buffer t)
    * �ּ�ư save�פ��񹳤ʤ��������ɤ���
    * ��ư�� C-x s (̤��¸�Хåե������� save)�ʤꤹ��������ƻ����
  * ��α�������ϰ�
      (setq howm-todo-priority-defer-init -14)  ;; ����� = ����
      (setq howm-todo-priority-defer-peak 0) ;; ���
  * !+-~. �ν��٤Υ������ޥ���
    * ��: ��˥塼��, �����������ɽ���סֺѤ�ɽ����
        (setq howm-menu-todo-priority -50000)
        (setq howm-todo-priority-done-bottom -44444)
    * howm-todo-priority-normal-bottom ��. ������(howm-reminder.el)����.

* action-lock
  * { } (�ȥ��륹���å�)���ѹ�
      ;; howm �� load ����
      (setq action-lock-switch-default '("{ }" "{*}" "{-}")) ;; ���ĤǤ�
  * {_} (̤����)���ѹ�
      (setq howm-dtime-format "[%a %b %d %H:%M:%S %Y]") ;; {_}
      (setq howm-template-date-format "[%Y-%m-%d %H:%M]") ;; �ƥ�ץ졼��
  * ��file://�ġפ��http://�ġפ��ѹ� (�ޤ��������罸)
    ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/945>))
      ;; howm (���Τˤ� action-lock.el) �Υ��ɤ������.
      ;; ��file://��
      (setq action-lock-open-regexp
            "\\<file://\\(localhost\\)?\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>")
      (setq action-lock-open-regexp-pos 2) ;; 2 ���ܤΡ�\\(��\\)�פ��ե�����̾
      ;; ��http://��
      (setq action-lock-browse-regexp
            "\\<\\([htp]\\{3,5\\}s?\\|ftp\\)://\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>"
      (setq action-lock-browse-regexp-pos 0) ;; �ޥå��������Τ� URL
  * action-lock �ɲ���:
    ��Message-ID: �ġפǥ꥿���󲡤�����, �����᡼��� namazu �Ǹ���
      ;; howm �� load �������
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

* ��˥塼�򹹿����뤿�Ӥ�, ���������ؤ� export �⹹�� (��((<�����ġ���>)))
    (defun my-howm-menu-hook ()
      (shell-command "tag2plan ~/howm/*/*/*.howm > ~/.dayplan_tag &")
      (switch-to-buffer howm-menu-name))
    (add-hook 'howm-menu-hook 'my-howm-menu-hook)

* ((<RD|URL:http://www2.pos.to/~tosh/ruby/rdtool/ja/>))��Ȥ����:
  ((<"��Ƭ�� * �ǥ���ȥ�γ��Ĥ��Ǥ���褦��"|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/237-238n>))

* ���ޤ�
    (setq howm-congrats-format
          '(
            "%s��������������(���ϡ�)����������!!!!"
            "(���ϡ�) %s!"
            "�ʡ맥��)%s��"
            "�ʡ���_��`�ˡ㡡%s"
            ;; �İʲ�ά��
            ))

* ��äȤ���������ˤ�, *.el ��Ƭ�򻲾�

= GNU Emacs �ʳ��ξ��

* ��Ϥ褯�狼��ʤ��Τ�, �������Ƥ�������
  * meadow:
    ((<����Ѥ� Meadow|URL:http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=meadowmemo%20edition>))
    �ʤ�ǽ餫��Ȥ��ޤ�
    * cygwin + grep ���ѤΤȤ���,
      ���ǥ��쥯�ȥ�ȥ��ޥ�ɤ�ɥ饤�֥쥿��������ꤹ��.
      * ~/.emacs(.emacs.el ����) �Ǣ��Τ褦��
          (setq howm-directory "c:/cygwin/home/howm/")
      * cygwin ���鸫�� / �� emacs ���鸫�� / �������㤦�Ȥ�������.
  * xemacs:
    * font-lock �Υ�å����������������®���ʤ뤽��.
      thx > ((<�޸�����|URL:http://eron.info/k/diary/>))
        (setq font-lock-verbose nil)
  * Linux Zaurus:
    ((<Wiki|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
    �򻲾Ȥ�������.
    thx > ((<100|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))

= �Ť����ͥ��

* ((<Wiki|URL:http://c2.com/cgi/wiki>)):
  web ��ï�Ǥ��Խ��ܤ���ڥ�󥯡ܤ���ڥե����ޥå�
  * ((<WikiModeDiscussion|URL:http://www.emacswiki.org/cgi-bin/wiki.pl/WikiModeDiscussion>))
    (EmacsWiki): Emacs �Ǥ� Wiki
  * ((<RWiki-mode|URL:http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=rwiki-mode>))
    (RWiki): RWiki �� Emacs ����Ȥ���
  * ((<QP-Wiki|URL:http://pitecan.com/UnixMagazine/>))
    (�����Ƿ����): PDA �� Wiki
  * ((<HashedWiki|URL:http://www.google.com/search?q=hashedwiki>))
    (SHIMADA Keiki ����): �ѥ饰��ջظ� Wiki
  * ((<ishinao ����γƼ�ġ���|URL:http://ishinao.net/>)):
    Wiki �ˤȤ���ʤ������ǥ�����
  * ((<������ȯ�� wiki ������ꥹ�ȡ�|URL:http://www.yamdas.org/column/technique/clonelist.html>))
    ((<��2��|URL:http://www.yamdas.org/column/technique/clonelist2.html>))
    (yomoyomo ����)
* HyperCard: card �� database Ū visual script ����Ķ�???
  * ((<��HyperCard��|URL:http://www.hyuki.com/yukiwiki/wiki.cgi?HyperCard>))
    (YukiWiki)
  * ((<��HyperCard�Υꥢ�륿��������|URL:http://web.archive.org/web/20040111061953/http://mwave.sppd.ne.jp/wiki/pukiwiki.php?%5b%5bHyperCard%A4%CE%A5%EA%A5%A2%A5%EB%A5%BF%A5%A4%A5%E0%C0%AD%5d%5d>))
    (SsPukiWiki)
  * ((<�֥ϥ��ѡ������ɤǤĤ��륪�ե��������ƥ��|URL:http://www.kanzaki.com/hc/MacUser.html>))
    (�������Ѥ���)
* ���Ȥ�Ķ�
  * ʬ�ऻ��, ���ֽ����ʸ�����Ǵ���
    * ((<Q-Pocket|URL:http://pitecan.com/UnixMagazine/>))
      (�����Ƿ����):
      PDA �Ǥ�
    * ChangeLog ���
      * ((<��Unix�Υ�⵻�ѡ�|URL:http://namazu.org/~satoru/unimag/1/>))
        (����ů����)
      * ((<�ֻ�� ChangeLog ������ˡ��|URL:http://nais.to/~yto/doc/zb/0016.html>))
        (����ãͺ����)
      * ((<��ChangeLog �����Ƥߤ褦��|URL:http://pop-club.hp.infoseek.co.jp/emacs/changelog.html>))
        (������Ƿ����)
  * ((<�·ϡ�Ķ��|URL:http://www.google.com/search?q=%E2%C0%8Cn+%92%B4%96%AA>))
    (syo ����): ChangeLog + �ܼ����¤٤�����hyper link �äƴ���?
  * ������åץ֥å�
    * ((<�� 2001|URL:http://www.vector.co.jp/soft/win95/writing/se120325.html>))
      (�����������): ����
    * ((<WeBoX|URL:http://webox.sakura.ne.jp/software/webox/>))
      (��¼��ˤ���): �����������餷��
  * ����¾�� Emacs �ѥġ���
    * ((<notes-mode|URL:http://www.isi.edu/~johnh/SOFTWARE/NOTES_MODE/>))
      (John Heidemann ����):
      link �����������Τ�ޤ���
      * ((<notes-mode �� memo-mode �������|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/others/compare-notes-and-memo-mode.html>))
        (���ܹ��֤���)
    * ((<memoma|URL:http://web.archive.org/web/20040803170229/http://www.jaist.ac.jp/~tetsu/memoma/memoma.html>))
      (����ů������): MH ���� �� �᡼��꡼���Ǥ��ɤ��
    * ((<Um4|URL:http://www.d4.dion.ne.jp/~usuda/emacs/index.html>))
      (������ˤ���): ��������¸��˥塼
    * rd-memo
      (�ۺ�. ��ȯ��λ �� ((<tar.gz|URL:https://howm.osdn.jp/a/rd-memo.tar.gz>)))
      * ((<�֥���ԥ塼���Ķ��ǤΥ���|URL:http://pub.cozmixng.org/~the-rwiki/rw-cgi.rb?cmd=view;name=%A5%B3%A5%F3%A5%D4%A5%E5%A1%BC%A5%BF%B4%C4%B6%AD%A4%C7%A4%CE%A5%E1%A5%E2>))
        (Tosh����): Wiki �����ܤ������ä���
  * howm ��Ϣ
    * �ܿ�
      * ((<howm-mode.vim|URL:http://sworddancer.funkyboy.jp/howm_vim/>))
        (������줵��)
        ((<�ܦ�|URL:http://www.google.com/search?q=vim+howm+%82%AD%82%D3%82%F1>)): vim ��
      * ((<howm-wrap|URL:http://homepage3.nifty.com/~ko-ji/>))
        (kimura ����)��
        ((<howm-helper|URL:http://www.geocities.co.jp/Milano-Cat/2067/howm-init.html>))
        (deecay ����): xyzzy ��
      * ((<howm.mac|URL:http://mrm.seesaa.net/category/789739.html>))
        (Mr.M ����)
        ((<�ܦ�|URL:https://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HideMaru>)): ������
    * ������ todo �ꥹ��
      * ((<wikilog|URL:http://web.archive.org/web/20040308005605/http://koten.hypermart.net/wikilog_rc01.l>))
        (Gonza ����): xyzzy ���ǥ����Ѥ�, Wiki + ChangeLog ���
        �� ((<�а�|URL:http://pc2.2ch.net/test/read.cgi/win/1053880433/n29-36>))
      * ((<howm��TODO����WEB���ץ�|URL:http://web.archive.org/web/20060128122538/http://www.lyricfathom.com/pukiwiki/pukiwiki.php?howm%BC%B0TODO%B4%C9%CD%FDWEB%A5%A2%A5%D7%A5%EA>))
        (�����): PHP �Ǥμ���
      * ((<wema|URL:http://wema.sourceforge.jp/>))
        (�դ��Ϥ餫�󤵤�): ��䵥١����� Wiki Ū�ʤ��.
        ��䵼��Τ��岼�˰�ư. æ˹.
      * ((<LesserWiki|URL:http://lesserwiki.org/>))
        (yatsu����): Ajax �� Wiki
      * ((<Whem|URL:http://www.n314.com/whem/?action=ExecLogin&mail=guest>))
        (Nishimura����): Web�ѥޥ���桼�����ġ���. goto/come-from ��󥯤���.
* �����ˤ���
  * ((<memo-mode|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/>))
    (OSHIRO Naoki ����): �վ�񤭻ٱ�. �٤��ܤ�.
  * ((<get-date|URL:http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/>))
    (OSHIRO Naoki ����): ���������դ�ȿ��Ū������. �٤��ܤ�.
  * ((<migemo|URL:http://migemo.namazu.org/>))
    (����ů����): ���޻����������������ܸ�⸡��. ����.
  * ((<rdtool|URL:http://www.google.com/search?q=rdtool>))
    (Tosh����): ���� README �ǻȤäƤ�ɥ�����ȥե����ޥå�. ����.
  * ((<elscreen|URL:http://www.morishima.net/~naoto/j/software/elscreen/>))
    (Naoto Morishima����): GNU screen �� Emacs ��. ����.

=end
