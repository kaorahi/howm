=begin

= ChangeLog Memo �� howm

ChangeLog Memo ��Ǥ� howm ���Ȥ��ޤ�.
(ChangeLog Memo ��Ǽ��ʥ��) --- Case I

�ޤ�, ChangeLog Memo �� howm ��ʻ�Ѥ�Ǥ��ޤ�.
(ChangeLog Memo �� howm memo �Ȥδ֤���ߥ��) --- Case II

�� I �� II ��ξΩ�ϤǤ��ޤ���.
���󥹥ȡ���ˡ�ʤɤ�㤤�ޤ��Τ�,
�ɤ��餫������ǳ�����������ɤߤ�������.

== �������줷��?

* ChangeLog Memo ��� howm �Υ�󥯵�ǽ���Ȥ���

* M-x occur, clgrep �˲ä���
  * @ �򲡤���ɽ��������ȥ��� (occur �� ���� clgrep ��)
  * occur ��ɽ���Ǥ�, RET �򲡤��ʤ��Ƥ�ꥢ�륿��������Ƥ�ɽ��
  * ����˥����Ȥ�¿�ʤιʤ���ߤ��ǽ

* ��������� todo list ���Ȥ���

* ������Τ�����ʤ�
  * ���ĤǤ����, �Ǥ� ChangeLog ������

= �� (Case I) ChangeLog Memo ��� howm

ChangeLog Memo ��Ǽ��ʥ�󥯤�ĥ��ޤ�.

== �Ǥ��뤳��

* �Ǥ� ChangeLog �˲ä���
  * goto link
    * ��>>> �ۤ��פξ�� RET �� �֤ۤ��פ򸡺�
    * ��>>> ~/hoge.txt�פξ�� RET ��2 �� �ե�����򳫤�
    * ��>>> ~/hoge.pdf�פξ�� RET ��2 �� ���� viewer �ǳ���
  * come-from link
    * ��* �ۤ�: �դ��դ��פȤ��� entry ��񤱤�,
      �����Ρ֤ۤ��פ����٤ƥ�󥯤�
    * �֤ۤ��פξ�� RET �� �֤ۤ��פΰ���
  * wiki link
    * [[�ۤ�]] �ξ�� RET �� entry �֤ۤ��פ��ɲ�
    * �ʸ��, [[�ۤ�]] �ξ�� RET ��2 �� entry �֤ۤ��פ�����

== ���󥹥ȡ���

* ������
  * ~/elisp/howm/ �˰켰��Ÿ��
  * ~/memo/clmemo.txt �˥���Ȥ�

* �����
  * ��˥塼�ե�����򥳥ԡ�
      cp ~/elisp/howm/ja/0000-00-00-000000.txt ~/.howm-menu
  * �ʲ��� .emacs ��
      ;; ����˱�����
      (setq load-path (cons "~/elisp/howm" load-path))
      (setq howm-directory "~/memo")
      (setq howm-file-name-format "clmemo.txt")
      ;; �ʲ��Ϸ�ޤ�ʸ��
      (setq howm-menu-lang 'ja)
      (setq howm-menu-file "~/.howm-menu")
      (require 'howm-mode)
      (howm-setup-change-log)

* ���󥤥󥹥ȡ�����
  * ������Τϱ����ʤ��Τ�, ��������� OK
      rm ~/.howm-*
      rm -rf ~/elisp/howm
      vi ~/.emacs
  
== �Ȥ���

* �դĤ��� ChangeLog Memo ��ȤäƤ�������
* entry �˲������Ĥ��Τ�, ���ξ�� RET �� ������̤ΰ���
* �����Ǥ�
  * RET �� jump
  * q �� quit
* �ܤ����� README ���򻲾�

= �� (Case II) ChangeLog Memo �� howm ��ʻ��

ChangeLog Memo �� howm memo �Ȥδ֤���ߥ�󥯤�ĥ��ޤ�.

== �Ǥ��뤳��

* ChangeLog Memo ��Ǥ��<<< �ۤ��ס�>>> �ۤ��ס�[[�ۤ�]]�פ���ǽ
* <<< �ǻ��ꤷ��������ɤ�, ChangeLog Memo ��ǤⲼ�� �� ������

== ���󥹥ȡ���

* howm �����̤˥��󥹥ȡ���
* .emacs �˰ʲ����ɲ�
    (add-hook 'change-log-mode-hook 'howm-mode)
    (eval-after-load "howm-view"
      '(setq howm-view-title-regexp
             (concat howm-view-title-regexp
                     "\\|^[0-9-]+[ \t]+<.*>$")))

== �Ȥ���

* ~/howm/ChangeLog �� ChangeLog Memo ��ȤäƤ�������.
* ChangeLog ���� howm ��
  * ChangeLog Memo ��� M-x howm-from-change-log
    �� howm �ǿ������򳫤��ƥ����ȥ�򥳥ԡ�
  * ChangeLog Memo ��� [[�ۤ�]] �Ƚ�, (���������ᤷ��)���ξ�� RET
    �� <<< �ۤ� �Ȥ��� howm memo ����
* howm ���� ChangeLog ��
  * howm memo ��� M-x howm-to-change-log
    �� ChangeLog Memo �򳫤��ƥ����ȥ�򥳥ԡ�
* ��ߤ�
  * �ɤ���Υ�⤫���, ���վ�� RET
    �� �������դΥ���ξ������

== ��­

howm memo �� RD �����򤪻Ȥ�������,
((<URL:https://howm.osdn.jp/a/rd-mode-plus.el>))
��ʻ�Ѥ�����������⤷��ޤ���.
��� howm-to-change-log �Τ�����, ���ε�ǽ���Ȥ��ޤ�.

* M-x rd-to-change-log �� ChangeLog Memo �򳫤��ƾϡ���Υ����ȥ�򥳥ԡ�
  * ��
    * howm memo �ˤ����񤤤Ƥ����
        = �ۤ�
        ��
        == �դ�
        ��
        == �Ԥ�
        ��
        �� �� ��������
    * ����� ChangeLog ���Ǥ���
        2003-12-03  Foo Bar  <foo@bar.baz>
        	* �ۤ�
        	- �Ԥ�
  * ��
    * �������뤬°�����(=), ��(==)�Υ����ȥ����Ф��ޤ�
    * �֤ۤ��פ˥���������֤������, == �Υ����ȥ�ϥ��ԡ�����ޤ���
    * C-u M-x rd-to-change-log �ʤ�, ���ξϤΤ��٤Ƥ���Υ����ȥ����Ф��ޤ�
        2003-12-03  Foo Bar  <foo@bar.baz>
        	* �ۤ�
        	- �դ�
        	- �Ԥ�


=end
