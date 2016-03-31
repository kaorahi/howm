;;; howm-lang-ja.el --- Wiki-like note-taking tool
;;; -*- Coding: iso-2022-7bit -*-
;;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-lang-ja.el,v 1.11 2011-12-31 15:07:29 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

(require 'howm-common)

(defvar howm-day-of-week-ja '("日" "月" "火" "水" "木" "金" "土"))

(defvar howm-menu-command-table-ja
  `(
    ("[速記]" howm-remember previous)
    ("[新規]" (lambda () (howm-create ,howm-menu-action-arg)))
    ("[追加]" (lambda () (howm-create-here ,howm-menu-action-arg)))
    ("[複製]" howm-dup)
    ("[更新]" howm-initialize-buffer previous)
    ("[正規]" howm-list-grep)
    ("[固定]" howm-list-grep-fixed)
    ("[roma]" howm-list-migemo)
    ("[今日]" howm-find-today)
    ("[昨日]" howm-find-yesterday)
    ("[一覧]" howm-list-all)
    ("[最近]" howm-list-recent)
    ("[前後]" howm-list-around)
    ("[予定]" howm-list-schedule)
    ("[バ内]" (lambda () (call-interactively 'howm-occur)) previous)
    ("[全バ]" (lambda () (howm-list-buffers ,howm-menu-action-arg)))
    ("[mark]" howm-list-mark-ring previous)
    ("[履歴]" howm-history)
    ("[題↑]" howm-keyword-to-kill-ring)
    ("[名↑]" (lambda () (howm-keyword-to-kill-ring t)))
    ("[鍵↓]" howm-insert-keyword previous)
    ("[日↓]" howm-insert-date previous)
    ("[時↓]" howm-insert-dtime previous)
    ("[Todo]" howm-list-todo)
    ("[全消]" howm-kill-all)
    ("[強制全消]" (lambda () (interactive) (howm-kill-all t)))
    ("[menu 編集]" howm-menu-edit current)
    ("[menu 更新]" howm-menu-refresh current)
    ("[設定]" (lambda () (customize-group 'howm)))
    ("[酔歩]" howm-random-walk previous)
    ))

(provide 'howm-lang-ja)

;;; howm-lang-ja.el ends here
