;;; -*- lexical-binding: nil; -*-
;;; howm-lang-en.el --- Wiki-like note-taking tool
;;; Copyright (C) 2005-2025
;;;   HIRAOKA Kazuyuki <kakkokakko@gmail.com>
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

(defvar howm-day-of-week-en '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(defvar howm-menu-command-table-en
  `(
    ("[Remember]" howm-remember previous)
    ("[New]" (lambda () (howm-create ,howm-menu-action-arg)))
    ("[Add]" (lambda () (howm-create-here ,howm-menu-action-arg)))
    ("[Dup]" howm-dup)
    ("[Update]" howm-menu-refresh-note previous)
    ("[Regexp]" howm-list-grep)
    ("[String]" howm-list-grep-fixed)
    ;;         ("[roma]" howm-list-migemo)
    ("[Today]" howm-find-today)
    ("[Yesterday]" howm-find-yesterday)
    ("[All]" howm-list-all)
    ("[Recent]" howm-list-recent)
    ("[Around]" howm-list-around)
    ("[Schedule]" howm-list-schedule)
    ("[Occur]" (lambda () (call-interactively 'howm-occur)) previous)
    ("[Buffers]" (lambda () (howm-list-buffers ,howm-menu-action-arg)))
    ("[Marks]" howm-list-mark-ring previous)
    ("[History]" howm-history)
    ("[<Title]" howm-keyword-to-kill-ring)
    ("[<Name]" (lambda () (howm-keyword-to-kill-ring t)))
    ("[Key>]" howm-insert-keyword previous)
    ("[Date>]" howm-insert-date previous)
    ("[DTime>]" howm-insert-dtime previous)
    ("[Todo]" howm-list-todo)
    ("[Killall]" howm-kill-all)
    ("[Force Killall]" (lambda () (interactive) (howm-kill-all t)))
    ("[Edit Menu]" howm-menu-edit current)
    ("[Update Menu]" howm-menu-refresh current)
    ("[Preference]" (lambda () (customize-group 'howm)))
    ("[Random Walk]" howm-random-walk previous)
    ))

;; based on https://github.com/kaorahi/howm/issues/57
(defun howm-menu-legend-en ()
  (cl-labels ((p (text face) (propertize text 'font-lock-face face)))
    (concat
     ;; first line
     "[Schedule, Todo] -- "
     (p "@schedule" howm-reminder-schedule-face) ", "
     (p "!deadline" howm-reminder-deadline-face) " "
     (format "(until %s days from now)" howm-menu-schedule-days) "\n"
     ;; second line
     (p "!deadline" howm-reminder-deadline-face) ", "
     (p "+todo" howm-reminder-todo-face) ", "
     (p "-reminder" howm-reminder-normal-face) ", "
     (p "~defer" howm-reminder-defer-face) " "
     (format "(top %s entries)" howm-menu-todo-num) " "
     "[" (p "today" howm-reminder-today-face) "] "
     "[" (p "tomorrow" howm-reminder-tomorrow-face) "]"
     )))

(provide 'howm-lang-en)

;;; howm-lang-en.el ends here
