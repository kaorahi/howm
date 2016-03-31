;;; howm-mkmenu.el --- Wiki-like note-taking tool
;;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-mkmenu.el,v 1.11 2011-12-31 15:07:29 hira Exp $
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

;; emacs -q --no-site-file -batch -l <this file>

(defvar howm-mkmenu-rules
  '(
    ;; (<var> <src> [<src-coding> <dest-coding>]) ==> <var>.el
    (howm-menu-en "en/0000-00-00-000000.txt")
    (howm-menu-ja "ja/0000-00-00-000000.txt" euc-jp iso-2022-7bit)
    ))

(defmacro howm-mkmenu-insert (&rest clauses)
  (declare (indent 0))
  (let ((commands (mapcar (lambda (c)
                            (let ((format (car c))
                                  (parameters (cdr c)))
                              `(insert (format ,(concat format "\n")
                                               ,@parameters))))
                          clauses)))
    `(progn ,@commands)))

(defun howm-mkmenu (rule)
  (let ((var (car rule))
        (src (cadr rule))
        (opt (cddr rule)))
    (let ((dest (concat (symbol-name var) ".el"))
          (src-coding  (and opt (car opt)))
          (dest-coding (and opt (cadr opt))))
      ;; read src
      (when (and src-coding (featurep 'mule))
        (prefer-coding-system src-coding))
      (with-temp-buffer
        (insert-file-contents src)
        (let ((str (buffer-substring-no-properties (point-min) (point-max))))
          ;; write to dest
          (find-file dest)
          (delete-region (point-min) (point-max))
          (when dest-coding
            (set-buffer-file-coding-system dest-coding)
            (howm-mkmenu-insert
              (";;; -*- Coding: %s -*-" dest-coding)))
          (howm-mkmenu-insert
            (";;; automatically generated from %s" src)
            (";;; by %s.\n" (file-name-nondirectory load-file-name))
            ("(require 'howm-vars)\n")
            ("(howm-defconst-risky %s %S)\n" var str)
            ("(provide '%s)" var))
          (let ((make-backup-files nil))
            (basic-save-buffer))
          t)))))

(mapcar #'howm-mkmenu howm-mkmenu-rules)

;;; howm-mkmenu.el ends here
