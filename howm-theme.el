;;; -*- lexical-binding: nil; -*-
;;; howm-theme.el --- Wiki-like note-taking tool
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

;; ref. https://github.com/kaorahi/howm/issues/34

(defun howm-auto-theme (&rest _)
  ;; copied from https://github.com/kaorahi/howm/issues/34#issue-2815035530
  "Adjust to the current theme by borrowing the Org-mode colors."
  (custom-set-faces
   `(action-lock-face ((t :inherit button)))
   `(howm-mode-keyword-face ((t :inherit org-keyword)))
   `(howm-mode-ref-face ((t :inherit org-link)))
   `(howm-mode-title-face ((t :inherit org-level-1)))
   `(howm-mode-wiki-face ((t :inherit org-link)))
   `(howm-reminder-deadline-face ((t :inherit org-scheduled-today)))
   `(howm-reminder-late-deadline-face ((t :inherit bold :inherit org-deadline-overdue)))
   `(howm-reminder-defer-face ((t :inherit org-scheduled)))
   `(howm-reminder-scheduled-face ((t :inherit org-scheduled)))
   `(howm-reminder-done-face ((t :inherit org-done)))
   `(howm-reminder-todo-face ((t :inherit org-todo)))
   `(howm-reminder-normal-face ((t :inherit org-default)))
   `(howm-reminder-today-face ((t :inherit bold :inherit org-scheduled-today)))
   `(howm-reminder-tomorrow-face ((t :inherit bold :inherit org-scheduled)))
   `(howm-simulate-todo-mode-line-face ((t :inherit bold)))
   `(howm-view-empty-face ((t :inherit shadow)))
   `(howm-view-hilit-face ((t :inherit isearch)))
   `(howm-view-name-face ((t :inherit org-document-title)))))

(define-minor-mode howm-themed-minor-mode
  ;; cf. pdf-view-themed-minor-mode
  ;; https://github.com/vedang/pdf-tools/blob/30b50544e55b8dbf683c2d932d5c33ac73323a16/lisp/pdf-view.el#L1329-L1345
  "Apply the current theme by borrowing the Org-mode colors."
  :init-value nil ;; default = off
  :lighter " Thm" ;; mode-line
  (cond
   (howm-themed-minor-mode
    (add-hook 'enable-theme-functions #'howm-auto-theme)
    (howm-auto-theme))
   (t
    (remove-hook 'enable-theme-functions #'howm-auto-theme)
    ;; How to cancel howm-auto-theme?
    )))

(provide 'howm-theme)

;;; howm-theme.el ends here
