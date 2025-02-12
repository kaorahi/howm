;;; -*- lexical-binding: nil; -*-
;;; howm-markdown.el --- Wiki-like note-taking tool
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

;; [example 1]
;;
;; (require 'howm-markdown)  ;; before loading howm
;; ;; (setq howm-file-name-format "%Y-%m-%d.md")  ;; overwrite
;; (require 'howm)

;; [example 2]
;;
;; (use-package howm
;;   :init
;;   (require 'howm-markdown)
;;   ;; (setq howm-file-name-format "%Y-%m-%d.md")  ;; overwrite
;;   )

;; ref.
;; https://github.com/kaorahi/howm/issues/38
;; https://github.com/kaorahi/howm/issues/38#issuecomment-2642585171

(setq howm-file-name-format "%Y-%m-%d-%H%M%S.md")
(setq howm-view-title-header "#")
(setq howm-menu-file-extension ".md")
(setq howm-menu-skel-replace-rules '(("^= " . "# ") ("^== " . "## ")))

;; https://github.com/kaorahi/howm/issues/38#issuecomment-2640020651
(add-hook 'howm-mode-hook
          (lambda () (set-face-italic 'howm-mode-ref-face nil)))

(provide 'howm-markdown)

;;; howm-markdown.el ends here
