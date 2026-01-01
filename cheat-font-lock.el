;;; -*- lexical-binding: nil; -*-
;;; cheat-font-lock.el --- modify font-lock-keywords
;;; Copyright (C) 2002, 2003, 2004, 2005-2026
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

;; no "cheat" now [2025-02-07]

;; renamed from howm-font-lock.el [2003-12-12]

(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cheat-font-lock-append-keywords (entries)
  (font-lock-add-keywords nil entries 'append))
(defun cheat-font-lock-merge-keywords (&rest keywords-list)
  (font-lock-add-keywords nil nil 'set)
  (mapc #'cheat-font-lock-append-keywords keywords-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cheat-font-lock-mode (&optional dummy)
  ;; keep dummy arg for backward compatibility with outside code
  "Just enable font-lock-mode."
  ;; font-lock-defaults seems necessary for "C-c , a" (howm-list-all) [2025-02-07]
  (when (null font-lock-defaults)
    (set (make-local-variable 'font-lock-defaults) '(nil)))
  (font-lock-mode 1)
  (font-lock-set-defaults))

(defun cheat-font-lock-fontify (&optional dummy)
  ;; keep dummy arg for backward compatibility with outside code
  (font-lock-flush)
  (font-lock-ensure))

(provide 'cheat-font-lock)

;;; cheat-font-lock.el ends here
