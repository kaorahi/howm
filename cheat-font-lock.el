;;; cheat-font-lock.el --- modify font-lock-keywords
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: cheat-font-lock.el,v 1.24 2011-12-31 15:07:28 hira Exp $
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

;; depends on internal implementation of font-lock.el

;; renamed from howm-font-lock.el [2003-12-12]

(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code is canceled because it caused a bug on howm-1.2.2rc5.
;; cheat-font-lock-merge-keywords must support compiled keywords for current
;; implementation of riffle-contents-mode. [2005-04-28]
;; See below.
;; snap:///~/elisp/howm/howm-view.el#223:(define-derived-mode howm-view-contents-mode riffle-contents-mode "HowmC"
;; snap:///~/elisp/howm/howm-view.el#256:(cheat-font-lock-merge-keywords howm-view-contents-font-lock-keywords
;; 
;; (if (and (fboundp 'font-lock-add-keywords) (>= emacs-major-version 21))
;;     (progn
;;       (defun cheat-font-lock-merge-keywords (&rest keywords-list)
;;         ;; compiled keywords are not supported in keywords-list.
;;         (font-lock-add-keywords nil (apply #'append keywords-list) 'set))
;;       (defun cheat-font-lock-append-keywords (entries)
;;         (font-lock-add-keywords nil entries 'append))
;;       (defun cheat-font-lock-prepend-keywords (entries)
;;         (font-lock-add-keywords nil entries))
;;       ;; inhibit warning. sigh...
;;       (defun cheat-font-lock-20040624-format-p () nil)
;;       (defun cheat-font-lock-compiled-p (keywords) nil)
;;       (defun cheat-font-lock-compiled-body (keywords) nil)
;;       )
;;   (progn
;;     ;; for xemacs and emacs20
;;     ))

(defun cheat-font-lock-20040624-format-p ()
  ;; need to call font-lock-set-defaults before font-lock-compile-keywords.
  ;; see http://lists.gnu.org/archive/html/emacs-diffs/2005-12/msg00961.html
  (font-lock-set-defaults)
  (>= (length (font-lock-compile-keywords '(("dummy" . 'dummy)))) 3)) ;; dirty
(defun cheat-font-lock-compiled-p (keywords)
  (eq (car-safe keywords) t))
(defun cheat-font-lock-compiled-body (keywords)
  (cdr keywords))
(when (cheat-font-lock-20040624-format-p)
  ;; re-defun for avoiding the warning:
  ;; "the function `...' is not known to be defined."
  (defun cheat-font-lock-compiled-body (keywords)
    (cddr keywords)))
(defun cheat-font-lock-keywords (keywords)
  (if (cheat-font-lock-compiled-p keywords)
      (cheat-font-lock-compiled-body keywords)
    keywords))
(defun cheat-font-lock-merge-keywords (&rest keywords-list)
  (let ((bodies-list (mapcar #'cheat-font-lock-keywords keywords-list)))
    (setq font-lock-keywords
          (apply #'append bodies-list))))
(defun cheat-font-lock-append-keywords (entries)
  (cheat-font-lock-merge-keywords font-lock-keywords entries))
(defun cheat-font-lock-prepend-keywords (entries)
  (cheat-font-lock-merge-keywords entries font-lock-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cheat-font-lock-mode (&optional silent)
  "Enable font-lock-mode without calling fontify-buffer."
  ;; For xemacs. But this seems to have no effect. ;_; [2004-01-14]
  (when silent
    (set (make-local-variable 'font-lock-verbose) nil))
  ;; Keywords are not highlighted on the fly in emacs-21.3.50.1
  ;; when font-lock-defaults is nil. I don't understand this. [2003-11-28]
  (when (null font-lock-defaults)
    (set (make-local-variable 'font-lock-defaults) '(nil)))
  ;; Without the next line, global value is changed to t. [2003-12-30]
  ;; (emacs-20.7.2 on Vine Linux 2.6)
  (make-local-variable 'font-lock-fontified)
  (let* ((font-lock-fontified t) ;; adjourn fontify-buffer
         (bname (buffer-name))
         (need-rename (eq (aref (buffer-name) 0) ?\ )))
    ;; Rename invisible buffer in order to force font-lock-mode.
    ;; cf. snap:///usr/share/emacs/21.2/lisp/font-lock.el#694:(define-minor-mode font-lock-mode
    (when need-rename
      (rename-buffer (concat "xxx-" bname) t))
    (font-lock-mode 1)
    (when need-rename
      (rename-buffer bname)))
  (font-lock-set-defaults))

(defun cheat-font-lock-fontify (&optional dummy)
  (font-lock-fontify-buffer))

(provide 'cheat-font-lock)

;;; cheat-font-lock.el ends here
