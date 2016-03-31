;;; action-lock.el --- invoke magic action by RET key on spell strings

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;; $Id: action-lock.el,v 1.72 2011-12-31 15:07:28 hira Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License is available by anonymouse ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;;; Commentary:

;;; Code:

;; rules = (rule rule ...)
;; rule = (regexp action) or (regexp action hilit-pos)
;; action = function with one argument which corresponds to (interactive "P").

(require 'cl-lib)
(require 'easy-mmode)
(require 'font-lock)
(require 'cheat-font-lock)
(require 'howm-common)

(defgroup action-lock nil
  "Invoke magic action by RET key on spell strings."
  :group 'convenience)

(defvar action-lock-face 'action-lock-face
  "*Face for action-lock spells.")

(defface action-lock-face
  (let ((underline (if (and (fboundp 'set-face-underline)
                            window-system)
                       '(((class color)) (:underline "dark cyan"))
                     '(((class color)) (:underline t))))
        (fail-safe '(t (:inverse-video t))))
    (list underline fail-safe))
  "*Face for action-lock spells."
  :group 'action-lock
  :group 'howm-faces)

(defvar action-lock-magic-return-key "\C-m")
(put 'action-lock-magic-return-key 'risky-local-variable t)
(defvar action-lock-lighter " AL")
(defvar action-lock-silent t
  "Inhibit font-lock-verbose if non-nil.")

;; If you want to change these values,
;; you must set them before loading this file.
(defvar action-lock-switch-default '("{ }" "{*}" "{-}"))  ;; any number
(defvar action-lock-date-default '("{_}" "[%Y-%m-%d %H:%M]"))  ;; before after

(easy-mmode-define-minor-mode action-lock-mode
  "With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\[action-lock-magic-return]  Envoke the action on the field
"
  nil ;; default = off
  action-lock-lighter ;; mode-line
  `(
    (,action-lock-magic-return-key . action-lock-magic-return)
    ))

;; emacs20's easy-mmode-define-minor-mode can't have body. sigh...
(add-hook 'action-lock-mode-on-hook 'action-lock-initialize-buffer)
(add-hook 'action-lock-mode-off-hook 'action-lock-restore-buffer)

(defvar action-lock-rules nil)
(defvar action-lock-original-font-lock-keywords nil)
(defvar action-lock-original-return nil)
(put 'action-lock-rules 'risky-local-variable t)
(put 'action-lock-original-font-lock-keywords 'risky-local-variable t)
(put 'action-lock-original-return 'risky-local-variable t)

(make-variable-buffer-local 'action-lock-rules)
(make-variable-buffer-local 'action-lock-original-font-lock-keywords)
(make-variable-buffer-local 'action-lock-original-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample

(defun action-lock-switch (label-list)
  (let ((regexp (mapconcat 'regexp-quote label-list "\\|")))
;   (let ((regexp (regexp-opt label-list))) ;; emacs19 lacks regexp-opt
    (list regexp
          `(lambda (&optional dummy)
             (let* ((b (match-beginning 0))
                    (e (match-end 0))
                    (ring ',(append label-list (list (car label-list))))
                    (s (match-string-no-properties 0))
                    (next (cadr (member s ring))))
               (delete-region b e)
               (insert next)
               (goto-char b))))))

(defun action-lock-date (regexp time-format)
  (list regexp
        `(lambda (&optional dummy)
           (delete-region (match-beginning 0) (match-end 0))
           (insert (format-time-string ,time-format)))))

(defun action-lock-open (regexp arg-pos &optional hilit-pos)
  (action-lock-general #'action-lock-find-file
                       regexp arg-pos hilit-pos t))
(defun action-lock-find-file (f u)
  (if u
      (find-file-other-window f)
    (find-file f)))

;; (defun action-lock-open (regexp arg-pos &optional hilit-pos)
;;   (action-lock-general #'find-file regexp arg-pos hilit-pos))

(defvar action-lock-no-browser nil)
(defun action-lock-browse-url (url)
  (setq url (replace-regexp-in-string "^[htp]+\\(s?\\)://" "http\\1://" url))
  (message "%s" url)
  (if action-lock-no-browser
      (kill-new url)
    (browse-url url)))
(defun action-lock-browse (regexp arg-pos &optional hilit-pos)
  (action-lock-general #'action-lock-browse-url regexp arg-pos hilit-pos))

(defun action-lock-general (func regexp arg-pos &optional hilit-pos arg-p)
  "Generate an action-lock rule.
FUNC is called when action-lock is invoked on a string which matches
to REGEXP. ARG-POS specifies a position of subexpression in REGEXP,
and matched substring is passed to FUNC.
HILIT-POS specifies another position of subexpression in REGEXP,
and matched substring is highlighted in buffers.
FUNC will receive an additional argument for action, as is described
at the beginning of this file, when ARG-P is non-nil."
  (list regexp
        `(lambda (&optional arg)
           (,func (match-string ,arg-pos)
                  ,@(and arg-p '(arg))))
        hilit-pos))

; (defun action-lock-escape-quote (s)
;   (apply 'concat
;        (mapcar '(lambda (x) (if (string= x "'") "\\x27" x)) ;; for zsh
;                (split-string s ""))))

;; copied and modified from thingatpt.el [2004-01-30]
(defvar action-lock-url-path-regexp
  "\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)"
;;   "\\([^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+\\)"
  "A regular expression probably matching the host, path or e-mail part of a URL.")
;; (defvar action-lock-url-scheme-regexp
;;   "\\<\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)")
(defun action-lock-url-regexp (head &optional tail)
  (concat head
          action-lock-url-path-regexp
          (or tail "")))

(defvar action-lock-open-regexp
  (action-lock-url-regexp "\\<file://\\(localhost\\)?\\(" "\\>/?\\)"))
(defvar action-lock-open-regexp-pos 2)

;; emacs20 doesn't support "[htp]\\{3,5\\}"
(defvar action-lock-browse-regexp
  (action-lock-url-regexp "\\<\\([htp][htp][htp][htp]?[htp]?s?\\|ftp\\)://" "\\>/?"))
(defvar action-lock-browse-regexp-pos 0)

(defvar action-lock-default-rules
  (list (action-lock-switch action-lock-switch-default)
        (action-lock-date (regexp-quote (car action-lock-date-default))
                          (cadr action-lock-date-default))
        (action-lock-open (action-lock-url-regexp "URL:\\(file://\\)?\\(localhost\\)?" ">))")
                          3) ;; ((<URL:...>))
        (action-lock-open action-lock-open-regexp
                          action-lock-open-regexp-pos) ;; file://...
        (action-lock-browse action-lock-browse-regexp
                            action-lock-browse-regexp-pos) ;; http://...
        ))
(put 'action-lock-default-rules 'risky-local-variable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(defvar action-lock-bury-minor-mode-p t)
(defun action-lock-initialize-buffer ()
  (interactive)
  (action-lock-initialize-magic-return)
  (action-lock-set-rules action-lock-default-rules)
  (when action-lock-bury-minor-mode-p
    (action-lock-bury-minor-mode 'action-lock-mode))
)

(defun action-lock-restore-buffer ()
  (action-lock-restore-font-lock))

(defun action-lock-magic-return (&optional arg)
  (interactive "P")
  (or (action-lock-invoke arg)
      (if action-lock-mode
          (let* ((action-lock-mode nil)
                 (f (key-binding action-lock-magic-return-key)))
            (call-interactively f))
        ;; Can't happen normally
        (call-interactively action-lock-original-return))))

(defun action-lock-invoke (&optional arg)
;;   (interactive)
  (let ((action (action-lock-get-action)))
    (if (null action)
        nil
      (progn
;;         (message "%s" action) ;; debug
        (funcall action arg)
;;         (apply action nil)
        t))))

(defun action-lock-initialize-magic-return ()
  (when (null action-lock-original-return)
    (let ((action-lock-mode nil))
      (setq action-lock-original-return
            (key-binding action-lock-magic-return-key)))))

(defun action-lock-rules ()
  action-lock-rules)
(defun action-lock-set-rules (rules)
  (setq action-lock-rules (howm-cl-remove-duplicates* rules))
;;   (message "Font lock...")
  (action-lock-font-lock)
;;   (message "...Done.")
  )
(defun action-lock-add-rules (rules &optional prepend-p)
  (action-lock-set-rules (if prepend-p
                             (append rules (action-lock-rules))
                           (append (action-lock-rules) rules))))

(defun action-lock-bury-minor-mode (mode)
  "Bury MODE to the last in minor-mode-map-alist"
  (let ((pair (assoc mode minor-mode-map-alist)))
    (when pair
      (setq minor-mode-map-alist
            ;; Duplications must be removed.
            `(,@(remove pair minor-mode-map-alist) ,pair)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock

;; experimental [2003-10-25]
(defvar action-lock-case-fold-search nil)
(defvar action-lock-use-case-fold-search t)

(defun action-lock-matcher (regexp)
  (if action-lock-use-case-fold-search
      `(lambda (limit)
         (let ((case-fold-search action-lock-case-fold-search))
           (re-search-forward ,regexp limit t)))
    regexp))

(defun action-lock-font-lock ()
  (cheat-font-lock-mode action-lock-silent)
  (if (null action-lock-original-font-lock-keywords)
      (setq action-lock-original-font-lock-keywords font-lock-keywords)
    (setq font-lock-keywords action-lock-original-font-lock-keywords))
  (when action-lock-rules
    (let* ((entries (mapcar (lambda (pair)
                              (let* ((regexp (car pair))
                                     (matcher (action-lock-matcher regexp))
                                     (pos (or (cl-caddr pair) 0))
                                     (hilit (list pos 'action-lock-face
                                                  'prepend)))
                                (cons matcher hilit)))
                            action-lock-rules)))
      (cheat-font-lock-append-keywords entries)
;;       (cheat-font-lock-prepend-keywords entries)
      (cheat-font-lock-fontify t)
      )))

(defun action-lock-restore-font-lock ()
  (setq font-lock-keywords action-lock-original-font-lock-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-lock-get-action ()
  (car (action-lock-get-action/range)))

(defun action-lock-get-range ()
  (cdr (action-lock-get-action/range)))

(defun action-lock-get-action/range ()
  (let* ((rules action-lock-rules)
         (current nil)
         (found nil))
    (while (and rules (not found))
      (save-excursion
        (setq current (car rules)
              rules (cdr rules))
        (let* ((regexp (car current))
               (action (cadr current))
               (pos (cl-caddr current))
               (range (action-lock-regexp-range regexp pos)))
          (if range
              (setq found (cons action range))))))
    found))

(defun action-lock-regexp-range (regexp &optional pos)
  (setq pos (or pos 0))
  (save-excursion
    (let ((c (point))
          (eol (line-end-position))
          (range nil)
          (case-fold-search (if action-lock-use-case-fold-search
                                action-lock-case-fold-search
                              case-fold-search))
          )
      (beginning-of-line)
      (while (and (<= (point) c)
                  (re-search-forward regexp eol 'no-error)
                  (not range))
        (let ((beg (match-beginning pos))
              (end (match-end pos)))
          (when (and (<= beg c) (< c end))
            (setq range (list beg end)))))
      range)))

(defun action-lock-regexp ()
  (mapconcat 'car action-lock-rules "\\|"))

(defun action-lock-skip-one-link (reverse)
  (let* ((r (action-lock-get-range))
         (border (if reverse 0 1)))
    (when r
      (goto-char (nth border r)))))

(defun action-lock-goto-next-link (&optional reverse)
  (interactive)
  (let* ((move (if reverse #'backward-char #'forward-char)))
    (action-lock-skip-one-link reverse)
    (funcall move)
    (while (not (action-lock-get-action))
      (funcall move))
    (when reverse
      (action-lock-skip-one-link reverse))))

(defun action-lock-goto-previous-link ()
  (interactive)
  (action-lock-goto-next-link t))

;;;;;;;;;;;;;

(provide 'action-lock)

;;; action-lock.el ends here
