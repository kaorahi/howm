;;; -*- lexical-binding: nil; -*-
;;; iigrep.el - incremental interactive grep
;;; Copyright (C) 2004, 2005-2025
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
;;;--------------------------------------------------------------------

;;; Commentary:

;; examples
;; 
;; Search the directry ~/foo incrementally.
;;     M-x iigrep RET ~/foo RET
;; 
;; Search the directry ~/foo with migemo incrementally.
;;     M-x iigrep-migemo RET ~/foo RET

;; links
;; 
;; The original (obsolete) iigrep.el
;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?IncrementalGrep
;; 
;; migemo
;; http://0xcc.net/migemo/
;; http://www.kaoriya.net/software/cmigemo/

;; brief history
;; 
;; [2022-08-23] export to howm
;; [2004-12-01] rename to iigrep.el
;; [2004-11-30] prototype ingrep.el

;;; Code:

(provide 'iigrep)
(require 'howm)

(require 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize

(defvar iigrep-maximum-output 40000)
(defvar iigrep-command "egrep")
(defvar iigrep-option "-nIe")
(defvar iigrep-recursive-option "-r")
(defvar iigrep-default-show-what 'full
  "One of \\='full, \\='contents, \\='counts, or nil.")

(defvar iigrep-counts-face-rules
  '(
    ;; (threshold-of-hits . face)
    (10 . iigrep-counts-face1)
    (50 . iigrep-counts-face2)
    (150 . iigrep-counts-face3)
    (300 . iigrep-counts-face4)
    (500 . iigrep-counts-face5)
    ))

(defvar iigrep-buffer-name "*iigrep*")
(defvar iigrep-process-name "iigrep")
(defvar iigrep-mode-name "iigrep")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces

(defgroup iigrep nil
  "Incremental grep"
  :group 'applications)

(defface iigrep-counts-face1
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (t ()))
  "*Face for iigrep counts."
  :group 'iigrep)

(defface iigrep-counts-face2
  '((((class color) (background light)) (:foreground "dark green"))
    (((class color) (background dark)) (:foreground "green"))
    (t ()))
  "*Face for iigrep counts."
  :group 'iigrep)
(defface iigrep-counts-face3
  '((((class color)) (:foreground "orange"))
    (t ()))
  "*Face for iigrep counts."
  :group 'iigrep)
(defface iigrep-counts-face4
  '((((class color)) (:foreground "red"))
    (t ()))
  "*Face for iigrep counts."
  :group 'iigrep)
(defface iigrep-counts-face5
  '((((class color)) (:foreground "purple"))
    (t ()))
  "*Face for iigrep counts."
  :group 'iigrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main command

(defun iigrep (dir)
  (interactive "Ddirectory: ")
  (cd dir)
  (iigrep-with-grep-internal (iigrep-command-for-pattern-on-dir dir)
      iigrep-default-show-what
    (read-from-minibuffer (iigrep-prompt))))

(defun iigrep-command-for-pattern-on-dir (dir)
  (lambda (pattern)
    (list iigrep-command iigrep-option pattern
          iigrep-recursive-option (expand-file-name "."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main lib (iigrep-with-grep)

(defvar iigrep-command-for-pattern nil
  "For internal use")
(defvar iigrep-show-what nil
  "For internal use")
(defvar iigrep-target-minibuffer nil
  "For internal use")

(defmacro iigrep-with-grep (command-for-pattern show-what &rest body)
  (declare (indent 2))
  `(save-window-excursion
     (unwind-protect
         (iigrep-with-grep-internal ,command-for-pattern ,show-what
           ,@body)
       (let ((buf (iigrep-buffer nil t)))
         (when buf (kill-buffer buf))))))
(defmacro iigrep-with-grep-internal (command-for-pattern show-what &rest body)
  (declare (indent 2))
  `(save-excursion
     (let* ((iigrep-command-for-pattern ,command-for-pattern)
            (iigrep-show-what ,show-what))
       (if (or (null iigrep-command-for-pattern) (null iigrep-show-what))
           (progn ,@body)
         (iigrep-setup-window t (not (eq iigrep-show-what 'counts)))
         (unwind-protect
             (minibuffer-with-setup-hook #'iigrep-minibuffer-setup
               ,@body)
           (iigrep-minibuffer-cleanup))))))

(defun iigrep-target-minibuffer-p ()
  (eq (current-buffer) iigrep-target-minibuffer))
(defun iigrep-minibuffer-setup ()
  (when iigrep-target-minibuffer
    (message "iigrep: terminated previos session")
    (iigrep-minibuffer-cleanup))
  (setq iigrep-target-minibuffer (current-buffer))
  (add-hook 'after-change-functions #'iigrep-update))
(defun iigrep-minibuffer-cleanup ()
  (let* ((buf iigrep-target-minibuffer)
         (alivep (and (bufferp buf) (buffer-name buf))))
    (when alivep
      (with-current-buffer buf
        (remove-hook 'after-change-functions #'iigrep-update))))
  (setq iigrep-target-minibuffer nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buf & win

(defun iigrep-setup-window (&optional create show-p)
  (let* ((buf (iigrep-buffer create))
         (win (get-buffer-window buf)))
    (cond (win (select-window win))
          (show-p (pop-to-buffer buf))
          (t (set-buffer buf)))))

(defun iigrep-buffer (&optional create silent)
  (if create
      (iigrep-get-buffer-create)
    (or (get-buffer iigrep-buffer-name)
        (if silent nil
          (error "iigrep buffer is lost.")))))

(defun iigrep-get-buffer-create ()
  (let ((buf (get-buffer-create iigrep-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (compilation-mode iigrep-mode-name))
    buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

;; In the minibuffer...

(defun iigrep-update (&rest dummy)
  (let ((pattern (and (iigrep-target-minibuffer-p)
                      (minibuffer-contents-no-properties))))
    (when pattern
      (iigrep-convert-call pattern #'iigrep-search))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search

(defvar iigrep-last-pattern nil
  "For internal use.")
(defvar iigrep-last-command nil
  "For debug.")

(defun iigrep-search (pattern)
  (setq iigrep-last-pattern pattern)
  (when (not (string= pattern ""))
    (save-window-excursion
      (iigrep-setup-window)
      (iigrep-kill-process)
      (iigrep-erase-buffer)
      (iigrep-grep pattern))))

(defun iigrep-grep (pattern)
  (let* ((args (funcall iigrep-command-for-pattern pattern))
         (p (apply #'start-process iigrep-process-name nil args)))
    (setq iigrep-last-command args)
    (set-process-filter p iigrep-process-filter)
    (set-process-sentinel p #'iigrep-sentinel)
    (set-process-query-on-exit-flag p nil)))

(defvar iigrep-process-filter #'iigrep-process-filter
  "Filter for grep process.
This value is also used for identification of iigrep processes.")

(defun iigrep-process-filter (p output)
  (let ((buf (iigrep-buffer nil t)))
    (when buf ;; Don't accept output from previous search
      (with-current-buffer buf
        (if (> (point-max) iigrep-maximum-output)
            (iigrep-exceed-limit p)
          (iigrep-append-output output))))))

(defun iigrep-append-output (output)
  (let ((buffer-read-only nil))
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (when (eq iigrep-show-what 'contents)
      (iigrep-hide-paths))
    (set-buffer-modified-p nil)))

(defun iigrep-hide-paths ()
  (font-lock-mode -1)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.*?:[0-9]+:" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'invisible t))))

(defun iigrep-get-counts-face (hits)
  (let* ((filtered (mapcan (lambda (pair)
                             (and (<= hits (car pair)) (list pair)))
                           iigrep-counts-face-rules))
         (face (cdr-safe (car-safe filtered))))
    face))

(defun iigrep-erase-buffer ()
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun iigrep-exceed-limit (p)
  (iigrep-kill-process)
  (iigrep-append-output "\nSize limit exceeded."))

(defvar *iigrep-post-sentinel* nil)

(defun iigrep-sentinel (proc msg)
  (let ((stat (process-status proc))
        (buf (iigrep-buffer nil t)))
    (when (and buf (member stat '(exit signal)))
      (with-current-buffer buf
        (let* ((hits (count-lines (point-min) (point-max)))
               (s (format "%s" hits)))
          (when (> hits 0)
            (put-text-property 0 (length s) 'face (iigrep-get-counts-face hits) s)
            (let ((message-log-max nil))
              (message "%s hits" s))
            (when (and *iigrep-post-sentinel* (eq stat 'exit))
              (funcall *iigrep-post-sentinel* hits iigrep-last-pattern))))))))

(defun iigrep-kill-process ()
  (mapcar (lambda (p)
            (when (iigrep-process-p p)
              (set-process-buffer p nil)
              (kill-process p)))
          (process-list)))

(defun iigrep-process-p (p)
  (eq (process-filter p) iigrep-process-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; converter

;; Continuation passing style is used partially
;; so that we can easily control when to call grep, and we can accept
;; fast typing while getting pattern from migemo.
;; In normal search, (iigrep-convert-call pat func) is just (func pat).

(defvar *iigrep-convert-call* #'iigrep-identity-converter
  "For internal use.")
(defvar *iigrep-prompt* "grep: "
  "For internal use.")

(defun iigrep-identity-converter (pattern continuation)
  (funcall continuation pattern))

(defun iigrep-convert-call (pattern continuation)
  (funcall *iigrep-convert-call* pattern continuation))

(defun iigrep-prompt () *iigrep-prompt*)

(defmacro iigrep-with-converter (func prompt &rest body)
  (declare (indent 2))
  `(let ((*iigrep-convert-call* (or ,func *iigrep-convert-call*))
         (*iigrep-prompt* (or ,prompt *iigrep-prompt*)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migemo

;; Use continuation to avoid slow response

(defun iigrep-migemo (dir)
  (interactive "Ddirectory: ")
  (require 'migemo)
  (iigrep-with-converter #'iigrep-migemo-converter "migemo: "
    (iigrep dir)))

(defvar iigrep-migemo-process nil)
;; (defvar iigrep-migemo-options '("-q" "--emacs" "--nonewline")  ;; for GNU grep
(defvar iigrep-migemo-options '("-q")
  "*Options for migemo command for iigrep.
The default value is for cmigemo.
Use \\='(\"-S\" \"migemo\" \"-t\" \"egrep\") for the original migemo.")
(defmacro iigrep-with-our-migemo (&rest body)
  (declare (indent 0))
  `(let ((iigrep-original-migemo-process migemo-process)
         (migemo-process iigrep-migemo-process)
         (migemo-options iigrep-migemo-options))
     (unwind-protect
         (progn
           ,@body)
       (setq iigrep-migemo-process migemo-process
             migemo-process iigrep-original-migemo-process))))

(defun iigrep-migemo-converter (roma continuation)
  (iigrep-with-our-migemo
    (iigrep-migemo-search roma continuation)))

;; copied and modified from migemo-get-pattern in migemo.el (migemo-0.32)

(when (not (fboundp 'migemo-init))
  (defun migemo-init ()
    (howm-inhibit-warning-in-compilation)))

(defun iigrep-migemo-search (word continuation)
  (migemo-init)
  (set-process-filter migemo-process
                      (iigrep-migemo-filter continuation))
  (with-current-buffer (process-buffer migemo-process)
    (delete-region (point-min) (point-max))
    (process-send-string migemo-process (concat word "\n"))))

(defvar iigrep-migemo-last-pattern nil
  "For internal use.")
(defvar iigrep-migemo-last-buffer nil
  "For debug.")
(defun iigrep-migemo-filter (continuation)
  `(lambda (process message)
     (with-current-buffer (process-buffer process)
       (insert message)
       (when (and (> (point-max) 1)
                  (eq (char-after (1- (point-max))) ?\n))
         ;; AD HOC!
         ;; I don't understand this.
         ;; Observe iigrep-migemo-last-pattern and iigrep-migemo-last-buffer
         ;; after typing keys fast.
         (goto-char (point-min))
         (skip-chars-forward "\n")
         (let ((pattern (buffer-substring (point) (line-end-position))))
           (setq iigrep-migemo-last-pattern pattern)
           (setq iigrep-migemo-last-buffer
                 (buffer-substring (point-min) (point-max)))
           (erase-buffer)
           (funcall (function ,continuation)
                    pattern))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key binding

;; backward compatibility for my old .emacs
(defun iigrep-define-key-for (command &optional force map))

;;; iigrep.el ends here
