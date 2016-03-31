;;; howm-view.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-view.el,v 1.251 2012-12-29 08:57:18 hira Exp $
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

(provide 'howm-view)
(require 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables

;; customize
(defvar howm-view-summary-sep "|")
(defvar howm-view-summary-format
  (let* ((path (format-time-string howm-file-name-format))
         (width (length (file-name-nondirectory path))))
    (concat "%-" (format "%s" (1+ width)) "s" howm-view-summary-sep " ")))
(defvar howm-view-header-format
  "\n==========================>>> %s\n"
  "Format string of header for howm-view-contents.
%s is replaced with file name. See `format'.")
(defvar howm-view-header-regexp "^==========================>>> .*$")
(defvar howm-view-open-recenter howm-view-search-recenter)
(defvar howm-view-title-header "=")
;; howm-view-title-regexp is assumed to have a form "^xxxxxxx$"
(defvar howm-view-title-regexp (format "^%s\\( +\\(.*\\)\\|\\)$"
                                     (regexp-quote howm-view-title-header)))
(defvar howm-view-title-regexp-pos 2)
(defvar howm-view-title-regexp-grep (format "^%s +"
                                     (regexp-quote howm-view-title-header)))
(defun howm-view-title-regexp-grep ()
  (if howm-view-use-grep
      howm-view-title-regexp-grep
    howm-view-title-regexp))

(howm-defvar-risky howm-view-sort-methods
  '(("random" . howm-view-sort-by-random)
    ("name" . howm-view-sort-by-name)
    ("name-match" . howm-view-lift-by-name)
    ("numerical-name" . howm-view-sort-by-numerical-name)
    ("summary" . howm-view-sort-by-summary)
    ("summary-match" . howm-view-lift-by-summary)
    ("summary-match-string" . howm-view-lift-by-summary-substring)
;     ("atime" . howm-view-sort-by-atime) ;; nonsense
;     ("ctime" . howm-view-sort-by-ctime) ;; needless
    ("mtime" . howm-view-sort-by-mtime)
    ("date" . howm-view-sort-by-reverse-date)
    ("reminder" . howm-view-sort-by-reminder)
    ("reverse" . howm-view-sort-reverse)))

(howm-defvar-risky howm-view-filter-methods
  '(("name" . howm-view-filter-by-name)
    ("summary" . howm-view-filter-by-summary)
    ("mtime" . howm-view-filter-by-mtime)
;     ("ctime" . howm-view-filter-by-ctime) ;; needless
    ("date" . howm-view-filter-by-date)
    ("reminder" . howm-view-filter-by-reminder)
    ("contents" . howm-view-filter-by-contents)
    ("Region" . howm-view-filter-by-region)
    ("Around" . howm-view-filter-by-around)
;     ("uniq" . howm-view-filter-uniq))
  ))

;; referred only when howm-view-use-grep is nil
(defvar howm-view-watch-modified-buffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; item

(defun howm-view-item-basename (item &optional nonempty)
  (let* ((f (howm-item-name item))
         (b (file-name-nondirectory f)))
    (if (and (string= b "") nonempty)
        f
      b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; riffle

(defalias 'riffle-home:howm              'howm-view-item-home)
(defalias 'riffle-summary-item:howm      'howm-view-summary-item)
(defalias 'riffle-contents-item:howm     'howm-view-contents-item)
(defalias 'riffle-summary-set-mode:howm  'howm-view-summary-mode)
(defalias 'riffle-contents-set-mode:howm 'howm-view-contents-mode)

(defun riffle-summary-name-format:howm ()
  howm-view-summary-name)
(defun riffle-contents-name-format:howm ()
  howm-view-contents-name)
(defun riffle-post-update:howm (item)
  (howm-message-nolog "View: %s" (howm-view-item-filename item)))

;;; aliases

;; Only howm-view.el should call riffle-xxx.
;; Define alias if it is used in howm-xxx besides howm-view.el.
(defalias 'howm-view-name          #'riffle-name)          
(defalias 'howm-view-item-list     #'riffle-item-list)     
(defalias 'howm-view-line-number   #'riffle-line-number)   
(defalias 'howm-view-summary-check #'riffle-summary-check) 
(defalias 'howm-view-persistent-p  #'riffle-persistent-p)  
(defalias 'howm-view-kill-buffer   #'riffle-kill-buffer)   
(defalias 'howm-view-set-place     #'riffle-set-place)     
(defalias 'howm-view-get-place     #'riffle-get-place)     
(defalias 'howm-view-summary-current-item  #'riffle-summary-current-item)
(defalias 'howm-view-contents-current-item #'riffle-contents-current-item)
(defalias 'howm-view-summary-to-contents   #'riffle-summary-to-contents)
(defalias 'howm-view-restore-window-configuration #'riffle-restore-window-configuration)

;; for howmoney.el
;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?howmoney
(defun howm-view-get-buffer (name-format &optional name new)
  (let ((riffle-type ':howm)) ;; cheat
    (riffle-get-buffer name-format name new)))
(defun howm-view-summary-buffer (&optional new)
  (let ((riffle-type ':howm)) ;; cheat
    (riffle-summary-buffer new)))
(defalias 'howm-view-summary-show 'riffle-summary-show)
(defalias 'howm-view-set-item-list 'riffle-set-item-list)

;; for howmz
;; http://noir.s7.xrea.com/archives/000136.html
;; http://noir.s7.xrea.com/pub/zaurus/howmz.el
(defalias 'howm-view-sort-items 'howm-sort)

;;; variables

(defvar howm-view-font-lock-silent t
  "Inhibit font-lock-verbose if non-nil.")
(howm-defvar-risky howm-view-summary-font-lock-keywords
  `((,(concat "\\(^[^ \t\r\n].*?\\)" (regexp-quote howm-view-summary-sep))
     1 howm-view-name-face)
    ("^ +" . howm-view-empty-face)))
(howm-defvar-risky howm-view-contents-font-lock-keywords nil)

(howm-defvar-risky *howm-view-font-lock-keywords* nil
  "For internal use. Don't set this variable.
This is a shameful global variable and should be clearned in future.")
(howm-defvar-risky howm-view-font-lock-keywords nil
  "For internal use.")
(defvar howm-view-font-lock-first-time t
  "For internal use.")
(make-variable-buffer-local 'howm-view-font-lock-keywords)
(make-variable-buffer-local 'howm-view-font-lock-first-time)

;;; modes

(riffle-define-derived-mode howm-view-summary-mode riffle-summary-mode "HowmS"
  "memo viewer (summary mode)
key	binding
---	-------
\\[howm-view-summary-open]	Open file
\\[next-line]	Next item
\\[previous-line]	Previous item
\\[riffle-pop-or-scroll-other-window]	Pop and scroll contents
\\[scroll-other-window-down]	Scroll contents
\\[riffle-scroll-other-window]	Scroll contents one line
\\[riffle-scroll-other-window-down]	Scroll contents one line
\\[riffle-summary-to-contents]	Concatenate all contents
\\[howm-view-filter-uniq]	Remove duplication of same file
\\[howm-view-summary-shell-command]	Execute command in inferior shell

\\[delete-other-windows]	Delete contents window
\\[riffle-pop-window]	Pop contents window
\\[riffle-toggle-window]	Toggle contents window
\\[howm-list-toggle-title]	Show/Hide Title

\\[howm-view-filter]	Filter (by date, contents, etc.)
\\[howm-view-filter-by-contents]	Search (= filter by contents)
\\[howm-view-sort]	Sort (by date, summary line, etc.)
\\[howm-view-sort-reverse]	Reverse order
\\[howm-view-dired]	Invoke Dired-X
\\[describe-mode]	This help
\\[riffle-kill-buffer]	Quit
"
  (make-local-variable 'font-lock-keywords)
  (cheat-font-lock-mode howm-view-font-lock-silent)
  (when howm-view-font-lock-first-time
    (setq howm-view-font-lock-first-time nil)
    (cheat-font-lock-merge-keywords howm-user-font-lock-keywords
                                    howm-view-summary-font-lock-keywords
                                    ;; dirty! Clean dependency between files.
                                    (howm-reminder-today-font-lock-keywords)))
  (when *howm-view-font-lock-keywords*
    (setq howm-view-font-lock-keywords *howm-view-font-lock-keywords*))
  (when howm-view-font-lock-keywords
    (cheat-font-lock-merge-keywords howm-view-font-lock-keywords
                                    howm-user-font-lock-keywords
                                    howm-view-summary-font-lock-keywords))
  ;; font-lock-set-defaults removes these local variables after 2008-02-24
  (set (make-local-variable 'font-lock-keywords-only) t)
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  ;;     (setq font-lock-keywords-case-fold-search
  ;;           howm-view-grep-ignore-case-option)
  (cheat-font-lock-fontify)
  )

(riffle-define-derived-mode howm-view-contents-mode riffle-contents-mode "HowmC"
  "memo viewer (contents mode)
key	binding
---	-------
\\[howm-view-contents-open]	Open file
\\[next-line]	Next line
\\[previous-line]	Previous line
\\[scroll-up]	Scroll up
\\[scroll-down]	Scroll down
\\[riffle-scroll-up]	Scroll one line up
\\[riffle-scroll-down]	Scroll one line down
\\[riffle-contents-to-summary]	Summary
\\[riffle-contents-goto-next-item]	Next item
\\[riffle-contents-goto-previous-item]	Previous item

\\[howm-view-filter]	Filter (by date, contents, etc.)
\\[howm-view-filter-by-contents]	Search (= filter by contents)
\\[howm-view-sort]	Sort
\\[howm-view-sort-reverse]	Reverse order
\\[howm-view-dired]	Invoke Dired-X
\\[describe-mode]	This help
\\[riffle-kill-buffer]	Quit
"
;   (kill-all-local-variables)
  (make-local-variable 'font-lock-keywords)
  (cheat-font-lock-mode howm-view-font-lock-silent)
  (let ((ck `((,howm-view-header-regexp (0 howm-view-hilit-face))))
        (sk (or (howm-view-font-lock-keywords)
                *howm-view-font-lock-keywords*)))
;;         ;; extremely dirty!! [2003/10/06 21:08]
;;         (sk (or (with-current-buffer (riffle-summary-buffer)
;;                   font-lock-keywords)
;;                 *howm-view-font-lock-keywords*)))
    (cheat-font-lock-merge-keywords sk ck
                                    howm-user-font-lock-keywords
                                    howm-view-contents-font-lock-keywords)
    ;; font-lock-set-defaults removes these local variables after 2008-02-24
    (set (make-local-variable 'font-lock-keywords-only) t)
    (set (make-local-variable 'font-lock-keywords-case-fold-search)
         howm-view-grep-ignore-case-option)
    (cheat-font-lock-fontify)
    ))

(defun howm-view-font-lock-keywords ()
  (with-current-buffer (riffle-summary-buffer)
    howm-view-font-lock-keywords))

;;; keymaps

;; (defvar howm-view-summary-mode-map nil)
;; (defvar howm-view-contents-mode-map nil)

(defun howm-view-define-common-key (keymap)
  (let ((m keymap))
;;     (define-key m "?" 'howm-view-help)
    (define-key m "f" 'howm-view-filter)
    (define-key m "G" 'howm-view-filter-by-contents)
    (define-key m "S" 'howm-view-sort)
    (define-key m "R" 'howm-view-sort-reverse)
    (define-key m "q" 'howm-view-kill-buffer)
    (define-key m "X" 'howm-view-dired)
    ))

(let ((m howm-view-summary-mode-map))
  (define-key m "\C-m" 'howm-view-summary-open)
  (define-key m "\C-j" 'howm-view-summary-open)
  (define-key m "u" 'howm-view-filter-uniq)
  (define-key m "!" 'howm-view-summary-shell-command)
  (define-key m "T" 'howm-list-toggle-title) ;; defined in other file. dirty!
  ;;     (define-key m howm-reminder-quick-check-key 'howm-reminder-quick-check)
  ;;     (define-key m ";" 'howm-view-invoke-action-lock)
  (define-key m "\C-i" 'howm-view-summary-next-section)
  (define-key m "\M-\C-i" 'howm-view-summary-previous-section)
  (define-key m [tab] 'howm-view-summary-next-section)
  (define-key m [(meta tab)] 'howm-view-summary-previous-section)
  (howm-view-define-common-key m))

(let ((m howm-view-contents-mode-map))
  (define-key m "\C-m" 'howm-view-contents-open)
  (define-key m "\C-j" 'howm-view-contents-open)
  (howm-view-define-common-key m))

;;; summary

(defun howm-view-summary (&optional name item-list fl-keywords)
  (let* ((*howm-view-font-lock-keywords* fl-keywords) ;; ok? [2008-07-11]
         (r (riffle-summary name item-list ':howm
                           (howm-view-in-background-p))))
    (if (null r)
        (message "No match")
      ;; We want to entry font-lock keywords even when background-p.
      (when *howm-view-font-lock-keywords*
        (setq howm-view-font-lock-keywords *howm-view-font-lock-keywords*)))
    r))

;; (defun howm-view-summary (&optional name item-list)
;;   (let ((*howm-view-font-lock-keywords* t))
;;     (riffle-summary name item-list ':howm)))

(defun howm-view-summary-open (&optional reverse-delete-p)
  (interactive "P")
  (when (not (and howm-view-summary-keep-cursor
                  (get-buffer-window (riffle-contents-buffer))))
    (riffle-summary-check t))
  (let* ((p (riffle-persistent-p howm-view-summary-persistent))
         (persistent (if reverse-delete-p
                         (not p)
                       p)))
    (howm-record-view-window-configuration)
    (howm-view-summary-open-sub (not persistent))))

(defun howm-view-summary-open-sub (&optional kill)
  (interactive "P")
  (let ((b (riffle-contents-buffer))
        (looking-at-str (buffer-substring-no-properties (point)
                                                        (line-end-position))))
    (riffle-pop-to-buffer b howm-view-summary-window-size)
    (let ((howm-view-open-hook nil)) ;; Don't execute it in contents-open.
      (howm-view-contents-open-sub kill))
    (end-of-line)
    (or (search-backward looking-at-str (line-beginning-position) t)
        (beginning-of-line))
    (run-hooks 'howm-view-open-hook)))

(defvar howm-view-summary-item-previous-name nil
  "for internal use")
(defun howm-view-summary-item (item)
  ;; Clean me. This depends on implementation of `riffle-summary-show'
  ;; severely.
  (when (eq (point) (point-min))
    (setq howm-view-summary-item-previous-name ""))
  (let* ((f (howm-item-name item))
         (name (if (and howm-view-summary-omit-same-name
                        (string= f howm-view-summary-item-previous-name))
                   ""
                 (progn
                   (setq howm-view-summary-item-previous-name f)
                   (howm-view-item-basename item t))))
         (h (format howm-view-summary-format name)))
    (concat h (howm-view-item-summary item))))

(defun howm-view-summary-next-section (&optional n)
  (interactive "P")
  (setq n (or n 1))
  (let ((i (abs n))
        (step (if (>= n 0) 1 -1)))
    (while (and (> i 0)
                (howm-view-summary-next-section-sub step))
      (setq i (1- i)))))
(defun howm-view-summary-previous-section (&optional n)
  (interactive "P")
  (setq n (or n 1))
  (howm-view-summary-next-section (- n)))
(defun howm-view-summary-next-section-sub (step)
  ;; inefficient. so what?
  (let* ((f (lambda ()
              (howm-view-item-filename (riffle-summary-current-item))))
;;               (riffle-controller 'section (riffle-summary-current-item))))
         (cont-p (lambda ()
                   (save-excursion
                     (let ((a (funcall f)))
                       (forward-line -1)
                       (string= a (funcall f)))))))
    (while (and (= (forward-line step) 0)
                (funcall cont-p))
      ;; no body
      )))

;;; contents

(defun howm-view-contents-open (&optional reverse-delete-p)
  (interactive "P")
  (let* ((p (riffle-persistent-p howm-view-contents-persistent))
         (persistent (if reverse-delete-p
                         (not p)
                       p)))
    (howm-record-view-window-configuration)
    (howm-view-contents-open-sub (not persistent))))

(defvar *howm-view-item-privilege* nil) ;; dirty

(defun howm-view-contents-open-sub (&optional kill)
  (let* ((item (riffle-contents-current-item))
         (page (howm-item-page item))
         (offset (howm-view-item-offset item))
         (pos (- (point) offset))
         (viewer (howm-view-external-viewer page)))
    (when kill
      (riffle-kill-buffer))
    (when (howm-view-item-privilege item)
      (riffle-restore-window-configuration)) ;; force without mode check
    (setq *howm-view-item-privilege* (howm-view-item-privilege item)) ;; dirty
    (run-hooks 'howm-view-before-open-hook)
    (if viewer
        (howm-view-call-external-viewer viewer page)
      (howm-view-open-item item
                           (lambda ()
                             (when (or (< pos (point-min)) (<= (point-max) pos))
                               (widen))
                             (goto-char pos))
                           t))
    (run-hooks 'howm-view-open-hook)))

(defun howm-view-open-item (item &optional position-setter merely)
  (howm-page-open (howm-item-page item))
  (howm-view-set-mark-command)
  (if position-setter
      (funcall position-setter)
    (howm-view-set-place (howm-item-place item)))
  (recenter howm-view-open-recenter)
  (when (not merely)
    (howm-view-open-postprocess)))
(defun howm-view-open-postprocess ()
  (run-hooks 'howm-view-open-hook))

(defvar howm-view-previous-section-page nil "For internal use")
(defvar howm-view-previous-section-beg nil "For internal use")
(defvar howm-view-previous-section-end nil "For internal use")

(defun howm-view-contents-item (item)
  (when (howm-buffer-empty-p)
    (setq howm-view-previous-section-page ""
          howm-view-previous-section-beg nil
          howm-view-previous-section-end nil))
  (let* ((page (howm-item-page item))
         (place (howm-view-item-place item))
         (peq (howm-page= page howm-view-previous-section-page)) ;; dirty!
         (done-p (if place
                     (and peq
                          (<= howm-view-previous-section-beg place)
                          (<= place howm-view-previous-section-end))
                   peq)))
    (if done-p
        ""
      (let* ((header (if (null (cdr (howm-view-item-list))) ;; dirty!
                         ""
                       (format howm-view-header-format
                               (howm-page-abbreviate-name page))))
             (header-length (howm-view-string-point-count header))
             (viewer (howm-view-external-viewer page)))
        (concat header
                (howm-view-contents-item-sub item page place header viewer
                                             (+ (point) header-length)))))))

(defvar howm-view-string-point-count-strict nil)
(defun howm-view-string-point-count (str)
  "Count points of string STR.
Namely, it is the difference between start position and end position
of STR if STR is inserted to a buffer.
It looks to be simply equal to (length STR) on emacs-21.1.1.
But I'm not sure for multi-byte characters on other versions of emacsen."
  (if howm-view-string-point-count-strict
      (with-temp-buffer
        (insert str)
        (- (point) (point-min)))
    ;; I assume (length (buffer-substring-no-properties START END))
    ;; is equal to (abs (- START END))). Is it correct?
    ;; (cf.) snap://Info-mode/elisp#Positions
    (length str)))

(defun howm-view-contents-item-sub (item page place header viewer c)
  (with-temp-buffer
    (let (b e h)
      (if viewer
          (howm-view-contents-indicator viewer page)
        (howm-page-insert page))
      (if place
          (progn
            (riffle-set-place place)
            (setq h (point))
            (let ((r (howm-view-contents-region page)))
              (setq b (car r)
                    e (cadr r))))
        (setq b (point-min)
              e (point-max)
              h b))
      (howm-view-item-set-offset item (- c b))
      (howm-view-item-set-home item (+ c (- b) h))
      (setq howm-view-previous-section-page page ;; dirty!
            howm-view-previous-section-beg (riffle-get-place b)
            howm-view-previous-section-end (riffle-get-place e))
      (buffer-substring-no-properties b e))))

(defvar howm-view-preview-narrow t)
(defun howm-view-contents-region (filename)
  (when filename
    (howm-page-set-configuration filename))
  (if (or howm-view-preview-narrow
          (not (riffle-preview-p)))
      (howm-view-paragraph-region)
    (list (point-min) (point-max))))

(defun howm-view-contents-indicator (viewer fname)
  (insert (howm-viewer-indicator viewer fname)))

(defun howm-view-paragraph-region (&optional include-following-blank-p)
  (let ((b (save-excursion
             (end-of-line)
             (re-search-backward howm-view-title-regexp
                                 nil 'to-limit)
             (line-beginning-position)))
        (e (save-excursion
             (end-of-line)
             (let ((found (re-search-forward howm-view-title-regexp
                                             nil 'to-limit)))
               (if include-following-blank-p
                   (if found (match-beginning 0) (point-max))
                 (progn
                   (if found
                       (forward-line -1)
                     (goto-char (point-max)))
;                   (end-of-line)
                   (while (and (looking-at "^$")
                               (= (forward-line -1) 0)) ;; successful
                     nil) ;; dummy
                   (end-of-line)
                   (point)))))))
    (list b e)))

(defun howm-view-set-mark-command ()
  (set-mark-command nil)
  (howm-deactivate-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc.

(defun howm-view-file-list (&optional item-list)
  (howm-cl-remove-duplicates* (mapcar #'howm-view-item-filename
                                      (or item-list (howm-view-item-list)))
                              :test #'howm-page=))

(defun howm-view-mtime (file)
  (howm-view-time-to-string (howm-page-mtime file)))

;; (defun howm-view-xtime (file x)
;;   (let* ((a (file-attributes file))
;;          (n (cdr (assoc x '((a . 4) (m . 5) (c . 6)))))
;;          (ti (nth n a)))
;;     (howm-view-time-to-string ti)))

(defun howm-view-time-to-string (ti)
  (format-time-string "%Y%m%d-%H%M%S" ti))

(defun howm-view-string> (a b)
  (string< b a))

(defun howm-view-string<= (a b)
  (not (string< b a)))

(defun howm-view-string< (a b)
  (string< a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dir

(defun howm-view-directory (dir &optional recursive-p)
  (howm-view-summary "" (howm-folder-items dir recursive-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filter

(defun howm-view-filter (&optional remove-p)
  (interactive "P")
  (let* ((table howm-view-filter-methods)
         (command (completing-read (if remove-p
                                       "(Reject) filter by: "
                                     "filter by: ")
                                   table nil t)))
    (call-interactively (cdr (assoc command table)))))

(defun howm-view-filter-uniq ()
  (interactive)
  (howm-view-filter-doit #'howm-filter-items-uniq))

(defun howm-view-filter-by-name (&optional remove-p regexp)
  (interactive "P")
  (howm-view-filter-by-name/summary #'howm-filter-items-by-name
                                    regexp remove-p))

(defun howm-view-filter-by-summary (&optional remove-p regexp)
  (interactive "P")
  (howm-view-filter-by-name/summary #'howm-filter-items-by-summary
                                    regexp remove-p))

(defun howm-view-filter-by-name/summary (filter regexp remove-p)
  (let* ((r (or regexp (howm-view-filter-read-from-minibuffer "Regexp: "
                                                              remove-p)))
         (f `(lambda (item-list rmv-p)
               (funcall #',filter item-list ,r rmv-p))))
    (howm-view-filter-doit f remove-p)))

(defun howm-view-filter-by-date (&optional remove-p)
  (interactive "P")
  (howm-view-filter-by-time-range #'howm-filter-items-by-date
                                  remove-p))

(defun howm-view-filter-by-reminder (&optional remove-p)
  (interactive "P")
  (howm-view-filter-by-time-range #'howm-filter-items-by-reminder remove-p))

(defun howm-view-filter-by-mtime (&optional remove-p range)
  (interactive "P")
  (howm-view-filter-by-time-range #'howm-filter-items-by-mtime remove-p range))

(defun howm-view-filter-by-time-range (filter &optional remove-p range)
  (let* ((r (or range (howm-view-ask-time-range remove-p)))
         (from (car r))
         (to (cadr r))
         (f `(lambda (item-list rmv-p)
               (funcall #',filter item-list ',from ',to rmv-p))))
    (howm-view-filter-doit f remove-p)))

(defun howm-view-filter-by-region (beg end)
  (interactive "r")
  (let ((r (mapcar #'howm-view-line-number (list beg end))))
    (howm-view-filter-by-line-range (car r) (cadr r))))

(defvar howm-view-filter-by-around-default 10)
(defun howm-view-filter-by-around (&optional distance)
  (interactive "P")
  (let* ((d (or distance howm-view-filter-by-around-default))
         (c (howm-view-line-number)))
    (howm-view-filter-by-line-range (- c d) (+ c d))))

(defun howm-view-filter-by-line-range (beg end)
  (let ((f `(lambda (item-list remove-p)
              (when remove-p
                (error "Not supported."))
              ;; beg and end are counted as 1,2,3,...
              (cl-subseq item-list
                              (max (1- ,beg) 0)
                              ;; end is included.
                              (min ,end (length item-list))))))
    (howm-view-filter-doit f)))

(defun howm-view-filter-by-contents (&optional remove-p regexp)
  (interactive "P")
  (let ((r (or regexp (howm-view-filter-read-from-minibuffer
                       "Search in result (grep): "
                       remove-p))))
    (if remove-p
        (howm-view-remove-by-contents r)
      (howm-view-search-in-result r))))

(howm-if-ver1dot3 nil
  (defcustom howm-view-search-in-result-correctly t
    "*Non nil if search-in-result should be aware of paragraph."
    :type 'boolean
    :group 'howm-search))

(defun howm-view-search-in-result (regexp)
;;   (interactive "sSearch in result (grep): ")
  (let* ((orig (howm-view-name))
         (name (if (string= orig "")
                   regexp
                 (format "%s&%s" orig regexp)))
         (orig-item-list (howm-view-item-list))
         (folder (howm-make-folder-from-items orig-item-list)))
    (howm-write-history regexp)
    (howm-view-search-folder regexp folder name)
    (when howm-view-search-in-result-correctly
      (howm-view-summary-rebuild (howm-item-list-filter (howm-view-item-list)
                                                        orig-item-list)))))

(defun howm-view-remove-by-contents (regexp)
;;   (interactive "s(Reject) Search in result (grep): ")
  (let ((howm-v-r-b-c-regexp regexp))
    (howm-view-sort/filter-doit
     (lambda (item-list switch)
       (howm-filter-items-by-contents item-list howm-v-r-b-c-regexp t)))))

(defun howm-view-sort/filter-doit (proc &optional switch)
  (let ((kw font-lock-keywords))
    (prog1
        ;; return item-list for
        ;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder
        (howm-view-summary-rebuild (funcall proc (howm-view-item-list) switch))
      (setq font-lock-keywords kw))))

(defalias 'howm-view-filter-doit 'howm-view-sort/filter-doit)

;; For backward compatibility with howmoney. Don't use this.
(defun howm-view-filter-general (pred)
  (howm-view-filter-doit (lambda (item-list dummy)
                           (cl-remove-if-not pred item-list))))
;; (defun howm-view-filter-general (pred &optional remove-p with-index)
;;   (let* ((item-list (howm-view-item-list))
;;          (s (if with-index
;;                 (howm-map-with-index #'list item-list)
;;               item-list))
;;          (r (if remove-p
;;                 (cl-remove-if pred s)
;;               (cl-remove-if-not pred s)))
;;          (filtered (if with-index
;;                        (mapcar #'car r)
;;                      r)))
;;     (howm-view-summary-rebuild filtered)))

(defmacro howm-filter-items (pred lis &optional remove-p)
  `(if ,remove-p
       (cl-remove-if ,pred ,lis)
     (cl-remove-if-not ,pred ,lis)))

(defun howm-filter-items-uniq (item-list &optional remove-p)
  (when remove-p
    (error "Not supported."))
  (let* ((howm-view-filter-uniq-prev (if howm-view-search-in-result-correctly
                                         (cons "" nil)
                                       ""))
         (pred (if howm-view-search-in-result-correctly
                   (lambda (item)
                     (let ((page (howm-item-page item))
                           (place (howm-item-place item))
                           (range (howm-item-range item))
                           (p-page  (car howm-view-filter-uniq-prev))
                           (p-range (cdr howm-view-filter-uniq-prev)))
                       (prog1
                           (not (and (howm-page= page p-page)
                                     (and place p-range
                                          (<= (car p-range) place)
                                          (<= place (cadr p-range)))))
                         (setq howm-view-filter-uniq-prev (cons page range)))))
                 ;; old code
                 (lambda (item)
                   (let ((f (howm-view-item-filename item)))
                     (prog1
                         (not (howm-page= f howm-view-filter-uniq-prev))
                       (setq howm-view-filter-uniq-prev f)))))))
    (cl-remove-if-not pred item-list)))

(defun howm-filter-items-by-name (item-list regexp &optional remove-p)
  (howm-filter-items-by-name/summary #'howm-view-item-basename
                                     item-list regexp remove-p))

(defun howm-filter-items-by-summary (item-list regexp &optional remove-p)
  (howm-filter-items-by-name/summary #'howm-view-item-summary
                                     item-list regexp remove-p))

(defun howm-filter-items-by-name/summary (accessor item-list regexp remove-p)
  (howm-filter-items (lambda (item)
                       (string-match regexp (funcall accessor item)))
                     item-list remove-p))

(defun howm-filter-items-by-date (item-list from to &optional remove-p)
  (let* ((form (howm-view-file-name-format))
         (fts (mapcar (lambda (x)
                        (file-name-nondirectory (format-time-string form x)))
                      (list from to)))
         (fs (car fts))
         (ts (cadr fts)))
    (howm-filter-items (lambda (item)
                         (let ((cs (howm-view-item-basename item)))
                           (and (howm-view-string<= fs cs)
                                (howm-view-string< cs ts))))
                       item-list remove-p)))

(defun howm-filter-items-by-reminder (item-list from to &optional remove-p)
  (let* ((from-str (format-time-string howm-date-format from))
         (to-str (format-time-string howm-date-format to))
         (reg (howm-reminder-regexp howm-reminder-types)))
    (howm-filter-items
     (lambda (item)
       (let ((s (howm-view-item-summary item)))
         (and (string-match reg s)
              (let* ((x (match-string-no-properties 0 s)) ;; [2004-02-07]@
                     (d (and (string-match howm-date-regexp x)
                             (match-string-no-properties 0 x)))) ;; [2004-02-07]
                (and (howm-view-string<= from-str d)
                     (howm-view-string< d to-str))))))
     item-list remove-p)))

(defun howm-filter-items-by-mtime (item-list from to &optional remove-p)
  (let ((fs (howm-view-time-to-string from))
        (ts (howm-view-time-to-string to)))
    (howm-filter-items
     (lambda (item)
       (let ((cs (howm-view-mtime (howm-view-item-filename item))))
         (and (howm-view-string<= fs cs)
              (howm-view-string< cs ts))))
     item-list remove-p)))

(defun howm-filter-items-by-contents (item-list regexp &optional remove-p)
  (let* ((match (howm-view-search-folder-items-fi regexp item-list)))
    (if howm-view-search-in-result-correctly
        (howm-item-list-filter item-list match remove-p)
      ;; old behavior
      (let ((match-names (howm-cl-remove-duplicates*
                          (mapcar #'howm-item-name match))))
        (howm-filter-items (lambda (item)
                             (member (howm-item-name item) match-names))
                           item-list remove-p)))))

(defun howm-view-file-name-format ()
  howm-file-name-format) ;; defined in howm-common.el

(defun howm-view-ask-time-range (&optional remove-p)
  (let* ((now (current-time))
         (from (howm-view-ask-time "From" now t remove-p))
         (to (howm-view-ask-time "To" from nil remove-p)))
    (list from to)))

(defvar howm-view-min-year 1950)
(defvar howm-view-max-year 2030)
(defun howm-view-ask-time (prompt default &optional from-p remove-p)
  (let* ((z (decode-time default))
         (yd (nth 5 z))
         (md (nth 4 z))
         (dd (nth 3 z)))
    (let (y0 m0 d0 hour0 min0 sec0)
      (if from-p
          (setq y0 howm-view-min-year m0 1 d0 1
                hour0 0 min0 0 sec0 0)
        (setq y0 howm-view-max-year m0 12 d0 'last-day-of-month
              hour0 24 min0 0 sec0 0))
      (let ((y (howm-ask-time-sub prompt "year" yd remove-p)))
        (if (null y)
            (howm-view-encode-time sec0 min0 hour0 d0 m0 y0)
          (let ((m (howm-ask-time-sub prompt "month" md remove-p)))
            (if (null m)
                (howm-view-encode-time sec0 min0 hour0 d0 m0 y)
              (let ((d (or (howm-ask-time-sub prompt "date" dd remove-p) d0)))
                (howm-view-encode-time sec0 min0 hour0 d m y)))))))))

(defun howm-ask-time-sub (prompt ymd default remove-p)
  (let* ((message (format "%s %s (* = no limit) [%d]: " prompt ymd  default))
         (raw (howm-view-filter-read-from-minibuffer message remove-p))
         (n (if (string= raw "")
                default
              (string-to-number raw))))
    (if (= n 0)
        nil
      n)))

(defun howm-view-encode-time (sec min hour d m y)
  (when (eq d 'last-day-of-month)
    (setq m (+ m 1))
    (setq d -1))
  (encode-time sec min hour d m y))

(defun howm-view-filter-read-from-minibuffer (message &optional remove-p)
  (read-from-minibuffer (if remove-p
                            (concat "(Reject) " message)
                          message)))

(defun howm-view-summary-rebuild (item-list &optional fl-keywords)
  (howm-view-summary (howm-view-name) item-list fl-keywords))

(let* ((h (regexp-quote howm-view-title-header))
       (t1 (format "Skip \"%s \"" howm-view-title-header))
       (r1 (format "^\\(%s\\)? *$" h))
       (t2 (format "Skip \"%s \" and \"[xxxx-xx-xx xx:xx]\""
                   howm-view-title-header))
       (r2 (format "\\(%s\\)\\|\\(^\\[[-: 0-9]+\\]\\)" r1)))
  (howm-if-ver1dot3 nil
    (defcustom howm-view-title-skip-regexp r2
      "*Regular expression for lines which should not be titles.
If the original title matches this regexp, the first non-matched line
is shown as title instead.
Nil disables this feature.

This feature does not work when `howm-view-search-in-result-correctly' is nil."
      :type `(radio (const :tag "Off" nil)
                    (const :tag ,t1 ,r1)
                    (const :tag ,t2 ,r2)
                    regexp)
      :group 'howm-title
      :group 'howm-efficiency)))

(defcustom howm-view-list-title-type 1
  "*Type of showing title in summary buffer.
Value 1 means \"show title instead of summary\".
Value 2 means \"show title before summary\".
You may want to set `howm-view-summary-format' to be \"\" if you never need
to see file names."
  :type '(radio (const :tag "title instead of summary"
                       1)
                (const :tag "title before summary"
                       2))
  :group 'howm-experimental)

(defun howm-view-list-title (title-regexp)
  (howm-view-summary-rebuild (howm-entitle-items
                              title-regexp (howm-view-item-list))))

(defun howm-entitle-items (title-regexp item-list)
  (if (= howm-view-list-title-type 1)
      (howm-entitle-items-style1 title-regexp item-list)
    (howm-entitle-items-style2 title-regexp item-list)))

(defun howm-entitle-items-style1 (title-regexp item-list)
  "Put title instead of summary."
  (let ((items (howm-view-search-folder-items-fi title-regexp item-list)))
    (if howm-view-search-in-result-correctly
        (let* ((r (howm-item-list-filter items item-list 'with-rest))
               (hit-items (car r))
               (nohit-items (cdr r))
               ;; should I use (howm-classify #'howm-item-place nohit-items) ?
               (noplace-nohit-items
                (cl-remove-if #'howm-item-place nohit-items))
               (rest-items
                (howm-item-list-filter (cl-remove-if-not #'howm-item-place
                                                              nohit-items)
                                       items t))
               (all-items (append hit-items noplace-nohit-items rest-items)))
          (when howm-view-title-skip-regexp
            (mapc #'howm-view-change-title all-items))
          all-items)
      (let* ((pages (howm-cl-remove-duplicates* (mapcar #'howm-item-page
                                                        item-list)))
             (hit-pages (mapcar #'howm-item-page items))
             (nohit-pages (cl-remove-if
                           (lambda (p) (cl-member p hit-pages
                                                        :test #'howm-page=))
                           pages))
             (nohit-items (mapcar #'howm-make-item nohit-pages))
             (all-items (if (null nohit-items)
                            items
                          (append items nohit-items))))
        all-items))))

(defvar howm-entitle-items-style2-max-length 20)
(defvar howm-entitle-items-style2-format "%-13s | %s") ;; for title and summary
(defvar howm-entitle-items-style2-title-line nil) ;; independent title line?
(defun howm-entitle-items-style2 (title-regexp item-list)
  "Put title before summary."
  ;; fix me: howm-item-place is not set for howm-list-all
  (let ((last-title ""))
    (cl-mapcan
     (lambda (item)
       (let ((orig (howm-item-summary item))
             (titles (howm-item-titles item)))
         (cl-mapcan
          (lambda (s)
            (if (string= s last-title)
                (setq s "")
              (setq last-title s))
            (when (> (length s) howm-entitle-items-style2-max-length)
              (setq s (substring s 0 howm-entitle-items-style2-max-length)))
            (mapcar (lambda (x)
                      (let ((i (howm-item-dup item)))
                        (howm-item-set-summary i x)
                        i))
                    (if (and howm-entitle-items-style2-title-line
                             (not (string= s "")))
                        (list (format howm-entitle-items-style2-format
                                      s "")
                              (format howm-entitle-items-style2-format
                                      "" orig))
                      (list (format howm-entitle-items-style2-format
                                    s orig)))))
          (or titles (list "")))))
     item-list)))

;;; detect items in same paragraph (= entry = memo. sorry for inconsistent terminology)

(defun howm-item-with-temp-buffer (item proc)
  (with-temp-buffer
    (howm-page-insert (howm-item-page item))
    (let* ((p (howm-item-place item))
           (r (if (null p)
                  (list (point-min) (point-max))
                (progn
                  (riffle-set-place p)
                  (howm-view-paragraph-region)))))
      (narrow-to-region (car r) (cadr r))
      (funcall proc item))))

(defun howm-item-titles (item)
  "List of titles of ITEM.
When place (see `howm-item-place') is specified, ITEM has at most one title.
Otherwise, ITEM can have two or more titles."
  (howm-item-with-temp-buffer
   item
   (lambda (i)
     (let ((titles nil))
       (goto-char (point-min))
       (while (re-search-forward (howm-list-title-regexp) nil t)
         (setq titles
               (cons (buffer-substring-no-properties (match-beginning 0)
                                                     (line-end-position))
                     titles)))
       (mapcar (lambda (x)
                 (if (string-match howm-view-title-regexp x)
                     (match-string-no-properties howm-view-title-regexp-pos x)
                   x))
               (reverse titles))))))

(defun howm-item-range (item)
  "List of beginning-place and end-place of paragraph to which ITEM belongs."
  (howm-item-with-temp-buffer
   item
   (lambda (i)
     (let ((r (list (point-min) (point-max))))
       (widen)
       (list (progn
               (goto-char (car r))
               (riffle-get-place))
             (progn
               (goto-char (cadr r))
               (riffle-get-place)))))))
;;   (with-temp-buffer
;;     (howm-page-insert (howm-item-page item))
;;     (let* ((p (howm-item-place item))
;;            (r (if (null p)
;;                   (list (point-min) (point-max))
;;                 (progn
;;                   (riffle-set-place p)
;;                   (howm-view-paragraph-region)))))
;;       (list (progn
;;               (goto-char (car r))
;;               (riffle-get-place))
;;             (progn
;;               (goto-char (cadr r))
;;               (riffle-get-place))))))

(defun howm-item-list-rangeset (item-list)
  "Make assoc list of page to rangeset.
ITEM-LIST is list of items.
Return value is assoc list; each element of it is a cons pair of page
and rangeset which indicates ranges of places of paragraphs to which items
in ITEM-LIST belongs."
  (let ((alist nil))  ;; key = page, value = rangeset of place
    (cl-labels ((add-to-alist (page rs)
                           (setq alist (cons (cons page rs) alist))))
      (mapc (lambda (item)
              (let* ((page (howm-item-page item))
                     (place (howm-item-place item))
                     (rs (cdr (assoc page alist))))
                (cond ((null place)
                       (add-to-alist page (howm-make-rangeset)))
                      ((null rs)
                       (add-to-alist page (howm-make-rangeset
                                           (howm-item-range item))))
                      ((howm-rangeset-belong-p place rs)
                       nil) ;; do nothing
                      (t
                       (howm-rangeset-add! rs (howm-item-range item))))))
            item-list)
      alist)))

(defun howm-item-list-filter (item-list reference-item-list
                                        &optional remove-match)
  "Select items in ITEM-LIST according to REFERENCE-ITEM-LIST.
When REMOVE-MATCH is nil, return value is list of items i in ITEM-LIST
which satisfy the condition \"there exists i' in REFERENCE-ITEM-LIST
such that i and i' belong to same paragraph\" (case 1).
When REMOVE-MATCH is non-nil and not the symbol 'with-rest',
return value is complement of the above list;
list of items in ITEM-LIST which do not satisfy the above condition (case 2).
When REMOVE-MATCH is the symbol 'with-rest',
return value is (A . B), where A is the return value of case 1 and
B is items in REFERENCE-ITEM-LIST that do not match in case 1."
  ;; 
  ;; split no-place items:
  ;; Though implementation 1 calls grep many times,
  ;; implementation 2 is slower in construction of folder from items.
  ;; [2012-12-28]
  ;; 
  ;; implementation 1 (call grep many times)
  (setq item-list
        (cl-mapcan (lambda (item)
                          (if (howm-item-place item)
                              (list item)
                            (or (howm-view-search-folder-items-fi
                                 (howm-view-title-regexp-grep) (list item))
                                (list item))))
                        item-list))
  ;; 
  ;; ;; implementation 2 (making items-folder is slow)
  ;; (let* ((place-items (cl-remove-if-not #'howm-item-place item-list))
  ;;        (no-place-items (cl-remove-if #'howm-item-place item-list))
  ;;        (split-items (howm-view-search-folder-items-fi
  ;;                      (howm-view-title-regexp-grep) no-place-items))
  ;;        ;;; !!!!!!!!! use CL !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;;        (no-title-items (set-difference no-place-items split-items
  ;;                                        :key #'howm-item-page)))
  ;;   (setq item-list (append place-items split-items no-title-items)))
  ;;
  (let* ((alist (howm-item-list-rangeset reference-item-list))
         (matcher (lambda (item)
                    (let* ((page (howm-item-page item))
                           (place (howm-item-place item))
                           (rs (cdr (assoc page alist))))
                      (cond ((null rs) nil)
                            ((howm-rangeset-belong-p place rs) rs)
                            (t nil))))))
    (cond ((eq remove-match 'with-rest)
           (let ((match (cl-remove-if-not
                         (lambda (item)
                           (let ((rs (funcall matcher item)))
                             (and rs (howm-rangeset-hit! rs))))
                         item-list)))
             (cons match
                   (cl-mapcan
                    (lambda (a) (and (not (howm-rangeset-hit-p (cdr a)))
                                     (list (howm-make-item (car a)))))
                    alist))))
          (remove-match (cl-remove-if matcher item-list))
          (t (cl-remove-if-not matcher item-list)))))

;;; rangeset
;;; ex.
;;; (*rangeset* (1 . 4) (5 . 6) (8 . 14))
;;; (*rangeset*) ==> "almighty"
;;; (*rangeset-hit* (1 . 4) (5 . 6) (8 . 14)) ==> "hit" is recorded

(defun howm-make-rangeset (&optional beg-end)
  (if (null beg-end)
      (cons '*rangeset* nil)
    (let ((rs (howm-make-rangeset)))
      (howm-rangeset-add! rs beg-end))))

(defun howm-rangeset-belong-p (point rs)
  (or (null (cdr rs))
      (cl-member-if (lambda (pair)
                           (and (<= (car pair) point) (<= point (cdr pair))))
                         (cdr rs))))

(defun howm-rangeset-add! (rs beg-end)
  ;; "almighty" is ignored here. sorry for confusion...
  ;; c = cursor (pointing its cdr)
  ;; p = pair
  (let ((c rs)
        (beg (car beg-end))
        (end (cadr beg-end)))
    (while (and (cdr c) beg)
      (let ((p (cadr c)))
        (cond ((< end (car p)) ;; insert [beg, end] here
               (rplacd c (cons (cons beg end) (cdr c)))
               (setq beg nil))
              ((< (cdr p) beg) ;; skip this
               (setq c (cdr c)))
              (t ;; merge into [beg, end]
               (setq beg (min beg (car p))
                     end (max end (cdr p)))
               (rplacd c (cddr c))))))
    (when beg
      (rplacd c (list (cons beg end)))))
  rs)

(defvar howm-rangeset-hit-indicator '*rangeset-hit*)

(defun howm-rangeset-hit! (rs)
  (setcar rs howm-rangeset-hit-indicator))

(defun howm-rangeset-hit-p (rs)
  (eq (car rs) howm-rangeset-hit-indicator))

;; check

(let ((tests '(
               (()
                ())
               (((3 . 5))
                ((3 . 5)))
               (((3 . 5) (0 . 1))
                ((0 . 1) (3 . 5)))
               (((3 . 5) (6 . 8))
                ((3 . 5) (6 . 8)))
               (((3 . 5) (1 . 4))
                ((1 . 5)))
               (((3 . 5) (4 . 7))
                ((3 . 7)))
               (((3 . 5) (1 . 9))
                ((1 . 9)))
               (((3 . 1) (4 . 1) (5 . 9))
                ((1 . 4) (5 . 9)))
               (((3 . 1) (4 . 1) (5 . 9) (2 . 6) (5 . 3))
                ((1 . 9)))
               ))
       ;; inhibit 'reference to free variable' warning in byte-compilation
      (check nil))
  (cl-labels ((check (ans result)
                  (cond ((null ans) (null result))
                        ((not (equal (car ans) (car result))) nil)
                        (t (funcall check (cdr ans) (cdr result))))))
    (mapc (lambda (z)
            (apply (lambda (prob ans)
                     (let* ((rs (howm-make-rangeset)))
                       (mapc (lambda (pair)
                               (let ((a (car pair))
                                     (b (cdr pair)))
                                 (howm-rangeset-add! rs
                                                     (list (min a b)
                                                           (max a b)))))
                             prob)
                       (when (not (equal (cdr rs) ans))
                         (error "howm-rangeset-add: %s ==> %s" prob rs))))
                   z))
          tests)))

(let ((rs '(*rangeset* (1 . 4) (5 . 6) (8 . 14))))
  (if (and (howm-rangeset-belong-p 1 rs)
           (howm-rangeset-belong-p 3 rs)
           (howm-rangeset-belong-p 4 rs)
           (howm-rangeset-belong-p 5 rs)
           (not (howm-rangeset-belong-p 0 rs))
           (not (howm-rangeset-belong-p 4.5 rs))
           (not (howm-rangeset-belong-p 7 rs))
           (not (howm-rangeset-belong-p 15 rs)))
      t
    (error "howm-rangeset-belong-p: wrong result")))

(defun howm-view-change-title (item)
  (when (string-match howm-view-title-skip-regexp (howm-item-summary item))
    (let ((title-line (with-temp-buffer
                        (howm-page-insert (howm-item-page item))
                        (howm-view-set-place (or (howm-item-place item)
                                                 (howm-view-get-place
                                                  (point-min))))
                        (howm-view-get-title-line))))
      (howm-item-set-summary item title-line))))

(defun howm-view-get-title-line ()
  (while (and (looking-at howm-view-title-skip-regexp)
              (= (forward-line 1) 0))
    ;; do nothine
    )
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search

(defun howm-view-search (str file-list &optional
                             name summarizer fixed-p hilit-keywords)
  "This function is not used in howm any more."
  (howm-view-search-folder str (howm-make-folder:files file-list)
                           name summarizer fixed-p hilit-keywords))

(defun howm-view-search-items (str file-list &optional summarizer fixed-p)
  (howm-view-search-folder-items str (howm-make-folder:files file-list)
                                 summarizer fixed-p))

(defun howm-view-search-folder (&rest args)
  (howm-view-search-folder-doit (apply #'howm-view-search-folder-internal
                                       args)))

(defun howm-view-search-folder-internal (str folder
                                             &optional name summarizer
                                             fixed-p hilit-keywords)
  ;; clean me. str-orig can be string or list of strings.
  (let* ((str-orig str)
         (str-list (if (listp str-orig) str-orig (list str-orig)))
         (str-principal (if (listp str-orig) (car str-orig) str-orig)))
    ;; rename str
    (setq str str-principal)
    (setq name (or name str))
    (when howm-view-update-search-ring
      (isearch-update-ring str (not fixed-p)))
    (let* ((items (howm-view-search-folder-items str-orig
                                                 folder summarizer fixed-p))
           (kw (or hilit-keywords
                   (let ((r (if fixed-p
                                (regexp-opt str-list)
                              (mapconcat (lambda (x) (concat "\\(" x "\\)"))
                                         str-list
                                         "\\|"))))
                     `((,r . howm-view-hilit-face))))))
      (let* ((f (expand-file-name str)))
        (when (file-exists-p f)
          (let ((fi (howm-view-make-item f)))
            (howm-view-item-set-privilege fi t)
            (setq items (cons fi items)))))
      (list kw name items))))

(defun howm-view-search-folder-doit (p)
  (howm-view-summary (cadr p) (cl-caddr p) (car p)))

(defun howm-view-search-folder-items (str folder &optional summarizer fixed-p)
  (let ((found (howm-folder-grep folder str fixed-p))
        (summarizer (or summarizer
                        (lambda (file place content)
                          (string-match "^ *\\(.*\\)" content)
                          (match-string-no-properties 1 content)))))
    (mapc (lambda (i)
            (let ((file (howm-page-name (howm-item-page i)))
                  (place (howm-item-place i))
                  (content (howm-item-summary i)))
              (howm-item-set-summary i (funcall summarizer
                                                file place content))))
          found)
    found))

;; sorry for confusing functions...

(defun howm-view-search-folder-items-fi (regexp item-list &rest args)
  (apply #'howm-view-search-folder-items
         regexp (howm-make-folder-from-items item-list) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sort

(defun howm-view-sort ()
  (interactive)
  (let* ((table howm-view-sort-methods)
         (command (completing-read "sort by: " table nil t)))
    (call-interactively (cdr (assoc command table)))))

(defmacro howm-view-defun-sort-by (name)
  "Define an interactive command howm-view-sort-by-NAME,
which simply calls howm-sort-items-by-NAME."
  (let ((command (howm-get-symbol nil "howm-view-sort-by-" name))
        (internal (howm-get-symbol nil "howm-sort-items-by-" name)))
    `(defun ,command (&optional reverse-p)
       (interactive "P")
       (howm-view-sort-doit #',internal reverse-p))))
(howm-view-defun-sort-by "random")
(howm-view-defun-sort-by "name")
(howm-view-defun-sort-by "numerical-name")
(howm-view-defun-sort-by "date")
(howm-view-defun-sort-by "reverse-date")
(howm-view-defun-sort-by "summary")
(howm-view-defun-sort-by "reminder")
(howm-view-defun-sort-by "mtime")
(howm-view-defun-sort-by "reverse")

(defalias 'howm-view-sort-reverse 'howm-view-sort-by-reverse)

(defalias 'howm-view-sort-doit 'howm-view-sort/filter-doit)

(defmacro howm-sort-items (evaluator comparer item-list
                                             &optional reverse-p)
  `(let* ((howm-view-s-i-comparer ,comparer)
          (cmp (if reverse-p
                   (lambda (a b) (funcall howm-view-s-i-comparer b a))
                 howm-view-s-i-comparer)))
     (howm-sort ,evaluator cmp item-list)))

;; ;; generate the below aliases for howm-test080714
;; (let ((methods '("random" "name" "numerical-name" "date" "reverse-date"
;;                  "summary" "reminder" "mtime" "reverse")))
;;   (mapcar (lambda (m)
;;             (let* ((command
;;                     (howm-get-symbol nil "howm-view-sort-by-" m))
;;                    (internal
;;                     (howm-get-symbol nil "howm-sort-items-by-" m))
;;                    (obsolete
;;                     (howm-get-symbol nil command "-internal")))
;;               `(defalias ',obsolete ',internal)))
;;           methods))

;; for backward compatibility with howm-test080714 only
(defalias 'howm-view-sort-by-random-internal 'howm-sort-items-by-random)
(defalias 'howm-view-sort-by-name-internal 'howm-sort-items-by-name)
(defalias 'howm-view-sort-by-numerical-name-internal
  'howm-sort-items-by-numerical-name)
(defalias 'howm-view-sort-by-date-internal 'howm-sort-items-by-date)
(defalias 'howm-view-sort-by-reverse-date-internal
  'howm-sort-items-by-reverse-date)
(defalias 'howm-view-sort-by-summary-internal 'howm-sort-items-by-summary)
(defalias 'howm-view-sort-by-reminder-internal 'howm-sort-items-by-reminder)
(defalias 'howm-view-sort-by-mtime-internal 'howm-sort-items-by-mtime)
(defalias 'howm-view-sort-by-reverse-internal 'howm-sort-items-by-reverse)

(defun howm-sort-items-by-random (item-list &optional reverse-p)
  (howm-sort-items #'(lambda (dummy) (random)) #'< item-list reverse-p))

(defun howm-sort-items-by-name (item-list &optional reverse-p)
  (howm-sort-items #'howm-view-item-basename #'string< reverse-p))

(defun howm-sort-items-by-numerical-name (item-list &optional reverse-p)
  (howm-sort-items (lambda (i)
                             (let ((b (howm-view-item-basename i)))
                               (if (string-match "^[0-9]+$" b)
                                   (string-to-number b)
                                 howm-infinity)))
                           #'< reverse-p))

(defvar howm-view-sort-by-date-ignore-regexp "^[a-zA-Z]")
(defun howm-sort-items-by-date (item-list &optional reverse-p)
  (let ((sorted (howm-sort-items #'howm-view-item-basename #'string<
                                         item-list reverse-p)))
    (cdr (howm-view-lift-internal #'howm-view-item-basename
                                  sorted
                                  howm-view-sort-by-date-ignore-regexp
                                  t))))

(defun howm-sort-items-by-reverse-date (item-list &optional reverse-p)
  (howm-sort-items-by-date item-list (not reverse-p)))

(defun howm-sort-items-by-summary (item-list &optional reverse-p)
  (howm-sort-items #'howm-view-item-summary #'string<
                           item-list reverse-p))

(defun howm-sort-items-by-reminder (item-list &optional reverse-p)
  (let* ((howm-view-s-b-r-i-regexp (howm-reminder-regexp howm-reminder-types))
         (howm-view-s-b-r-i-max (format-time-string
                                 howm-reminder-today-format
                                 (encode-time 59 59 23 31 12
                                              howm-view-max-year)))
         (evaluator (lambda (item)
                      (let ((s (howm-view-item-summary item)))
                        (if (string-match howm-view-s-b-r-i-regexp s)
                            (match-string-no-properties 0 s)
                          howm-view-s-b-r-i-max)))))
    (howm-sort-items evaluator #'string< item-list reverse-p)))

(defun howm-sort-items-by-mtime (item-list &optional reverse-p)
  (howm-sort-items (lambda (item)
                     (howm-view-mtime (howm-view-item-filename item)))
                   #'howm-view-string>
                   item-list reverse-p))

(defun howm-sort-items-by-reverse (item-list &optional dummy)
  (reverse item-list))

;;; lift (move matched items to the top)

(defun howm-view-lift-by-name (&optional reverse-p regexp path-p)
  (interactive "P")
  (howm-view-lift-doit (if path-p
                           #'howm-view-lift-by-path-internal
                         #'howm-view-lift-by-name-internal)
                       reverse-p regexp))

(defun howm-view-lift-by-summary (&optional reverse-p regexp)
  (interactive "P")
  (howm-view-lift-doit #'howm-view-lift-by-summary-internal
                       reverse-p regexp))

(defun howm-view-lift-by-summary-substring (&optional reverse-p regexp
                                                      regexp-pos)
  (interactive "P")
  (howm-view-lift-doit #'howm-view-lift-by-summary-substring-internal
                       reverse-p regexp regexp-pos))

(defun howm-view-lift-doit (sorter &optional reverse-p regexp
                                            regexp-pos)
  (let* ((howm-view-s-b-m-d-regexp (or regexp
                                       (read-from-minibuffer "Regexp: ")))
         (howm-view-s-b-m-d-regexp-pos regexp-pos)
         (howm-view-s-b-m-d-sorter sorter)
         (howm-view-s-b-m-d-matched nil))
    (howm-view-sort-doit (lambda (item-list rvs-p)
                           (let ((p (apply howm-view-s-b-m-d-sorter
                                           item-list
                                           howm-view-s-b-m-d-regexp
                                           rvs-p
                                           howm-view-s-b-m-d-regexp-pos)))
                             (setq howm-view-s-b-m-d-matched (car p))
                             (cdr p)))
                         reverse-p)
    howm-view-s-b-m-d-matched))

(defun howm-view-lift-internal (picker item-list regexp
                                       &optional reverse-p regexp-pos)
  "Sort items and return (matched . sorted-list).
matched can be nil, single, or multi."
  (let* ((howm-view-l-i-matched nil)
         (evaluator (lambda (item)
                      (let ((str (funcall picker item)))
                        (if (string-match regexp str)
                            (progn
                              (setq howm-view-l-i-matched
                                    (if howm-view-l-i-matched 'multi 'single))
                              (if regexp-pos
                                  (match-string-no-properties regexp-pos str)
                                1))
                          0))))
         (comparer (if regexp-pos
                       (lambda (x y)
                         (cond ((eq x 0) nil)
                               ((eq y 0) t)
                               (t (string< x y))))
                     #'>)))
    (let ((sorted-list (howm-sort-items evaluator comparer item-list
                                        reverse-p)))
      (cons howm-view-l-i-matched sorted-list))))

(defun howm-view-lift-by-name-internal (item-list regexp &optional reverse-p)
  (howm-view-lift-internal #'howm-view-item-basename
                           item-list regexp reverse-p))

(defun howm-view-lift-by-path-internal (item-list regexp &optional reverse-p)
  (howm-view-lift-internal #'howm-item-name item-list regexp reverse-p))

(defun howm-view-lift-by-summary-internal (item-list regexp &optional reverse-p)
  (howm-view-lift-internal #'howm-view-item-summary item-list regexp reverse-p))

(defun howm-view-lift-by-summary-substring-internal (item-list regexp
                                                               &optional
                                                               reverse-p
                                                               regexp-pos)
  (howm-view-lift-internal #'howm-view-item-summary item-list regexp reverse-p
                           (or regexp-pos 0)))

;; backward compatibility
(defalias 'howm-view-sort-by-name-match 'howm-view-lift-by-name)
(defalias 'howm-view-sort-by-summary-match 'howm-view-lift-by-summary)
(defalias 'howm-view-sort-by-summary-match-string
  'howm-view-lift-by-summary-substring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired-X

(defvar howm-view-dired-buffer-name "*howm-dired*")
(howm-defvar-risky howm-view-dired-ls-command "ls")
(howm-defvar-risky howm-view-dired-ls-options '("-l"))

(defun dired-virtual (dir)
  (howm-inhibit-warning-in-compilation))

(defun howm-view-dired ()
  (interactive)
  (require (if (howm-xemacsp) 'dired-vir 'dired-x))
  (when (not (member major-mode
                     '(howm-view-summary-mode howm-view-contents-mode)))
    (error "Invalid mode for this command."))
;;   ;; bug in emacs-21.3.50?
;;   (when (not (fboundp 'dired-insert-headerline))
;;     (defun dired-insert-headerline (dir);; also used by dired-insert-subdir
;;       ;; Insert DIR's headerline with no trailing slash, exactly like ls
;;       ;; would, and put cursor where dired-build-subdir-alist puts subdir
;;       ;; boundaries.
;;       (save-excursion (insert "  " (directory-file-name dir) ":\n"))))
  (let* ((i2f (lambda (item)
                (file-relative-name (howm-view-item-filename item))))
         (current-file (funcall i2f (riffle-summary-current-item)))
         (files (howm-cl-remove-duplicates* (mapcar i2f (howm-view-item-list))
                                            :test #'equal))
;;          (pos (cl-position f files :test #'string=))
         (args (append howm-view-dired-ls-options files))
         (a `((howm-view-summary-mode . ,howm-view-summary-persistent)
              (howm-view-contents-mode . ,howm-view-contents-persistent)))
         (p (howm-view-persistent-p (cdr (assoc major-mode a)))))
    (if p
        (howm-view-restore-window-configuration)
      (howm-view-kill-buffer))
    (switch-to-buffer (get-buffer-create howm-view-dired-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (howm-call-process-here howm-view-dired-ls-command args)
    (set-buffer-modified-p nil)
    (dired-virtual default-directory)
    (howm-view-dired-goto current-file)))

(defun howm-view-dired-goto (rname)
"In dired buffer, search file name RNAME and move cursor to corresponding line.
RNAME must be relative name."
  (goto-char (point-min))
  ;; Raw call of `dired-get-filename' and `dired-next-line' causes
  ;; warnings in compilation.
  (while (let ((c (howm-funcall-if-defined (dired-get-filename 'no-dir t))))
           (not (and c (equal (file-relative-name c) rname))))
    (howm-funcall-if-defined (dired-next-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell

(howm-defvar-risky howm-view-summary-shell-hist '("ls -l FILE" "FILE"))
(howm-defvar-risky howm-view-summary-shell-last-file "FILE")
(defun howm-view-summary-shell-command ()
  (interactive)
  (when (not (member major-mode
                     '(howm-view-summary-mode)))
    (error "Invalid mode for this command."))
  (let* ((n (howm-view-line-number))
         (item (nth (1- n) (howm-view-item-list)))
         (file (howm-page-abbreviate-name (howm-view-item-filename item)))
         (last-reg (regexp-quote howm-view-summary-shell-last-file)))
    (setq howm-view-summary-shell-hist
          (mapcar (lambda (h)
                    (replace-regexp-in-string last-reg file h t))
                  howm-view-summary-shell-hist))
    (setq howm-view-summary-shell-last-file file)
    (let* ((default (car howm-view-summary-shell-hist))
           (c (read-string "command: "
                           (cons default 0)
                           '(howm-view-summary-shell-hist . 1))))
      (shell-command c))
    (let ((item-list (cl-remove-if (lambda (item)
                                          (not (file-exists-p
                                                (howm-view-item-filename item))))
                                        (howm-view-item-list))))
      (setq *riffle-summary-check* nil) ;; dirty
      (howm-view-summary (howm-view-name) item-list)
      (howm-goto-line n)
      (save-selected-window
        (let ((b (get-buffer "*Shell Command Output*")))
          (cond ((not (howm-buffer-empty-p b))
                 (switch-to-buffer-other-window b))
                ((eq item (riffle-summary-current-item))
                 nil)
                (t (progn
                     (setq *riffle-summary-check* t) ;; dirty
                     (howm-view-summary-check t))))))
      )))

;;; howm-view.el ends here
