;;; howm-mode.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-mode.el,v 1.318 2012-12-29 08:57:18 hira Exp $
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward compatibility

;; (require 'howm-mode) in .emacs is obsolete. Use (require 'howm) instead.

;; This must be earlier than (require 'howm-common), because
;; howm-common needs cl, and (require 'cl) should be written in howm.el.
(when (not (featurep 'howm-version))
  (message "Warning: Requiring howm-mode is obsolete. Require howm instead.")
;;   (beep)
;;   (sit-for 1)
  (require 'howm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require

(provide 'howm-mode)
(require 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

;;; --- level 1 ---

;; You can easily modify them.

(howm-defvar-risky howm-template
  (concat howm-view-title-header " %title%cursor\n%date %file\n\n")
  "Contents of new file. %xxx are replaced with specified items.
If it is a list, <n>-th one is used when you type C-u <n> M-x howm-create.
If it is a function, it is called to get template string with the argument <n>.")
(defvar howm-keyword-header "<<<"
  "Header string for declaration of keyword (implicit link).")
(defvar howm-ref-header ">>>"
  "Header string for explicit link.")
(defvar howm-lighter " howm"
  "Mode line for howm-mode")

(defvar howm-inhibit-title-file-match t
  "If non-nil, inhibit howm-list-title when search string matches file name")
(defvar howm-list-all-title nil) ;; obsolete [2003-11-30]
(defvar howm-list-recent-title nil) ;; obsolete [2003-11-30]

(defvar howm-default-key-table
  '(
    ;; ("key" func list-mode-p global-p)
    ("r" howm-refresh)
    ("l" howm-list-recent t t)
    ("a" howm-list-all t t)
    ("g" howm-list-grep t t)
    ("s" howm-list-grep-fixed t t)
    ("m" howm-list-migemo t t)
    ("t" howm-list-todo t t)
    ("y" howm-list-schedule t t)
    ("b" howm-list-buffers t t)
    ("x" howm-list-mark-ring t t)
    ("o" howm-occur t t)
    ("c" howm-create t t)
    ("e" howm-remember t t)
    ("," howm-menu t t)
    ("." howm-find-today nil t)
    (":" howm-find-yesterday nil t)
    ("A" howm-list-around)
    ("h" howm-history nil t)
    ("D" howm-dup)
    ("i" howm-insert-keyword nil t)
    ("d" howm-insert-date nil t)
    ("T" howm-insert-dtime nil t)
    ("K" howm-keyword-to-kill-ring t t)
    ("n" action-lock-goto-next-link)
    ("p" action-lock-goto-previous-link)
    ("Q" howm-kill-all t t)
    (" " howm-toggle-buffer nil t)
    ("N" howm-next-memo)
    ("P" howm-previous-memo)
    ("H" howm-first-memo)
    ("L" howm-last-memo)
    ("C" howm-create-here nil t)
    ("I" howm-create-interactively nil t)
    ("w" howm-random-walk nil t)
    ("M" howm-open-named-file t t)
    )
  "List of (key function list-mode-p global-p).
`howm-prefix' + this key is real stroke.
If optional argument list-mode-p is non-nil,
same key is also available in view mode.
It is further registered globally if global-p is non-nil."
  )

(howm-defvar-risky howm-migemo-client nil
  "Command name of migemo-client.
Try (setq howm-migemo-client \"migemo-client\") for howm-migemo-*.")
(howm-defvar-risky howm-migemo-client-option nil
  "List of option for migemo-client.
e.g. (\"-H\" \"::1\")")

;;; --- level 2 ---

;; Be careful to keep consistency.

(howm-defvar-risky howm-keyword/ref-regexp-format
  "\\(%s\\)[ \t]*\\([^ \t\r\n].*\\)")
(howm-defvar-risky howm-keyword-format
  (format "%s %%s" howm-keyword-header)
  "Format for declaration of keyword. See `format'.")
(howm-defvar-risky howm-keyword-regexp
  (format howm-keyword/ref-regexp-format (regexp-quote howm-keyword-header)))
(howm-defvar-risky howm-keyword-regexp-hilit-pos 1)
(howm-defvar-risky howm-keyword-regexp-pos 2)
(howm-defvar-risky howm-ref-regexp
  (format howm-keyword/ref-regexp-format (regexp-quote howm-ref-header))
  "Regexp for explicit link.")
(howm-defvar-risky howm-ref-regexp-hilit-pos 0
  "Position of search string in `howm-ref-regexp'")
(howm-defvar-risky howm-ref-regexp-pos 2
  "Position of search string in `howm-ref-regexp'")
(howm-defvar-risky howm-wiki-regexp "\\[\\[\\([^]\r\n]+\\)\\]\\]"
  "Regexp for explicit link.")
(howm-defvar-risky howm-wiki-regexp-hilit-pos 1
  "Position of hilight in `howm-wiki-regexp'")
(howm-defvar-risky howm-wiki-regexp-pos 1
  "Position of search string in `howm-wiki-regexp'")
(howm-defvar-risky howm-wiki-format "[[%s]]"
  "Format for declaration of wiki word. See `format'.")

(howm-defvar-risky howm-template-rules
  '(("%title" . howm-template-title)
    ("%date" . howm-template-date)
    ("%file" . howm-template-previous-file)
    ("%cursor" . howm-template-cursor))) ;; Cursor must be the last rule.
(defvar howm-template-date-format howm-dtime-format
  "%date is replaced with `howm-template-date-format'
in `howm-template'. See `format-time-string'")
(defvar howm-template-file-format (concat howm-ref-header " %s")
  "%file is replaced with `homw-template-file-format'
in `howm-template'. %s is replaced with name of last file. See `format'.")

;;; --- level 3 ---

;; As you like.

(defun howm-action-lock-general (command regexp pos
                                         &optional hilit-pos
                                         &rest options)
  (list regexp
        `(lambda (&optional dummy)
           (let ((s (match-string-no-properties ,pos)))
;;             (when howm-keyword-case-fold-search
;;               (setq s (downcase s)))
             (,command s ,@options)))
        (or hilit-pos 0)
        t))

(defun howm-action-lock-search (regexp
                                pos
                                &optional hilit-pos create-p open-unique-p)
  (howm-action-lock-general 'howm-keyword-search
                            regexp pos hilit-pos create-p open-unique-p))
(defun howm-action-lock-related (regexp pos hilit-pos)
  (howm-action-lock-general 'howm-list-related regexp pos hilit-pos))

(defun howm-action-lock-date-rule ()
  (action-lock-general 'howm-action-lock-date howm-date-regexp 0 0))

(defun howm-action-lock-quote-keyword (keyword)
  (let ((q (regexp-quote keyword)))
    ;; when a regexp is specified, leave unmatched keywords.
    (if (and (stringp howm-check-word-break)
             (not (string-match howm-check-word-break keyword)))
        q
      ;; add word break checks
      (concat "\\b" q "\\b"))))

(defun howm-action-lock-setup ()
  (setq action-lock-case-fold-search howm-keyword-case-fold-search)
  (action-lock-mode t)
  (let* ((date-al (action-lock-date "{_}" howm-dtime-format)))
    ;; override the rule in action-lock.el
    (action-lock-add-rules (list date-al) t))
  (let* ((ks (howm-keyword-for-goto))
         (r (mapconcat (if howm-check-word-break
                           #'howm-action-lock-quote-keyword
                         #'regexp-quote)
                       ks "\\|"))
         ;; The following optimization causes an error
         ;; "Variable binding depth exceeds max-specpdl-size".
         ;; (r (cond ((stringp howm-check-word-break)
         ;;           (mapconcat #'howm-action-lock-quote-keyword ks "\\|"))
         ;;          (t
         ;;           (regexp-opt ks (and howm-check-word-break 'word)))))
         (wiki (howm-action-lock-search howm-wiki-regexp
                                        howm-wiki-regexp-pos
                                        howm-wiki-regexp-hilit-pos
                                        t))
         (explicit (howm-action-lock-search howm-ref-regexp
                                            howm-ref-regexp-pos
                                            howm-ref-regexp-hilit-pos))
         (implicit (howm-action-lock-search r 0))
         (rev (howm-action-lock-related howm-keyword-regexp
                                        howm-keyword-regexp-pos
                                        howm-keyword-regexp-hilit-pos))
         (date (howm-action-lock-date-rule))
         (done (howm-action-lock-reminder-done-rule))
         (all `(
                ,explicit
                ,rev
                ,@(if ks (list implicit) nil)
                ,wiki
                ,@(if (howm-menu-p) nil (list date done))
                ))
         )
    ;; don't override the rule in action-lock.el
    ;; esp. http://xxx should call browser even if "<<< http" exists
    (action-lock-add-rules all)))

(defun howm-file-name (&optional time)
  (format-time-string howm-file-name-format
                      (or time (current-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(easy-mmode-define-minor-mode howm-mode
  "With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When the mode is enabled, underlines are drawn on texts which match
to titles of other files. Typing \\[action-lock-magic-return] there,
you can jump to the corresponding file.

key	binding
---	-------
\\[action-lock-magic-return]	Follow link
\\[howm-refresh]	Refresh buffer
\\[howm-list-all]	List all files
\\[howm-list-grep]	Search (grep)
\\[howm-create]	Create new file
\\[howm-dup]	Duplicate current file
\\[howm-insert-keyword]	Insert keyword
\\[howm-insert-date]	Insert date
\\[howm-insert-dtime]	Insert date with time
\\[howm-keyword-to-kill-ring]	Copy current keyword to kill ring
\\[action-lock-goto-next-link]	Go to next link
\\[action-lock-goto-previous-link]	Go to previous link
\\[howm-next-memo]	Go to next entry in current buffer
\\[howm-previous-memo]	Go to previous entry in current buffer
\\[howm-first-memo]	Go to first entry in current buffer
\\[howm-last-memo]	Go to last entry in current buffer
\\[howm-create-here]	Add new entry to current buffer
\\[howm-create-interactively]	Create new file interactively (not recommended)
\\[howm-random-walk]	Browse random entries automtically
"
  nil ;; default = off
  howm-lighter ;; mode-line
  (mapcar (lambda (entry)
            (let ((k (car entry))
                  (f (cadr entry)))
              (cons (concat howm-prefix k) f)))
          howm-default-key-table)
  )

;; emacs20's easy-mmode-define-minor-mode can't have body. sigh...
(add-hook 'howm-mode-on-hook 'howm-initialize-buffer)
(add-hook 'howm-mode-off-hook 'howm-restore-buffer)

(defun howm-set-keymap ()
  (mapc (lambda (entry)
          (let* ((k (car entry))
                 (f (cadr entry))
                 (list-mode-p (cl-caddr entry))
                 (global-p (cl-cadddr entry))
                 (pk (concat howm-prefix k)))
            (define-key howm-mode-map pk f)
            (when list-mode-p
              (mapc (lambda (m)
                      (define-key m k f)
                      (define-key m pk f))
                    (list howm-view-summary-mode-map
                          howm-view-contents-mode-map)))
            (when global-p
              (define-key global-map pk f))))
        howm-default-key-table)
  (define-key howm-mode-map "\C-x\C-s" 'howm-save-buffer))
(howm-set-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functions

(defun howm-refresh ()
  (interactive)
  (if (howm-menu-p)
      (howm-menu-refresh)
    (howm-initialize-buffer)))

(defun howm-initialize-buffer ()
  (interactive)
  (when (not howm-mode)
    (error "Not howm-mode"))
  (howm-message-time "init-buf"
    (save-restriction
      (widen)
      (howm-set-configuration-for-major-mode major-mode)
      (howm-action-lock-setup)
      (howm-mode-add-font-lock)
      (howm-reminder-add-font-lock)
      (cheat-font-lock-fontify)
      ;; make-local-hook is obsolete for emacs >= 21.1.
      (howm-funcall-if-defined (make-local-hook 'after-save-hook))
      (add-hook 'after-save-hook 'howm-after-save t t))))

(defun howm-after-save ()
  (when howm-mode
    (howm-keyword-add-current-buffer)
    (when howm-refresh-after-save
      (howm-initialize-buffer))
    (when (and howm-menu-refresh-after-save
               (> howm-menu-expiry-hours 0))
      (howm-menu-refresh-background))
    (run-hooks 'howm-after-save-hook)))

(defun howm-restore-buffer ()
  (action-lock-mode 0))

(defun howm-list-all ()
  (interactive)
  (howm-set-command 'howm-list-all)
  (howm-normalize-show "" (howm-all-items))
  ;; for backward compatibility
  (cond ((howm-list-title-p) t)  ;; already done in howm-normalize-show
        (howm-list-all-title (howm-list-title-internal))))

(defun howm-all-items ()
  "Returns list of all items in the first search path."
  (howm-folder-items (car (howm-search-path)) t))

(defun howm-list-recent (&optional days)
  (interactive "P")
  (howm-set-command 'howm-list-recent)
  (let* ((d (or days howm-list-recent-days))
         (now (current-time))
         (from (howm-days-before now d))
         (item-list (howm-folder-items howm-directory t)))
    (howm-normalize-show "" (howm-filter-items-by-mtime item-list from now))
    ;; clean me [2003-11-30]
    (cond ((howm-list-title-p) t)  ;; already done in howm-normalize-show
          (howm-list-recent-title (howm-list-title-internal))
          ((not days) (howm-view-summary-to-contents)))))

;; clean me: direct access to howm-view-* is undesirable.

(defvar howm-list-title-previous nil
  "For internal use")
(make-variable-buffer-local 'howm-list-title-previous)
(defun howm-list-title-put-previous (&optional item-list)
  (when howm-list-title-undo
    (setq howm-list-title-previous (or item-list (howm-view-item-list)))))
(defun howm-list-title-clear-previous ()
  (setq howm-list-title-previous nil))
(defun howm-list-title-get-previous ()
  (if howm-list-title-undo
      (let ((prev howm-list-title-previous))
        (setq howm-list-title-previous nil)
        (howm-view-summary-rebuild prev))
    (error "Undo is not enabled.")))
(defun howm-list-title-regexp ()
  (or howm-list-title-regexp (howm-view-title-regexp-grep)))
(defalias 'howm-list-title 'howm-list-toggle-title) ;; backward compatibility
(defun howm-list-toggle-title (&optional undo)
  (interactive "P")
  (if (or undo howm-list-title-previous)
      (howm-list-title-get-previous)
    (howm-list-title-internal)))
(defun howm-list-title-internal ()
  (let ((b (current-buffer)))
    (howm-list-title-put-previous)
    (howm-view-list-title (howm-list-title-regexp))
    ;;       (howm-view-filter-by-contents (howm-list-title-regexp))
    (let ((c (current-buffer)))
      (when (not (eq b c))
        (set-buffer b)
        (howm-view-kill-buffer)
        (switch-to-buffer c)
        (howm-view-summary-check t)))))

(defun howm-list-title-p ()
  (let ((a (howm-get-value howm-list-title)))
    (cond ((null a) nil) ;; I know this is redundant.
          ((listp a) (member (howm-command) a))
          (t a))))

(defun howm-days-after (ti days &optional hours)
  (let* ((ne (howm-decode-time ti))
         (hour-pos 2)
         (day-pos 3)
         (nh (nth hour-pos ne))
         (nd (nth day-pos ne)))
    (setf (nth hour-pos ne) (+ nh (or hours 0)))
    (setf (nth day-pos ne) (+ nd days))
    (apply #'encode-time ne)))

(defun howm-days-before (ti days)
  (howm-days-after ti (- days)))

(defun howm-list-grep (&optional completion-p)
  (interactive "P")
  (howm-set-command 'howm-list-grep)
  (howm-list-grep-general completion-p))

(defun howm-list-grep-fixed ()
  (interactive)
  (howm-set-command 'howm-list-grep-fixed)
  (howm-list-grep-general t))

(defun howm-list-grep-general (&optional completion-p)
  (let* ((regexp (if completion-p
                     (howm-completing-read-keyword)
                   (read-from-minibuffer "Search all (grep): "))))
    (when completion-p  ;; Goto link works only for fixed string at now.
      (howm-write-history regexp))
    (howm-search regexp completion-p)))

(defun howm-search (regexp fixed-p &optional emacs-regexp filter)
  (if (string= regexp "")
      (howm-list-all)
    (howm-message-time "search"
      (let* ((trio (howm-call-view-search-internal regexp fixed-p emacs-regexp))
             (kw (car trio))
             (name (cadr trio))
             (items (cl-caddr trio)))
        (when filter
          (setq items (funcall filter items)))
        (howm-normalize-show name items (or emacs-regexp regexp) nil nil kw)
        (howm-record-view-window-configuration)))))

(defvar *howm-view-window-configuration* nil
  "For internal use")
(defun howm-view-window-configuration ()
  *howm-view-window-configuration*)
(defun howm-set-view-window-configuration (conf)
  (setq *howm-view-window-configuration* conf))
(defun howm-record-view-window-configuration ()
  (howm-set-view-window-configuration (current-window-configuration)))
(defun howm-restore-view-window-configuration ()
  (set-window-configuration (howm-view-window-configuration)))
(defun howm-return-to-list ()
  (interactive)
  (howm-restore-view-window-configuration))

(defun howm-call-view-search-internal (regexp fixed-p &optional emacs-regexp)
  (let ((hilit (if emacs-regexp
                   `((,emacs-regexp . howm-view-hilit-face))
                 nil)))
    (howm-view-search-folder-internal regexp (howm-search-path-folder)
                                      nil nil fixed-p hilit)))

(defun howm-list-migemo (&optional completion-p)
  (interactive "P")
  (howm-set-command 'howm-list-migemo)
  (if completion-p
      (howm-list-grep t)
    (let* ((roma (read-from-minibuffer "Search all (migemo): "))
           (e-reg (howm-migemo-get-pattern roma "emacs"))
           (g-reg (if howm-view-use-grep
                      (howm-migemo-get-pattern roma "egrep")
                    e-reg)))
      (if (and e-reg g-reg)
          (howm-search g-reg nil e-reg)
        (message "No response from migemo-client.")))))

(defun howm-migemo-get-pattern (roma type)
  (when (and (null howm-migemo-client) (not howm-view-use-grep))
    (require 'migemo))
  (if (and (featurep 'migemo) (string= type "emacs"))
      (howm-funcall-if-defined (migemo-get-pattern roma))
;;       (migemo-get-pattern roma)
    (car (howm-call-process (or howm-migemo-client "migemo-client")
                            `(,@howm-migemo-client-option "-t" ,type ,roma)
                            0))))

;; (defun howm-migemo-get-pattern (roma type)
;;   (when (and (null (howm-migemo-client)) (not howm-view-use-grep))
;;     (require 'migemo))
;;   (if (and (featurep 'migemo) (string= type "emacs"))
;;       (howm-funcall-if-defined (migemo-get-pattern roma))
;; ;;       (migemo-get-pattern roma)
;;     (car (howm-call-process (howm-migemo-client)
;;                             `(,@(howm-migemo-client-option) "-t" ,type ,roma)
;;                             0))))

;; (defun howm-migemo-client ()
;;   (if (stringp howm-migemo-client)
;;       howm-migemo-client
;;     (or (car howm-migemo-client) "migemo-client")))

;; (defun howm-migemo-client-option ()
;;   (cdr-safe howm-migemo-client))

(defun howm-normalize-oldp ()
  howm-list-normalizer)

;; ;; generate conv in howm-normalizer-pair
;; (let ((methods '("random" "name" "numerical-name" "date" "reverse-date"
;;                  "summary" "reminder" "mtime" "reverse")))
;;   (mapcar (lambda (m)
;;             (let ((command
;;                    (howm-get-symbol nil "howm-view-sort-by-" m))
;;                   (internal
;;                    (howm-get-symbol nil "howm-sort-items-by-" m)))
;;               (cons command internal)))
;;           methods))

(defun howm-normalizer-pair ()
  (let* ((old howm-list-normalizer)
         (new howm-normalizer)
         (conv '((howm-view-sort-by-random . howm-sort-items-by-random)
                 (howm-view-sort-by-name . howm-sort-items-by-name)
                 (howm-view-sort-by-numerical-name
                  . howm-sort-items-by-numerical-name)
                 (howm-view-sort-by-date . howm-sort-items-by-date)
                 (howm-view-sort-by-reverse-date
                  . howm-sort-items-by-reverse-date)
                 (howm-view-sort-by-summary . howm-sort-items-by-summary)
                 (howm-view-sort-by-reminder . howm-sort-items-by-reminder)
                 (howm-view-sort-by-mtime . howm-sort-items-by-mtime)
                 (howm-view-sort-by-reverse . howm-sort-items-by-reverse)))
         (p (assoc old conv))
         (q (assoc new conv)))
    (when q
      (message "Warning: %s is wrong for howm-normalizer. Use %s." (car q) (cdr q))
      (setq new (cdr q)))
    (cond ((null old) (cons old new))
          (p (cons nil (cdr p)))
          (t (cons old #'identity)))))

(defmacro howm-with-normalizer (&rest body)
  (declare (indent 0))
  (let ((g (cl-gensym)))
    `(progn
       (when (howm-normalize-oldp)
         (message
          "Warning: howm-list-normalizer is obsolete. Use howm-normalizer."))
       (let* ((,g (howm-normalizer-pair))
              (howm-list-normalizer (car ,g))
              (howm-normalizer (cdr ,g)))
         ,@body))))

(defun howm-normalize-show (name item-list
                                 &optional keyword comefrom-regexp no-list-title
                                 fl-keywords)
  ;; comefrom-regexp and no-list-title are never used now. [2009-07-23]
  (howm-with-normalizer
    (if (howm-normalize-oldp)
        ;; for backward compatibility.
        (progn
          (howm-view-summary name item-list fl-keywords)
          (howm-list-normalize-old keyword comefrom-regexp no-list-title))
      (let* ((r (howm-normalize item-list keyword
                                comefrom-regexp no-list-title)))
        (howm-call-view-summary name (cdr r) fl-keywords)
        (car r)))))

(defun howm-call-view-summary (name item-list-pair fl-keywords)
  (let ((orig (car item-list-pair))
        (entitled (cdr item-list-pair)))
    (howm-view-summary name (or entitled orig) fl-keywords)
    ;; side effect
    (if entitled
        (howm-list-title-put-previous orig)
      (howm-list-title-clear-previous))))

(defun howm-normalize (item-list
                       &optional keyword comefrom-regexp no-list-title)
  ;; no-list-title is never used now. [2009-07-23]
  "Sort ITEM-LIST in the standard order."
  (let ((matched nil)
        (entitled-item-list nil))
    (setq item-list (funcall howm-normalizer item-list))
    (when keyword
      (let ((key-reg (or comefrom-regexp
                         (howm-make-keyword-regexp1 keyword)))
            (word-reg (format "\\<%s\\>"
                              (if (stringp keyword)
                                  (regexp-quote keyword)
                                (regexp-opt keyword t))))
            (wiki-reg (regexp-quote (howm-make-wiki-string keyword)))
            (file-reg (and
                       (stringp keyword)
                       (format "^%s$"
                               (regexp-quote (expand-file-name keyword)))))
            (case-fold-search howm-keyword-case-fold-search))
        (cl-labels ((check (tag flag reg &optional tag-when-multi-hits)
                        (when flag
                          (let ((r (howm-normalize-check item-list tag reg
                                                         tag-when-multi-hits)))
                            (setq matched (append (car r) matched))
                            (setq item-list (cdr r))))))
          ;; not efficient. should I do them at once?
          (check 'word            howm-list-prefer-word word-reg)
          (check 'wiki            howm-list-prefer-wiki wiki-reg)
          (check 'related-keyword t howm-keyword-regexp)
          (check 'keyword         t key-reg 'keyword-multi-hits)
          (check 'file            file-reg file-reg))))
    (when (and (howm-list-title-p)
               (not no-list-title)
               (not (and (member 'file matched)
                         howm-inhibit-title-file-match)))
      (setq entitled-item-list
            (howm-entitle-items (howm-list-title-regexp) item-list)))
    (cons matched (cons item-list entitled-item-list))))

(defun howm-normalize-check (item-list tag reg tag-when-multi-hits)
  (let* ((r (if (eq tag 'file)
                (howm-view-lift-by-path-internal item-list reg)
              (howm-view-lift-by-summary-internal item-list reg)))
         (m (car r))
         (item-list (cdr r))
         (matched (cond ((and tag-when-multi-hits (eq m 'multi))
                         (list tag-when-multi-hits tag))
                        (m (list tag))
                        (t nil))))
    (cons matched item-list)))

(defun howm-list-normalize-old (&optional keyword comefrom-regexp no-list-title)
  "Sort displayed items in the standard order.
This function is obsolete. Use `howm-normalize' insteadly.
--- Sorry, below documentation is incomplete. ---
When KEYWORD is given, matched items are placed on the top.
KEYWORD can be a string or a list of strings.
"
  (prog1
      (howm-view-in-background
        (howm-list-normalize-subr keyword comefrom-regexp no-list-title))
    (howm-view-summary)))

(defun howm-list-normalize-subr (keyword comefrom-regexp no-list-title)
  "Obsolete. Do not use this any more."
  (let ((matched nil))
    (funcall howm-list-normalizer)
    (when keyword
      (let ((key-reg (or comefrom-regexp
                         (howm-make-keyword-regexp1 keyword)))
            (word-reg (format "\\<%s\\>"
                              (if (stringp keyword)
                                  (regexp-quote keyword)
                                (regexp-opt keyword t))))
            (wiki-reg (regexp-quote (howm-make-wiki-string keyword)))
            (file-reg (and
                       (stringp keyword)
                       (format "^%s$"
                               (regexp-quote (expand-file-name keyword)))))
            (case-fold-search howm-keyword-case-fold-search))
        ;; clean me.
        (let ((check (lambda (tag flag reg &optional tag-when-multi-hits)
                       (when flag
                         (let ((m (if (eq tag 'file)
                                      (howm-view-lift-by-name nil reg t)
                                    (howm-view-lift-by-summary nil reg))))
                           (when m
                             (setq matched (cons tag matched)))
                           (when (and tag-when-multi-hits (eq m 'multi))
                             (setq matched
                                   (cons tag-when-multi-hits matched))))))))
          (funcall check 'word            howm-list-prefer-word word-reg)
          (funcall check 'wiki            howm-list-prefer-wiki wiki-reg)
          (funcall check 'related-keyword t howm-keyword-regexp)
          (funcall check 'keyword         t key-reg 'keyword-multi-hits)
          (funcall check 'file            file-reg file-reg))))
    (when (and (howm-list-title-p)
               (not no-list-title)
               (not (and (member 'file matched)
                         howm-inhibit-title-file-match)))
      (howm-list-title-internal))
    matched))

(defun howm-make-keyword-string (keyword)
  (format howm-keyword-format keyword))
(defun howm-make-wiki-string (keyword)
  (format howm-wiki-format keyword))

;; clean me
(defvar howm-keyword-regexp-format "%s$"
  "Format to make entire-match regexp from keyword string.
Default is \"%s$\" because we want to make regexp \"<<< foo$\"
from keyword string \"<<< foo\",
so that we can accept \"<<< foo\" and reject \"<<< foobar\".
We need entire-match in order to
(1) place \"<<< foo\" on the top when \"foo\" is searched, and
(2) judge existence of \"<<< foo\" when [[foo]] is hit.")
(defun howm-make-keyword-regexp1 (keyword)
  (howm-make-keyword-regexp-general keyword #'howm-make-keyword-regexp1-sub))
(defun howm-make-keyword-regexp2 (keyword)
  (howm-make-keyword-regexp-general keyword #'howm-make-keyword-regexp2-sub))
(defun howm-make-keyword-regexp1-sub (keyword)
  (format howm-keyword-regexp-format
          (regexp-quote (howm-make-keyword-string keyword))))
(defun howm-make-keyword-regexp2-sub (keyword)
  (format howm-keyword-regexp-format
          (howm-make-keyword-string (regexp-quote keyword))))
(defun howm-make-keyword-regexp-general (keyword regexp-generator)
  (cond ((stringp keyword)
         (funcall regexp-generator keyword))
        ((listp keyword)
         (mapconcat (lambda (s)
                      (concat "\\("
                              (funcall regexp-generator s)
                              "\\)"))
                    keyword
                    "\\|"))
        (t (error "Wrong type: %s" keyword))))

(defun howm-list-related (str)
  (howm-set-command 'howm-list-related)
  (let* ((keys (mapcar (lambda (k)
                         (if howm-keyword-case-fold-search
                             (downcase k)
                           k))
                       (howm-subkeyword str)))
         (filter `(lambda (items)
                    (howm-filter-items-by-summary items ,(regexp-opt keys)))))
    ;; Note that regexp-opt returns a regexp for emacs (not for grep).
    (howm-search (howm-make-keyword-string ".*") nil nil filter)))

(defun howm-subkeyword (str)
  (with-temp-buffer
    (insert str)
    (howm-keyword-for-goto)))

(defun howm-list-around ()
  (interactive)
  (howm-set-command 'howm-list-around)
  (let ((f (buffer-file-name))
        (item-list (howm-view-sort-by-reverse-date-internal
                    (howm-all-items))))
    (let ((howm-normalizer #'identity))
      (howm-normalize-show "" item-list))
    (let ((pos (cl-position-if (lambda (item)
                                      (string= (howm-item-name item) f))
                                    (howm-view-item-list))))
      (goto-char (point-min))
      (when pos
        (forward-line pos)))
    (howm-view-summary-check t)))

(defun howm-history ()
  (interactive)
  (unless (file-exists-p howm-history-file)
    (error "No history."))
  ;; disable expansion of %schedule etc.
  (let ((howm-menu-display-rules nil)) ;; dirty
    (howm-menu-open howm-history-file)))

;; (defvar howm-history-exclude
;;   (let ((strings '("[0-9][0-9][0-9][0-9]" "^[*=] [^ ]")))
;;     `("| %.*%$"
;;       ,(mapconcat 'regexp-quote strings "\\|"))))
;; (defun howm-history ()
;;   (interactive)
;;   (howm-menu-open howm-history-file)
;;   (howm-edit-read-only-buffer
;;     (mapc #'flush-lines
;;           howm-history-exclude)))

(defvar *howm-command* nil
  "For internal use")
(defun howm-set-command (com)
  (setq *howm-command* com))
(defun howm-command ()
  *howm-command*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create

(defun howm-create (&optional which-template here)
  (interactive "p")
  (let* ((t-c (howm-create-default-title-content))
         (title (car t-c))
         (content (cdr t-c)))
    (howm-create-file-with-title title which-template nil here content)))

(howm-dont-warn-free-variable transient-mark-mode)
(howm-dont-warn-free-variable mark-active)
(defun howm-create-default-title-content ()
  (let* ((p (point))
         (m (or (mark t) -777))
         (beg (min p m))
         (end (max p m))
         (search-str (howm-view-name)))
    (let* ((transient-mark-p (and (boundp 'transient-mark-mode)
                                  transient-mark-mode))
           (mark-active-p (and (boundp 'mark-active) mark-active))
           (active-p (if transient-mark-p
                         mark-active-p
                       t))
           (strictly-active-p (and transient-mark-p mark-active-p))
           (title-p (let* ((b (line-beginning-position))
                           (e (line-end-position)))
                      (and active-p
                           (< 0 beg) (<= b beg) (<= end e) (not (= beg end)))))
           (content-p (and strictly-active-p
                           howm-content-from-region))
           (search-p (and howm-title-from-search
                          (stringp search-str)))
           (s (cond ((or title-p content-p) (buffer-substring-no-properties beg
                                                                            end))
                    (search-p search-str))))
      (cond ((null s) (cons "" ""))
            ((eq content-p t) (cons "" s))
            ((or title-p search-p) (cons s ""))
            (content-p (cons "" s))
            (t (cons "" ""))))))

(defun howm-create-here (&optional which-template)
  (interactive "p")
  (howm-create which-template t))

(defun howm-create-file-with-title (title &optional
                                    which-template not-use-file here content)
  (let ((b (current-buffer)))
    (when (not here)
      (howm-create-file))
    (cond ((howm-buffer-empty-p) nil)
          ((and here howm-create-here-just) (beginning-of-line))
          (t (howm-create-newline)))
    (let ((p (point))
          (insert-f (lambda (switch)
                      (howm-insert-template (if switch title "")
                                            b which-template (not switch))))
          (use-file (not not-use-file)))
      ;; second candidate which appears when undo is called
      (let ((end (funcall insert-f not-use-file)))
        (save-excursion
          (goto-char end)
          (insert (or content "")))
        (undo-boundary)
        (delete-region p end))
      (funcall insert-f use-file))
    (howm-create-finish)))

(defun howm-create-finish ()
  (howm-set-mode)
  (run-hooks 'howm-create-hook))

(defun howm-create-newline ()
  (widen)
  (if howm-prepend
      (howm-create-newline-prepend)
    (howm-create-newline-append)))
(defun howm-create-newline-prepend ()
  (goto-char (point-min)))
(defun howm-create-newline-append ()
  (goto-char (point-max))
  (delete-blank-lines)
  (when (not (= (line-beginning-position) (point))) ;; not empty line
    (insert "\n"))
  (insert "\n"))

(defun howm-insert-template (title &optional
                                   previous-buffer which-template not-use-file)
  (let* ((beg (point))
         (f (buffer-file-name previous-buffer))
         (af (and f (howm-abbreviate-file-name f))))
    (insert (howm-template-string which-template previous-buffer))
    (let* ((date (format-time-string howm-template-date-format))
           (use-file (not not-use-file))
           (file (cond ((not use-file) "")
                       ((null f) "")
                       ((string= f (buffer-file-name)) "")
                       (t (format howm-template-file-format af)))))
      (let ((arg `((title . ,title) (date . ,date) (file . ,file)))
            (end (point-marker)))
        (howm-replace howm-template-rules arg beg end)
        end))))

(defvar howm-template-receive-buffer t
  "Non nil if howm-template should receive previous-buffer
when howm-template is a function.
Set this option to nil if backward compatibility with howm-1.2.4 or earlier
is necessary.")

(defun howm-template-string (which-template previous-buffer)
  ;; which-template should be 1, 2, 3, ...
  (setq which-template (or which-template 1))
  (cond ((stringp howm-template) howm-template)
        ((functionp howm-template) (let ((args (if howm-template-receive-buffer
                                                   (list which-template
                                                         previous-buffer)
                                                 (list which-template))))
                                     (apply howm-template args)))
        ((listp howm-template) (nth (- which-template 1) howm-template))))

(defun howm-replace (rules arg &optional beg end)
  (mapc (lambda (pair)
          (let ((spell (car pair))
                (disp-f (cdr pair)))
            (goto-char (or beg (point-min)))
            (while (re-search-forward spell end t)
              (delete-region (match-beginning 0) (match-end 0))
              (funcall disp-f arg))))
        rules))

;; Use dynamic bindings dirtily!
(defun howm-template-title (arg)
  (insert (cdr (assoc 'title arg))))
(defun howm-template-date (arg)
  (insert (cdr (assoc 'date arg))))
(defun howm-template-previous-file (arg)
  (insert (cdr (assoc 'file arg))))
(defun howm-template-cursor (arg)) ;; do nothing

(defun howm-dup ()
  (interactive)
  (let* ((r (howm-view-paragraph-region))
         (s (buffer-substring-no-properties (car r) (cadr r))))
    (howm-create-file)
    (howm-set-mode)
    (insert "\n" s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword

(defun howm-completing-read-keyword ()
  (message "Scanning...")
  (let* ((kl (howm-keyword-list))
         (table (mapcar #'list kl))
         (completion-ignore-case howm-keyword-case-fold-search))
    (completing-read "Keyword: " table)))

(defun howm-insert-keyword ()
  (interactive)
  (insert (howm-completing-read-keyword)))

(defun howm-keyword-to-kill-ring (&optional filename-p)
  (interactive "P")
  (let ((title (howm-title-at-current-point filename-p)))
    (if title
        (howm-string-to-kill-ring title)
      (error "No keyword."))))

(defun howm-title-at-current-point (&optional filename-p
                                              title-regexp title-regexp-pos)
  (let ((reg (or title-regexp howm-view-title-regexp))
        (pos (or title-regexp-pos howm-view-title-regexp-pos)))
    (save-excursion
      (end-of-line)
      (cond ((and (not filename-p)
                  (re-search-backward reg nil t))
             (match-string-no-properties pos))
            ((buffer-file-name)
             (howm-abbreviate-file-name (buffer-file-name)))
            (t nil)))))

(defun howm-string-to-kill-ring (str)
  (if str
      (progn
        (kill-new str)
        (message "%s" str))
    (error "Empty.")))

(defun howm-keyword-for-comefrom ()
  (save-excursion
    (goto-char (point-min))
    (let ((keyword-list nil))
      (while (re-search-forward howm-keyword-regexp nil t)
        (setq keyword-list
              (cons (match-string-no-properties howm-keyword-regexp-pos)
                    keyword-list)))
      (reverse keyword-list))))

(defun howm-keyword-list ()
  (let ((sep (format "[\n%s]" (or howm-keyword-list-alias-sep ""))))
    (with-current-buffer (howm-keyword-buffer)
      (delete ""
              (split-string (buffer-substring (point-min) (point-max)) sep)))))

(defun howm-keyword-add (keyword-list)
  (interactive "sKeyword: ")
  (setq keyword-list (if (stringp keyword-list)
                         (list keyword-list)
                       keyword-list))
  (with-current-buffer (howm-keyword-buffer)
    (save-excursion
      (goto-char (point-max))
      (mapc (lambda (k)
              (when (howm-keyword-new-p k)
                (insert k "\n")))
            keyword-list)
      (when (buffer-file-name)
        (howm-basic-save-buffer)))))

(defun howm-keyword-new-p (str)
  (save-excursion
    (let ((r (format "^%s$" (regexp-quote str)))
          (case-fold-search howm-keyword-case-fold-search))
      (goto-char (point-min))
      (not (re-search-forward r nil t)))))

(defun howm-support-aliases-p ()
  howm-keyword-list-alias-sep)
(defun howm-aliases ()
  (if (howm-support-aliases-p)
      (howm-read-aliases)
    nil))
(defun howm-read-aliases ()
  (with-current-buffer (howm-keyword-buffer)
    (save-excursion
      (let ((ans nil))
        (goto-char (point-min))
        (while (search-forward howm-keyword-list-alias-sep nil t)
          (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position)))
                 (keys (split-string line howm-keyword-list-alias-sep))
                 (ks (if howm-keyword-case-fold-search
                         (mapcar #'downcase keys)
                       keys)))
            (setq ans (cons ks ans))
            (end-of-line)))
        ans))))

(defun howm-expand-aliases-recursively (keyword aliases)
  (let ((keys (list keyword))
        (prev nil))
    (cl-labels ((expand (keys)
                     (sort (cl-remove-duplicates
                            (cl-mapcan (lambda (k)
                                              (cl-mapcan
                                               (lambda (a) (if (member k a)
                                                               (copy-sequence a)
                                                             nil))
                                               aliases))
                                            keys) :test #'string=)
                           #'string<)))
      (while (not (equal prev keys))
        (setq prev keys)
        (setq keys (expand keys))))
    keys))
(cl-assert (equal (howm-expand-aliases-recursively "a"
                                                '(("d" "e" "f") ("a" "b" "c")))
               '("a" "b" "c")))
(cl-assert (equal (howm-expand-aliases-recursively "a"
                                                '(("d" "e" "b") ("a" "b" "c")))
               '("a" "b" "c" "d" "e")))

(defun howm-keyword-aliases (keyword)
  "List of strings which are equivalent to KEYWORD.
KEYWORD itself is always at the head of the returneded list.
"
  ;; Return the original keyword (not downcased) for backward compatibility.
  ;; I'm not sure whether this behavior is really needed.
  (let* ((key (if howm-keyword-case-fold-search
                  (downcase keyword)
                keyword))
         (aliases (howm-aliases))
         (equiv (if howm-keyword-aliases-recursive
                    (howm-expand-aliases-recursively key aliases)
                  (cl-remove-duplicates
                   (apply #'append
                          (cl-remove-if-not (lambda (a) (member key a))
                                                 aliases))))))
    (if (null equiv)
        keyword
      (cons keyword (remove key equiv)))))

(defun howm-keyword-search (keyword &optional create-p open-unique-p)
  (howm-message-time "key-search"
    (howm-set-command 'howm-keyword-search)
    (howm-with-normalizer
      (howm-keyword-search-subr keyword create-p open-unique-p))))

(defun howm-keyword-search-subr (keyword create-p open-unique-p)
  (let* ((aliases (if (howm-support-aliases-p)
                      (howm-keyword-aliases keyword)
                    keyword))
         (menu-p (howm-menu-keyword-p keyword))
         (comefrom-regexp (if menu-p ;; clean me
                              nil
                            (howm-make-keyword-regexp2 aliases)))
         (trio (let ((howm-search-other-dir (if menu-p ;; clean me
                                                 nil
                                               howm-search-other-dir))
                      (*howm-view-force-case-fold-search*
                       howm-keyword-case-fold-search)) ;; dirty!
                 (howm-call-view-search-internal aliases t)))
;; code for <http://pc8.2ch.net/test/read.cgi/unix/1077881095/823>.
;; but this change is canceled; I'll try more fundamental fix. [2005-11-04]
;;                   (if open-unique-p
;;                       (let ((r (concat "^" (regexp-quote keyword) "$")))
;;                         (howm-call-view-search r nil))
;;                     (howm-call-view-search aliases t))))
         (kw (car trio))
         (name (cadr trio))
         (items (cl-caddr trio))
         (items-pair nil)
         (found (if items t nil)) ;; want to forget items as soon as possible
         (matched (and found
                       (let* ((howm-keyword-format
                               (if menu-p ;; clean me
                                   (default-value 'howm-keyword-format)
                                 howm-keyword-format))
                              (r (howm-normalize items aliases
                                                 comefrom-regexp)))
                         (setq items-pair (cdr r))
                         (car r))))
         (keyword-matched (member 'keyword matched))
         (keyword-matched-multi (member 'keyword-multi-hits matched))
         (file-matched (member 'file matched))
         (title (howm-make-keyword-string keyword)))
    ;; main processing (clean me!) [2003-12-01]
    (cond
     ;; for %foo%
     ((and menu-p keyword-matched)
      (howm-keyword-search-open-menu keyword (car items-pair)
                                     keyword-matched-multi))
     ;; for [[foo]]
     ((and create-p (not keyword-matched))
      (howm-keyword-search-create title))
     ;; open if unique match
     ((and open-unique-p (howm-single-element-p items))
      (howm-keyword-search-open-unique items))
     (t
      (howm-call-view-summary name items-pair kw)
      (when (howm-normalize-oldp)
        ;; sorry for redundancy & inefficiency
        (howm-list-normalize-old aliases comefrom-regexp t))))
    ;; record history
    (when (not menu-p)
      (howm-write-history keyword))
    ;; return information
    `((menu-p . ,menu-p)
      (found . ,found)
      (matched . ,matched)
      (keyword-matched . ,keyword-matched)
      (create-p . ,create-p))
    ))

(defun howm-keyword-search-open-menu (keyword item-list multi-hits-p)
  "Open KEYWORD as menu."
  ;; dirty. peeking howm-view.el
  (let* ((item (car item-list))
         (fname (howm-view-item-filename item))
         (place (howm-view-item-place item)))
    (let ((howm-search-other-dir nil))
      (howm-menu-open fname place (howm-menu-name keyword))))
  (when multi-hits-p
    (message "Warning: found two or more %s." keyword)))

(defun howm-keyword-search-create (title)
  "create new memo <<< TITLE."
  (howm-create-file-with-title title)
  (message "New keyword."))

(defun howm-keyword-search-open-unique (items)
  "Open unique match."
  (howm-view-open-item (car items)))

;; (defvar *howm-keyword-buffer* nil) ;; for internal use
(defun howm-keyword-for-goto (&optional keyword-list)
  (save-excursion
    (let ((case-fold-search howm-keyword-case-fold-search))
      (sort (cl-mapcan (lambda (k)
                              (goto-char (point-min))
                              ;; when howm-check-word-break is non-nil,
                              ;; checking word breaks is desired for efficiency.
                              ;; it is not implemented yet.
                              (if (search-forward k nil 'noerr)
                                  (list k)
                                nil))
                            (or keyword-list (howm-keyword-list)))
            (lambda (x y)
              (> (length x) (length y)))))))

(defun howm-keyword-add-current-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let ((m (current-message))
          (keyword-list nil))
      (while (re-search-forward howm-keyword-regexp nil t)
        (let ((key-str (if howm-keyword-list-alias-sep
                           (mapconcat #'identity
                                      (howm-keyword-read)
                                      howm-keyword-list-alias-sep)
                         (match-string-no-properties howm-keyword-regexp-pos))))
          (setq keyword-list (cons key-str keyword-list))))
      (howm-keyword-add keyword-list)
      (message "%s" m))))
(defun howm-keyword-add-items (items)
  (let ((files (mapcar #'howm-view-item-filename items)))
    (with-temp-buffer
      (mapc (lambda (f)
              (erase-buffer)
              (insert-file-contents f)
              (howm-set-configuration-for-file-name f)
              (howm-keyword-add-current-buffer))
            files))))

(defun howm-keyword-read ()
  (let ((ks nil)
        (beg (line-beginning-position)))
    (end-of-line)
    (skip-chars-backward " ")
    (while (re-search-backward howm-keyword-regexp beg t)
      (setq ks (cons (match-string-no-properties howm-keyword-regexp-pos) ks))
      (skip-chars-backward " "))
    (end-of-line)
    ks))

;;; howm-mode.el ends here
