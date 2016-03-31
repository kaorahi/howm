;;; howm-menu.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-menu.el,v 1.106 2012-09-23 11:34:59 hira Exp $
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

(provide 'howm-menu)
(require 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize

;;; general

(howm-defvar-risky howm-menu-mode-map nil)
(let ((m (make-keymap)))
  (define-key m action-lock-magic-return-key 'howm-menu-invoke)
  (define-key m [tab] 'action-lock-goto-next-link)
  (define-key m [(meta tab)] 'action-lock-goto-previous-link)
  (define-key m "\C-i" 'action-lock-goto-next-link)
  (define-key m "\M-\C-i" 'action-lock-goto-previous-link)
  (define-key m " " 'scroll-up)
  (define-key m [backspace] 'scroll-down)
  (define-key m "\C-h" 'scroll-down)
  (define-key m "q" 'bury-buffer)
  (define-key m "?" 'describe-mode)
  (setq howm-menu-mode-map m)
  )

;;; schedule, todo, recent, random

;; Set random seed.
;; snap://Info-mode/elisp#Random Numbers
(defvar howm-randomize t)
(when howm-randomize
  (random t))

(defvar howm-menu-reminder-format "> %s | %s"
  "Format to show schedule/todo list in `howm-menu-mode'.")
(defvar howm-menu-list-format
  (let* ((path (format-time-string howm-file-name-format))
         (width (length (file-name-sans-extension
                         (file-name-nondirectory path)))))
    (concat "> %-" (format "%s" width) "s | %s"))
  "Format to show recent/random list in `howm-menu-mode'.")
(defvar howm-menu-list-regexp "^\\(>\\([^|\r\n]*|\\)\\) +\\(.*\\)$"
  "Regexp to find and parse schedule/todo/recent/random list in `howm-menu-mode'.
`howm-menu-list-regexp-action-pos' must cover header part.
Otherwise, `howm-action-lock-forward' may be invoked unintentionally.")
(defvar howm-menu-list-regexp-key-pos 3
  "Position of target string for action-lock in history buffer.
This target is searched when action-lock is invoked.")
(defvar howm-menu-list-regexp-action-pos 1
  "Position of action-lock hilight on schedule/todo/recent/random list
in `howm-menu-mode'.")
(defvar howm-menu-list-regexp-face-pos 2
  "Position to apply `howm-menu-list-face' on schedule/todo/recent/random list
in `howm-menu-mode'.")

;;; shortcut

;; %"..." or %"...%"
(defvar howm-menu-key-regexp
  "%\"\\(\\([^\r\n%\"]\\)[^\r\n%\"]*\\(%+[^\r\n%\"]+\\)*\\)\\(%\\)?\"")
(defvar howm-menu-key-regexp-word-pos 1)
(defvar howm-menu-key-regexp-key-pos 2)
(defvar howm-menu-key-regexp-moveonly-pos 4)

;;; dynamic contents

(howm-defvar-risky howm-menu-allow
  '(howm-menu-schedule
    howm-menu-todo
    howm-menu-reminder
    howm-menu-recent
    howm-menu-random
    howm-menu-search
    howm-menu-categorized-reminder
    ))

(howm-defvar-risky howm-menu-display-rules
  `(
    ;; static
    ("%sdays"    . "%here%howm-menu-schedule-days")
    ("%tnum"     . "%here%howm-menu-todo-num")
    ("%schedule" . "%here%(howm-menu-schedule)")
    ("%todo"     . "%here%(howm-menu-todo)")
    ("%reminder" . "%here%(howm-menu-reminder)")
    ("%recent"   . "%here%(howm-menu-recent)")
    ("%random"   . "%here%(howm-menu-random)")
    ;; dynamic
    ("%here%" . howm-menu-here)
    (,howm-menu-key-regexp . howm-menu-shortcut)
    )
  "List of rules for dynamic contents in howm menu.
((R1 . T1) (R2 . T2) ...):
Regexp R1 is replaced by T1 if T1 is a string.
(T1) is called at R1 if T1 is a function.")

;;; command table

;; howm-menu-command-table-* = ((MATCHER FUNC ONBUF) ...)
;; 
;; (FUNC) is evalueted on ONBUF when return key is hit on MATCHER.
;; 
;; MATCHER = regexp | (regexp position)
;; (optional) ONBUF = nil | 'previous | 'current
;;   nil: previous non-menu buffer (set-buffer)
;;   'previous: previous non-menu buffer (switch-to-buffer)
;;   'current: current menu buffer

(howm-defvar-risky howm-menu-command-table-common
  '(
    (("%eval%\\(.*$\\)" 1) howm-menu-eval previous)
    (("%call%\\(.*$\\)" 1) howm-menu-call previous)
     ))

;;; which is opened as menu?

(howm-defvar-risky howm-menu-keyword-regexp "^%.*%$")
(howm-defvar-risky howm-menu-top "%menu%")

;;; misc.

(howm-defvar-risky howm-menu-toggle-invisible "%|")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal

(defvar *howm-menu-force-refresh* nil) ;; dirty. clean me. [2003/09/29 21:39]

(defvar *howm-menu-shortcut-keys* nil)
(defvar *howm-menu-shortcut-multidef-keys* nil)
(defvar *howm-menu-shortcut-markers* nil)
(make-variable-buffer-local '*howm-menu-shortcut-markers*)

(defvar howm-menu-previous-buffer nil)
(defvar howm-menu-next-expiry-time (current-time))
(defvar howm-menu-last-time (current-time))
(defvar howm-menu-buffer-file nil)
(defvar howm-menu-buffer-file-place nil)
(howm-defvar-risky howm-menu-mode-local-map nil)
(make-variable-buffer-local 'howm-menu-previous-buffer)
(make-variable-buffer-local 'howm-menu-next-expiry-time)
(make-variable-buffer-local 'howm-menu-last-time)
(make-variable-buffer-local 'howm-menu-buffer-file)
(make-variable-buffer-local 'howm-menu-buffer-file-place)
(make-variable-buffer-local 'howm-menu-mode-local-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode

(defun howm-menu-mode ()
  "howm menu
key	binding
---	-------
\\[action-lock-magic-return]	Follow link
\\[action-lock-goto-next-link]	Next link
\\[action-lock-goto-previous-link]	Prev link
\\[describe-mode]	This help
\\[bury-buffer]	Quit
"
  (interactive)
  (setq major-mode 'howm-menu-mode
        mode-name "HM")
  (setq howm-menu-mode-local-map (copy-keymap howm-menu-mode-map))
  (use-local-map howm-menu-mode-local-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(defun howm-menu (&optional force-refresh last-chance)
  (interactive)
  (when (and (eq (howm-folder-type howm-directory) ':dir)
             (not (file-exists-p howm-directory)))
    (make-directory howm-directory t))
  (let ((*howm-menu-force-refresh* force-refresh)
        ;; force to use the original howm-directory
        (*howm-independent-directories* nil))
    (if (and howm-menu-keyword-regexp (null howm-menu-file))
        (let ((m (howm-keyword-search howm-menu-top)))
          (when (and (cdr (assoc 'menu-p m))
                     (not (cdr (assoc 'keyword-matched m))))
            (howm-menu-initialize-skel last-chance)))
      (howm-menu-open howm-menu-file))))

(defun howm-menu-open (file &optional place name)
  (setq name (or name (howm-menu-name file)))
  (let ((f (if (file-name-absolute-p file)
               file
             (expand-file-name file howm-directory))))
    (if (file-exists-p f)
        (howm-menu-open-sub f place name)
      (progn
        (find-file f)
        (howm-mode)))))

(defun howm-menu-open-sub (f place name)
  (let* ((pb (current-buffer))
         (pm major-mode)
         (b (get-buffer name))
         (mtime (nth 5 (file-attributes f))))
    (if (or *howm-menu-force-refresh*
            (null b)
            (progn
              (set-buffer b)
              (or (howm-time< howm-menu-last-time mtime)
                  (howm-time< howm-menu-next-expiry-time
                              (current-time)))))
        (howm-menu-refresh f place name)
      (switch-to-buffer b))
    (let ((cm major-mode))
      (save-excursion
        (while (eq pm cm)
          (set-buffer pb)
          (setq pb howm-menu-previous-buffer)
          (set-buffer pb)
          (setq pm major-mode)))
      (setq howm-menu-previous-buffer pb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refresh

(howm-defvar-risky howm-menu-shortcut-assoc nil)
(make-variable-buffer-local 'howm-menu-shortcut-assoc)
(howm-defvar-risky howm-menu-invisible t
  "*Non nil if 'invisible' property should be used in menu.
This must be t at now.
When this is nil, delete-region is used instead, and bug appears.")

(defun howm-menu-refresh (&optional file place name)
  (interactive)
  ;; preprocess
  (when name
    (switch-to-buffer (get-buffer-create name)))
  (howm-menu-mode)
  (setq howm-menu-buffer-file (or file howm-menu-buffer-file))
  (setq howm-menu-buffer-file-place (or place
                                        howm-menu-buffer-file-place
                                        1))
  (setq howm-menu-shortcut-assoc nil)
  ;; main
  (howm-rewrite-read-only-buffer
    (howm-menu-insert-paragraph howm-menu-buffer-file
                                howm-menu-buffer-file-place)
    (howm-menu-dynamic-setup) ;; shotcut & dynamic contents
    (howm-menu-set-face))
  ;; postprocess
  (goto-char (point-min))
  (setq howm-menu-last-time (current-time))
  (setq howm-menu-next-expiry-time
        (howm-days-after (current-time) 0
                         howm-menu-expiry-hours))
  (howm-menu-shortcut-warn)
  (run-hooks 'howm-menu-hook))

(defun howm-menu-insert-paragraph (file place)
  (insert-file-contents (expand-file-name file
                                          howm-directory))
  (howm-view-set-place place)
  (let* ((r (howm-view-paragraph-region))
         (b (car r))
         (e (cadr r)))
    (delete-region e (point-max))
    (delete-region (point-min) b))
  (goto-char (point-max))
  (insert (howm-menu-footer)))

;; (defun howm-menu-dynamic-setup ()
;;   (let* ((action-lock-default-rules (howm-menu-action-lock-rules)))
;;     (if howm-mode
;;         (howm-initialize-buffer)
;;       (howm-mode 1)))
;;   (howm-menu-shortcut-initialize)
;;   (howm-menu-replace howm-menu-display-rules))

(defun howm-menu-dynamic-setup ()
  (howm-menu-shortcut-initialize)
  (howm-menu-replace howm-menu-display-rules)
  (let* ((action-lock-default-rules (howm-menu-action-lock-rules)))
    (if howm-mode
        (howm-initialize-buffer)
      (howm-mode 1))))

(defun howm-menu-set-face ()
  (set (make-local-variable 'font-lock-keywords-only) t)
  (howm-menu-add-font-lock)
  (font-lock-fontify-buffer)
  (when howm-menu-toggle-invisible
    (howm-menu-make-invisible)))

(defun howm-menu-footer ()
  (or howm-menu-footer
      (let* ((r (howm-menu-command-table-raw))
             (buttons (mapcar (lambda (f)
                                (cdr (assoc f
                                            (mapcar (lambda (z)
                                                      (cons (cadr z)
                                                            (car z)))
                                                    r))))
                              '(howm-menu-refresh howm-menu-edit)))
             (footer (apply #'concat `("\n-- \n" ,@buttons))))
        (setq howm-menu-footer footer)
        footer)))

(defun howm-menu-refresh-background ()
  ;; save-current-buffer doesn't work on GNU Emacs 21.4.1
  (let ((b (current-buffer)))
    (howm-menu t)
    (switch-to-buffer b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action-lock

(defun howm-menu-invoke (arg)
  (interactive "P")
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at howm-menu-list-regexp))
         (beginning-of-line)
         (action-lock-invoke arg))
        ((howm-menu-list-get-item)
         (howm-view-open-item (howm-menu-list-get-item)))
        (t
         (error "Not on spell string."))))

(defun howm-menu-action-lock-rules ()
  (let* ((d action-lock-default-rules)
         (f (howm-action-lock-reminder-forward-rules))
         (j (howm-menu-list-rules))
         (m (mapcar (lambda (pair)
                      (let* ((h (car pair))
                             (r (if (listp h) (car h) h))
                             (n (if (listp h) (cadr h) nil))
                             (args (if n
                                       `(list (match-string-no-properties ,n))
                                     nil))
                             (functab (cdr pair))
                             (c (howm-menu-action functab args)))
                        (list r c)))
                    (howm-menu-command-table))))
    (append m d j f)))

;; Elisp is not Scheme. Lambda is not closure. Don't forget dynamic binding.
;; Check
;;   (pp (car (howm-menu-action-lock-rules)))
;; for debug. [2003/09/25]
(defun howm-menu-action (function-table args)
  (let* ((func (car function-table))
         (onbuf (cadr function-table))
         (switch-p (eq onbuf 'previous)))
    (let* ((s-buf (if (eq onbuf 'current) 'cur 'prev))
           (s-switch `(switch-to-buffer ,s-buf))
           (s-apply `(apply #',func ,(if args 'a nil))))
;;            (s-apply `(apply #',func ,(if args '(list a) nil))))
      (let* ((s-body (if switch-p
                         `(progn ,s-switch ,s-apply)
                       `(with-current-buffer ,s-buf ,s-apply))))
        `(lambda (&optional ,howm-menu-action-arg)
           (let ((a ,args)
                 (cur (current-buffer))
                 (prev (if (howm-buffer-alive-p howm-menu-previous-buffer)
                           howm-menu-previous-buffer
                         (current-buffer))))
             ,s-body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortcut

(defun howm-menu-shortcut-get-marker ()
  (let ((m (make-marker)))
    (set-marker m (point))
    (add-to-list '*howm-menu-shortcut-markers* m)
    m))

(defun howm-menu-shortcut-clear-markers ()
  (mapc (lambda (m) (set-marker m nil))
        *howm-menu-shortcut-markers*)
  (setq *howm-menu-shortcut-markers* nil))

(defun howm-menu-shortcut-initialize ()
  (setq *howm-menu-shortcut-keys* nil)
  (setq *howm-menu-shortcut-multidef-keys* nil)
  (howm-menu-shortcut-clear-markers))

(defun howm-menu-shortcut-sort (keys)
  (mapconcat #'identity
             (sort (copy-sequence keys) #'string<)
             ""))

(defun howm-menu-shortcut-warn ()
  (when *howm-menu-shortcut-multidef-keys*
    (beep)
    (message "Multiple definitions for key(s): \"%s\" in \"%s\""
             (howm-menu-shortcut-sort *howm-menu-shortcut-multidef-keys*)
             (howm-menu-shortcut-sort *howm-menu-shortcut-keys*))))

;; Check howm-menu-mode-local-map if you want to debug howm-menu-shortcut.
(defun howm-menu-shortcut ()
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (wbeg (match-beginning howm-menu-key-regexp-word-pos))
         (wend (match-end  howm-menu-key-regexp-word-pos))
         (key (match-string-no-properties howm-menu-key-regexp-key-pos))
         (move-only (match-beginning howm-menu-key-regexp-moveonly-pos)))
    ;; 'end' must be first.
    ;; howm-menu-invisible-region can be delete-region indeed,
    ;; and points after the region can be slided.
    (howm-menu-invisible-region wend end)
    (howm-menu-invisible-region beg wbeg)
    (let ((p (howm-menu-shortcut-get-marker)))
      (setq howm-menu-shortcut-assoc
            (cons (cons key p) howm-menu-shortcut-assoc))
      (define-key howm-menu-mode-local-map key
        (howm-menu-shortcut-func key p move-only)))
    (when (member key *howm-menu-shortcut-keys*)
      (setq *howm-menu-shortcut-multidef-keys*
            (cons key *howm-menu-shortcut-multidef-keys*)))
    (setq *howm-menu-shortcut-keys*
          (cons key *howm-menu-shortcut-keys*))))

(defun howm-menu-shortcut-func (key p move-only)
  (if howm-menu-invisible
      (howm-menu-shortcut-func1 p move-only)
    (howm-menu-shortcut-func2 key p move-only)))

;; old code. it works.
(defun howm-menu-shortcut-func1 (p move-only)
  `(lambda (arg)
     (interactive "P")
     (let ((pos ,p))
       (if ,move-only
           (goto-char pos)
         (save-excursion
           (goto-char pos)
           (let ((case-fold-search nil)) ;; temporaly
             (when (null (action-lock-get-action))
               (action-lock-goto-next-link))
             (action-lock-invoke arg)))))))

;; new code. broken.
;; It doesn't work because action can be
;; (let ((s (match-string-no-properties 0))) (howm-keyword-search s nil nil)).
(defun howm-menu-shortcut-func2 (key p move-only)
  (if move-only
      `(lambda (arg) (interactive "P") (goto-char ,p))
    (save-excursion
      (goto-char p)
      (let ((case-fold-search nil)) ;; temporaly
        (when (null (action-lock-get-action))
          (action-lock-goto-next-link))
        (let ((action (action-lock-get-action)))
          (if (null action)
              (lambda (arg) (interactive "P") nil)
            (progn
              (rplacd (assoc key howm-menu-shortcut-assoc)
                      action)
              `(lambda (arg)
                 (interactive "P")
                 (funcall (cdr (assoc ,key howm-menu-shortcut-assoc))
                          arg)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action

(defun howm-menu-edit ()
  (interactive)
  (let ((place howm-menu-buffer-file-place))
    (find-file (expand-file-name howm-menu-buffer-file howm-directory))
    (howm-mode t)
    (when place
      (howm-view-set-place place)
      (recenter 0))))

(defun howm-menu-eval (s)
  (let ((expr (read s)))
    (eval expr)))

(defun howm-menu-call (s)
  (let ((expr (read s)))
    (call-interactively expr)))

(defun howm-open-today ()
  (interactive)
  (and (howm-create-file t)
       (howm-insert-template ""))
  (howm-set-mode))

(defun howm-open-past (&optional days-before)
  (interactive "p")
  (setq days-before (or days-before 1))
  (if (= days-before 0)
      (howm-open-today)
    (howm-open-past-sub days-before)))

(defun howm-open-past-sub (days-before)
  (let ((f (expand-file-name (howm-file-name (howm-days-after (current-time)
                                                              (- days-before)))
                             howm-directory)))
    (if (file-exists-p f)
        (find-file f)
      (error "No such file: %s" f)))
  (howm-set-mode))

(defun howm-find-past (&optional days-before)
  (interactive "p")
  (cond ((howm-one-file-one-day-p) (howm-open-past days-before))
        (t (howm-search-past days-before))))

(defun howm-find-today (&optional days-before)
  (interactive "P")
  (howm-find-past (or days-before 0)))

(defun howm-find-yesterday (&optional days-before)
  (interactive)
  (howm-find-past (or days-before 1)))

(defun howm-one-file-one-day-p ()
  (let* ((now (decode-time))
         (d (nth 3 now))
         (m (nth 4 now))
         (y (nth 5 now))
         (beginning-of-day (encode-time 0 0 0 d m y))
         (end-of-day (encode-time 59 59 23 d m y)))
    (string= (howm-file-name beginning-of-day)
             (howm-file-name end-of-day))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; face

(defun howm-menu-make-invisible ()
  (save-excursion
    (goto-char (point-min))
    (let (visible-p
          invisible-beg)
      (while (not (= (point) (point-max)))
        (setq visible-p t)
        (while (re-search-forward howm-menu-toggle-invisible
                                  (line-end-position) t)
          (if visible-p
              (setq invisible-beg (match-beginning 0))
            (howm-menu-invisible-region invisible-beg (match-end 0)))
          (setq visible-p (not visible-p)))
        (when (not visible-p)
          (howm-menu-invisible-region invisible-beg
                                      (save-excursion (forward-line) (point))))
        (forward-line)))))

(defun howm-menu-font-lock-rules ()
  `((,howm-menu-key-regexp
     (,howm-menu-key-regexp-key-pos howm-menu-key-face t))
    ;; In menu-list form "> FILE-NAME | ",
    ;; I want to hide annoying long underlines drawn by action-lock.
    (,howm-menu-list-regexp
     (,howm-menu-list-regexp-face-pos howm-menu-list-face t))
    ;; But some users may want to highlight today's YYYY-MM-DD even if
    ;; it is a part of a FILE-NAME.
    ;; The next code makes duplicated entries; they are already put into
    ;; font-lock-keywords by howm-reminder-add-font-lock
    ;; in howm-initialize-buffer because menu is howm-mode.
    ;; They are hidden by the above rule in FILE-NAME columns,
    ;; and I need to put them again now. Sigh...
    ;; Clean me!
    ,@(howm-reminder-today-font-lock-keywords)))
(defun howm-menu-add-font-lock ()
  (cheat-font-lock-append-keywords (howm-menu-font-lock-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic contents

(defun howm-menu-replace (rules)
  (mapc (lambda (pair)
          (let* ((reg (car pair))
                 (to (cdr pair)))
            (goto-char (point-min))
            (while (re-search-forward reg nil t)
              (cond ((stringp to) (replace-match to))
                    ((functionp to) (funcall to))
                    (t (error "Invalid to-part: %s." to))))))
        rules))

;; (defun howm-menu-func ()
;;   (let ((b (match-beginning 0))
;;         (e (match-end 0))
;;         (f (read (match-string-no-properties 1))))
;;     (if (or (eq howm-menu-allow t)
;;             (member f howm-menu-allow))
;;         (howm-replace-region b e (funcall f))
;;       (message "%s is not allowed." f))))

;; (defun howm-menu-var ()
;;   (let ((b (match-beginning 0))
;;         (e (match-end 0))
;;         (f (read (match-string-no-properties 1))))
;;     (howm-replace-region b e (eval f))))

(defun howm-menu-here ()
  (let* ((beg (match-beginning 0))
         (expr-beg (match-end 0))
         (expr-end (progn (forward-sexp) (point)))
         (expr (read (buffer-substring-no-properties expr-beg expr-end))))
    (cond ((symbolp expr) (howm-menu-here-var expr beg expr-end))
          ((listp expr) (howm-menu-here-func (car expr) (cdr expr)
                                              beg expr-end))
          (t (message "Unknown expr: %s" expr)))))

(defun howm-menu-here-var (expr beg end)
  (if (boundp expr)
      (howm-replace-region beg end (symbol-value expr))
    (message "Unknown symbol: %s" expr)))

(defun howm-menu-here-func (func args beg end)
;;   (let ((allowed (or (eq howm-menu-allow t) (member func howm-menu-allow))))
  (let ((allowed (member func howm-menu-allow)))
    (cond ((not allowed) (message "Not allowed: %s" func))
          ((not (fboundp func)) (message "Unknown function: %s" func))
          (t (howm-replace-region beg end (apply func args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; schedule, todo, recent, random

;;; command

(defun howm-menu-schedule ()
  (howm-menu-general "schedule" 'schedule
                     (howm-schedule-menu howm-menu-schedule-days
                                         howm-menu-schedule-days-before)))

(defvar howm-menu-todo-show-day-of-week t)
(defun howm-menu-todo ()
  (howm-menu-general "todo" 'todo
                     (howm-todo-menu howm-menu-todo-num
                                     howm-menu-todo-priority
                                     howm-menu-reminder-separators)))
(defun howm-menu-reminder ()
  (howm-menu-general "reminder" 'todo
                     (howm-reminder-menu howm-menu-todo-num
                                         howm-menu-todo-priority
                                         howm-menu-reminder-separators)))

(defun howm-menu-recent (&optional evaluator label)
  (howm-menu-general (or label "recent")
                     nil
                     (howm-recent-menu howm-menu-recent-num evaluator)))

(defun howm-menu-random () (howm-menu-recent t "random"))

(defun howm-menu-general (label formatter item-list)
  "Generate output string for items in howm menu.
LABEL is only used for message.
FORMATTER is a function which receives an item and returns an output string
 (without newline).
FORMATTER can be nil for standard style, 'todo for todo style,
'schedule for schedule style, or 'full for full note.
ITEM-LIST is list of items which should be shown."
  (let ((f (cond ((null formatter) #'howm-menu-format-item)
                 ((eq 'todo formatter) #'howm-menu-format-todo)
                 ((eq 'schedule formatter) #'howm-menu-format-reminder)
                 ((eq 'full formatter) #'howm-menu-format-full)
                 (t formatter))))
    (let* ((msg "scanning %s...")
           (msg-done (concat msg "done")))
      (message msg label)
      ;;     (delete-region (match-beginning 0) (match-end 0))
      (prog1
          (mapconcat f item-list "\n")
        (message msg-done label)))))

;;; schedule/todo

(defun howm-menu-format-todo (item)
  ;; item can be a separator.
  (if (eq (howm-page-type (howm-item-page item)) ':nil)
      (howm-item-summary item)
    (let ((dow-str (cond (howm-menu-todo-show-day-of-week nil)
                         (t "  "))))
      (howm-menu-format-reminder item dow-str t))))

(defun howm-menu-format-reminder (item &optional day-of-week-str show-priority)
  (let* ((p (howm-todo-parse item))
         (late (floor (car p)))
         (dow (cl-cadddr p))
         (dow-str (or day-of-week-str
                      (howm-day-of-week-string dow)))
         (priority (if (and howm-menu-todo-priority-format
                            show-priority)
                       (format howm-menu-todo-priority-format
                               (howm-todo-priority item))
                     ""))
         (h (format "%s%3s%s" dow-str late priority)))
    (howm-menu-list-format h (howm-view-item-summary item) item
                           howm-menu-reminder-format)))

(defun howm-day-of-week-string (&optional day-of-week)
  ;; 0 = Sunday
  (let ((dow (or day-of-week (nth 6 (decode-time))))
        (names (howm-day-of-week)))
    (cond ((stringp names) (substring names dow (1+ dow))) ;; backward compatibility
          ((listp names) (nth dow names))
          (t "  "))))

(defun howm-menu-format-full (item)
  (let ((text (format "%s %s\n%s"
                      howm-ref-header
                      (howm-item-name item)
                      (with-temp-buffer
                        (howm-page-insert (howm-item-page item))
                        (howm-view-set-place (howm-view-item-place item))
                        (apply 'buffer-substring-no-properties
                               (howm-view-paragraph-region))))))
    (howm-menu-list-put-item text item)
    text))

;;; recent/random

(defun howm-recent-menu (num &optional evaluator)
  ;; Bug: (length howm-recent-menu) can be smaller than NUM
  ;; when empty files exist.
  (let* ((randomp (eq evaluator t))
         (summarizer #'(lambda (file line content) content))
         ;; Unique name is needed for dynamic binding. Sigh...
         (h-r-m-evaluator (if randomp
                              (lambda (f) (number-to-string (random)))
                            (or evaluator #'howm-view-mtime)))
         (sorted (howm-sort (lambda (f) (funcall h-r-m-evaluator f))
                            #'howm-view-string>
                            (mapcar #'howm-item-name
                                    (howm-folder-items howm-directory t))))
         (files (howm-first-n sorted num)))
    (let ((r (howm-menu-recent-regexp)))
      (if randomp
          (cl-mapcan (lambda (f)
                            (let ((is (howm-view-search-items r (list f)
                                                              summarizer)))
                              (and is (list (nth (random (length is))
                                                 is)))))
                          files)
        (howm-first-n (howm-view-search-items r files summarizer) num)))))

(defun howm-menu-recent-regexp ()
  (or howm-menu-recent-regexp (howm-view-title-regexp-grep)))

;;; common

(defun howm-menu-list-put-item (text item)
  ;; put it to whole text, because I don't assume "> ..." format here.
  (put-text-property 0 (length text) 'howm-menu-list-item item text))
(defun howm-menu-list-get-item (&optional text)
  (get-text-property (if text 0 (point)) 'howm-menu-list-item text))
(defun howm-menu-list-getput-item (from-text to-text)
  (howm-menu-list-put-item to-text
                           (howm-menu-list-get-item from-text)))

(defun howm-menu-list-action (&optional keyword)
  (let ((item (howm-menu-list-get-item keyword)))
    (cond (item (howm-view-open-item item)) ;; schedule, todo, etc.
          (keyword (howm-keyword-search keyword)) ;; history
          (t (error "Target is not specified."))))) ;; can't happen

(defun howm-menu-format-item (item &optional list-format)
  (let* ((info (file-name-sans-extension (howm-view-item-basename item)))
         (line (howm-view-item-summary item)))
    (howm-menu-list-format info line item list-format)))

(defun howm-menu-list-format (info line item &optional list-format)
  (let ((s (format (or list-format howm-menu-list-format) info line)))
    (howm-menu-list-put-item s item)
    s))

(defun howm-menu-list-rules ()
  (list (action-lock-general #'howm-menu-list-action
                             howm-menu-list-regexp
                             howm-menu-list-regexp-key-pos
                             howm-menu-list-regexp-action-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; embed search result

(defun howm-menu-search (key &optional formatter regexp-p)
  "Embed search result of KEY into menu.
See `howm-menu-general' for FORMATTER.
KEY is a regular expression if REGEXP-P is not nil.

Bugs: If you write %here%(howm-menu-search \"foo\" full) in your menu,
- menu file itself is also searched.
Write %here%(howm-menu-search \"[f]oo\" full t) insteadly.
- same note is shown twice if \"foo\" is hit twice in it."
  (let ((fixed-p (not regexp-p)))
    (howm-menu-general "menu-search"
                       formatter
                       (howm-view-search-folder-items key (howm-folder)
                                                      nil fixed-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; categorized todo-list

;; Experimental [2006-01-16]

(defun howm-menu-classified-reminder (classifier &optional comparer
                                                 title-format)
  "Generate string of classified reminder-list.
CLASSIFIER is a function which receives an item and answers its class.
Class can be an arbitrary lisp object.
When class is nil, corresponding item is not shown in this list.
COMPARER is a function which receives two keys and answer t or nil.
It is used for sorting of keys.
TITLE-FORMAT is a format string for class title."
  (let* ((a (howm-classify classifier
                           (howm-reminder-menu nil
                                               howm-menu-todo-priority
                                               nil)))
         ;; key 'nil' is skipped.
         (keys (remove nil (mapcar #'car a)))
         (tform (concat (or title-format "--%s--") "\n")))
    (when comparer
      (setq keys (sort keys comparer)))
    (mapconcat (lambda (k)
                 (let* ((item-list (howm-first-n (cdr (assoc k a))
                                                 howm-menu-todo-num))
                        (is (howm-with-reminder-setting
                              (howm-todo-insert-separators
                               item-list
                               howm-menu-reminder-separators
                               t))))
                   (concat (format tform k)
                           (howm-menu-general (format "reminder(%s)" k) 'todo
                                              is))))
               keys "\n")))

(defun howm-menu-categorized-reminder (categories &optional title-format
                                                  erase-p omit-misc-p)
  "Generate string of categorized reminder-list.

Write %here%(howm-menu-categorized-reminder (\"foo\" \"bar\" \"baz\"))
to show categorized list in menu. (You don't need quote(')
before the above list; arguments are not evaluated in %here%
because I don't have enough courage to call eval.)

If you like to erase category label from summary string, try
%here%(howm-menu-categorized-reminder (\"foo\" \"bar\" \"baz\") nil t)
instead.

If you don't like misc. category, try
%here%(howm-menu-categorized-reminder (\"foo\" \"bar\" \"baz\") nil nil t)."
  ;; Using categories, matcher, etc. in lambda is bad indeed
  ;; because of dynamic binding.
  (let* ((matcher (lambda (cat str item)
                    (and (string-match (regexp-quote cat) str)
                         (progn
                           (when erase-p
                             (howm-item-set-summary item
                                                    (replace-match "" nil nil
                                                                   str)))
                           t))))
         (classifier (lambda (item)
                       (let ((s (howm-item-summary item)))
                         (or (cl-find-if (lambda (c)
                                                (funcall matcher c s item))
                                              categories)
                             (if omit-misc-p nil "misc.")))))
         (pos (lambda (c) (or (cl-position c categories) howm-infinity)))
         (comparer (lambda (a b) (< (funcall pos a) (funcall pos b)))))
    (howm-menu-classified-reminder classifier comparer title-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate initial menu

(defun howm-menu-initialize-skel (&optional dummy)
  (let ((menu-name (howm-get-symbol nil "howm-menu-" howm-menu-lang)))
    (require menu-name)
    (howm-menu-copy-skel (symbol-value menu-name))
    (howm-view-kill-buffer)
    (howm-menu nil t)))

(defun howm-menu-copy-skel (contents)
  (let ((menu-file (or howm-menu-file
                       (expand-file-name "0000-00-00-000000.txt"
                                         howm-directory)))
        (r "^="))
    (if (file-exists-p menu-file)
        ;; I have no courage to erase existing file.
        (progn
          (setq howm-menu-file menu-file)
          (message "Assume %s as menu file." menu-file))
      (progn
        (find-file menu-file)
        (insert contents)
        (goto-char (point-min))
        (while (re-search-forward r nil t)
          (replace-match howm-view-title-header nil nil))
        (howm-mode 1)
        (save-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch language

(defun howm-require-lang (&optional lang)
  (require (howm-get-symbol nil "howm-lang-" (or lang howm-menu-lang))))

(defun howm-lang-ref (var)
  (let ((lang howm-menu-lang))
    (howm-require-lang lang)
    ;; For backward compatibility, I use howm-day-of-week-en
    ;; rather than howm-day-of-week:en.
    (symbol-value (howm-get-symbol t var "-" lang))))

(defun howm-menu-command-table-raw ()
  (howm-lang-ref "howm-menu-command-table"))

(defun howm-menu-command-table ()
  (append howm-menu-command-table-common
          (mapcar (lambda (pair) (cons (regexp-quote (car pair)) (cdr pair)))
                  (howm-menu-command-table-raw))))

(defun howm-day-of-week ()
  (howm-lang-ref "howm-day-of-week"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.

(defun howm-menu-p ()
  (string= major-mode "howm-menu-mode"))

(defun howm-menu-name (file)
  (format howm-menu-name-format file))

(defun howm-buffer-alive-p (buf)
  (and buf (buffer-name buf)))

(defun howm-menu-keyword-p (keyword)
  (and howm-menu-keyword-regexp
       (stringp keyword) ;; perhaps unnecessary
       (string-match howm-menu-keyword-regexp keyword)))

(defun howm-time< (t1 t2)
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (cadr t1) (cadr t2)))))

(defun howm-menu-invisible-region (beg end)
  (if howm-menu-invisible
      (put-text-property beg end 'invisible t)
    (delete-region beg end))
;;   (put-text-property beg end 'intangible t)
  )

;;; howm-menu.el ends here
