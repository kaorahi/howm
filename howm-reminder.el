;;; howm-reminder.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-reminder.el,v 1.83 2012-12-29 08:57:18 hira Exp $
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

(provide 'howm-reminder)
(require 'howm)

(defvar howm-list-schedule-name "{schedule}")
(defvar howm-list-todo-name "{todo}")
;   "This is used for buffer name of `howm-list-reminder'.
; See `howm-view-summary-name'.")

(howm-defvar-risky howm-todo-priority-func
      '(("-" . howm-todo-priority-normal)
        (" " . howm-todo-priority-normal)
        ("+" . howm-todo-priority-todo)
        ("~" . howm-todo-priority-defer)
        ("!" . howm-todo-priority-deadline)
        ("@" . howm-todo-priority-schedule)
        ("." . howm-todo-priority-done)))
(defvar howm-todo-priority-normal-laziness 1)
(defvar howm-todo-priority-todo-laziness 7)
(defvar howm-todo-priority-todo-init -7)
(defvar howm-todo-priority-defer-laziness 30)
(defvar howm-todo-priority-defer-init -14)
(defvar howm-todo-priority-defer-peak 0)
(defvar howm-todo-priority-deadline-laziness 7)
(defvar howm-todo-priority-deadline-init -2)
(defvar howm-todo-priority-schedule-laziness 1)
(defvar howm-todo-priority-normal-bottom   (- howm-huge))
(defvar howm-todo-priority-todo-bottom     (- howm-huge))
(defvar howm-todo-priority-defer-bottom    (- howm-huge))
(defvar howm-todo-priority-deadline-bottom (- howm-huge))
(defvar howm-todo-priority-schedule-bottom (- howm-huge++)
  "Base priority of schedules in the bottom.
Its default value is extremely negative so that you never see
schedules outside the range in %reminder in the menu.")
(defvar howm-todo-priority-deadline-top    howm-huge)
(defvar howm-todo-priority-schedule-top    howm-huge)
(defvar howm-todo-priority-unknown-top     howm-huge+)

(defvar howm-action-lock-reminder-done-default nil)

(defvar howm-congrats-count 0)

;;; --- level ? ---

;; Fix me: redundant (howm-date-* & howm-reminder-*)

;; (defun howm-reminder-regexp-grep (types)
;;   (howm-inhibit-warning-in-compilation))
;; (defun howm-reminder-regexp (types)
;;   (howm-inhibit-warning-in-compilation))

(if howm-reminder-old-format
    (progn ;; old format
      (defvar howm-reminder-regexp-grep-format
        "@\\[[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\]%s")
      (defvar howm-reminder-regexp-format
        "\\(@\\)\\[\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\]\\(%s\\)\\([0-9]*\\)")
      (defun howm-reminder-regexp-grep (types)
        (format howm-reminder-regexp-grep-format types))
      (defun howm-reminder-regexp (types)
        (format howm-reminder-regexp-format types))
      (defvar howm-reminder-regexp-command-pos 1)
      (defvar howm-reminder-regexp-year-pos 2)
      (defvar howm-reminder-regexp-month-pos 3)
      (defvar howm-reminder-regexp-day-pos 4)
      (defvar howm-reminder-regexp-type-pos 5)
      (defvar howm-reminder-regexp-laziness-pos 6)
      (defvar howm-reminder-today-format "@[%Y/%m/%d]")
      (howm-defvar-risky howm-reminder-font-lock-keywords
        `(
          (,(howm-reminder-regexp "[-]?") (0 howm-reminder-normal-face prepend))
          (,(howm-reminder-regexp "[+]") (0 howm-reminder-todo-face prepend))
          (,(howm-reminder-regexp "[~]") (0 howm-reminder-defer-face prepend))
          (,(howm-reminder-regexp "[!]")
           (0 howm-reminder-deadline-face prepend)
           (,howm-reminder-regexp-type-pos (howm-reminder-deadline-type-face) prepend))
          (,(howm-reminder-regexp "[@]") (0 howm-reminder-schedule-face prepend))
          (,(howm-reminder-regexp "[.]") (0 howm-reminder-done-face prepend))
          ))
      (defun howm-reminder-font-lock-keywords ()
        howm-reminder-font-lock-keywords)
      (defun howm-action-lock-done (&optional command)
        (save-excursion
          (let ((at-beg (match-beginning howm-reminder-regexp-command-pos))
                (at-end (match-end  howm-reminder-regexp-command-pos))
                (type-beg (match-beginning howm-reminder-regexp-type-pos))
                (type-end (match-end howm-reminder-regexp-type-pos))
                (lazy-beg (match-beginning howm-reminder-regexp-laziness-pos))
                (lazy-end (match-end howm-reminder-regexp-laziness-pos)))
            (let* ((s (or command
                          (read-from-minibuffer
                           "RET (done), x (cancel), symbol (type), num (laziness): ")))
                   (c (cond ((string= s "") ".")
                            ((= 0 (string-to-number s)) ". give up")
                            (t nil))))
              (when (string= s "")
                (howm-congrats))
              (if c
                  (progn
                    (goto-char at-beg)
                    (delete-region at-beg at-end)
                    (insert (howm-reminder-today))
                    (insert (format "%s " c)))
                (progn
                  (goto-char lazy-beg)
                  (delete-region lazy-beg lazy-end)
                  (when (string= (buffer-substring-no-properties type-beg type-end)
                                 " ")
                    (goto-char type-beg)
                    (insert "-")) ;; "no type" = "normal"
                  (insert s)))))))
      )
  (progn ;; new format
    (defvar howm-reminder-regexp-grep-format
      (concat "\\[" howm-date-regexp-grep "[ :0-9]*\\]%s"))
    (defvar howm-reminder-regexp-format
      (concat "\\(\\[" howm-date-regexp "[ :0-9]*\\]\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))
;;     (defvar howm-reminder-regexp-grep-format
;;       (concat "\\[" howm-date-regexp-grep "\\]%s"))
;;     (defvar howm-reminder-regexp-format
;;       (concat "\\[" howm-date-regexp "\\]\\(\\(%s\\)\\([0-9]*\\)\\)"))
    (defun howm-reminder-regexp-grep (types)
      (format howm-reminder-regexp-grep-format types))
    (defun howm-reminder-regexp (types)
      (format howm-reminder-regexp-format types))
    (defvar howm-reminder-regexp-date-pos 1)
    (defvar howm-reminder-regexp-year-pos  (+ howm-date-regexp-year-pos 1))
    (defvar howm-reminder-regexp-month-pos (+ howm-date-regexp-month-pos 1))
    (defvar howm-reminder-regexp-day-pos   (+ howm-date-regexp-day-pos 1))
    (defvar howm-reminder-regexp-command-pos 5)
    (defvar howm-reminder-regexp-type-pos 6)
    (defvar howm-reminder-regexp-laziness-pos 7)
    (defvar howm-reminder-today-format
      (format howm-insert-date-format howm-date-format))
    (howm-defvar-risky howm-reminder-font-lock-keywords
      `(
        (,(howm-reminder-regexp "[-]") (0 howm-reminder-normal-face prepend))
        (,(howm-reminder-regexp "[+]") (0 howm-reminder-todo-face prepend))
        (,(howm-reminder-regexp "[~]") (0 howm-reminder-defer-face prepend))
        (,(howm-reminder-regexp "[!]")
         (0 howm-reminder-deadline-face prepend)
         (,howm-reminder-regexp-type-pos (howm-reminder-deadline-type-face) prepend))
        (,(howm-reminder-regexp "[@]") (0 howm-reminder-schedule-face prepend))
        (,(howm-reminder-regexp "[.]") (0 howm-reminder-done-face prepend))
        ))
    (defun howm-reminder-font-lock-keywords ()
      howm-reminder-font-lock-keywords)
    (defun howm-action-lock-done-prompt ()
      (format "RET (done), x (%s), symbol (type), num (laziness): "
              howm-reminder-cancel-string))
    (defun howm-action-lock-done (&optional command)
      ;; parse line
      (let* ((pos (point))
             (beg (match-beginning 0))
             (end (match-end 0))
             (date (match-string-no-properties howm-reminder-regexp-date-pos))
             (type (match-string-no-properties howm-reminder-regexp-type-pos))
             (lazy (match-string-no-properties howm-reminder-regexp-laziness-pos))
             (desc (buffer-substring-no-properties end (line-end-position))))
        ;; parse input command
        (let* ((s (or command
                      (howm-read-string (howm-action-lock-done-prompt)
                                        "x-+~!.@"
                                        "0123456789")))
               (type-or-lazy (string-match (format "^\\(%s?\\)\\([0-9]*\\)$"
                                                   howm-reminder-types)
                                           s))
               (new-type (and type-or-lazy (match-string-no-properties 1 s)))
               (new-lazy (and type-or-lazy (match-string-no-properties 2 s))))
          (when (string= new-type "")
            (setq new-type type))
          (when (string= new-lazy "")
            (setq new-lazy lazy))
          ;; dispatch and get new contents
          (let ((new (cond ((string= s "")
                            (howm-action-lock-done-done date type lazy desc))
                           ((string= s "x")
                            (howm-action-lock-done-cancel date type lazy
                                                          desc))
                           (type-or-lazy
                            (howm-action-lock-done-modify date
                                                          new-type new-lazy
                                                          desc))
                           (t
                            (error "Can't understand %s" s)))))
            ;; replace contents
            (goto-char beg)
            (delete-region (point) (line-end-position))
            (insert new)
            (goto-char pos)))))
    (defun howm-action-lock-done-done (date type lazy desc &optional done-mark)
      (when (null done-mark)
        (setq done-mark ".")
        (howm-congrats))
      (concat (howm-reminder-today) done-mark " "
              date ":" type lazy desc))
    (defun howm-action-lock-done-cancel (date type lazy desc)
      (howm-action-lock-done-done date type lazy desc
                                  (format ". %s" howm-reminder-cancel-string)))
    (defun howm-action-lock-done-modify (date type lazy desc)
      (concat date type lazy desc))
    ))

(defun howm-reminder-deadline-type-face ()
  (let ((late (cadr (howm-todo-parse-string (match-string-no-properties 0)))))
    (if (>= late 0)
        howm-reminder-late-deadline-face
      howm-reminder-deadline-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reminder: schedule & todo

(define-key howm-view-summary-mode-map "." 'howm-reminder-goto-today)

;; Clean me.
;; I cannot remember why I wrote howm-with-schedule-summary-format.
(defmacro howm-with-schedule-summary-format (&rest body)
  (declare (indent 0))
  `(let ((howm-view-summary-format (if howm-view-split-horizontally ;; dirty!
                                      ""
                                    howm-view-summary-format)))
     ,@body))

(defun howm-list-schedule ()
  (interactive)
  (howm-with-need
    (howm-with-schedule-summary-format
      (let ((items (need (howm-list-reminder-internal howm-schedule-types))))
        (howm-list-reminder-final-setup howm-list-schedule-name
                                        (howm-schedule-sort-items items)))
      (howm-reminder-goto-today)
      (howm-view-summary-check))))

(defun howm-list-reminder-internal (types)
  (let* ((r (howm-reminder-regexp types))
         (rg (howm-reminder-regexp-grep types))
         (summarizer (howm-reminder-summarizer r t))
         (folder (howm-reminder-search-path-folder)))
    (cl-caddr (howm-view-search-folder-internal rg folder nil summarizer))))

(defun howm-list-reminder-final-setup (&optional name item-list)
  (howm-view-summary name item-list
                     (append (howm-reminder-add-font-lock-internal)
                             (howm-mode-add-font-lock-internal)))
  (let ((action-lock-default-rules
         (howm-action-lock-reminder-forward-rules t)))
    (action-lock-mode t)))

(let ((rs (mapcar #'regexp-quote
                  (list howm-date-format howm-reminder-today-format))))
  (defcustom howm-highlight-date-regexp-format (car rs)
    "Time format for highlight of today and tommorow.
This value is passed to `format-time-string', and the result must be a regexp."
    :type `(radio ,@(mapcar (lambda (r) `(const ,r)) rs)
                    string)
    :group 'howm-faces))

(defun howm-reminder-today-font-lock-keywords ()
  (let ((today    (howm-reminder-today 0 howm-highlight-date-regexp-format))
        (tomorrow (howm-reminder-today 1 howm-highlight-date-regexp-format)))
    `((,today (0 howm-reminder-today-face prepend))
      (,tomorrow (0 howm-reminder-tomorrow-face prepend)))))

(defun howm-reminder-add-font-lock ()
  (cheat-font-lock-append-keywords (howm-reminder-add-font-lock-internal)))

(defun howm-reminder-add-font-lock-internal ()
  (append (howm-reminder-font-lock-keywords)
          (howm-reminder-today-font-lock-keywords)))

(defun howm-reminder-omit-before (regexp str)
  (string-match regexp str)
  (substring str (match-beginning 0)))

(defun howm-reminder-summarizer (regexp &optional show-day-of-week)
  `(lambda (file line content)
     (let ((s (howm-reminder-omit-before ,regexp content)))
;;                 (string-match ,regexp content)
;;                 (substring content (match-beginning 0)))))
       ,(if show-day-of-week
            '(let* ((p (howm-todo-parse-string s))
                    (late (floor (nth 1 p)))
                    (dow (howm-day-of-week-string (nth 4 p))))
               (format "%s%3s %s" dow late s))
          's))))

(defun howm-reminder-today (&optional days-after fmt)
  (format-time-string (or fmt howm-reminder-today-format)
                      (howm-days-after (current-time) (or days-after 0))))

;; dirty. peek howm-view-*
(defun howm-reminder-goto-today ()
  (interactive)
  (let* ((today (howm-reminder-today))
         (r (howm-reminder-regexp "."))
         (summaries (mapcar (lambda (item)
                              (howm-reminder-omit-before
                               r (howm-view-item-summary item)))
                           (howm-view-item-list))))
;;         (summaries (mapcar 'howm-view-item-summary (howm-view-item-list))))
    (let ((rest summaries)
          (n 0))
      (while (and rest
                  (string< (car rest) today))
        (setq rest (cdr rest)
              n (1+ n)))
      (howm-goto-line (1+ n)))))

(defun howm-schedule-menu (days &optional days-before)
  (let* ((today (howm-encode-day t)) 
         (from (- today (or days-before 0)))
         (to (+ today days 1))
         (howm-schedule-types howm-schedule-menu-types)  ;; dirty
         (raw (howm-reminder-search howm-schedule-types))
         (filtered (cl-remove-if #'(lambda (item)
                                          (let ((s (howm-schedule-date item)))
                                            (or (< s from)
                                                (< to s))))
                                      raw)))
    (howm-schedule-sort-items filtered)))

(defun howm-schedule-sort-items (items &optional reverse-p)
  (when reverse-p
    (error "Not supported."))
  (howm-with-schedule-summary-format
    (howm-sort #'howm-schedule-sort-converter #'howm-schedule-sort-comparer
               items)))
(defun howm-schedule-sort-by-date ()
  (interactive)
  (howm-view-sort-doit #'howm-schedule-sort-items))
(defun howm-schedule-sort-converter (item)
  (let ((z (howm-reminder-parse item)))
    (cons (car z)
          (if howm-schedule-sort-by-time
              (howm-item-summary item)
            (nth 5 z)))))
(defun howm-schedule-sort-comparer (a b)
  (if (= (car a) (car b))
      (string< (cdr a) (cdr b))
    (< (car a) (car b))))

(defun howm-schedule-date (item)
  (car (howm-reminder-parse item)))

(defun howm-reminder-search (types)
  (let* ((r (howm-reminder-regexp types))
         (rg (howm-reminder-regexp-grep types))
         (summarizer (howm-reminder-summarizer r))
         (folder (howm-reminder-search-path-folder)))
    (howm-view-search-folder-items rg folder summarizer)))

(defun howm-list-todo ()
  (interactive)
  (howm-list-todo-sub))

;; experimental [2006-06-26]
(defun howm-todo-sleeping-p (item)
  ;; (- howm-huge-) should be replaced with an appropreate variable.
  (< (howm-todo-priority item) (- howm-huge-)))
(defun howm-list-active-todo ()
  (interactive)
  (howm-list-todo-sub (lambda (item)
                        (not (howm-todo-sleeping-p item)))))
(defun howm-list-sleeping-todo ()
  (interactive)
  (howm-list-todo-sub #'howm-todo-sleeping-p))

(defun howm-list-todo-sub (&optional pred)
  (howm-with-need
    (howm-with-schedule-summary-format
      (let ((items (need (howm-list-reminder-internal howm-todo-types))))
        (when pred
          (setq items
                (need (cl-remove-if-not pred items))))
        (setq items (howm-todo-sort-items items))
        (when howm-todo-separators
          (setq items
                (howm-todo-insert-separators items
                                             howm-todo-separators)))
      (howm-list-reminder-final-setup howm-list-todo-name items)))))

(defun howm-todo-menu (n limit-priority separators)
  "Find top N todo items, or all todo items if N is nil.
Returned value is a sorted list of items (see `howm-make-item').
Items whose priority is worse than LIMIT-PRIORITY are eliminated.
Separator strings are inserted to the returned list according to
the rule given as SEPARATORS.
See docstring of the variable `howm-menu-reminder-separators' for details."
  (let* ((cutted (cl-remove-if (lambda (item)
                                      (< (howm-todo-priority item)
                                         limit-priority))
                                    (howm-reminder-search howm-todo-menu-types)))
         (sorted (howm-todo-sort-items cutted)))
    (howm-todo-insert-separators (if n (howm-first-n sorted n) sorted)
                                 separators t)))

(defun howm-reminder-menu (n limit-priority separators)
  (howm-with-reminder-setting
    (howm-todo-menu n limit-priority separators)))

(defun howm-todo-insert-separators (item-list separators
                                              &optional relative-date-p)
  (let ((is (mapcar (lambda (item) (cons (howm-todo-priority item) item))
                    item-list))
        (sep (mapcar (lambda (pair)
                       (cons (if relative-date-p
                                 (- howm-todo-priority-schedule-top
                                    (or (car pair) howm-huge-))
                               (or (car pair) (- howm-huge-)))
                             (howm-make-item (howm-make-page:nil) (cdr pair))))
                     separators)))
    (mapcar #'cdr
            (sort (append is sep) #'(lambda (x y) (> (car x) (car y)))))))

(defun howm-todo-sort-items (items &optional reverse-p)
  (when reverse-p
    (error "Not supported."))
  (howm-sort #'howm-todo-priority-ext #'howm-todo-priority-ext-gt
             items))

(defun howm-todo-sort-by-priority ()
  (howm-view-sort-doit #'howm-todo-sort-items))

;; Clean me.
(defun howm-reminder-parse (item)
  (howm-todo-parse-string (howm-view-item-summary item)))
(defun howm-todo-parse (item)
  (cdr (howm-reminder-parse item)))
(defun howm-todo-parse-string (str)
  "Parse reminder format.
Example: (howm-todo-parse-string \"abcde [2004-11-04]@ hogehoge\")
==> (12725.625 0.022789351851315587 \"@\" nil 4 \" hogehoge\")"
  (let ((summary str))
    (string-match (howm-reminder-regexp ".") summary)
    (let ((y (match-string-no-properties howm-reminder-regexp-year-pos
                                         summary))
          (m (match-string-no-properties howm-reminder-regexp-month-pos
                                         summary))
          (d (match-string-no-properties howm-reminder-regexp-day-pos
                                         summary))
          (ty (match-string-no-properties howm-reminder-regexp-type-pos
                                          summary))
          (lz (match-string-no-properties howm-reminder-regexp-laziness-pos
                                          summary))
          (description (substring str (match-end 0))))
      (let* ((day (howm-encode-day d m y))
             (today (howm-encode-day))
             (late (- today day))
             (type (substring (or ty "-") 0 1)) ;; "-" for old format
             (lazy (cond ((string= type " ") nil)
                         ((null lz) nil)
                         (t (let ((z (string-to-number lz)))
                              (if (= z 0) nil z)))))
             ;;            (lazy (if (string= type " ")
             ;;                      0
             ;;                    (string-to-number (or lz "0"))))
             (day-of-week (nth 6
                               (decode-time (apply #'encode-time
                                                   (mapcar #'string-to-number
                                                           (list "0" "0" "0"
                                                                 d m y)))))))
        (list day late type lazy day-of-week description)))))

(defun howm-todo-priority (item)
  (let* ((p (howm-todo-parse item))
         (late (car p))
         (type (cadr p))
         (lazy (cl-caddr p))
         (f (or (cdr (assoc type howm-todo-priority-func))
                #'howm-todo-priority-unknown)))
    (funcall f late lazy item)))

(defun howm-todo-priority-ext (item)
  (cons (howm-todo-priority item) (howm-view-item-summary item)))
(defun howm-todo-priority-ext-gt (e1 e2)
  "Compare two results E1 and E2 of `howm-todo-priority-ext'.
Return true if E1 has higher priority than E2."
  (cond ((> (car e1) (car e2)) t)
        ((< (car e1) (car e2)) nil)
        (t (string< (cdr e1) (cdr e2)))))

(defun howm-todo-relative-late (late laziness default-laziness)
  (/ late (float (or laziness default-laziness))))

(defun howm-todo-priority-normal (late lz item)
  (let ((r (howm-todo-relative-late late lz
                                    howm-todo-priority-normal-laziness)))
    (cond ((< r 0) (+ r howm-todo-priority-normal-bottom))
          (t (- r)))))

(defun howm-todo-priority-todo (late lz item)
  (let ((r (howm-todo-relative-late late lz
                                    howm-todo-priority-todo-laziness))
        (c (- howm-todo-priority-todo-init)))
    (cond ((< r 0) (+ r howm-todo-priority-todo-bottom))
          (t (* c (- r 1))))))

(defun howm-todo-priority-defer (late lz item)
  (let* ((r (howm-todo-relative-late late lz
                                     howm-todo-priority-defer-laziness))
         (p howm-todo-priority-defer-peak)
         (c (- p howm-todo-priority-defer-init)))
    (let ((v (* 2 (abs (- (mod r 1) 0.5)))))
      (cond ((< r 0) (+ r howm-todo-priority-defer-bottom))
            (t (- p (* c v)))))))

;; ;; Clean me.
;; (defvar howm-todo-schedule-days nil)
;; (defvar howm-todo-schedule-days-before nil)
;; (defmacro howm-with-schedule-days (days days-before &rest body)
;;   `(let ((howm-todo-schedule-days ,days)
;;          (howm-todo-schedule-days-before ,days-before))
;;     ,@body))
;; (put 'howm-with-schedule-days 'lisp-indent-hook 2)
;; (defun howm-todo-priority-schedule (late lz item)
;;   (setq lz (or lz howm-todo-priority-schedule-laziness))
;;   (cond ((< late (- howm-todo-schedule-days))
;;          (+ late howm-todo-priority-schedule-bottom))
;;         ((< late (+ lz howm-todo-schedule-days-before))
;;          (+ late howm-todo-priority-schedule-top))
;;         (t
;;          (+ late howm-todo-priority-schedule-bottom))))

(defun howm-todo-priority-deadline (late lz item)
  (if howm-reminder-schedule-interval
      (howm-todo-priority-deadline-1 late lz item)
    (howm-todo-priority-deadline-2 late lz item)))

(defun howm-todo-priority-deadline-1 (late lz item)
  (let ((r (howm-todo-relative-late late lz
                                    howm-todo-priority-deadline-laziness))
        (c (- howm-todo-priority-deadline-init))
        (d (- (howm-reminder-schedule-interval-to)))
        (top howm-todo-priority-deadline-top)
        (bot howm-todo-priority-deadline-bottom))
    ;; I dare to use late in the first case below so that
    ;; deadline behaves like schedule after its deadline date.
    (cond ((< d late) (+ top late))
          ((< r -1) (+ bot r))
          (t (* c r)))))

(defun howm-todo-priority-deadline-2 (late lz item)
  "This function may be obsolete in future.
`howm-todo-priority-deadline-1' will be used instead."
  (let ((r (howm-todo-relative-late late lz
                                    howm-todo-priority-deadline-laziness))
        (c (- howm-todo-priority-deadline-init)))
    (cond ((> r 0) (+ r howm-todo-priority-deadline-top))
          ((< r -1) (+ r howm-todo-priority-deadline-bottom))
          (t (* c r)))))

(defun howm-todo-priority-schedule (late lz item)
  (if howm-reminder-schedule-interval
      (howm-todo-priority-schedule-1 late lz item)
    (howm-todo-priority-schedule-2 late lz item)))

(defun howm-todo-priority-schedule-1 (late lz item)
  (let ((lazy (or lz howm-todo-priority-schedule-laziness))
        (from (howm-reminder-schedule-interval-from))
        (to   (howm-reminder-schedule-interval-to))
        (top  howm-todo-priority-schedule-top)
        (bot  howm-todo-priority-schedule-bottom))
    (cond ((< late (- to))        (+ bot late))
          ((< late (+ from lazy)) (+ top late))
          (t (+ bot late)))))

(defun howm-todo-priority-schedule-2 (late lz item)
  "This function may be obsolete in future.
`howm-todo-priority-schedule-1' will be used instead."
  (let ((r (howm-todo-relative-late late lz
                                    howm-todo-priority-schedule-laziness)))
    (cond ((> r 0) (+ r howm-todo-priority-schedule-bottom))
          (t r))))

(defun howm-todo-priority-done (late lz item)
  (+ late howm-todo-priority-done-bottom))

(defun howm-todo-priority-unknown (late lz item)
  (+ late howm-todo-priority-unknown-top))

(defun howm-encode-day (&optional d m y)
  "Convert date Y-M-D to a float number, days from the reference date.
When D is omitted, the current time is encoded.
When D is t, the beginning of today is encoded."
  (let* ((e (apply #'encode-time (cond ((eq d t)
                                        (let ((now (howm-decode-time)))
                                          (append '(0 0 0) (cl-cdddr now))))
                                       (d
                                        (mapcar #'string-to-number
                                                (list "0" "0" "0" d m y)))
                                       (t
                                        (howm-decode-time)))))
         (hi (car e))
         (low (cadr e))
         (daysec (* 60 60 24.0)))
    (+ (* hi (/ 65536 daysec)) (/ low daysec))))

(defun howm-congrats ()
  (setq howm-congrats-count (1+ howm-congrats-count))
  (let* ((n (length howm-congrats-format))
         (r (random n)))
    (message (nth r howm-congrats-format) howm-congrats-count)
    (when howm-congrats-command
      (howm-congrats-run howm-congrats-command))
    (run-hooks 'howm-congrats-hook)))
(defun howm-congrats-run (com-arg-list)
  (let* ((name "howm-congrats")
         (command (car com-arg-list))
         (args (cdr com-arg-list))
         (prev (get-process name)))
    (when prev
      (delete-process prev))
    (apply #'start-process-shell-command `(,name nil ,command ,@args))))

(defun howm-action-lock-reminder-done-rule ()
  (list (howm-reminder-regexp howm-reminder-types)
        `(lambda (&optional arg)
           (let ((command (if arg
                              nil
                            howm-action-lock-reminder-done-default)))
             (howm-action-lock-done command)))
        howm-reminder-regexp-command-pos))

(defun howm-reminder-search-path ()
  (howm-search-path t))

(defun howm-reminder-search-path-folder ()
  (howm-search-path-folder t))

;;; direct manipulation of items from todo list

;; I'm sorry for dirty procedure here.
;; If we use naive howm-date-regexp, it matches to file name "2004-05-11.txt"
;; in summary mode.
(defun howm-action-lock-reminder-forward-rules (&optional summary-mode-p)
  (let* ((action-maker (lambda (pos)
                         `(lambda (&optional dummy)
                            (howm-action-lock-forward (match-beginning ,pos)))))
         (reminder-rule (list (howm-reminder-regexp howm-reminder-types)
                              (funcall action-maker 0)
                              howm-reminder-regexp-command-pos))
         (summary-date-reg (format ".*%s.*\\(%s\\)"
                                   (regexp-quote howm-view-summary-sep)
                                   howm-date-regexp))
         (summary-date-reg-pos 1)
         (summary-date-rule (list summary-date-reg
                                  (funcall action-maker summary-date-reg-pos)
                                  summary-date-reg-pos))
         (menu-date-rule (list howm-date-regexp
                               (funcall action-maker 0)))
         (date-rule (if summary-mode-p
                        summary-date-rule
                      menu-date-rule)))
    (list reminder-rule date-rule)))

(defvar howm-action-lock-forward-wconf nil
  "for internal use")
(defun howm-action-lock-forward-escape ()
  (setq howm-action-lock-forward-wconf
        (current-window-configuration)))
(defmacro howm-action-lock-forward-block (&rest body)
  (declare (indent 0))
  `(prog2
       (setq howm-action-lock-forward-wconf nil)
       (progn
         ,@body)
     (when howm-action-lock-forward-wconf
       (set-window-configuration howm-action-lock-forward-wconf))))

(defun howm-action-lock-forward (form-pos)
  (howm-action-lock-forward-block
    (let* ((cursor-pos (point))
           (form-reg (howm-line-tail-regexp form-pos))
           (cursor-reg (howm-line-tail-regexp cursor-pos)))
      (let* ((mt (buffer-modified-tick))
             (original-tail (buffer-substring form-pos (line-end-position)))
             (modified-tail (howm-action-lock-forward-invoke form-reg
                                                             cursor-reg))
             (untouched-p (= mt (buffer-modified-tick))))
        ;; Current-buffer may be already updated according to
        ;; howm-menu-refresh-after-save because save-buffer in
        ;; howm-action-lock-forward-invoke can run howm-after-save-hook.
        ;; We have to exclude such cases.
        (when (and untouched-p
                   (not (string= original-tail modified-tail)))
          (let ((buffer-read-only nil))
            (howm-menu-list-getput-item original-tail modified-tail)
            (delete-region form-pos (line-end-position))
            (insert modified-tail)))
        (goto-char cursor-pos)
        (howm-action-lock-forward-update)))))

(defun howm-line-tail-regexp (pos)
  (concat (regexp-quote (buffer-substring-no-properties pos
                                                        (line-end-position)))
          "$"))

(defun howm-action-lock-forward-invoke (form-reg cursor-reg)
  (howm-modify-in-background (lambda (&rest dummy)
                               ;; open the target file
                               ;; and go to the corresponding line
                               (howm-action-lock-forward-open))
                             (lambda (form-reg cursor-reg)
                               (howm-action-lock-forward-modify-current-line
                                form-reg cursor-reg))
                             howm-action-lock-forward-save-buffer
                             howm-action-lock-forward-kill-buffer
                             form-reg
                             cursor-reg))

(defun howm-modify-in-background (opener modifier save-p kill-p &rest args)
  (save-excursion
    (save-window-excursion
      (let ((original-buffers (buffer-list)))
        (apply opener args)
        ;; We are in the target buffer now.
        (let ((initially-modified-p (buffer-modified-p)))
          (prog1
              (apply modifier args)
            (when (and save-p
                       (not initially-modified-p)
                       (buffer-modified-p))
              (save-buffer))
            (when (and kill-p
                       (not (buffer-modified-p))
                       (not (member (current-buffer) original-buffers)))
              (kill-buffer (current-buffer)))))))))

(defun howm-action-lock-forward-modify-current-line (form-reg cursor-reg)
  (howm-modify-form #'action-lock-invoke form-reg cursor-reg))

(defun howm-modify-form (proc form-reg cursor-reg &rest args)
  (cl-labels
      ((f-cursor ()
                 (beginning-of-line)
                 (re-search-forward cursor-reg
                                    (line-end-position
                                     (+ 1 howm-action-lock-forward-fuzziness))
                                    t))
       (b-cursor ()
                 (end-of-line)
                 (re-search-backward cursor-reg
                                     (line-beginning-position
                                      (- 1 howm-action-lock-forward-fuzziness))
                                     t))
       (b-form ()
               (end-of-line)
               (re-search-backward form-reg (line-beginning-position) t)))
    (or (save-excursion (and (f-cursor) (b-form)))
        (save-excursion (and (b-cursor) (b-form)))
        (error "Can't find corresponding line.")))
  (goto-char (match-beginning 0))
  ;; Now we are at the beginning of the form.
  ;; Remember this position to report the modified tail.
  (save-excursion
    (when (not (re-search-forward cursor-reg (line-end-position) t))
      (error "Can't find corresponding string."))
    (goto-char (match-beginning 0))
    ;; update display. I don't understand why this is needed.
    ;; Without this, cursor is placed at the end of buffer if I delete many
    ;; lines before the form position in the below setting (GNU Emacs 21.4.1).
    ;;   (setq howm-menu-refresh-after-save nil)
    ;;   (setq howm-menu-expiry-hours 3)
    ;;   (setq howm-action-lock-forward-fuzziness 20000)
    ;; Sigh...
    (switch-to-buffer (current-buffer) t)
    ;; Now we are at the corresponding position.
    ;; Let's call proc to modify the form!
    (undo-boundary)
    (apply proc args))
  ;; We are back to the beginning of the form.
  ;; Report the modified tail.
  (buffer-substring-no-properties (point) (line-end-position)))

(defun howm-action-lock-forward-open ()
  (cond ((eq major-mode 'howm-menu-mode)
         (progn
           (howm-menu-list-action)
           (when (eq major-mode 'howm-view-summary-mode)
             (error "Several candidates."))))
        ((eq major-mode 'howm-view-summary-mode)
         (howm-view-summary-open))
        (t
         (error "Not supported on this buffer."))))

(defun howm-action-lock-forward-update ()
  (cond ((eq major-mode 'howm-menu-mode)
         nil) ; do nothing
        ((eq major-mode 'howm-view-summary-mode)
         (howm-view-summary-check t))
        (t
         (error "Not supported on this buffer."))))

;;; extend deadlines (experimental)

(put 'howm-extend-deadlines 'disabled t)
(defun howm-extend-deadlines (days)
  "Extend all overdue deadlines for DAYS from today."
  (interactive "nHow many days? ")
  (let ((hit (cl-remove-if (lambda (item)
                             (< (cadr (howm-reminder-parse item)) 0))
                           (howm-reminder-search "!"))))
    (mapc (lambda (item)
            (howm-modify-in-background (lambda (item dummy)
                                         (howm-view-open-item item))
                                       #'howm-extend-deadline-here
                                       nil nil item days))
          hit)
    (howm-menu-refresh-background)
    (message "Extended %s deadline(s)." (length hit))))

(defun howm-extend-deadline-here (item days)
  (apply (lambda (form-reg cursor-reg) ;; use apply for destructuring-bind
           (howm-modify-form #'howm-extend-deadline-doit
                             form-reg cursor-reg days))
         (let ((summary (howm-item-summary item)))
           (string-match (howm-reminder-regexp ".") summary)
           (mapcar (lambda (p)
                     (concat (regexp-quote
                              (substring summary (match-beginning p)))
                             "$"))
                   (list howm-reminder-regexp-date-pos
                         howm-reminder-regexp-year-pos)))))

(defun howm-extend-deadline-doit (days)
  (or (looking-at howm-date-regexp)
      (re-search-backward howm-date-regexp (line-beginning-position) t)
      (error "Can't find corresponding date form."))
  (howm-datestr-replace
   (howm-datestr-shift (howm-time-to-datestr) 0 0 days)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize

(defun howm-define-reminder (letter priority-func face schedule todo
                                    &optional reminder)
  "Define reminder type LETTER whose priority is determined by PRIORITY-FUNC.
It appears with FACE in schedule list when SCHEDULE is non-nil, and in
todo list when TODO is non-nil.  It also appears in menu if SCHEDULE
or TODO is t."
  (add-to-list 'howm-todo-priority-func
               (cons letter priority-func))
  (add-to-list 'howm-reminder-font-lock-keywords
               `(,(howm-reminder-regexp (format "[%s]" letter))
                 (0 ,face prepend)))
  (let* ((schedule-menu (eq schedule t))
         (todo-menu (eq todo t))
         (reminder-menu (or schedule-menu todo-menu)))
    ;; Don't modify howm-reminder-marks.
    ;; Otherwise, defcustom will be confused for howm-reminder-menu-types, etc.
    (cl-mapcar (lambda (var flag)
                       (howm-modify-reminder-types var letter flag))
                     '(howm-reminder-types
                       howm-schedule-types howm-todo-types
                       howm-schedule-menu-types howm-todo-menu-types
                       howm-reminder-menu-types)
                     (list t schedule todo
                           schedule-menu todo-menu reminder-menu))))

(defun howm-modify-reminder-types (var letter flag)
  "Modify variable VAR whose value is \"[...]\".
Example:
 (setq foo \"[abc]\")
 (howm-modify-reminder-types 'foo \"d\" t)  foo ==> \"[abcd]\"
 (howm-modify-reminder-types 'foo \"b\" nil)  foo ==> \"[acd]\"
"
  (let ((val (symbol-value var)))
    (when (not (string-match "^\\[\\(.*\\)\\]$" val))
      (error "Wrong format - %s: %s" var val))
    (let* ((old (match-string-no-properties 1 val))
           (removed (remove (string-to-char letter) old))
           (new (if flag
                    ;; This order is important when val is "[-+~!.]".
                    (concat removed letter)
                  removed)))
      (set var (format "[%s]" new)))))

;; (example)
;; If you write like below in your memo, it will appear
;; under today's schedule in reminder list.
;; The date "2004-11-01" is dummy and "0" means the position "today - 0".
;;   [2004-11-01]_0 ========================
;; (defun howm-todo-priority-separator (late lazy item)
;;   (- howm-huge (or lazy 0) -1))
;; (defface howm-reminder-separator-face
;;   ;; invisible :p
;;   '((((class color) (background light)) (:foreground "white"))
;;     (((class color) (background dark)) (:foreground "black"))
;;     (t ()))
;;   "Face for `howm-list-reminder'. This is obsolete and will be removed in future."
;;   :group 'howm-faces)
;; (defvar howm-reminder-separator-face 'howm-reminder-separator-face)
;; (defvar howm-reminder-separator-type "_")
;; (howm-define-reminder howm-reminder-separator-type
;;                       #'howm-todo-priority-separator
;;                       'howm-reminder-separator-face t t)

;;; howm-reminder.el ends here
