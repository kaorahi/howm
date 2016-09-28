;;; howm-common.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-common.el,v 1.90 2012-12-29 08:57:18 hira Exp $
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

(require 'howm-vars)

(defun howm-cl-remove-duplicates* (&rest args)
  ":from-end version of `remove-duplicates'"
  (apply #'cl-remove-duplicates `(,@args :from-end t)))

(defun howm-sort (evaluator comparer obj-list)
  (let* ((orig (mapcar (lambda (obj) (cons (funcall evaluator obj) obj))
                       obj-list))
         (sorted (sort orig (lambda (x y)
                              (funcall comparer (car x) (car y))))))
    (mapcar #'cdr sorted)))

(defun howm-subdirectory-p (dir target &optional strict)
  "For the directory DIR, check whether TARGET is under it.
When TARGET and DIR are same, (not STRICT) is returned."
  (and (stringp dir)
       ;; avoid unnecessary password prompting
       ;; (I'm not sure about the return value of file-remote-p.)
       (eq (not (file-remote-p dir)) (not (file-remote-p target)))
       (progn
         (setq target (howm-normalize-file-name target))
         (setq dir (howm-normalize-file-name dir))
         (if (string= target dir)
             (not strict)
           (and (string-match (regexp-quote dir) target)
                (= 0 (match-beginning 0)))))))

(defun howm-normalize-file-name (filename)
  (let* ((r (file-remote-p filename))
         (f (if r
                (concat r filename)
              (file-truename (expand-file-name filename)))))
    ;; for meadow
    (if (string-match "^[A-Z]:" f)
        (let ((drive (substring f 0 1))
              (rest (substring f 1)))
          (concat (downcase drive) rest))
      f)))

(defvar howm-abbreviate-file-name t)
(defun howm-abbreviate-file-name (f)
  (if (howm-abbreviate-file-name-p)
      (abbreviate-file-name f)
    f))
;; for compatibility (incomplete)
(howm-dont-warn-free-variable howm-template-file-abbrev)
(howm-dont-warn-free-variable howm-view-header-abbrev)
(defun howm-abbreviate-file-name-p ()
  (cond ((boundp 'howm-template-file-abbrev) howm-template-file-abbrev)
        ((boundp 'howm-view-header-abbrev) howm-view-header-abbrev)
        (t howm-abbreviate-file-name)))
;; (defun howm-abbreviate-file-name-p () howm-abbreviate-file-name)

(defun howm-expand-file-names (file-list)
  (mapcar (lambda (f) (directory-file-name (expand-file-name f)))
          file-list))

(defun howm-insert-file-contents (file)
  (insert-file-contents file nil nil howm-view-contents-limit))

;;; for XEmacs fallback
;; (if (not (fboundp 'font-lock-fontify-block))
;;     (defalias 'font-lock-fontify-block 'font-lock-fontify-buffer))
;;; If you use APEL, you can replace a below block with (require 'poe).
(if (not (fboundp 'line-beginning-position))
    (defalias 'line-beginning-position 'point-at-bol))
(if (not (fboundp 'line-end-position))
    (defalias 'line-end-position 'point-at-eol))
;;; Imported from APEL 10.6
(if (not (fboundp 'match-string-no-properties))
    ;; Emacs 20.3 and later: (match-string-no-properties NUM &optional STRING)
    (defun match-string-no-properties (num &optional string)
      "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
      (if (match-beginning num)
          (if string
              (let ((result
                     (substring string (match-beginning num) (match-end num))))
                (set-text-properties 0 (length result) nil result)
                result)
            (buffer-substring-no-properties (match-beginning num)
                                            (match-end num))))))

(defmacro howm-message-time (name &rest body)
  (declare (indent 1))
  `(let ((howm-message-time-0 (current-time)))
     (prog1
         (progn
           ,@body)
       (when howm-message-time
         (message "%s (%s: %.2f sec)"
                  (or (current-message) "")
                  ,name
                  (howm-time-difference-second (current-time)
                                               howm-message-time-0))))))
;; (defun howm-message-time-from (ti0)
;;   (when howm-message-time
;;     (let ((ti1 (current-time)))
;;       (message "took %.2f sec."
;;                (howm-time-difference-second ti1 ti0)))))
(defun howm-time-difference-second (ti1 ti0)
  (let ((h (- (car ti1) (car ti0)))
        (l (- (cadr ti1) (cadr ti0)))
        (m (- (or (cl-caddr ti1) 0) (or (cl-caddr ti0) 0)))
        )
    (+ (* h 65536) l
       (* m 1e-6)
       )))

(defun howm-xor (a b)
  (if a (not b) b))
(defun howm-buffer-empty-p (&optional buf)
  ;; emacs20 doesn't have (buffer-size buf)
  (with-current-buffer (or buf (current-buffer))
    (= (buffer-size) 0)))
(defun howm-point-beginning-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))
(defun howm-point-end-of-line ()
  (save-excursion
    (end-of-line)
    (point)))

(defun howm-inhibit-warning-in-compilation (&rest dummy)
  (error "This is dummy function to inhibit warning in compilation."))

;; (defvar howm-reminder-quick-check-key ";")

(defun howm-get-value (z)
  (if (functionp z)
      (funcall z)
    z))

;; (howm-map-with-index #'cons '(a b c)) ==> ((a . 0) (b . 1) (c . 2))
(defun howm-map-with-index (f seq)
  "Map with index. For example, 
(howm-map-with-index #'cons '(a b c)) returns ((a . 0) (b . 1) (c . 2))."
  (let ((howm-map-with-index-count -1))
    (mapcar (lambda (x)
              (setq howm-map-with-index-count (1+ howm-map-with-index-count))
              (apply f (list x howm-map-with-index-count)))
            seq)))

(defun howm-capital-p (str)
  "Return nil iff STR has no capital letter."
  (let ((case-fold-search nil)) 
    (string-match "[A-Z]" str)))

(defun howm-single-element-p (a)
  (and a (null (cdr a))))

(defun howm-read-string (prompt &optional immediate-chars continued-chars
                                pass-through pass-ret-through)
  "Read a string from minibuffer with some extensions to `read-string'.
(1) When the first input char is in IMMEDIATE-CHARS string,
this function returns the char as one letter string
without waiting for rest input and RET key.
(2) Otherwise, when the char is in CONTINUED-CHARS,
this function is equivalent to read-string.
(3) When the char is not in either IMMEDIATE-CHARS or CONTINUED-CHARS,
the behavior depends on PASS-THROUGH.
If PASS-THROUGH is nil, error is raised.
If PASS-THROUGH is non-nil, the input event is unread and nil is returned.
(4) Note that RET key at the first char is the special case.
If PASS-RET-THROUGH is nil, empty string is returned.
If PASS-RET-THROUGH is non-nil, RET is unread and nil is returned.
"
  (if (null immediate-chars)
      (read-string prompt)
    (save-window-excursion
      (message "%s" prompt)
      (select-window (minibuffer-window))
      (let* ((ev (howm-read-event))
             (ch (howm-event-to-character ev)))
        (cond ((howm-characterp ch)
               (howm-read-string-sub ev ch
                                     prompt immediate-chars continued-chars
                                     pass-through pass-ret-through))
              (t
               (howm-read-string-fail ev pass-through pass-ret-through)))))))

(defun howm-read-string-sub (event char
                             prompt immediate-chars continued-chars
                             pass-through pass-ret-through)
  (let* ((ichars (string-to-list (or immediate-chars "")))
         (cchars (string-to-list (or continued-chars "")))
         (first-char char)
         (first-str (char-to-string first-char)))
    (cond ((member first-char ichars)
           first-str)
          ((member first-char cchars)
           (read-string prompt (cons first-str (1+ (length first-str)))))
          (t
           (howm-read-string-fail event pass-through pass-ret-through)))))

(defun howm-read-string-fail (event pass-through pass-ret-through)
  (cond ((and (howm-ret-key-event-p event) (not pass-ret-through))
         "")
        (pass-through
         (progn
           (howm-unread-event event)
           nil))
        (t
         (error "Invalid input."))))

(defun howm-unread-event (event)
  (setq unread-command-events
        (cons event unread-command-events)))

(defun howm-first-n (seq n)
  "Return the subsequence of SEQ from start to N-th item.
(howm-first-n '(a b c d e) 3) ==> (a b c)
(howm-first-n '(a b c d e) 10) ==> (a b c d e)
"
  ;; GNU emacs: (subseq '(a b c d e) 0 7) ==> (a b c d e nil nil)
  ;; xemacs:    (subseq '(a b c d e) 0 7) ==> Args out of range
  (if (<= (length seq) n)
      seq
    (cl-subseq seq 0 n)))

;; check
(let ((seq '(a b c d e))
      (qa '((0 . nil)
            (4 . (a b c d))
            (5 . (a b c d e))
            (7 . (a b c d e)))))
  (mapc (lambda (z)
          (let ((ans (howm-first-n seq (car z))))
            (when (not (equal ans (cdr z)))
              (error "howm-first-n is wrong: %s for %s" ans z))))
        qa))

(defun howm-replace-region (beg end val)
  (delete-region beg end)
  ;; `format' in xemacs doesn't keep text properties.
  (insert (if (stringp val)
              val
            (format "%s" val))))

(defmacro howm-edit-read-only-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (buffer-disable-undo)
     (setq buffer-read-only nil)
     ,@body
     (set-buffer-modified-p nil)
     (setq buffer-read-only t)))

(defmacro howm-rewrite-read-only-buffer (&rest body)
  (declare (indent 0))
  `(howm-edit-read-only-buffer
     (erase-buffer)
     ,@body))

;; (put 'howm-rewrite-read-only-buffer 'lisp-indent-hook 0)
;; (defmacro howm-rewrite-read-only-buffer (&rest body)
;;   `(progn
;;      (setq buffer-read-only nil)
;;      (erase-buffer)
;;      ,@body
;;      (set-buffer-modified-p nil)
;;      (setq buffer-read-only t)))

(defun howm-get-buffer-for-file (file &optional buffer-name)
  "Get buffer for FILE, and rename buffer if BUFFER-NAME is given."
  ;; This may cause "File XXX no longer exists!" message if the file
  ;; is deleted and the corresponding buffer still exists.
  (let ((buf (find-file-noselect file)))
    (when buffer-name
      (with-current-buffer buf
        (rename-buffer buffer-name)))
    buf))

(defun howm-basic-save-buffer ()
  "Silent version of `basic-save-buffer' without \"Wrote ...\" message."
  (let ((original-write-region (symbol-function 'write-region)))
    ;; make silent `write-region', which doesn't say "Wrote ...".
    ;; I borrowed the idea from Okuyama's auto-save-buffers. thx.
    ;; http://homepage3.nifty.com/oatu/emacs/misc.html
    (cl-flet ((write-region (start end filename
                                &optional append visit lockname must)
                         (funcall original-write-region
                                  start end filename append
                                  'dont-say-wrote-foobar
                                  lockname must)))
      (basic-save-buffer)))
  ;; As a side effect, basic-save-buffer does not update buffer-modified-p.
  (set-visited-file-modtime)
  (set-buffer-modified-p nil))

(defvar howm-log-buffer-name-format " *howm-log:%s*")
(defun howm-write-log (message fmt file &optional limit remove-fmt)
  "Write MESSAGE with format FMT to the top of FILE.
FORMAT must have two %s; the formar is time and the latter is message.
When LIMIT is non-nil, only that number of recent messages are recorded.
When REMOVE-FMT is non-nil, it is used to generate regular expression
to remove matched lines. REMOVE-FMT must have one %s."
  (save-excursion
    (let ((buffer-name (format howm-log-buffer-name-format
                               (file-name-nondirectory file))))
      (with-current-buffer (howm-get-buffer-for-file file buffer-name)
        (goto-char (point-min))
        (when remove-fmt
          (save-excursion
            (flush-lines (format remove-fmt (regexp-quote message)))))
        (insert (format fmt
                        (format-time-string howm-dtime-format (current-time))
                        message)
                "\n")
        (when limit
          ;; Since I don't understand `selective-display' in goto-line,
          ;; I avoid it.
          (goto-char (point-min))
          (when (= (forward-line limit) 0) ;; buffer lines > LIMIT
            (delete-region (point) (point-max))))
        (howm-basic-save-buffer)))))

(defun howm-get-symbol (soft &rest args)
  "Return the canonical symbol for a specified name.
If SOFT is non-nil, return nil when the corresponding symbol does not exist.
Name of returned symbol is concatenation of ARGS.
Both strings and symbols are acceptable in ARGS."
  (funcall (if soft #'intern-soft #'intern)
           (mapconcat (lambda (s)
                        (cond ((sequencep s) s)
                              ((symbolp s) (symbol-name s))
                              (t (error "Not supported: %S" s))))
                      args
                      "")))

;; snap://Info-mode/elisp#Killing Buffers
(defun howm-buffer-killed-p (buffer)
  "Return t if BUFFER is killed."
  (not (buffer-name buffer)))

(defun howm-classify (classifier lis &optional reverse)
  "Classify elements in given list.
CLASSIFIER is criterion of classification for list LIS.
If REVERSE is non-nil, order of elements are reversed (faster).
For example,
  (howm-classify (lambda (s) (substring s 0 1)) '(\"aaa\" \"abc\" \"xyz\"))
returns ((\"a\" \"aaa\" \"abc\") (\"x\" \"xyz\"))."
  (let ((ans nil))
    (mapc (lambda (x)
            (let* ((label (funcall classifier x))
                   (pair (assoc label ans)))
              (if (null pair)
                  (setq ans (cons (cons label (list x)) ans))
                (setcdr pair (cons x (cdr pair))))))
          lis)
    (if reverse
        ans
      (reverse (mapcar (lambda (pair) (cons (car pair) (reverse (cdr pair))))
                       ans)))))
;; (howm-classify (lambda (s) (substring s 0 1)) '("aaa" "abc" "xyz"))

(defun howm-message-nolog (str &rest args)
  (let ((message-log-max nil))
    (apply #'message `(,str ,@args))))

(defun howm-decode-time (&optional specified-time)
  "Decode SPECIFIED-TIME and remove DOW, DST, ZONE.
When we do something like (encode-time (decode-time)), we use this function
instead of the original `decode-time', so that we can force
current timezone rule uniformly to avoid inconsistency."
  (butlast (decode-time specified-time) 3))

(defmacro howm-with-need (&rest body)
  "Execute BODY where (need xxx) exits from this form if xxx is nil."
  (declare (indent 0))
  (let ((g (cl-gensym)))
    `(catch ',g
       (cl-labels ((need (x) (or x (throw ',g nil))))
         ,@body))))

(defun howm-goto-line (n)
  ;; see the document of `goto-line'
  (goto-char (point-min)) (forward-line (1- n)))

;; view-in-background

(defvar *howm-view-in-background* nil
  "for internal use.
Don't set this variable directly.
Use `howm-view-in-background' and `howm-view-in-background-p' instead.")

(defmacro howm-view-in-background (&rest body)
  "Obsolete. Do not use this any more."
  (declare (indent 0))
  `(let ((*howm-view-in-background* t))
     ,@body))

(defun howm-view-in-background-p ()
  *howm-view-in-background*)

;;; history of search

(defvar howm-history-format "> %s | %s")
(defvar howm-history-remove-format "| %s$")

(defun howm-write-history (message)
  (when (and howm-history-file
             (or (null howm-history-limit) (> howm-history-limit 0)))
    (howm-write-log message howm-history-format howm-history-file
                    howm-history-limit
                    (and howm-history-unique howm-history-remove-format))))

;;; call process

(defvar howm-call-process-last-command nil
  "List of arguments for last `howm-call-process'.
This variable exists only for debug. You can reproduce the last call
with the below code.
 (apply #'howm-call-process howm-call-process-last-command)")

(defmacro howm-with-coding-system (coding-system &rest body)
  "With CODING-SYSTEM, execute BODY.
examples:
 (howm-with-coding-system 'euc-jp-unix ...)  ;; for both read and write
 (howm-with-coding-system '(utf-8-unix . sjis-unix) ...)  ;; (read . write)
 (howm-with-coding-system nil ...)  ;; howm-process-coding-system is used."
  (declare (indent 1))
  (let ((g (cl-gensym))
        (cs (or coding-system 'howm-process-coding-system)))
    `(let* ((,g ,cs)
            (coding-system-for-read  (or (car-safe ,g) ,g))
            (coding-system-for-write (or (cdr-safe ,g) ,g)))
       ,@body)))

(defun howm-call-process (command args
                                  &optional expected-return-value stdin-string)
  (setq howm-call-process-last-command (list command args
                                             expected-return-value
                                             stdin-string))
  (with-temp-buffer
    (howm-with-coding-system nil
      (let ((r (howm-call-process-here command args stdin-string)))
        (when (and expected-return-value
                   (not (= expected-return-value r)))
          (error "Process returns %s instead of expected %s."
                 r expected-return-value))
        (howm-buffer-lines)))))

(defun howm-call-process-here (command args &optional stdin-string)
  (let* ((beg (point))
         (end (progn
                (insert (or stdin-string ""))
                (point)))
         (a `(,beg ,end ,command t (t nil) nil ,@args)))
    (howm-with-coding-system nil
      (apply #'call-process-region a))))

(defun howm-buffer-lines (&optional buf)
  (save-excursion
    (when buf
      (set-buffer buf))
    (split-string (buffer-substring (point-min) (point-max)) "\n")))

(defun howm-call-process* (command common-args rest-args &rest options)
  ;; (howm-call-process* "grep" '("pattern") '("001" ... "999"))
  ;; is expanded to concatenation of
  ;; (howm-call-process "grep" '("pattern" "001" ... "099"))
  ;; (howm-call-process "grep" '("pattern" "101" ... "199"))
  ;; ..., depending on howm-command-length-limit.
  (cl-labels ((div (a limit measure)
                ;; (div '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8) 20 #'identity)
                ;; ==> ((3 1 4 1 5) (9 2 6) (5 3 5) (8 9) (7 9 3) (2 3 8))
                ;; [create new group when sum >= 20]
                (let ((sum limit) ;; measure >= 0 is assumed.
                      (ans nil))
                  (mapc (lambda (x)
                          (let* ((w (funcall measure x))
                                 (new-sum (+ sum w)))
                            (if (< new-sum limit)
                                (setq sum new-sum
                                      ans (cons (cons x (car ans)) (cdr ans)))
                              (setq sum w
                                    ans (cons (list x) ans)))))
                        a)
                  (reverse (mapcar #'reverse ans)))))
    ;; XEmacs 21.4 lacks "string-bytes".
    (let* ((len (symbol-function
                 (cl-find-if #'fboundp '(string-bytes length))))
           (limit (apply #'- howm-command-length-limit
                         (mapcar len (cons command common-args))))
           (as (div rest-args limit len)))
      (cl-mapcan (lambda (args)
                        (apply #'howm-call-process
                               command (append common-args args) options))
                      as))))

;;; schedule-interval & reminder-setting (clean me)

(defvar howm-reminder-schedule-interval nil
  "For internal use. Do not setq this variable.
Use `howm-with-schedule-interval' instead.")
(defun howm-reminder-schedule-interval-from ()
  (car howm-reminder-schedule-interval))
(defun howm-reminder-schedule-interval-to ()
  (cdr howm-reminder-schedule-interval))
(defmacro howm-with-schedule-interval (interval &rest body)
  "Set the interval of visible schedule items in reminder list on menu.
INTERVAL is a form like (-1 2), which means 'from yesterday to the day
after tomorrow'. BODY is evaluated under this setting;
`howm-reminder-schedule-interval-from' returns -1 and
`howm-reminder-schedule-interval-to' returns 2."
  (declare (indent 1))
  `(let ((howm-reminder-schedule-interval ,(cons 'cons interval)))
    ,@body))

(defmacro howm-with-reminder-setting  (&rest body)
  (declare (indent 0))
  `(howm-with-schedule-interval
       (howm-menu-schedule-days-before howm-menu-schedule-days)
     (let ((howm-todo-menu-types howm-reminder-menu-types))  ;; dirty!
       ,@body)))

;;; xemacs

;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=switch%20config
;; http://pc2.2ch.net/test/read.cgi/software/1056601345/510
(eval-and-compile
  (defun howm-xemacsp ()
    (featurep 'xemacs)))

(defmacro howm-defun-xemacs (func args emacs-f xemacs-f)
  (declare (indent 'defun))
  `(defun ,func ,args
     ,(if (howm-xemacsp)
          xemacs-f
        emacs-f)))

(howm-defun-xemacs howm-deactivate-mark ()
  (deactivate-mark)
  (zmacs-deactivate-region))

(howm-defun-xemacs howm-read-event ()
  (read-event)
  ;; We have to skip #<magic-event Expose> when howm-action-lock-date is
  ;; called (RET is hit on date format like [2005-10-15]) in menu buffer.
  ;; 
  ;; Though (make-event 'key-press `(key ,(read-char))) looks to be a simpler
  ;; solution, it causes error when RET RET is hit in the above situation.
  ;; I don't have enough time to examine it now.
  (let ((ev (next-event)))
    (if (key-press-event-p ev)
        ev
      (howm-read-event))))

;; Though this function is used only once, I dare to define it
;; with howm-defun-xemacs macro in order to avoid warning
;; in byte-compilation on GNU emacs. I don't have enough energy now.
(howm-defun-xemacs howm-ret-key-event ()
  (event-convert-list '(return))
  (make-event 'key-press '(key return)))

(defvar howm-ret-key-event (howm-ret-key-event))

(defun howm-ret-key-event-p (event)
  (or (equal event 13) (equal event howm-ret-key-event)))

(howm-defun-xemacs howm-event-to-character (event)
  (and (howm-characterp event) event)
  (event-to-character event))

(howm-defun-xemacs howm-characterp (x)
  (numberp x)
  (characterp x))

(defvar howm-infinity
  (if (howm-xemacsp)
      1.0e+100  ;; xemacs info on my machine is broken :(
    1.0e+INF))

;;; cl

;; (defmacro howm-define-maybe (fname fargs &rest fbody)
;;   (when (not (fboundp fname))
;;     `(defun ,fname ,fargs
;;        ,@fbody)))

;; (howm-define-maybe caddr (x)
;;                    (car (cdr (cdr x))))

;; (howm-define-maybe second (x)
;;                    (cadr x))

;; (howm-define-maybe third (x)
;;                    (caddr x))

;;; regexp

;; (defun howm-regexp-opt (strings &optional paren)
;;   "Imitation of `regexp-opt' without optimization.
;; This is used for large set of strings when `regexp-opt' causes an error
;; \"Variable binding depth exceeds max-specpdl-size\"."
;;   (let* ((open (if paren "\\(" ""))
;;          (close (if paren "\\)" ""))
;;          (re (concat open (mapconcat 'regexp-quote strings "\\|") close)))
;;     (if (eq paren 'words)
;;         (concat "\\<" re "\\>")
;;       re)))

;;; 

(provide 'howm-common)

;;; howm-common.el ends here
