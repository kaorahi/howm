;;; howm-misc.el --- Wiki-like note-taking tool
;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-misc.el,v 1.96 2012-12-29 08:57:18 hira Exp $
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

(provide 'howm-misc)
(require 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc.

(defun howm-version ()
  (interactive)
  (message "howm-%s" howm-version))

(defun howm-keyword-file ()
  ;; create .howm-keys
  (when (not (file-exists-p howm-keyword-file))
    (save-excursion
      (find-file howm-keyword-file)
      (when howm-menu-top
        (goto-char (point-min))
        (insert howm-menu-top "\n"))
      (set-buffer-modified-p t)
      (save-buffer)
      (kill-buffer nil)
      (message "Generating %s ..." howm-keyword-file)
      (howm-keyword-add-items (howm-all-items))
      (message "Done.")))
  howm-keyword-file)

(add-hook 'howm-view-open-hook 'howm-set-mode)
(defun howm-set-mode ()
  (when (howm-set-mode-p)
    (howm-set-configuration-for-major-mode major-mode)
    (howm-mode 1)))

(defun howm-set-mode-p (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let ((hdir (car (howm-search-path))))
      (and (buffer-file-name)
           (howm-folder-territory-p hdir (buffer-file-name))))))

(defvar howm-configuration-for-major-mode nil)
;; ;; sample
;; (setq howm-configuration-for-major-mode
;;   '(
;;     ;; fix me
;;     (emacs-lisp-mode
;;      . (
;;         (howm-keyword-format . "(def[a-z*]+ +%s[ \t\r\n]")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "(\\(def[a-z]+\\) +'?\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)") ;; ' for (defalias 'xxx ...)
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^(.*$")
;; ;;         (howm-view-title-regexp . "^[^; \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^; \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (scheme-mode
;;      . (
;;         (howm-keyword-format . "(def[a-z]+ +[(]?%s[) \t\r\n]")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "(\\(def[a-z]+\\) +[(]?\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^[^; \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^; \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (ruby-mode
;;      . (
;;         (howm-keyword-format . "\\(def\\|class\\) +%s\\b")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "\\(def\\|class\\) +\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^[^# \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^# \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (yatex-mode
;;      . (
;;         (howm-keyword-format . "\\\\label%s")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "\\(\\\\label\\)\\({[^}\r\n]+}\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "\\\\\\(\\(sub\\)*section\\|chapter\\|part\\|begin\\)")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "\\\\((sub)*section|chapter|part|begin)")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     ))

(defun howm-set-configuration-for-file-name (f)
  (let ((mode (howm-auto-mode f)))
    (howm-set-configuration-for-major-mode mode)))

(defun howm-set-configuration-for-major-mode (mode)
  (let ((a (cdr (assoc mode howm-configuration-for-major-mode))))
    (when a  ;; I know this is redundant.
      (mapc (lambda (sv)
              (let ((symbol (car sv))
                    (value (cdr sv)))
                (set (make-local-variable symbol) value)))
            a))))

(defmacro howm-if-unbound (var &rest alt-body)
  `(if (boundp ',var) ,var ,@alt-body))

;; copied and modified from set-auto-mode in /usr/share/emacs/21.2/lisp/files.el
;; (I don't want to set the mode actually. Sigh...)
(howm-dont-warn-free-variable auto-mode-interpreter-regexp)
(defvar howm-auto-mode-interpreter-regexp
  (howm-if-unbound auto-mode-interpreter-regexp
                   ;; xemacs doesn't have it.
                   "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
(defun howm-auto-mode (&optional file-name)
  "Major mode appropriate for current buffer.
This checks for a -*- mode tag in the buffer's text,
compares the filename against the entries in `auto-mode-alist',
or checks the interpreter that runs this file against
`interpreter-mode-alist'.

It does not check for the `mode:' local variable in the
Local Variables section of the file; for that, use `hack-local-variables'.

If `enable-local-variables' is nil, this function does not check for a
-*- mode tag.

This function merely returns the mode; it does not set the mode.
"
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let (beg end done modes ans)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (and enable-local-variables
           ;; Don't look for -*- if this file name matches any
           ;; of the regexps in inhibit-first-line-modes-regexps.
           (let ((temp (howm-if-unbound inhibit-first-line-modes-regexps
                                        inhibit-local-variables-regexps))
                 (name (file-name-sans-versions (or file-name ""))))
             (while (let ((sufs (howm-if-unbound inhibit-first-line-modes-suffixes
                                                 inhibit-local-variables-suffixes)))
                      (while (and sufs (not (string-match (car sufs) name)))
                        (setq sufs (cdr sufs)))
                      sufs)
               (setq name (substring name 0 (match-beginning 0))))
             (while (and temp
                         (not (string-match (car temp) name)))
               (setq temp (cdr temp)))
             (not temp))
           (search-forward "-*-" (save-excursion
                                   ;; If the file begins with "#!"
                                   ;; (exec interpreter magic), look
                                   ;; for mode frobs in the first two
                                   ;; lines.  You cannot necessarily
                                   ;; put them in the first line of
                                   ;; such a file without screwing up
                                   ;; the interpreter invocation.
                                   (end-of-line (and (looking-at "^#!") 2))
                                   (point)) t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (if (save-excursion (search-forward ":" end t))
                 ;; Find all specifications for the `mode:' variable
                 ;; and execute them left to right.
                 (while (let ((case-fold-search t))
                          (or (and (looking-at "mode:")
                                   (goto-char (match-end 0)))
                              (re-search-forward "[ \t;]mode:" end t)))
                   (skip-chars-forward " \t")
                   (setq beg (point))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
                         modes))
               ;; Simple -*-MODE-*- case.
               (push (intern (concat (downcase (buffer-substring beg end))
                                     "-mode"))
                     modes)))))
    ;; If we found modes to use, set done.
    (dolist (mode (nreverse modes))
      (when (functionp mode)
        (setq ans mode)
        (setq done t)))
    ;; If we didn't find a mode from a -*- line, try using the file name.
    (if (and (not done) file-name)
        (let ((name file-name)
              (keep-going t))
	  ;; Remove backup-suffixes from file name.
	  (setq name (file-name-sans-versions name))
	  (while keep-going
	    (setq keep-going nil)
	    (let ((alist auto-mode-alist)
		  (mode nil))
	      ;; Find first matching alist entry.
	      (let ((case-fold-search
		     (memq system-type '(vax-vms windows-nt))))
		(while (and (not mode) alist)
		  (if (string-match (car (car alist)) name)
		      (if (and (consp (cdr (car alist)))
			       (nth 2 (car alist)))
			  (setq mode (car (cdr (car alist)))
				name (substring name 0 (match-beginning 0))
				keep-going t)
			(setq mode (cdr (car alist))
			      keep-going nil)))
		  (setq alist (cdr alist))))
	      (if mode
                  (setq ans mode)
		;; If we can't deduce a mode from the file name,
		;; look for an interpreter specified in the first line.
		;; As a special case, allow for things like "#!/bin/env perl",
		;; which finds the interpreter anywhere in $PATH.
		(let ((interpreter
		       (save-excursion
			 (goto-char (point-min))
			 (if (looking-at howm-auto-mode-interpreter-regexp)
			     (match-string 2)
			   "")))
		      elt)
		  ;; Map interpreter name to a mode.
		  (setq elt (assoc (file-name-nondirectory interpreter)
				   interpreter-mode-alist))
                  (if elt
                      (setq ans (cdr elt)))))))))
    ans
    ))

;; copied from /usr/share/emacs/21.2/lisp/subr.el
;; for emacs20 and xemacs
(when (not (fboundp 'replace-regexp-in-string))
  (defun replace-regexp-in-string (regexp rep string &optional
                                          fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacments it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
          (start (or start 0))
          matches str mb me)
      (save-match-data
        (while (and (< start l) (string-match regexp string start))
          (setq mb (match-beginning 0)
                me (match-end 0))
          ;; If we matched the empty string, make sure we advance by one char
          (when (= me mb) (setq me (min l (1+ mb))))
          ;; Generate a replacement for the matched substring.
          ;; Operate only on the substring to minimize string consing.
          ;; Set up match data for the substring for replacement;
          ;; presumably this is likely to be faster than munging the
          ;; match data directly in Lisp.
          (string-match regexp (setq str (substring string mb me)))
          (setq matches
                (cons (replace-match (if (stringp rep)
                                         rep
                                       (funcall rep (match-string 0 str)))
                                     fixedcase literal str subexp)
                      (cons (substring string start mb) ; unmatched prefix
                            matches)))
          (setq start me))
        ;; Reconstruct a string from the pieces.
        (setq matches (cons (substring string start l) matches)) ; leftover
        (apply #'concat (nreverse matches)))))
  )

(howm-defvar-risky howm-kill-all-enable-force nil)
(defun howm-kill-all (&optional force-p)
  "Kill all buffers which is howm-mode and unmodified."
  (interactive "P")
  (let ((anyway (and force-p howm-kill-all-enable-force)))
    (when (if anyway
              (yes-or-no-p "Discard all unsaved changes on howm-mode buffers? ")
            (y-or-n-p "Kill all howm-mode buffers? "))
      (when (eq major-mode 'howm-view-summary-mode)
        (howm-view-restore-window-configuration))
      (mapc (lambda (b)
              (when (howm-buffer-p b)
                (when anyway
                  (switch-to-buffer b)
                  (set-buffer-modified-p nil))  ;; dangerous!
                (when (not (buffer-modified-p b))
                  (kill-buffer b))))
            (buffer-list))
      (message "Done."))))

(defun howm-toggle-buffer ()
  (interactive)
  (if (howm-buffer-p)
      (howm-switch-to-nonhowm-buffer)
    (howm-switch-to-howm-buffer)))
(defun howm-switch-to-howm-buffer ()
  (interactive)
  (let ((b (howm-find-buffer #'howm-buffer-p)))
    (if b
        (switch-to-buffer b)
      (howm-menu))))
(defun howm-switch-to-nonhowm-buffer ()
  (interactive)
  (switch-to-buffer (or (howm-find-buffer #'(lambda (b)
                                              (not (howm-buffer-p b))))
                        (error "No nonhowm buffer"))))

(defun howm-find-buffer (pred)
  (catch :found
    (mapc (lambda (b)
            (cond ((howm-internal-buffer-p b) nil) ;; skip
                  ((funcall pred b) (throw :found b))
                  (t t)))
          (buffer-list))
    nil))

(defun howm-internal-buffer-p (buf)
  (string= (substring (buffer-name buf) 0 1) " "))

(defun howm-buffer-p (&optional buf)
  (let* ((indep-dirs (cons nil *howm-independent-directories*))
         (keyword-bufs (mapcar
                        (lambda (d)
                          (let ((default-directory (or d default-directory)))
                            (howm-keyword-buffer)))
                        indep-dirs)))
    (with-current-buffer (or buf (current-buffer))
      (or howm-mode
          (member major-mode
                  '(howm-view-summary-mode
                    howm-view-contents-mode))
          (member buf keyword-bufs)))))

(defun howm-mode-add-font-lock ()
  (cheat-font-lock-append-keywords (howm-mode-add-font-lock-internal)))

(defun howm-mode-add-font-lock-internal ()
  (when howm-use-color
    `(,@howm-user-font-lock-keywords
      (,howm-view-title-regexp
       (0 howm-mode-title-face prepend))
      (,howm-keyword-regexp
       (,howm-keyword-regexp-hilit-pos howm-mode-keyword-face prepend))
      (,howm-ref-regexp
       (,howm-ref-regexp-hilit-pos howm-mode-ref-face prepend))
      (,howm-wiki-regexp
       (,howm-wiki-regexp-pos howm-mode-wiki-face prepend))
      )))

;;; unofficial. may be removed if no one needs them.

(defun howm-show-buffer-as-howm ()
  (interactive)
  (let* ((name (buffer-name))
         (pos (point))
         (s (buffer-substring-no-properties (point-min) (point-max)))
         (b (get-buffer-create (format "*howm[%s]*" name))))
    (set-buffer b)
    (howm-rewrite-read-only-buffer
      (insert s)
      (howm-mode 1)
      (howm-initialize-buffer))
    (goto-char pos)
    (switch-to-buffer b)))

;;; narrowing

(defun howm-narrow-to-memo ()
  (interactive)
  (apply #'narrow-to-region (howm-view-paragraph-region t)))

(defun howm-toggle-narrow ()
  (interactive)
  (if (howm-narrow-p)
      (widen)
    (howm-narrow-to-memo)))

(put 'howm-narrow-to-memo 'disabled t)
(put 'howm-toggle-narrow 'disabled t)

(defun howm-narrow-p ()
  (let ((b (point-min))
        (e (point-max)))
    (save-restriction
      (widen)
      (not (and (equal b (point-min))
                (equal e (point-max)))))))

(defun howm-auto-narrow ()
  (when (cond (*howm-view-item-privilege* nil)
              ((eq howm-auto-narrow t) t)
              (t (member (howm-command) howm-auto-narrow)))
    (howm-narrow-to-memo)))
;;   (when (and (member (howm-command) howm-auto-narrow)
;;              (not *howm-view-item-privilege*))

;;; select file for new memo by hand

(defun howm-create-interactively (&optional use-current-directory)
  (interactive "P")
  (find-file (read-file-name "Memo file: "
                             (if use-current-directory
                                 nil
                               howm-directory)))
  (goto-char (point-max))
  (howm-create-here))

;;; next/previous memo

(defmacro howm-save-narrowing (&rest body)
  (declare (indent 0))
  `(let ((narrowp (howm-narrow-p)))
     (when narrowp
       (widen))
     (unwind-protect
         (progn
           ,@body)
       (when narrowp
         (howm-narrow-to-memo)))))

(defun howm-next-memo (n)
  (interactive "p")
  (howm-save-narrowing
    (when (looking-at howm-view-title-regexp)
      (setq n (+ n 1)))
    (re-search-forward howm-view-title-regexp nil nil n)))

(defun howm-previous-memo (n)
  (interactive "p")
  (howm-save-narrowing
    (when (not (looking-at howm-view-title-regexp))
      (setq n (+ n 1)))
    (re-search-backward howm-view-title-regexp nil nil n)))

(defun howm-first-memo ()
  (interactive)
  (howm-save-narrowing
    (goto-char (point-min))))

(defun howm-last-memo ()
  (interactive)
  (howm-save-narrowing
    (goto-char (point-max)))
  (re-search-backward howm-view-title-regexp))

;;; random walk

(defvar howm-random-walk-buf nil "for internal use")
(defvar howm-random-walk-ro t "for internal use")

(defun howm-random-walk ()
  (interactive)
  (let ((orig-bufs (buffer-list))
        (howm-history-file nil))
    (while (let ((v (frame-visible-p (selected-frame))))
             (and v (not (eq v 'icon))
                  (not (input-pending-p))))
      (unwind-protect
          (cond ((eq major-mode 'howm-view-summary-mode)
                 (howm-random-walk-summary))
                (howm-mode
                 (howm-random-walk-text))
                (t
                 (howm-list-all)
                 (howm-random-walk-summary)))
        (mapc (lambda (b)
                (when (and (not (member b orig-bufs))
                           (null (get-buffer-window b)))
                  (kill-buffer b)))
              (buffer-list)))
      (sit-for howm-random-walk-wait))))

(defun howm-random-walk-summary ()
  (let ((n (length (riffle-item-list))))
    (goto-char (point-min))
    (forward-line (random n))
    (howm-view-summary-check)
    (sit-for howm-random-walk-wait)
    (howm-view-summary-open)))

(defun howm-random-walk-text ()
  (let* ((ks (howm-keyword-for-goto))
         (k (nth (random (length ks)) ks))
         (d (- (point-max) (point-min))))
    (goto-char (+ (point-min) (random d)))
    (if (search-forward k nil t)
        (goto-char (match-beginning 0))
      (search-backward k nil t))
    (sit-for howm-random-walk-wait)
    (howm-keyword-search k)))

;; named note

(defun howm-open-named-file ()
  "Ask a file name and open it as howm note if it is under howm directory."
  (interactive)
  (let* ((item-dir (lambda (item) (file-name-directory (howm-item-name item))))
         (dir (cond ((eq major-mode 'howm-view-summary-mode)
                     (funcall item-dir (howm-view-summary-current-item)))
                    ((eq major-mode 'howm-view-contents-mode)
                     (funcall item-dir (howm-view-contents-current-item)))
                    (t
                     howm-directory)))
         (fname (read-file-name "Howm file name: " dir)))
    (find-file fname)
    (if (file-exists-p fname)
        (howm-set-mode)
      (progn
        (howm-insert-template "")
        (howm-create-finish)))))

;; imitation of remember.el
;; http://www.emacswiki.org/cgi-bin/emacs-en/RememberMode

;; shamelessly copied from http://newartisans.com/johnw/Emacs/remember.el
;; (I cannot browse http://sacha.free.net.ph/notebook/emacs/dev today.)

(defvar howm-remember-wconf nil
  "for internal use")
(defvar howm-remember-buffer-name "*howm-remember*")
(defvar howm-remember-mode-hook nil)
(let ((m (make-sparse-keymap)))
  (define-key m "\C-c\C-c" 'howm-remember-submit)
  (define-key m "\C-c\C-k" 'howm-remember-discard)
  (howm-defvar-risky howm-remember-mode-map m))

(defun howm-remember ()
  "Add text to new note in howm."
  (interactive)
  (setq howm-remember-wconf (current-window-configuration))
  (switch-to-buffer-other-window (get-buffer-create howm-remember-buffer-name))
  (howm-remember-mode)
  (apply #'message
         `("Remember (%s) or discard (%s)."
           ,@(mapcar (lambda (f)
                       (key-description
                        (where-is-internal f howm-remember-mode-map t)))
                     '(howm-remember-submit howm-remember-discard)))))

(defun howm-remember-mode ()
  "Major mode for `howm-remember'.

\\{howm-remember-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (text-mode)
  (use-local-map howm-remember-mode-map)
  (setq major-mode 'howm-remember-mode
        mode-name "HowmRemember")
  (run-hooks 'howm-remember-mode-hook))

(defun howm-remember-submit ()
  (interactive)
  (save-excursion
    (let* ((title (howm-remember-get-title)) ;; has side effect
           (s (buffer-substring-no-properties (point-min) (point-max))))
      (set-window-configuration howm-remember-wconf)
      (howm-create-file-with-title title)
      (insert s "\n")
      (save-buffer)
      (kill-buffer (current-buffer))))
  (howm-remember-discard))

(defun howm-remember-get-title ()
  (if (not howm-remember-first-line-to-title)
      ""
    (progn
      (goto-char (point-min))
      (prog1
          (buffer-substring-no-properties (point-min)
                                          (line-end-position))
        (forward-line 1)
        (delete-region (point-min) (point))))))

(defun howm-remember-discard ()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration howm-remember-wconf))

;; Rename buffer
;; 
;; You can rename howm buffers based on their titles.
;; Only buffer names in emacs are changed; file names are kept unchanged.
;; 
;; Add the next lines to your .emacs if you like this feature.
;; (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
;; (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
;; 
;; The original idea and code are given by Mielke-san (peter at exegenix.com).
;; http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000020.html
;; thx!

(defvar howm-buffer-name-limit 20)
(defvar howm-buffer-name-total-limit howm-buffer-name-limit)
(defvar howm-buffer-name-format "%s"
  "Buffer name format for `howm-mode-set-buffer-name'.
For example, buffer name is _ABC_ if title is ABC and the value of
this variable is \"_%s_\".")

(defun howm-truncate-string (string limit &optional dots-str)
  "Truncate STRING if it is longer than LIMIT.
For example, \"1234567...\" is returned if string is \"123456789012\"
and limit is 10.
When DOTS-STR is non-nil, it is used instead of \"...\"."
  (let ((dots (or dots-str "...")))
    (when (> (length dots) limit)
      (setq dots (substring dots 0 limit)))
    (if (> (length string) limit)
        (concat (substring string 0 (- limit (length dots)))
                dots)
      string)))

(defun howm-set-buffer-name-from-title (checker title-regexp title-regexp-pos)
  "Set the buffer name to the title(s) of the file."
  (when (funcall checker)
    (save-excursion
      (goto-char 0)
      (let ((titles nil))
        (while (re-search-forward title-regexp nil t)
          (setq titles
                (cons (match-string-no-properties title-regexp-pos)
                      titles))) 
        (let ((name0 (mapconcat  
                      (lambda (s)
                        (howm-truncate-string s howm-buffer-name-limit))
                      (reverse (cl-remove-if (lambda (s) (string= s ""))
                                                  titles))
                      "/")))
          (when (not (string= name0 "")) ;; exclude "no title" case
            (let ((name (format howm-buffer-name-format
                                (howm-truncate-string
                                 name0
                                 howm-buffer-name-total-limit))))
              (rename-buffer name t))))))))

(defun howm-mode-set-buffer-name ()
  (howm-set-buffer-name-from-title (lambda ()
                                     (and howm-mode (buffer-file-name)))
                                   howm-view-title-regexp
                                   howm-view-title-regexp-pos))

;; memoize: used in howm-bayesian-set

(defun howm-memoize-put (fname value)
  (put fname 'howm-memoize value))
(defun howm-memoize-get (fname)
  (get fname 'howm-memoize))

(defun howm-memoize-call (fname func args)
  (let* ((p (assoc args (howm-memoize-get fname))))
    (if p
        (progn
;;           (message "hit %s" p)
          (cdr p))
      (let ((r (apply func args)))
        ;; We need to get it again because func can change memory.
        (howm-memoize-put fname `((,args . ,r) ,@(howm-memoize-get fname)))
        r))))

(defun howm-memoize-reset (fname)
  (howm-memoize-put fname nil))

(defmacro howm-defun-memoize (fname args &rest body)
  (declare (indent 2))
  `(progn
     (howm-memoize-reset ',fname)
     (defun ,fname ,args
       "Function generated by `howm-defun-memoize'"
       (howm-memoize-call ',fname (lambda ,args ,@body) (list ,@args)))))

;; ;; test
;; (howm-memoize-reset 'fib)
;; (howm-defun-memoize fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))
;; (fib 5)
;; (howm-memoize-get 'fib)

;; Bayesian set
;; 
;; "M-x howm-bayesian-set RET lisp scheme haskell RET" to estimate
;; related keywords with lisp, scheme, and haskell.
;; If you are lucky, you may find ruby, elisp, gauche, etc.
;; in estimated candidates.
;; 
;; (ref.)
;; Zoubin Ghahramani and Katherine Heller: "Bayesian Sets",
;; Advances in Neural Information Processing Systems,
;; Vol. 18, pp. 435-442, MIT Press, 2006.
;; http://books.nips.cc/nips18.html
;; http://books.nips.cc/papers/files/nips18/NIPS2005_0712.pdf

(defun howm-bset-nodup (f &rest args)
  (cl-remove-duplicates (apply f args) :test #'equal))
(defun howm-bset-mapcar (func lis)
  (howm-bset-nodup #'mapcar func lis))
(defun howm-bset-mapcan (func lis)
  (howm-bset-nodup (lambda (f z) (apply #'append (mapcar f z)))
                   func lis))

(defun howm-bset-message (&rest args)
  (let (message-log-max) ;; prevent it from being logged
    (apply #'message args)))

(defun howm-bset-matched-files (query)
;;   (howm-bset-message "Finding files for query (%s)..." query)
  (howm-bset-mapcar #'howm-item-name
                    (howm-view-search-folder-items query (howm-folder)
                                                   nil t)))

(howm-defun-memoize howm-bset-keywords-in-file* (file keyword-list)
;;   (howm-bset-message "Finding keywords in file (%s)..." file)
  (with-temp-buffer
    (insert-file-contents file)
    (howm-keyword-for-goto keyword-list)))

(defun howm-bset-keywords-in-file (file)
  (howm-bset-keywords-in-file* file nil))

(defun howm-bset-candidate-keywords (query-list)
;;   (howm-bset-message "Collecting keywords...")
  (let ((files (howm-bset-mapcan #'howm-bset-matched-files
                                 query-list)))
    (howm-bset-mapcan (lambda (f)
                        (howm-bset-message "Collecting keywords in file (%s)..."
                                           f)
                        (howm-bset-keywords-in-file f))
                      files)))

(howm-defun-memoize howm-bset-file-score (file query-list
                                               coef number-of-all-keywords)
;;   (howm-bset-message "Scoring file (%s)..." file)
  (let* ((m (/ (length (howm-bset-keywords-in-file file))
               (float number-of-all-keywords)))
         (a (* coef m))
         (b (* coef (- 1 m)))
         (s (length (howm-bset-keywords-in-file* file query-list)))
         (a2 (+ a s))
         (b2 (+ b (- (length query-list) s))))
    ;; log{(a2/a) * (b/b2)}
    (- (- (log a2) (log a)) (- (log b2) (log b)))))

(howm-defun-memoize howm-bset-keyword-score (keyword query-list
                                                     coef
                                                     number-of-all-keywords)
  (howm-bset-message "Scoring keyword (%s)..." keyword)
  (apply #'+
         (mapcar (lambda (file)
                   (howm-bset-file-score file query-list coef
                                         number-of-all-keywords))
                 (howm-bset-matched-files keyword))))

(defun howm-bset-reset ()
  (mapc #'howm-memoize-reset '(howm-bset-file-score
                               howm-bset-keyword-score
                               howm-bset-keywords-in-file*)))

(defun howm-bset (query-list)
  (howm-bset-reset)
  (unwind-protect
      (let ((n (length (howm-keyword-list)))
            (c 2.0)) ;; heuristic value
        (sort (copy-sequence (howm-bset-candidate-keywords query-list))
              (lambda (k1 k2)
                (apply #'>
                       (mapcar (lambda (k)
                                 (howm-bset-keyword-score k query-list c n))
                               (list k1 k2))))))
    (howm-bset-reset)))

(defun howm-bayesian-set (query-str)
  (interactive "sQueries: ")
  (switch-to-buffer (get-buffer-create "*howm-bayesian-set*"))
  (howm-rewrite-read-only-buffer
    (insert (mapconcat #'identity
                       (howm-bset (split-string query-str))
                       "\n"))
    (howm-mode 1))
  (goto-char (point-min))
  (howm-bset-message "Done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fellowship

;; xemacs: add-to-list doesn't have APPEND
;; (add-to-list 'auto-mode-alist '("\\.howm$" . text-mode) t)
(setq auto-mode-alist (append auto-mode-alist 
                              (list '("\\.howm$" . text-mode))))

;; xyzzy doesn't have eval-after-load.
;; It will be useless anyway.
(when (not (fboundp 'eval-after-load))
  (defun eval-after-load (file form)
    nil))

;; xemacs canna doesn't use minor-mode. [2004-01-30]
(defvar action-lock-mode-before-canna nil)
(make-variable-buffer-local 'action-lock-mode-before-canna)
(defadvice canna:enter-canna-mode (around action-lock-fix activate)
  (setq action-lock-mode-before-canna action-lock-mode)
  (setq action-lock-mode nil)
  ad-do-it)
(defadvice canna:quit-canna-mode (around action-lock-fix activate)
  (setq action-lock-mode action-lock-mode-before-canna)
  ad-do-it)
 
;; for mcomplete.el [2003-12-17]
;; http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
;; error when this-command is (lambda () (interactive) ...)
(defadvice mcomplete-p (around symbol-check activate)
  (and (symbolp this-command)
       ad-do-it))

;; for auto-save-buffers.el [2004-01-10]
;; http://www.namazu.org/~satoru/auto-save/
;; http://homepage3.nifty.com/oatu/emacs/misc.html
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=auto%20save
(defvar howm-auto-save-buffers-disposed nil)
(howm-dont-warn-free-variable auto-save-buffers-regexp)
(howm-dont-warn-free-variable auto-save-reject-buffers-regexp)
(defun howm-auto-save-buffers-p ()
  (let ((f (howm-file-name)))
    (and (if (boundp 'auto-save-buffers-regexp)
             (string-match auto-save-buffers-regexp f)
           nil)
         (if (boundp 'auto-save-reject-buffers-regexp)
             (not (string-match auto-save-reject-buffers-regexp f))
           t))))
(defun howm-auto-save-buffers-dispose ()
  (setq howm-menu-refresh-after-save nil)
  (setq howm-refresh-after-save nil)
  (setq howm-auto-save-buffers-disposed t)
  (message "howm: Automatic refresh is disabled when auto-save-buffers is called."))
(defadvice auto-save-buffers (around howm-dispose activate)
  (if (or howm-auto-save-buffers-disposed
          (not (howm-auto-save-buffers-p)))
      ad-do-it
    (howm-auto-save-buffers-dispose)))

;; howm on ChangeLog Memo
(defun howm-setup-change-log ()
  (setq howm-keyword-format "\t* %s")
  (setq howm-keyword-regexp "^\t\\(\\*\\)[ \t]+\\([^:\r\n]+\\)")
  (setq howm-keyword-regexp-hilit-pos 1) ;; 「関連キーワード」用
  (setq howm-keyword-regexp-pos 2)
  (setq howm-view-title-regexp "^$")
  (setq howm-view-title-regexp-pos 0)
  (setq howm-view-title-regexp-grep 'sorry-not-yet)
  (setq howm-use-color nil)
  (setq howm-menu-top nil)
  (defadvice howm-exclude-p (around change-log (filename) activate)
    (setq ad-return-value
          (not (find-if (lambda (dir)
                          (string= (howm-file-name)
                                   (file-relative-name filename dir)))
                        (howm-search-path)))))
  (defadvice howm-create-file-with-title (around change-log (title) activate)
    (howm-create-file)
    (when (string-match howm-keyword-regexp title)
      (setq title (match-string-no-properties howm-keyword-regexp-pos
                                              title)))
    (insert title))
  (defadvice howm-create-file (around change-log
                                      (&optional keep-cursor-p) activate)
    (let* ((default (howm-file-name))
           (file (expand-file-name default howm-directory))
           (dir (file-name-directory file))
           (buffer-file-name file)) ;; don't insert file name
      (make-directory dir t)
      (add-change-log-entry nil file)))
  (add-hook 'change-log-mode-hook 'howm-mode)
  )

;; howm with ChangeLog Memo
(defvar howm-change-log-file-name "ChangeLog")
(defun howm-to-change-log ()
  (interactive)
  (let* ((title (howm-title-at-current-point))
         (file (expand-file-name howm-change-log-file-name howm-directory))
         ;; cheat add-change-log-entry
         (buffer-file-name title)
         (default-directory howm-directory))
    (add-change-log-entry nil file)))
(defun howm-from-change-log ()
  (interactive)
  (let* ((title-regexp "^\t[*][ \t]*\\(.*\\)$")
         (title-regexp-pos 1)
         (title (howm-title-at-current-point nil
                                             title-regexp title-regexp-pos)))
    (howm-create-file-with-title title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug Report

;; Japanese is assumed at now.

(defun howm-test ()
  "Show bug report template for howm."
  (howm-set-lang)
  (howm-bug-report))

(defun howm-set-lang ()
  (set-language-environment "Japanese")
  (set-default-coding-systems 'euc-jp)
  (set-buffer-file-coding-system 'euc-jp-unix)
  (set-terminal-coding-system 'euc-jp)
  (set-keyboard-coding-system 'euc-jp)
  )

(defun howm-compiled-p ()
  (byte-code-function-p (symbol-function 'howm-compiled-p)))
(defun howm-make-file-p ()
  (eval-when-compile
    (getenv "HOWM_MAKE")))
(defun howm-test-p ()
  (getenv "HOWM_TEST"))

(defun howm-bug-report (&optional show-sym)
  (interactive "P")
  (let ((report-buf (format-time-string "howm-bug-report-%Y%m%d-%H%M%S"))
        (template "sample/bug-report.txt"))
    (switch-to-buffer report-buf)
    (when (not (howm-buffer-empty-p))
      (error "Buffer %s exists (and not empty)." report-buf))
    (if (file-exists-p template)
        (insert-file-contents template)
      (insert "Please copy the following text to your bug report.\n\n"))
    (goto-char (point-max))
    (mapc (lambda (sv)
            (insert (format "%s: %s\n" (car sv) (cdr sv))))
          `(
            ("howm" . ,(howm-version-long))
            ,@(honest-report-version-assoc)
            ))
    (when (eq howm-view-use-grep t)
      (insert
       (format "grep: %s - %s\n"
               (cl-mapcan (lambda (d)
                                 (let ((f (expand-file-name
                                           howm-view-grep-command d)))
                                   (and (file-executable-p f)
                                        (list f))))
                               exec-path)
               (car (howm-call-process "grep" '("--version"))))))
    (when show-sym
      (goto-char (point-max))
      (insert "\n(List of variables)\n")
      (insert (howm-symbols-desc)))
    (goto-char (point-min))))

(defun howm-version-long ()
  (format "%s (compile: %s, make: %s, test: %s)"
          howm-version
          (howm-compiled-p)
          (howm-make-file-p)
          (howm-test-p)))

(defun howm-symbols-desc (&optional max-desc-len)
  (when (null max-desc-len)
    (setq max-desc-len 50))
  (apply #'concat
         (mapcar (lambda (sym)
                   (when (boundp sym)
                     (let ((v (format "%S" (symbol-value sym))))
                       (when (and (numberp max-desc-len)
                                  (< max-desc-len (length v)))
                         (setq v
                               (let* ((tl (/ max-desc-len 4))
                                      (hd (- max-desc-len tl)))
                                 (concat (substring v 0 hd)
                                         " ... "
                                         (substring v (- tl))))))
                       (format "%s: %s\n" (symbol-name sym) v))))
                 (sort (howm-symbols)
                       (lambda (x y)
                         (string< (symbol-name x) (symbol-name y)))))))

(defvar howm-required-features '(
                                cheat-font-lock
                                action-lock
                                riffle
                                gfunc
                                illusion
                                honest-report
                                )
  "List of features which are required for, and distributed with, howm itself.")

(defun howm-prefix-names ()
  (mapcar #'symbol-name (cons 'howm howm-required-features)))

(defun howm-symbols ()
  (let* ((reg (format "^%s" (regexp-opt (howm-prefix-names) t)))
         (a nil))
    (mapatoms (lambda (s)
                (when (string-match reg (symbol-name s))
                  (setq a (cons s a)))))
    a))

(defun howm-elp ()
  (interactive)
  (mapcar #'elp-instrument-package
          (howm-prefix-names)))

(defvar howm-sample-directory (expand-file-name "sample/")
  "for internal use")
(defun howm-bug-shot ()
  (interactive)
  (let* ((version (concat "[howm] " (howm-version-long)))
         (init (and (howm-test-p)
                    (let ((f (expand-file-name "dot.emacs"
                                               howm-sample-directory)))
                      (and (file-readable-p f)
                           (with-temp-buffer
                             (insert-file-contents f)
                             (buffer-substring-no-properties (point-min)
                                                             (point-max)))))))
         (header (if init
                     (concat version "\n\n[init]\n" init)
                   version))
         (footer "--- your comment ---"))
    (honest-report header footer)
    (message "Please copy this buffer to your report.")))

;;; howm-misc.el ends here
