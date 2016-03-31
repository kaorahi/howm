;;; riffle.el --- template of list browser with immediate preview
;;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: riffle.el,v 1.42 2012-12-29 08:57:18 hira Exp $
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

;;; Commentary:

;; Not yet. See sample at the bottom of this file.

;;; Code:

(require 'cl-lib)
(require 'gfunc)
(require 'howm-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customize

;; These howm-view-xxx will be renamed to riffle-xxx in future.

(defcustom howm-view-summary-window-size nil
  "Size of summary window, or nil for half size."
  :type '(radio (const :tag "Half" nil)
                integer)
  :group 'howm-list-bufwin)
(defcustom howm-view-split-horizontally nil
  "If non-nil, split window horizontally to show summary and contents."
  :type 'boolean
  :group 'howm-list-bufwin)
(defcustom howm-view-keep-one-window nil
  "If nil, split windows automatically for summary and contents
even if you delete other windows explicitly."
  :type 'boolean
  :group 'howm-list-bufwin)
(defcustom howm-view-pop-up-windows t
  "If non-nil, override `pop-up-windows'."
  :type 'boolean
  :group 'howm-list-bufwin)

;; clean me: This value is copied to howm-view-open-recenter.
(defvar howm-view-search-recenter 5)

;; experimental [2008-05-23]
(defvar riffle-keep-window nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; internal variables and accessors

(defvar *riffle-summary-check* t)

(defvar riffle-name nil)
(defvar riffle-item-list nil)
(defvar riffle-type nil)
(defvar riffle-summary-last-line nil)
(defvar riffle-contents-end nil)
(make-variable-buffer-local 'riffle-name)
(make-variable-buffer-local 'riffle-item-list)
(make-variable-buffer-local 'riffle-type)
; update contents when changed
(make-variable-buffer-local 'riffle-summary-last-line)
; end points of items
(make-variable-buffer-local 'riffle-contents-end)

(defun riffle-name () riffle-name)
(defun riffle-item-list () riffle-item-list)
(defun riffle-set-item-list (item-list) (setq riffle-item-list item-list))

;; clean me
(defun riffle-p () riffle-type)
(defun riffle-contents-first-time-p () (null riffle-contents-end))

(defvar *riffle-preview-p* nil)
(defun riffle-preview-p () *riffle-preview-p*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro

;; In xemacs, define-derived-mode makes the mode call
;; derived-mode-merge-syntax-tables, which takes long time.
;; To avoid it, we need ":syntax-table nil". Sigh...

(defmacro riffle-define-derived-mode (child parent name
                                            &optional docstring
                                            &rest body)
  (declare (indent 3))
  `(define-derived-mode ,child ,parent ,name
     ,docstring
     :syntax-table nil
     :abbrev-table nil
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic function

(defun riffle-type (&rest r)
  riffle-type)
(defvar riffle-dispatchers (list #'riffle-type))
(put 'riffle-dispatchers 'risky-local-variable t)

(gfunc-with riffle-dispatchers
  (gfunc-def riffle-home (item))
  (gfunc-def riffle-summary-item (item))
  (gfunc-def riffle-contents-item (item))
  (gfunc-def riffle-summary-set-mode ())
  (gfunc-def riffle-contents-set-mode ())
  (gfunc-def riffle-summary-name-format ())
  (gfunc-def riffle-contents-name-format ())
  (gfunc-def riffle-post-update (item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common

(defcustom riffle-mode-hook nil
  "Hook run at the end of function `riffle-mode'"
  :type 'hook
  :group 'howm-hook)

(defvar riffle-mode-map nil)
(put 'riffle-mode-map 'risky-local-variable t)
(defvar riffle-mode-syntax-table (make-syntax-table))
(defvar riffle-mode-abbrev-table nil)

(defun riffle-mode ()
  "not yet"
  (setq major-mode 'riffle-mode
        mode-name "Riffle")
  (use-local-map riffle-mode-map)
  (set-syntax-table riffle-mode-syntax-table)
  (define-abbrev-table 'riffle-mode-abbrev-table nil)
  (run-hooks 'riffle-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; summary

(defun riffle-summary (&optional name item-list type background)
  "Create summary buffer for NAME, ITEM-LIST, and TYPE.
When NAME is nil, default values for them are selected.
Created buffer is shown immediately as far as BACKGROUND is nil.
This function returns effective value of ITEM-LIST."
  (when (null name)
    (setq name      (riffle-name)
          item-list (riffle-item-list)
          type      riffle-type))
  (if (null item-list)
      nil
    (let ((d default-directory))
      (riffle-setup-buffer #'riffle-summary-name-format name item-list type)
      (setq default-directory d)
      (when (not background)
        (riffle-summary-subr name item-list))
      item-list)))

(defun riffle-summary-subr (name item-list)
  (riffle-summary-set-mode)
  (riffle-summary-show item-list)
  (unless riffle-keep-window
    (riffle-summary-check t)))

(defun riffle-summary-show (item-list)
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapc 'riffle-summary-show-item item-list)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t
        truncate-lines t)
  (goto-char (point-min))
  (setq riffle-summary-last-line -777))

(defun riffle-summary-show-item (item)
  (insert (riffle-summary-item item) "\n"))

(riffle-define-derived-mode riffle-summary-mode riffle-mode "RiffleS"
  "not yet"
  ;; make-local-hook is obsolete for emacs >= 21.1.
  (when (fboundp 'make-local-hook) (make-local-hook 'post-command-hook))
  (add-hook 'post-command-hook 'riffle-post-command t t))

(defun riffle-post-command ()
  (unless riffle-keep-window
    (if *riffle-summary-check*
        (riffle-summary-check)
      (setq *riffle-summary-check* t))))

(defun riffle-summary-current-item ()
  (let ((n (riffle-line-number)))
    (nth (1- n) (riffle-item-list))))

(defun riffle-summary-check (&optional force)
  (let ((keep-one howm-view-keep-one-window))
    (when force
      (riffle-refresh-window-configuration)
      (setq keep-one nil))
    (let ((n (riffle-line-number))
          (howm-view-keep-one-window keep-one))
      (when (or (not (= n riffle-summary-last-line))
                force)
        (setq riffle-summary-last-line n)
        (let ((item (riffle-summary-current-item)))
          (when (and item *riffle-summary-check*)
            (riffle-summary-update item force)))))))

(defun riffle-summary-update (item &optional new)
  (unless (and howm-view-keep-one-window (one-window-p))
    (riffle-summary-update-subr item new)))
(defun riffle-summary-update-subr (item &optional new)
  (let* ((*riffle-preview-p* t) ;; dirty
         (vbuf (riffle-contents-buffer new))
         (cwin (selected-window))
         (pop-up-windows (or pop-up-windows howm-view-pop-up-windows))
;;          (section (riffle-controller 'section item))
         (name (riffle-name))
         (type riffle-type)) ;; be careful to buffer local var.
    (riffle-pop-to-buffer vbuf howm-view-summary-window-size)
    (riffle-contents name (list item) type default-directory)
    (goto-char (point-min))
    (let ((home (riffle-home item)))
;;     (let ((home (howm-view-item-home item)))
      (when (numberp home)
        (goto-char home)
        (recenter howm-view-search-recenter))
      (select-window cwin)
      (riffle-post-update item))))
;;       (message "View: %s" section)

(defun riffle-pop-window ()
  (interactive)
  (let ((r (one-window-p)))
    (when r
      (riffle-summary-check t))
    r))

(defun riffle-pop-or-scroll-other-window ()
  (interactive)
  (or (riffle-pop-window)
      (scroll-other-window)))

(defun riffle-toggle-window ()
  (interactive)
  (or (riffle-pop-window)
      (delete-other-windows)))

(defun riffle-summary-to-contents ()
  (interactive)
  (let ((b (current-buffer)))
    (unless riffle-keep-window
      (delete-other-windows)
      (set-buffer b))
    (let ((n (riffle-line-number)))
      (riffle-contents (riffle-name) (riffle-item-list) riffle-type
                       default-directory)
      (goto-char (riffle-contents-beginning (1- n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; contents

;; (defvar riffle-contents-mode-variant nil)

(defun riffle-contents (name item-list type default-dir)
  (if (null item-list)
      (message "Empty.")
    (progn
      (riffle-setup-buffer #'riffle-contents-name-format name item-list type)
      (setq default-directory default-dir)
      (when (riffle-contents-first-time-p)
        (riffle-contents-set-mode))
;;       (let ((cm (riffle-controller 'contents-mode)))
;;         (when (not (eq major-mode cm))
;;           (funcall cm)))
      (riffle-contents-show item-list))))

(riffle-define-derived-mode riffle-contents-mode riffle-mode "RiffleC"
  "not yet"
  )

(defun riffle-contents-show (item-list)
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq riffle-contents-end
        (mapcar (lambda (item) (riffle-contents-show-item item))
                item-list))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min))
  )

(defun riffle-contents-show-item (item)
  (insert (riffle-contents-item item))
  (point))

(defun riffle-contents-item-number (position)
  (let ((rest riffle-contents-end)
        (n 0))
    (while (and rest (<= (car rest) position))
      (setq rest (cdr rest)
            n (+ n 1)))
    (min n (1- (length riffle-contents-end))))) ;; for the last line

(defun riffle-contents-current-item ()
  (nth (riffle-contents-item-number (point)) (riffle-item-list)))

(defun riffle-contents-beginning (n)
  (nth n (cons 1 riffle-contents-end)))

(defun riffle-contents-to-summary ()
  (interactive)
  (let ((n (riffle-contents-item-number (point))))
    (riffle-summary (riffle-name) (riffle-item-list) riffle-type)
;    (howm-view-summary (riffle-name) (riffle-item-list))
    (howm-goto-line (1+ n)))) ;; top = 1 for goto-line

(defun riffle-contents-goto-next-item (&optional n)
  (interactive "p")
  (let* ((c (point))
         ;; remember that riffle-contents-end has duplicats
         (stops (cl-remove-duplicates
                 (sort `(1 ,c ,@(copy-sequence riffle-contents-end))
                       #'<)))
         (pos (cl-position c stops))
         (new (+ pos n)))
    (cond ((< new 0)
           (goto-char (point-min))
           (error "Beginning of buffer"))
          ((>= new (length stops))
           (goto-char (point-max))
           (error "End of buffer"))
          (t
           (goto-char (nth new stops))))))

(defun riffle-contents-goto-previous-item (&optional n)
  (interactive "p")
  (riffle-contents-goto-next-item (- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc.

(defun riffle-summary-buffer (&optional new)
  (riffle-get-buffer (riffle-summary-name-format) nil new))
(defun riffle-contents-buffer (&optional new)
  (riffle-get-buffer (riffle-contents-name-format) nil new))
;; (defun riffle-contents-buffer (&optional new)
;;   (riffle-get-buffer howm-view-contents-name nil new))
;; (defun riffle-summary-buffer (&optional new)
;;   (riffle-get-buffer howm-view-summary-name nil new))
(defun riffle-get-buffer (name-format &optional name new)
  (let* ((bufname (format name-format (or name (riffle-name))))
         (buf (get-buffer bufname)))
    (when (and new buf)
      (kill-buffer buf))
    (get-buffer-create bufname)))

(defun riffle-kill-buffer ()
  (interactive)
  (when (riffle-p)
    (let* ((s (riffle-summary-buffer))
           (c (riffle-contents-buffer))
           (sw (get-buffer-window s)))
      (when sw
        (select-window sw))
      (kill-buffer s)
      (kill-buffer c)
      (riffle-restore-window-configuration))))

(defun riffle-setup-buffer (name-format-func name item-list type)
  (let ((name-format (let ((riffle-type type))
                       (funcall name-format-func))))
    (switch-to-buffer (riffle-get-buffer name-format name))
    (setq riffle-type type)
    (setq riffle-name name
          riffle-item-list item-list)))

(defun riffle-line-number (&optional pos)
  (save-excursion
    (save-restriction
      (widen)
      (when pos
        (goto-char pos))
      (let ((raw (count-lines (point-min) (point))))
        (if (bolp)
            (+ raw 1)
          raw)))))

(defun riffle-persistent-p (z)
  "Return whether the buffer should be persistent or not.
Note that the value of Z is funcall-ed if it is a function;
consider to set `risky-local-variable' property.
(cf)
snap://Info-mode/elisp#File Local Variables
snap://Info-mode/emacs#File Variables
"
  (riffle-get-value z))

(defun riffle-get-value (z)
  (if (functionp z)
      (funcall z)
    z))

(defun riffle-restore-window-configuration ()
  (riffle-refresh-window-configuration))

(defun riffle-refresh-window-configuration ()
;;   (message "%s -- %s" (buffer-name) (if (riffle-p) t nil)) ;; debug
  (if (riffle-p)
      (riffle-setup-window-configuration)
    (unless riffle-keep-window
      (delete-other-windows))))

(defvar riffle-window-initializer 'delete-other-windows)
;; (setq riffle-window-initializer '(lambda () (pop-to-buffer nil)))
(put 'riffle-window-initializer 'risky-local-variable t)
(defun riffle-setup-window-configuration ()
  (let ((orig (current-buffer))
        (s (riffle-summary-buffer))
        (c (riffle-contents-buffer)))
    (when (functionp riffle-window-initializer)
      (funcall riffle-window-initializer))
    (switch-to-buffer c)
    (riffle-pop-to-buffer c howm-view-summary-window-size)
    (switch-to-buffer s)
    (select-window (get-buffer-window orig))))

(defun riffle-scroll-up (count)
  (interactive "p")
  (scroll-up count))
(defun riffle-scroll-down (count)
  (interactive "p")
  (scroll-down count))
(defun riffle-scroll-other-window (count)
  (interactive "p")
  (scroll-other-window count))
(defun riffle-scroll-other-window-down (count)
  (interactive "p")
  (scroll-other-window-down count))

(defvar even-window-heights nil)  ;; xemacs doesn't have it.
(defun riffle-pop-to-buffer (buf &optional size)
  (if riffle-keep-window
      (switch-to-buffer buf)
    (progn
      (when (one-window-p)
        (split-window nil size howm-view-split-horizontally))
      (let ((even-window-heights (if size
                                     nil
                                   even-window-heights))
            ;; Don't split windows further even when
            ;; riffle-pop-to-buffer is called twice.
            (pop-up-windows nil))
        (pop-to-buffer buf)))))

;; 'Place' is line number at now
(defun riffle-set-place (place)
  (howm-goto-line place))
(defun riffle-get-place (&optional point)
  (riffle-line-number point))

;; ;; needless?
;; (defun riffle-jump-to-summary ()
;;   (interactive)
;;   (riffle-jump-to-buffer (riffle-summary-buffer)))
;; (defun riffle-jump-to-contents ()
;;   (interactive)
;;   (riffle-jump-to-buffer (riffle-contents-buffer)))
;; (defun riffle-jump-to-buffer (buf)
;;   (let ((w (get-buffer-window buf)))
;;     (if w
;;         (select-window w)
;;       (switch-to-buffer buf))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; controller

;; (defun riffle-make-controller (alist)
;;   alist)
;; (defun riffle-send (object command &rest args)
;;   (if (eq command 'self)
;;       object
;;     (let ((func (cdr (assoc command object))))
;;       (apply func args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keymap

;;; riffle-mode

(let ((m (make-sparse-keymap)))
  (define-key m "n" 'next-line)
  (define-key m "p" 'previous-line)
  (define-key m "?" 'describe-mode)
  (define-key m "q" 'riffle-kill-buffer)
  (setq riffle-mode-map m))

;;; riffle-summary-mode

(let ((m riffle-summary-mode-map))
  (define-key m " " 'riffle-pop-or-scroll-other-window)
  (define-key m [backspace] 'scroll-other-window-down)
  (define-key m "\C-h" 'scroll-other-window-down)
  (define-key m "j" 'riffle-scroll-other-window)
  (define-key m "k" 'riffle-scroll-other-window-down)
  (define-key m "@" 'riffle-summary-to-contents)
  (define-key m "0" 'riffle-summary-to-contents)
  (define-key m "1" 'delete-other-windows)
  (define-key m "2" 'riffle-pop-window)
  (define-key m "v" 'riffle-toggle-window)
;;   (define-key m "o" 'riffle-jump-to-contents)
  )

;;; riffle-contents-mode

(let ((m riffle-contents-mode-map))
  (define-key m " " 'scroll-up)
  (define-key m [backspace] 'scroll-down)
  (define-key m "\C-h" 'scroll-down)
  (define-key m "j" 'riffle-scroll-up)
  (define-key m "k" 'riffle-scroll-down)
  (define-key m "@" 'riffle-contents-to-summary)
  (define-key m "0" 'riffle-contents-to-summary)
  (define-key m "\C-i" 'riffle-contents-goto-next-item)
  (define-key m "\M-\C-i" 'riffle-contents-goto-previous-item)
  (define-key m [tab] 'riffle-contents-goto-next-item)
  (define-key m [(meta tab)] 'riffle-contents-goto-previous-item)
;;   (define-key m "o" 'riffle-jump-to-summary)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample

;; Sample code 
;; (For more realistic example, see "riffle" section in howm-view.el.)
;; snap:///~/elisp/howm/howm-view.el#136:;;; riffle

;; Usage:
;; 1. M-x load-file <this file>
;; 2. M-x riffle-sample
;; 3. Move cursor. Type ? for help.

(defvar riffle-sample-item-list
  '(
    ("foo1" "foo1 line1\nfoo1 line2\nfoo1 line3\nfoo1 line4\n")
    ("foo2" "foo2 line1\nfoo2 line2\nfoo2 line3\nfoo2 line4\n")
    ("bar1" "bar1 line1\nbar1 line2\nbar1 line3\nbar1 line4\n")
    ("bar2" "bar2 line1\nbar2 line2\nbar2 line3\nbar2 line4\n")
    ))

(defvar riffle-sample-summary-name "sampleS:%s")
(defvar riffle-sample-contents-name "sampleC:%s")
(defvar riffle-sample-cursor-point 3)

(defun riffle-home:sample (item)
  riffle-sample-cursor-point)
(defun riffle-summary-item:sample (item)
  (car item))
(defun riffle-contents-item:sample (item)
  (concat (format "<%s>\n" (car item)) (cadr item) "\n"))
(defun riffle-summary-set-mode:sample ()
  (riffle-sample-summary-mode))
(defun riffle-contents-set-mode:sample ()
  (riffle-sample-contents-mode))
(defun riffle-summary-name-format:sample ()
  riffle-sample-summary-name)
(defun riffle-contents-name-format:sample ()
  riffle-sample-contents-name)
(defun riffle-post-update:sample (item)
  (message "%s" (car item)))

(riffle-define-derived-mode riffle-sample-summary-mode riffle-summary-mode
                            "SampleS"
  "Sample summary mode.
key	binding
---	-------
\\[next-line]	Next item
\\[previous-line]	Previous item
\\[riffle-pop-or-scroll-other-window]	Pop and scroll contents
\\[scroll-other-window-down]	Scroll contents
\\[riffle-scroll-other-window]	Scroll contents one line
\\[riffle-scroll-other-window-down]	Scroll contents one line
\\[riffle-summary-to-contents]	Concatenate all contents

\\[delete-other-windows]	Delete contents window
\\[riffle-pop-window]	Pop contents window
\\[riffle-toggle-window]	Toggle contents window

\\[describe-mode]	This help
\\[riffle-kill-buffer]	Quit
"
  )

(riffle-define-derived-mode riffle-sample-contents-mode riffle-contents-mode
                            "SampleC"
  "Sample contents mode.
key	binding
---	-------
\\[next-line]	Next line
\\[previous-line]	Previous line
\\[scroll-up]	Scroll up
\\[scroll-down]	Scroll down
\\[riffle-scroll-up]	Scroll one line up
\\[riffle-scroll-down]	Scroll one line down
\\[riffle-contents-to-summary]	Summary
\\[riffle-contents-goto-next-item]	Next item
\\[riffle-contents-goto-previous-item]	Previous item

\\[describe-mode]	This help
\\[riffle-kill-buffer]	Quit
"
  )

(defun riffle-sample ()
  (interactive)
  (riffle-summary "sample-list" riffle-sample-item-list ':sample))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; provide

(provide 'riffle)

;;; riffle.el ends here
