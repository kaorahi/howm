;;; illusion.el --- load, edit, and submit something which is not pure file
;;; Copyright (C) 2005-2022
;;;   HIRAOKA Kazuyuki <khi@users.osdn.me>
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

;; Not yet. See the example 'yalot13' at the bottom of this file.

;;; Code:

(require 'easy-mmode)
(require 'howm-common)

(defvar illusion-lighter " _i_")
(defvar illusion-submit-key "\C-c\C-c")

(defvar illusion-submit-func
  (lambda ()
    (error "Submission function is not defined."))
  "Value of this variable is called when `illusion-submit' is executed.
It must return non-nil value for successful case.")
(make-variable-buffer-local 'illusion-submit-func)
(put 'illusion-submit-func 'risky-local-variable t)

(defun illusion-submit ()
  (interactive)
  (funcall illusion-submit-func)
  (set-buffer-modified-p nil))

(defun illusion-generate (name loader submitter)
  (switch-to-buffer (generate-new-buffer name))
  (text-mode)
  (illusion-mode 1)
  (setq illusion-submit-func submitter)
  (funcall loader)
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(define-minor-mode illusion-mode
  "With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When the mode is enabled, \\[illusion-submit] submits the content
with a manner which is suitable to current buffer.

key	binding
---	-------
\\[illusion-submit]	Submit changes
"
  :init-value nil ;; default = off
  :lighter illusion-lighter ;; mode-line
  :keymap `(
            (,illusion-submit-key . illusion-submit)
            )
  (use-local-map illusion-mode-map)
)

;;; Example

;; M-x yarot13-find-file to open rot13ed file.
;; Edit it, and C-c C-c to save it.

;; (personal note) ruby -pe '$_.tr! "a-zA-Z", "n-za-mN-ZA-M"'

(defun yarot13-find-file (file)
  (interactive "Frot13 file: ")
  (illusion-generate (concat "rot13:" (file-name-nondirectory file))
                     `(lambda () (yarot13-insert-file-contents ,file))
                     `(lambda () (yarot13-save-buffer-to ,file))))

(defun yarot13-insert-file-contents (file)
  (if (file-exists-p file)
      (let ((s (with-temp-buffer
                 (howm-insert-file-contents file)
                 (yarot13-rotate-buffer)
                 (buffer-string))))
        (insert s))
    (message "(New file)")))

(defun yarot13-save-buffer-to (file)
  (let ((s (buffer-string)))
    (with-temp-buffer
      (insert s)
      (yarot13-rotate-buffer)
      (set-visited-file-name file)
      (basic-save-buffer))))

(defun yarot13-rotate-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((c (char-after)))
        (setq c (yarot13-rotate-char c ?a ?z))
        (setq c (yarot13-rotate-char c ?A ?Z))
        (delete-char 1)
        (insert-char c 1)))))

(defun yarot13-rotate-string (str)
  (with-temp-buffer
    (insert str)
    (yarot13-rotate-buffer)
    (buffer-string)))

(defun yarot13-rotate-char (x beg end)
  (let ((d (- x beg))
        (w (+ 1 (- end beg))))
    (if (and (<= beg x) (<= x end))
        (+ beg (mod (+ d (/ w 2)) w))
      x)))

(provide 'illusion)

;;; illusion.el ends here
