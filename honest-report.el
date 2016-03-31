;;; honest-report.el --- make bug report with screenshot and keylog

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;; $Id: honest-report.el,v 1.13 2011-12-31 15:07:29 hira Exp $
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

;; (For users)
;; This small tool helps you write clear bug report.
;; Just type M-x honest-report to show recent keys and screen shots.
;; Copy them into your bug report.

;; (For programmers)
;; Write a wrapper of `honest-report' with your favorite header and footer.

;; (Bug)
;; Text properties are ignored in screen shot.
;; In particular, too large region can be copied for outline-mode
;; because all closed items are shown as opened.

;;; Code:

(defun honest-report (&optional header footer)
  (interactive)
  (let ((ver (honest-report-version))
        (key (honest-report-recent-keys))
        (msg (honest-report-message))
        (scr (honest-report-screenshot)))
    (honest-report-setup)
    (mapc (lambda (a) (apply #'honest-report-insert a))
          `(
            ("Header"          ,header)
            ("Emacs version"   ,ver)
            ("Recent keys"     ,key)
            ("Recent messages" ,msg)
            ("Screen shot"     ,scr)
            ("Footer"          ,footer)
            ))
    (goto-char (point-max))))

(defun honest-report-insert (title content)
  (when content
    (insert "* " title ":\n\n" content "\n\n")))

;;;;;;;;;;;;;

(defun honest-report-setup ()
  (let ((report-buf (format-time-string "honest-report-%Y%m%d-%H%M%S")))
    (switch-to-buffer report-buf)))

;; snap:///usr/share/emacs/21.4/lisp/mail/emacsbug.el#136:(insert (mapconcat (lambda (key)
(defun honest-report-recent-keys ()
  (mapconcat (lambda (key)
               (if (or (integerp key)
                       (symbolp key)
                       (listp key))
                   (single-key-description key)
                 (prin1-to-string key nil)))
             (recent-keys)
             " "))

(defun honest-report-screenshot ()
  (mapconcat (lambda (w)
               (with-current-buffer (window-buffer w)
                 (let ((b (max (window-start w) (point-min)))
                       (e (min (window-end w t) (point-max))))
                   (format "--- %s ---\n%s"
                           w
                           (buffer-substring-no-properties b e)))))
             (honest-report-window-list)
             "\n"))

(defun honest-report-window-list ()
  "Mimic `window-list'.
This function exists only for emacs20 (and meadow-1.15),
which lack `window-list'."
  (let ((ws nil))
    (walk-windows (lambda (w) (setq ws (cons w ws))))
    (reverse ws)))

(defun honest-report-message ()
  (with-current-buffer (or (get-buffer "*Messages*")
                           (get-buffer " *Message-Log*"))
    (save-excursion
      (goto-char (point-max))
      (forward-line -10)
      (buffer-substring-no-properties (point) (point-max)))))

(defun honest-report-version ()
  (mapconcat (lambda (sv) (format "[%s] %s" (car sv) (cdr sv)))
             (honest-report-version-assoc)
             "\n"))

(defun honest-report-version-assoc ()
  (remove nil
          `(
            ("Emacs" . ,(format "%s (%s) of %s"
                              emacs-version
                              system-configuration
                              (honest-report-emacs-build-time)))
            ("system" . ,system-type)
            ("window system" . ,window-system)
            ,(let ((f 'Meadow-version))
               ;; cheat to avoid warning while byte-compilation.
               (and (fboundp f)
                    (cons "Meadow" (funcall f))))
            ("ENV" . ,(mapconcat (lambda (v) (format "%s=%s" v (getenv v)))
                                 '("LC_ALL" "LC_CTYPE" "LANGUAGE" "LANG")
                                 ", "))
            )))

(defun honest-report-emacs-build-time ()
  (if (stringp emacs-build-time)
      emacs-build-time  ;; xemacs
    (format-time-string "%Y-%m-%d"
                        emacs-build-time)))

;;;;;;;;;;;;;

(provide 'honest-report)

;;; honest-report.el ends here
