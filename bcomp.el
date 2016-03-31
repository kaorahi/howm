;;; -*- Emacs-Lisp -*-
(require 'cl)

(setq bcomp-files
      '(
	"howm.el" "howm-menu.el" "howm-reminder.el" "howm-date.el" "howm-misc.el" "howm-mode.el" "howm-view.el" "howm-backend.el" "howm-common.el" "howm-vars.el" "howm-version.el" "howm-lang-en.el" "howm-lang-ja.el" "howm-menu-en.el" "howm-menu-ja.el" "honest-report.el" "action-lock.el" "riffle.el" "gfunc.el" "illusion.el" "cheat-font-lock.el" 
	))

(let* ((dir (expand-file-name default-directory))
       (load-path (cons dir load-path))
       file)
  (message "deleting old .elc files...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt "c"))
    (if (file-exists-p file)
	(delete-file file)))

  (message "compiling...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt))
    (byte-compile-file file t))

  (message "done"))
