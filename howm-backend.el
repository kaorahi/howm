;;; howm-backend.el --- Wiki-like note-taking tool
;;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016
;;;   HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-backend.el,v 1.50 2012-12-29 08:57:18 hira Exp $
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

(provide 'howm-backend)
(require 'howm)

;; in preparation at now.
;; many WRONG COMMENTS and TENTATIVE CODES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class Folder

(defun howm-folder ()
  (howm-make-folder:files (howm-search-path)))

;; * class Folder: abstraction of directory

;; (Wrong comments. Ignore me.)
;;   * grep(pattern, fixed, case_insensitive)
;;     * list of items
;;   * new_page
;;   * all_pages
;;   * all_keys
;;   * add_keys
;;   * keys_in(page)
;;     * This method is optional.

(defun howm-folder-type (folder &rest r)
  (cond ((stringp folder) ':dir)
        ((eq folder 'buf) ':buf)
        ((listp folder) (car folder))))

(howm-defvar-risky howm-folder-dispatchers (list #'howm-folder-type))

(gfunc-with howm-folder-dispatchers
  (gfunc-def howm-folder-items (folder &optional recursive-p)
    "All pages in FOLDER is returned as list of items.
When RECURSIVE-P is non-nil, pages in subfolders are also listed.")
  (gfunc-def howm-folder-grep-internal (folder pattern &optional fixed-p)
    "In FOLDER, PATTERN is searched.
Result is returned as list of items. When FIXED-P is nil, PATTERN is
regarded as regular expression.")
  ;; need to suppor below for howm-directory
  (gfunc-def howm-folder-get-page-create (folder page-name)
    "In FOLDER, get page whose name is PAGE-NAME.
If corresponding page does not exist, new page is created.
Return value is a cons pair of page and flag.
Flag is non-nil if new page is created.")
  (gfunc-def howm-folder-territory-p (folder name)
    "Non nil if FOLDER should own NAME.")
  )

;; (gfunc-def-with howm-folder-dispatchers
;;   (howm-folder-items (folder &optional recursive-p)
;;     "All pages in FOLDER is returned as list of items.
;; When RECURSIVE-P is non-nil, pages in subfolders are also listed.")
;;   (howm-folder-grep-internal (folder pattern &optional fixed-p)
;;     "In FOLDER, PATTERN is searched.
;; Result is returned as list of items. When FIXED-P is nil, PATTERN is
;; regarded as regular expression.")
;;   )

(defun howm-folder-match-under-p (dir regexp filename)
  (and (eq (howm-folder-type dir) ':dir)
       (string-match regexp (file-relative-name filename dir))))

(defun howm-make-folder-from-items (items)
  (howm-make-folder:pages (howm-cl-remove-duplicates* (mapcar #'howm-item-page
                                                              items)
                                                      :test #'howm-page=)))

;;;
;;; dir folder: single directory
;;;

(defun howm-make-folder:dir (dir)
  dir)

(defun howm-folder-items:dir (dir &optional recursive-p)
  (let ((files (if recursive-p
                    (howm-files-in-directory dir)
                 (directory-files dir t))))
    (howm-folder-items:files (howm-make-folder:files files))))

(defun howm-folder-grep-internal:dir (folder pattern &optional fixed-p)
  (howm-grep-items pattern folder fixed-p #'howm-exclude-p))

(defun howm-files-in-directory (path &optional dummy-exclusion-checker)
  "List files in PATH recursively, when PATH is a directory.
When PATH is a file, list of it is returned.
Some files and directories are ignored according to `howm-exclude-p'.
DUMMY-EXCLUSION-CHECKER has no effect; it should be removed soon."
  (howm-files-in-directory-sub (expand-file-name path)))

(defun howm-files-in-directory-sub (full-path &optional under)
  (let* ((top-call-p (null under))
         (excluded-p (if top-call-p
                         nil
                       (or (howm-exclude-p full-path)
                           ;; exclude "." & ".."
                           (not (howm-subdirectory-p under full-path
                                                     'strict))))))
    (cond (excluded-p
           nil)
          ((file-directory-p full-path)
           (cl-mapcan (lambda (s)
                             (howm-files-in-directory-sub s full-path))
                   (directory-files full-path t)))
          ((file-exists-p full-path)
           (list full-path))
          (t
           nil))))

;; ;; list files recursively
;; (defun howm-files-in-directory (dir &optional exclusion-checker)
;;   (when (null exclusion-checker)
;;     (setq exclusion-checker (lambda (x) nil)))
;;   (cond ((file-directory-p dir) (howm-files-in-directory-sub dir
;;                                                              exclusion-checker))
;;         ((file-exists-p dir) (list dir))
;;         (t nil)))

;; (defun howm-files-in-directory-sub (dir exclusion-checker)
;;   (cl-mapcan (lambda (f)
;;             (cond
;;              ((funcall exclusion-checker f) nil)
;;              ((file-directory-p f) (if (howm-subdirectory-p dir f t)
;;                                        (howm-files-in-directory f exclusion-checker)
;;                                      nil)) ;; exclude "." & ".."
;;              ((file-regular-p f) (list f))
;;              (t nil)))
;;           (directory-files dir t)))

(defun howm-folder-get-page-create:dir (folder page-name)
  (let* ((file (expand-file-name page-name folder))
         (dir (file-name-directory file))
         (createp (not (file-exists-p file))))
    (make-directory dir t)
    (cons (howm-make-page:file file) createp)))

(defun howm-folder-territory-p:dir (folder name)
  (howm-subdirectory-p folder name))

;;;
;;; pages folder: list of 'pages'
;;;

(defun howm-make-folder:pages (pages)
  (cons ':pages pages))

(defun howm-folder-pages:pages (folder)
  (cdr folder))

(defun howm-folder-items:pages (folder &optional recursive-p)
  (let ((summary ""))
    (mapcar (lambda (p) (howm-make-item p summary))
            (howm-folder-pages:pages folder))))

;; should be removed, or renamed at least
(defun howm-folder-files:pages (folder &optional exclusion-checker)
  (remove nil (mapcar #'howm-page-name (howm-folder-pages:pages folder))))

(defun howm-folder-grep-internal:pages (folder pattern &optional fixed-p)
  (let ((h (howm-classify #'howm-page-type (howm-folder-pages:pages folder) t)))
    ;; get result for each type
    (apply #'append (mapcar (lambda (p)
                              (let ((type (car p))
                                    (searcher (cdr p)))
                                (let ((pages (reverse (cdr (assoc type h)))))
                                  (funcall searcher pages pattern fixed-p))))
                            howm-folder-grep-internal:pages-searcher))))

(howm-defvar-risky howm-folder-grep-internal:pages-searcher
  '((:file . howm-folder-grep-internal:pages-files)
    (:buf  . howm-folder-grep-internal:pages-buffers)))
(defun howm-folder-grep-internal:pages-files (pages pattern fixed-p)
  (let ((files (mapcar #'howm-page-name pages)))
    (howm-folder-grep-internal:files (howm-make-folder:files files)
                                     pattern fixed-p)))
(defun howm-folder-grep-internal:pages-buffers (pages pattern fixed-p)
  (let ((bufs pages)
        (r (howm-fake-grep-regexp pattern fixed-p))
        (c *howm-view-force-case-fold-search*))
    (let ((grep-result (cl-mapcan
                        (lambda (b)
                          (if (howm-buffer-killed-p b)
                              nil
                            (with-current-buffer b
                              (howm-fake-grep-current-buffer r b c))))
                        bufs)))
      (mapcar (lambda (g)
                (let ((buf (car g))
                      (place (cadr g))
                      (content (cl-caddr g)))
                  (howm-make-item (howm-make-page:buf buf) content place)))
              grep-result))))

(defun howm-list-buffers (&optional all)
  "Show buffer list. If ALL is non-nil, hidden buffers are also listed."
  (interactive "P")
  (let* ((bufs (if all
                   (buffer-list)
                 (cl-remove-if
                  (lambda (b)
                    (let ((name (buffer-name b)))
                      (or (null name)
                          (string-match "^ " name)
                          (member name howm-list-buffers-exclude)
                          (with-current-buffer b
                            (member major-mode
                                    '(howm-view-summary-mode
                                      howm-view-contents-mode))))))
                  (buffer-list))))
         (pages (mapcar (lambda (b) (howm-make-page:buf b)) bufs))
         (folder (howm-make-folder:pages pages)))
    (howm-view-directory folder)))
(defun howm-occur (regexp)
  "Show all lines in the current buffer containing a match for REGEXP."
  (interactive "sSearch (regexp): ")
  (let ((howm-view-use-grep (if howm-occur-force-fake-grep
                                nil
                              howm-view-use-grep)))
    (howm-view-search-folder regexp
                             (howm-make-folder:pages
                              (list (howm-make-page:buf (current-buffer)))))))
(defun howm-list-mark-ring ()
  "Show all marks in the current buffer."
  (interactive)
  (let* ((page (howm-make-page:buf (current-buffer)))
         (items (mapcar (lambda (m)
                          (let ((place (riffle-get-place m))
                                (summary (save-excursion
                                           (goto-char m)
                                           (let ((b (line-beginning-position))
                                                 (e (line-end-position)))
                                             (buffer-substring b e)))))
                            (howm-make-item page summary place)))
                        (howm-cl-remove-duplicates*
                         (cons (mark-marker) mark-ring)
                         :test #'howm-mark-same-line-p))))
    (howm-view-summary "<marks>" items)))
(defun howm-mark-same-line-p (m1 m2)
  (apply #'=
         (mapcar (lambda (m)
                   (save-excursion
                     (goto-char m)
                     (line-beginning-position)))
                 (list m1 m2))))

;;;
;;; files folder: list of file names
;;;

;;; This folder is treated specially for efficient search.

;;; Fix me: [2005-02-17]
;;; Sorry. I can't remember whether 'file' means really 'file' only.
;;; It may be 'file or directory'.

;; Try this to check it.
;; (setq howm-menu-top nil)
;; (setq howm-menu-file (expand-file-name "sample/0000-00-00-000000.howm"))
;; (setq howm-directory (howm-make-folder:files (mapcar (lambda (f) (expand-file-name f "sample/")) '("top.txt" "search.txt"))))

(defun howm-make-folder:files (files)
  (cons ':files files))

(defun howm-folder-items:files (folder &optional recursive-p)
  (let ((summary ""))
    (mapcar (lambda (f)
              (howm-make-item (howm-make-page:file f) summary))
            (howm-folder-files:files folder))))

(defun howm-folder-grep-internal:files (folder pattern &optional fixed-p)
  (howm-grep-items pattern (howm-folder-files:files folder) fixed-p))

;; should be removed, or renamed at least
(defun howm-folder-files:files (folder &optional exclusion-checker)
  (cdr folder))

;;;
;;; nest folder: list of folders
;;;

;; Try this to check it.
;; (setq howm-menu-top nil)
;; (setq howm-menu-file (expand-file-name "sample/0000-00-00-000000.howm"))
;; (setq howm-directory (howm-make-folder:nest (mapcar #'expand-file-name '("sample" "/usr/share/emacs/site-lisp/navi2ch"))))

(defun howm-make-folder:nest (list-of-folders)
  (cons ':nest list-of-folders))

(defun howm-folder-subfolders (self)
  (cdr self))

(defun howm-folder-items:nest (folder &optional recursive-p)
  (cl-mapcan (lambda (f) (howm-folder-items f recursive-p))
                  (howm-folder-subfolders folder)))

(defun howm-folder-grep-internal:nest (folder pattern &optional fixed-p)
  (cl-mapcan (lambda (f) (howm-folder-grep-internal f pattern fixed-p))
                  (howm-folder-subfolders folder)))

;;;
;;; namazu folder: namazu index directory
;;;

;; (cf.) Namazu: a Full-Text Search Engine http://www.namazu.org/index.html.en

;; test:
(defun howm-search-namazu (dir pattern)
  (interactive "Dindex directory: 
ssearch: ")
  (let ((folder (howm-make-folder:namazu (expand-file-name dir))))
    (howm-view-summary "<namazu>"
                       (howm-view-search-folder-items pattern folder))))

(defun howm-make-folder:namazu (index-dir)
  (cons ':namazu (expand-file-name index-dir)))

(defun howm-folder-items:namazu (folder &optional recursive-p)
  (let ((files (howm-folder-files:namazu folder)))
    (howm-folder-items:files (howm-make-folder:files files))))

;; should be removed, or renamed at least
(defun howm-folder-files:namazu (folder &optional exclusion-checker)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "NMZ.r"
                                            (cdr folder)))
    (split-string (buffer-substring-no-properties (point-min)
                                                  (point-max))
                  "[\n\r\v]+")))

(defun howm-folder-grep-internal:namazu (folder pattern-list &optional fixed-p)
  (let* ((index-dir (cdr folder))
         (namazu-pattern (mapconcat #'identity pattern-list " or "))
         (hits (with-temp-buffer
                 (call-process "namazu" nil t nil
                               "-l" "-a" namazu-pattern index-dir)
                 (split-string (buffer-substring-no-properties (point-min)
                                                               (point-max))
                               "[\n\r\v]+")))
         (files (cl-remove-if (lambda (f) (not (file-exists-p f))) hits)))
    ;; grep again
    (let ((howm-view-use-grep nil)) ;; Japanese encoding is annoying.
      (howm-folder-grep-internal (howm-make-folder:files files)
                                 pattern-list fixed-p))))

;;;
;;; rot13dir folder: almost same as dir folder except that files are rot13ed.
;;;

(defun howm-make-folder:rot13dir (dir)
  (cons ':rot13dir dir))

(defun howm-folder-items:rot13dir (folder &optional recursive-p)
  (let ((files (if recursive-p
                   (howm-files-in-directory (cdr folder))
                 (directory-files (cdr folder) t))))
    (mapcar (lambda (f)
              (howm-make-item (howm-make-page:rot13file f)))
            files)))

(defun howm-folder-grep-internal:rot13dir (folder pattern-list &optional fixed-p)
  (let* ((dir (cdr folder))
         (ps (mapcar (lambda (p) (yarot13-rotate-string p)) pattern-list))
         (is (howm-folder-grep-internal:dir dir ps fixed-p)))
    (mapc (lambda (i)
            (let ((file (howm-page-name (howm-item-page i)))
                  (summary (howm-item-summary i)))
              (howm-item-set-page i (howm-make-page:rot13file file))
              (howm-item-set-summary i (yarot13-rotate-string summary))))
            is)
    is))

;;; For backward compatibility. Don't use it any more.

(defalias 'howm-view-directory-items  #'howm-folder-items)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grep

;; Fix me:
;; Name of arguments are inappropriate.
;; Pattern and str may be list of strings.
;; File-list may be a string.

(defun howm-folder-grep (folder pattern &optional fixed-p)
  (when (stringp pattern)
    (setq pattern (list pattern)))
  (howm-folder-grep-internal folder pattern fixed-p))

(defvar *howm-view-force-case-fold-search* nil) ;; dirty!
(howm-defvar-risky howm-view-grep-log-file nil)
(defvar howm-view-grep-log-format "> %s | %s")

(defun howm-grep-items (str file-list &optional fixed-p exclusion-checker)
  (let* ((found (howm-grep str file-list fixed-p))
         (items (mapcar (lambda (z)
                          (let ((file (car z))
                                (place (cadr z))
                                (content (cl-caddr z)))
                            (if (and exclusion-checker
                                     (funcall exclusion-checker file))
                                nil
                              (howm-make-item file content place))))
                        found)))
    (if exclusion-checker
        (remove nil items)
      items)))

(defun howm-grep (str file-list &optional fixed-p)
  (when howm-view-grep-log-file
    (howm-write-log str howm-view-grep-log-format howm-view-grep-log-file))
  (when (stringp file-list)
    (setq file-list (list file-list)))
  (let ((grep-func (cond ((eq howm-view-use-grep t) 'howm-real-grep)
                         ((null howm-view-use-grep) 'howm-fake-grep)
                         ((functionp howm-view-use-grep) howm-view-use-grep)
                         (t (error "No function %s." howm-view-use-grep)))))
    (funcall grep-func
             str file-list fixed-p *howm-view-force-case-fold-search*)))

(defun howm-real-grep (str file-list &optional fixed-p force-case-fold)
  "Call grep and parse its result.
'((file line-number line) (file line-number line) ...)
"
  (if (howm-grep-multi-p)
      (howm-real-grep-multi str file-list fixed-p force-case-fold)
    (howm-real-grep-single str file-list fixed-p force-case-fold)))

(defun howm-grep-multi-p ()
  howm-view-grep-file-stdin-option)

;; obsolete
(defun howm-real-grep-single (str file-list
                                  &optional fixed-p force-case-fold)
  "Call grep and parse its result.
'((file line-number line) (file line-number line) ...)
"
  (when (listp str)
    (if (null (cdr str))
        (setq str (car str))
      (error "Multiple patterns are not supported: %s" str)))
  (let ((grep-command (or (and fixed-p howm-view-fgrep-command)
                          howm-view-grep-command))
        (opt (split-string howm-view-grep-option))
        (eopt (and howm-view-grep-expr-option
                   (list howm-view-grep-expr-option)))
        (case-fold (or force-case-fold
                       (not (let ((case-fold-search nil))
                              (string-match "[A-Z]" str))))))
    (cl-labels ((add-opt (pred x) (when (and pred x) (setq opt (cons x opt)))))
      (add-opt case-fold howm-view-grep-ignore-case-option)
      (add-opt fixed-p howm-view-grep-fixed-option)
      (add-opt (not fixed-p) howm-view-grep-extended-option))
    (with-temp-buffer
      (let* ((fs (howm-expand-file-names file-list))
             (lines (howm-call-process* grep-command
                                        `(,@opt ,@eopt ,str) fs))
             (parsed (mapcar 'howm-grep-parse-line lines)))
        (remove nil parsed)))))

(defun howm-real-grep-multi (str file-list &optional fixed-p force-case-fold)
  (let ((grep-command (or (and fixed-p howm-view-fgrep-command)
                          howm-view-grep-command))
        (opt (split-string howm-view-grep-option))
        (eopt (split-string howm-view-grep-file-stdin-option)))
    (let* ((str-list (cond ((stringp str) (list str))
                           ((listp str) str)
                           (t (error "Wrong type: %s" str))))
           (caps-p (cl-member-if (lambda (s) (howm-capital-p s)) str-list))
           (case-fold (or force-case-fold (not caps-p))))
      (cl-labels ((add-opt (pred x) (when (and pred x) (setq opt (cons x opt)))))
        (add-opt case-fold howm-view-grep-ignore-case-option)
        (add-opt fixed-p howm-view-grep-fixed-option)
        (add-opt (not fixed-p) howm-view-grep-extended-option))
      (with-temp-buffer
        (let* ((fs (howm-expand-file-names file-list))
               (pat (apply #'concat
                           (mapcar (lambda (s) (concat s "\n")) str-list)))
               (lines (howm-call-process* grep-command
                                          `(,@opt ,@eopt) fs
                                          nil pat))
               (parsed (mapcar 'howm-grep-parse-line lines)))
          (remove nil parsed))))))

(defun howm-fake-grep (str file-list &optional fixed-p force-case-fold)
  "Search STR in files.
Return a list ((name number str) (name number str) ...), where
name is file name, number is line number, and str is line content.
FILE-LIST is list of file names.
If FIXED-P is non-nil, regexp search is performed.
If FIXED-P is nil, fixed string search is performed.
When STR has no capital letters or FORCE-CASE-FOLD is non-nil,
difference of capital letters and small letters are ignored.

Extended feature:
STR can be list of strings. They are regarded as 'or' pattern of all elements."
  (cl-mapcan (lambda (file)
                    (howm-fake-grep-file (howm-fake-grep-regexp str fixed-p)
                                         file force-case-fold))
                  (cl-mapcan #'howm-files-in-directory file-list)))

(defun howm-fake-grep-regexp (str &optional fixed-p)
  (let ((str-list (if (stringp str) (list str) str)))
    (if fixed-p
        (regexp-opt str-list)
      (mapconcat (lambda (s) (format "\\(%s\\)" s)) str-list "\\|"))))

(defun howm-fake-grep-file (reg file force-case-fold)
  (let ((b (get-file-buffer file)))
    (if (and b howm-view-watch-modified-buffer)
        (with-current-buffer b
          (howm-fake-grep-current-buffer reg file force-case-fold))
      (with-temp-buffer
        (insert-file-contents file)
        (howm-fake-grep-current-buffer reg file force-case-fold)))))

(defun howm-fake-grep-current-buffer (reg file force-case-fold)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let* ((found nil)
             (case-fold-search (or force-case-fold (not (howm-capital-p reg)))))
        (while (re-search-backward reg nil t)
          (beginning-of-line)
          (setq found
                (cons (list file
                            (riffle-get-place)
                            (buffer-substring-no-properties (point)
                                                            (line-end-position)))
                      found)))
        found))))

(defun howm-grep-parse-line (line)
  (if (string-match "^\\(\\([a-zA-Z]:/\\)?[^:]*\\):\\([0-9]*\\):\\(.*\\)$"
                    line)
      (let ((file (match-string 1 line))
            (line (string-to-number (match-string 3 line)))
            (content (match-string 4 line)))
        (list file line content))
    nil))

;; For backward compatibility. Don't use them any more.
(defalias 'howm-view-grep #'howm-grep)
(defalias 'howm-view-call-process #'howm-call-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class Page

;; * class Page: abstraction of file

;; Fix me: confusion between 'page name' and 'file name',
;; especially for a buffer.

;; (Wrong comments. Ignore me.)
;;   * folder
;;   * name
;;   * created_time
;;   * modified_time
;;   * load
;;   * save(text)

(defun howm-page-type (page &rest r)
  (cond ((stringp page) ':file)
        ((bufferp page) ':buf)
        ((null page) ':nil)
        ((listp page) (car page))))

(howm-defvar-risky howm-page-dispatchers (list #'howm-page-type))

(gfunc-with howm-page-dispatchers
  (gfunc-def howm-page-name   (page))
  (gfunc-def howm-page-mtime  (page))
  (gfunc-def howm-page-open   (page))
  (gfunc-def howm-page-insert (page))
  (gfunc-def howm-page-viewer (page))
  (gfunc-def howm-page-set-configuration (page))
  )

(defun howm-page= (x y)
  (equal x y))

(defun howm-page-abbreviate-name (page)
  (howm-abbreviate-file-name (format "%s" (howm-page-name page))))

(defalias 'howm-save-buffer #'save-buffer)

(defun howm-insert-buffer-contents (buffer)
  (insert (with-current-buffer buffer
            (save-restriction
              (widen)
              (let ((limit (point-max)))
                (when howm-view-contents-limit
                  (setq limit (min limit howm-view-contents-limit)))
                (buffer-substring-no-properties (point-min) limit))))))

;; (defun howm-page-insert-range ()
;;   (let ((limit (point-max)))
;;     (when howm-view-contents-limit
;;       (setq limit (min limit howm-view-contents-limit)))
;;     (list (point-min) limit)))

;; (defun howm-page-save (&optional args)
;;   (interactive "p")
;;   (with-current-buffer (get-file-buffer (howm-page-name howm-buffer-page))
;;     (apply #'save-buffer args)))

;; (defun howm-save-buffer (&optional args)
;;   (interactive "p")
;;   (prog1
;;       (save-buffer args)
;;     (howm-after-save)))

;;;
;;; file page: name of file
;;;

(defun howm-make-page:file (filename)
  filename)

(defun howm-page-name:file (page)
  page)

(defun howm-page-mtime:file (page)
  (nth 5 (file-attributes (howm-page-name page))))

(defun howm-page-open:file (page)
  (find-file (howm-page-name page))
  ;; widen is desired when corresponding file is already opened and
  ;; its buffer is narrowed.
  (widen))

(defun howm-page-insert:file (page)
  (let ((b (get-file-buffer page)))
    (if (and b
             howm-view-watch-modified-buffer
             (not howm-view-use-grep))
        (howm-insert-buffer-contents b)
      (howm-insert-file-contents page))))

(defun howm-page-viewer:file (page)
  (let* ((ls (lambda (dir)
               (with-temp-buffer
                 (insert-directory dir "-l")
                 (buffer-substring-no-properties (point-min) (point-max)))))
         (dir-viewer (and (file-directory-p page)
                          (howm-make-viewer:func #'find-file ls)))
         (viewer (cdr (cl-assoc-if (lambda (reg) (string-match reg page))
                                        howm-view-external-viewer-assoc))))
    (or viewer dir-viewer
        (and howm-view-use-mailcap
             (let* ((ext (if (string-match "\\.[^\\.]+$" page)
                             (match-string 0 page)
                           ""))
                    (type (howm-funcall-if-defined
                              (mailcap-extension-to-mime ext)))
                    (type-match (lambda (r) (string-match r type))))
               (cond ((null type)
                      nil)
                     ((cl-member-if type-match howm-view-open-by-myself)
                      nil)
                     (t
                      (howm-funcall-if-defined
                          (mailcap-mime-info type)))))))))

(defun howm-page-set-configuration:file (page)
  (howm-set-configuration-for-file-name page))

;;;
;;; buffer page: buffer object
;;;

(defun howm-make-page:buf (buf)
  buf)

(defun howm-page-name:buf (page)
  (buffer-name page))

(defconst howm-dummy-mtime (encode-time 0 0 9 1 1 1970)
  "Dummy mtime which has no meaning.")

(defun howm-page-mtime:buf (page)
  howm-dummy-mtime)

(defun howm-page-open:buf (page)
  (switch-to-buffer page))

(defun howm-page-insert:buf (page)
  (when (not (howm-buffer-killed-p page))
    (howm-insert-buffer-contents page)))

(defun howm-page-viewer:buf (page)
  nil)
;;   (howm-make-viewer:func #'switch-to-buffer))

(defun howm-page-set-configuration:buf (page)
  (when (buffer-file-name page)
    (howm-set-configuration-for-file-name (buffer-file-name page))))

;;;
;;; nil page: dummy page
;;;

(defun howm-make-page:nil ()
  nil)

(defun howm-page-name:nil (page)
  "")

(defun howm-page-mtime:nil (page)
  howm-dummy-mtime)

(defun howm-page-open:nil (page)
  "Do nothing."
  nil)

(defun howm-page-insert:nil (page)
  "Do nothing."
  nil)

(defun howm-page-viewer:nil (page)
  nil)

(defun howm-page-set-configuration:nil (page)
  "Do nothing."
  nil)

;;;
;;; rot13file page: almost same as file except that it is rot13ed
;;;

(defun howm-make-page:rot13file (filename)
  (cons ':rot13file filename))

(defun howm-page-name:rot13file (page)
  (howm-page-name (cdr page)))

(defun howm-page-mtime:rot13file (page)
  (howm-page-mtime:file (cdr page)))

(defun howm-page-open:rot13file (page)
  (yarot13-find-file (howm-page-name page))
  )

(defun howm-page-insert:rot13file (page)
  (yarot13-insert-file-contents (howm-page-name page)))

(defun howm-page-viewer:rot13file (page)
  nil)

(defun howm-page-set-configuration:rot13file (page)
  (howm-set-configuration-for-file-name (howm-page-name page)))

;;; Clean me.

;; (defun howm-file-path (&optional time)
;;   (expand-file-name (howm-file-name time) howm-directory))

(defun howm-create-file (&optional keep-cursor-p)
  (let* ((pc (howm-folder-get-page-create howm-directory (howm-file-name)))
         (page (car pc))
         (createp (cdr pc)))
    (howm-page-open page)
    (when (not keep-cursor-p)
      (widen)
      (goto-char (point-max)))
    (when createp
      (run-hooks 'howm-create-file-hook))
    createp))

;; (defun howm-create-file (&optional keep-cursor-p)
;;   (let* ((file (howm-file-path))
;;          (dir (file-name-directory file))
;;          (createp (not (file-exists-p file))))
;;     (make-directory dir t)
;;     (howm-page-open file)
;;     (when createp
;;       (run-hooks 'howm-create-file-hook))
;;     (when (not keep-cursor-p)
;;       (widen)
;;       (goto-char (point-max)))
;;     createp))

;;; viewer

;; Viewer is one of the following.
;; func    ==> (func) is called after (find-file page).
;; (func)  ==> (func page) is called.
;; (func . previewer)
;;   ==> (func page) and (previewer page) are called for open and preview
;;   (previewer must return a string).
;; "str"   ==> (format "str" page) is externally executed on shell.

(defun howm-viewer-type (viewer &rest r)
  (cond ((stringp viewer)   ':str)
        ((functionp viewer) ':func0)
        ((listp viewer)     ':func)))

(howm-defvar-risky howm-viewer-dispatchers (list #'howm-viewer-type))

(gfunc-with howm-viewer-dispatchers
  (gfunc-def howm-viewer-call      (viewer page))
  (gfunc-def howm-viewer-indicator (viewer page))
)

(defun howm-make-viewer:func (f &optional previewer)
  (cons f previewer))

(when howm-view-use-mailcap
  (require 'mailcap)
  (howm-funcall-if-defined (mailcap-parse-mailcaps))
  (howm-funcall-if-defined (mailcap-parse-mimetypes)))

(defun howm-viewer-call:str (viewer page)
  (start-process "howm-view-external-viewer" nil
                 shell-file-name
                 shell-command-switch
                 (format viewer (howm-page-name page))))
(defun howm-viewer-call:func0 (viewer page)
  (howm-page-open page)
  (funcall viewer))
(defun howm-viewer-call:func (viewer page)
  (funcall (car viewer) page))

(defvar howm-viewer-indicator-format "%%%%%% %s %%%%%%")
(defun howm-viewer-indicator-gen (fmt &rest args)
  (format howm-viewer-indicator-format
          (apply #'format (cons fmt args))))
(defun howm-viewer-indicator:str (viewer page)
  (howm-viewer-indicator-gen viewer (howm-page-name page)))
(defun howm-viewer-indicator:func0 (viewer page)
  (howm-viewer-indicator-gen "%S %S" viewer page))
(defun howm-viewer-indicator:func (viewer page)
  (let ((func (car viewer))
        (previewer (cdr viewer)))
    (if previewer
        (funcall previewer page)
      (howm-viewer-indicator-gen "(%S %S)" func page))))

(defadvice action-lock-find-file (around external-viewer (f u) activate)
  (let ((viewer (howm-page-viewer f)))
    (if viewer
        (howm-viewer-call viewer (expand-file-name f))
      ad-do-it)))

;; For backward compatibility. Don't use them any more.
(defalias 'howm-view-external-viewer      #'howm-page-viewer)
(defalias 'howm-view-call-external-viewer #'howm-viewer-call)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class Item

;; Fix me: confusion between howm-item-page and howm-item-name

;; * class Item: abstraction of hit position in file
;;   * page
;;   * place
;;   * and conventional properties

(defun howm-make-item (page &optional summary place offset home privilege)
  (list page summary place offset home privilege))
(defun howm-item-page      (item) (nth 0 item)) ;; page can be nil.
(defun howm-item-summary   (item) (howm-item-nth 1 item ""))
(defun howm-item-place     (item) (howm-item-nth 2 item nil))
(defun howm-item-offset    (item) (howm-item-nth 3 item nil))
(defun howm-item-home      (item) (howm-item-nth 4 item nil))
(defun howm-item-privilege (item) (howm-item-nth 5 item nil))
(defun howm-item-nth (n item default)
  (or (nth n item) default))
(defun howm-item-set-page (item val)
  (setf (nth 0 item) val))
(defun howm-item-set-summary (item val)
  (setf (nth 1 item) val))
(defun howm-item-set-offset (item val)
  (setf (nth 3 item) val))
(defun howm-item-set-home (item val)
  (setf (nth 4 item) val))
(defun howm-item-set-privilege (item val)
  (setf (nth 5 item) val))

(defun howm-item-name (item)
  (format "%s" (howm-page-name (howm-item-page item))))

(defun howm-item-dup (item) (mapcar #'identity item))

;; For backward compatibility. Don't use them any more.
;; ;; item = (filename summary place offset home)
(defun howm-view-make-item (filename &rest r)
  (apply #'howm-make-item (cons (howm-make-page:file filename) r)))
(defalias 'howm-view-item-filename      #'howm-item-name)
(defalias 'howm-view-item-summary       #'howm-item-summary)
(defalias 'howm-view-item-place         #'howm-item-place)
(defalias 'howm-view-item-offset        #'howm-item-offset)
(defalias 'howm-view-item-home          #'howm-item-home)
(defalias 'howm-view-item-privilege     #'howm-item-privilege)
(defalias 'howm-view-item-set-summary   #'howm-item-set-summary)
(defalias 'howm-view-item-set-offset    #'howm-item-set-offset)
(defalias 'howm-view-item-set-home      #'howm-item-set-home)
(defalias 'howm-view-item-set-privilege #'howm-item-set-privilege)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search path

;; historical & awkward mechanism

(howm-defvar-risky howm-search-path nil)
(defvar howm-search-other-dir nil)
(defvar *howm-independent-directories* nil) ;; for internal use

(defun howm-independent-search-path ()
  (let ((c default-directory))
    (and c
         (car (cl-member-if (lambda (dir) (howm-subdirectory-p dir c))
                                 *howm-independent-directories*)))))

(defun howm-search-path (&optional ignore-independent-search-path)
  (let ((d (howm-independent-search-path)))
    (cond ((and d (not ignore-independent-search-path)) (list d))
          (howm-search-other-dir (howm-search-path-multi))
          (t (howm-search-path-single)))))
(defun howm-search-path-single ()
  (list howm-directory))
(defun howm-search-path-multi ()
  (cons howm-directory howm-search-path))

(defun howm-search-path-folder (&optional ignore-independent-search-path)
  (howm-make-folder:nest (howm-search-path ignore-independent-search-path)))

(defun howm-toggle-search-other-dir (&optional arg)
  "Change whether `howm-search-path' is searched or not.
With arg, search `howm-search-path' iff arg is positive."
  (interactive "P")
  (setq howm-search-other-dir
        (if arg
            (> (prefix-numeric-value arg) 0)
          (not howm-search-other-dir)))
  (message "howm search-path = %s" (howm-search-path)))

(defun howm-open-directory-independently (dir)
  (interactive "DDirectory: ")
  (add-to-list '*howm-independent-directories*
               (expand-file-name dir))
  (let ((default-directory dir))
    (howm-normalize-show "" (howm-folder-items dir t))
    (howm-keyword-add-items (howm-view-item-list))))

(defvar howm-keyword-buffer-name-format " *howm-keys:%s*")
(defun howm-keyword-buffer ()
  (let* ((dir (howm-independent-search-path))
         (buffer-name (format howm-keyword-buffer-name-format
                              (if dir (expand-file-name dir) ""))))
    (if dir
        (get-buffer-create buffer-name)
      (howm-get-buffer-for-file (howm-keyword-file) buffer-name))))

;;; exclusion

;; Fix me on inefficiency.
;; 
;; [2005-02-18] I can't remember why I checked relative path in old versions.
;; [2005-04-24] Now I remember the reason.
;; Some people like ~/.howm/ rather than ~/howm/ as their howm-directory.
;; It must be included even if it matches to howm-excluded-file-regexp.
;; 
;; Bug: (howm-exclude-p "~/howm/CVS") != (howm-exclude-p "~/howm/CVS/")
(defun howm-exclude-p (filename)
  (not (cl-find-if-not
        (lambda (dir) (howm-folder-match-under-p dir
                                                 howm-excluded-file-regexp
                                                 filename))
        (howm-search-path))))

;;; howm-backend.el ends here
