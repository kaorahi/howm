(setq load-path (cons default-directory load-path))
(setq debug-on-error t)
(setq inhibit-startup-message t)

(setq howm-sample-directory (expand-file-name "sample/"))
(setq howm-directory howm-sample-directory)
(setq howm-keyword-file (expand-file-name ".howm-keys" howm-sample-directory))
(setq howm-history-file (expand-file-name ".howm-history" howm-sample-directory))
;(setq howm-menu-lang 'ja)
(setq howm-history-limit nil)  ;; Don't erase my ~/.howm-history.

;; for experiments
(require 'org)
(require 'howm-org)
(setq howm-file-name-format "%Y-%m-%d.org")  ;; overwrite

;(require 'howm-markdown)
;(setq howm-file-name-format "%Y-%m-%d.md")  ;; overwrite

(require 'howm)
(howm-test)
