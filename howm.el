;;; -*- lexical-binding: nil; -*-
;;; howm.el --- Wiki-like note-taking tool
;;; howm.el.  Generated from howm.el.in by configure.

;; Copyright (C) 2002, 2003, 2004, 2005-2025 HIRAOKA Kazuyuki

;; Author: HIRAOKA Kazuyuki <kakkokakko@gmail.com>
;; URL: https://kaorahi.github.io/howm/
;; Version: 1.5.3
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; The GNU General Public License is available by anonymouse ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;--------------------------------------------------------------------

;;; Commentary:
;; See README.
;; HOWM is acronym of "Hitori Otegaru Wiki Modoki".

;;; Code:

(defconst howm-version "1.5.3")

;; Files [0]-[3] have only hierarchical dependencies,
;; whereas files in [5] have circular dependencies.
;; Each file in [5] provides itself first,
;; and then requires [4] (this file).

;;; [0] official

(require 'easy-mmode)
(require 'font-lock)

;;; [1] cl

(require 'cl-lib)

;;; [2] howm basic libraries

(require 'howm-vars)
(require 'howm-common)

;;; [3] my tools

;; This doesn't work in byte-compilation. I don't understand it. Sigh...
;; (mapcar #'require howm-required-features)

(require 'cheat-font-lock)
(require 'illusion)
(require 'gfunc)
(require 'riffle)
(require 'action-lock)
(require 'iigrep)
(require 'honest-report)

;;; [4] howm main (this file)

(provide 'howm)

;;; [5] howm modules

(require 'howm-backend)
(require 'howm-view)
(require 'howm-mode)
(require 'howm-misc)
(require 'howm-date)
(require 'howm-reminder)
(require 'howm-menu)

;;; for howmz.el [2006-02-02]
;;; http://noir.s7.xrea.com/archives/000136.html
;;; http://noir.s7.xrea.com/pub/zaurus/howmz.el

(howm-require-lang 'en)
(howm-require-lang 'fr)
(howm-require-lang 'ja)

;;; security fix [2006-12-16]
(mapc (lambda (symbol) (put symbol 'risky-local-variable t))
      (howm-symbols))

;;; howm.el ends here
