;;; howm-lang-fr.el --- Wiki-like note-taking tool
;;; Copyright (C) 2005-2022
;;;   HIRAOKA Kazuyuki <khi@users.osdn.me>
;;;
;;; French translation by french-speaking howm users
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

(require 'howm-common)

(defvar howm-day-of-week-fr '("Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"))

(defvar howm-menu-command-table-fr
  `(
    ("[Se souvenir]" howm-remember previous)
    ("[Nouveau]" (lambda () (howm-create ,howm-menu-action-arg)))
    ("[Ajouter]" (lambda () (howm-create-here ,howm-menu-action-arg)))
    ("[Dup]" howm-dup)
    ("[Màj]" howm-initialize-buffer previous)
    ("[Regexp]" howm-list-grep)
    ("[Chaine]" howm-list-grep-fixed)
    ;;         ("[roma]" howm-list-migemo)
    ("[Aujourd'hui]" howm-find-today)
    ("[Hier]" howm-find-yesterday)
    ("[Tout]" howm-list-all)
    ("[Récent]" howm-list-recent)
    ("[Autour]" howm-list-around)
    ("[Programme]" howm-list-schedule)
    ("[Occurences]" (lambda () (call-interactively 'howm-occur)) previous)
    ("[Fichiers tampons]" (lambda () (howm-list-buffers ,howm-menu-action-arg)))
    ("[Repères]" howm-list-mark-ring previous)
    ("[Historique]" howm-history)
    ("[<Titre]" howm-keyword-to-kill-ring)
    ("[<Nom]" (lambda () (howm-keyword-to-kill-ring t)))
    ("[Clé>]" howm-insert-keyword previous)
    ("[Date>]" howm-insert-date previous)
    ("[DateTemps>]" howm-insert-dtime previous)
    ("[A faire]" howm-list-todo)
    ("[Tout tuer]" howm-kill-all)
    ("[Forcer tout tuer]" (lambda () (interactive) (howm-kill-all t)))
    ("[Editer le menu]" howm-menu-edit current)
    ("[Màj du Menu]" howm-menu-refresh current)
    ("[Préferences]" (lambda () (customize-group 'howm)))
    ("[Parcours aléatoire]" howm-random-walk previous)
    ))

(provide 'howm-lang-fr)

;;; howm-lang-fr.el ends here
