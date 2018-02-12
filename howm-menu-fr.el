;;; -*- Coding: utf-8-unix -*-
;;; automatically generated from fr/0000-00-00-000000.txt
;;; by howm-mkmenu.el.

(require 'howm-vars)

(howm-defconst-risky howm-menu-fr "= <<< %menu%
 %\"e\"[Se souvenir] %\"c\"[Nouveau] %\"D\"[Dup] Search(%\"s\"[Chaine] %\"g\"[Regexp] %\"o\"[Occurences])
 %\"a\"[Tout] %\"l\"[Récent] %\"A\"[Autour] %\"y\"[Programme] %\"t\"[A faire] %\"b\"[Fichiers tampons] %\"x\"[Repères]
 %\"K\"[<Titre] [<Nom] %\"d\"[Date>] %\"i\"[Clé>] %\"r\"[Màj] %\"w\"[Parcours aléatoire] [Tout tuer]
 %\".\"[Aujourd'hui] %\":\"[Hier] %\"h\"[Historique] %\"R\"[Màj du Menu] [Editer le menu] [Préferences]
-------------------------------------
[Schedule, Todo] -- @schedule, !deadline (until %sdays days from now)
!deadline, +todo, -reminder, ~defer (top %tnum entries)
%reminder
-------------------------------------
Recent
%recent
-------------------------------------
Random -- If you do not like this, [Editer le menu] to erase it.
%random
-------------------------------------

Format of schedule and todo (Please replace {} with []):
{2002-10-21}@1  schedule -- (shown in schedule part. @3 = \"3 days schedule to 10-23\")
{2002-10-21}+7  todo -- float up slowly from the date in 7 days
{2002-10-21}!7  deadline -- float up fast from 7 days before the date
{2002-10-21}-1  reminder -- float at the date and sink slowly one unit per 1 day
{2002-10-21}~30 defer -- float at the date and repeat sinking and floating with 30 days period
{2002-10-21}.   done -- sink forever
(Numbers after marks are default values.)

How to remember:
* Schedule at(@) the date
* Reminder sinks down(-).
* Todo floats up(+).
* Deadline needs attention!
* Defer waves(~) up and down.
* Done is the end(.).

-------------------------------------

You can edit this menu itself.
>>> %Editing Menu%


= <<< %Editing Menu%
[Editer le menu] Hit RET on the left button to edit this menu.
[Màj du Menu] Hit RET on the left button to update this menu.
--------------------------------------------------------

*** Format of the menu file ***
(Please hit RET on [Editer le menu] and read the source file.)

== Basic format

As you see...

* [xxx] is button.
* %REMINDER (in small letters) is schedule and todo list.
* %RECENT (in small letters) is list of recent entries.
* %RANDOM (in small letters) is list of random entries.

You can arrange their places as you like.
Writing favorite files or goto links may be also convenient.
(ex) file:///etc/services   >>> wiki

== Shortcut

%\"foo\"[Tout]
This is displayed as foo[Tout], and the key \"f\" executes [Tout].
Exactly speaking, it executes the following sequence:
(1) move to closing \", (2) move to next underline, and (3) hit it.

%\"bar%\"
If you put % at the tail like this, the key \"b\" means \"move cursor here\".

== For lispers

Display:
%here%howm-congrats-count ;; embed value of variable howm-congrats-count
%here%(howm-menu-search \"search\")
;; embed result of (...), that is, search \"search\" and embed matched lines
Functions must be registered for safety.
(setq howm-menu-allow (append '(foo bar) howm-menu-allow)) ;; allow foo, bar

Action:
%eval%(message (buffer-name))  ;; evaluate S expr
%call%find-file  ;; call function interactively
Both are evaluated in the previous buffer before raising menu.

== Hiding

'%' + '|' toggles invisibility
like this: visible%|invisible%|appear%|disappear  - until the end of line
(Newline is removed when the end of line is invisible.)

== Multiple menus

Links to %xxx% open \"<< < %xxx%\" with menu-mode: >>> %menu%
When you add new menu, [[%menu%]] may be more convenient because corresponding
entry is generated automatically.

%eval%(howm-menu-open \"00000000-000000.txt\")  -- open file with menu-mode
")

(provide 'howm-menu-fr)
