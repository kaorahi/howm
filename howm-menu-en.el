;;; automatically generated from en/0000-00-00-000000.txt
;;; by howm-mkmenu.el.

(require 'howm-vars)

(howm-defconst-risky howm-menu-en "= <<< %menu%
 %\"e\"[Remember] %\"c\"[New] %\"D\"[Dup] Search(%\"s\"[String] %\"g\"[Regexp] %\"o\"[Occur])
 %\"a\"[All] %\"l\"[Recent] %\"A\"[Around] %\"y\"[Schedule] %\"t\"[Todo] %\"b\"[Buffers] %\"x\"[Marks]
 %\"K\"[<Title] [<Name] %\"d\"[Date>] %\"i\"[Key>] %\"r\"[Update] %\"w\"[Random Walk] [Killall]
 %\".\"[Today] %\":\"[Yesterday] %\"h\"[History] %\"R\"[Update Menu] [Edit Menu] [Preference]
-------------------------------------
[Schedule, Todo] -- @schedule, !deadline (until %sdays days from now)
!deadline, +todo, -reminder, ~defer (top %tnum entries)
%reminder
-------------------------------------
Recent
%recent
-------------------------------------
Random -- If you do not like this, [Edit Menu] to erase it.
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
[Edit Menu] Hit RET on the left button to edit this menu.
[Update Menu] Hit RET on the left button to update this menu.
--------------------------------------------------------

*** Format of the menu file ***
(Please hit RET on [Edit Menu] and read the source file.)

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

%\"foo\"[All]
This is displayed as foo[All], and the key \"f\" executes [All].
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

(provide 'howm-menu-en)
