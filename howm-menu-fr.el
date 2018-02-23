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
Récent
%recent
-------------------------------------
Aléatoire -- Si vous n'aimez pas, appuyez sur [Editer le menu] pour le retirer.
%random
-------------------------------------

Format des RDV et tâches (Attention à remplacer les {} par des []):
{2002-10-21}@1  RDV -- (Affiché dans la partie des RDV. @3 = \"3 days schedule to 10-23\")
{2002-10-21}+7  tâche -- flotte lentement depuis la date dans 7 jours
{2002-10-21}!7  deadline -- flotte rapidement à partir de 7 jours avant la date
{2002-10-21}-1  rappel -- flotte à la date et plonge lentement de une unité par jour
{2002-10-21}~30 report -- flotte à la date et monte et descend sur une période de 30jours
{2002-10-21}.   done -- sombre définitivement
(Les chiffres après la marque représentent les valeurs par défaut.)

Comment se souvenir de la syntaxe:
* On programme à(@) une date
* Les rappels plongent(-).
* Les todo flottent(+).
* Les deadline ont besoin d'attention(!)
* Les reports font des vagues(~).
* Une tâche faite signifie la fin(.).

-------------------------------------

Vous pouvez éditer ce menu.
>>> %Editing Menu%


= <<< %Editing Menu%
[Editer le menu] Appuyer sur RET sur le bouton à gauche pour éditer ce menu.
[Màj du Menu] Appuyer sur RET sur le bouton à gauche pour mettre à jour ce menu.
--------------------------------------------------------

*** Format du fichier menu ***
(Appuyez sur RET sur [Editer le menu] et lisez le fichier source.)

== Basic format

Comme vous pouvez le voir...

* [xxx] est un bouton.
* %REMINDER (en minuscules) liste les todo et les rendez-vous.
* %RECENT (en minuscules) liste les entrées récentes.
* %RANDOM (en minuscules) list les entrées aléatoires.

Vous pouvez les placer comme bon vous semble.
Ajouter vos fichiers favoris ou les liens \"goto\" peut être pratique.
(ex) file:///etc/services   >>> wiki

== Raccourci

%\"foo\"[Tout]
S'affiche foo[Tout], et \"f\" exécute [Tout].
Pour être plus précis, cela suit la séquence suivante:
(1) aller sur le dernier \" , (2) move to next underline, and (3) hit it.

%\"bar%\"
Si vous metter un signe % en fin de mot comme ceci, le \"b\" signifie \"place le curseur ici\".

== Pour les amateurs de lisp

Affichage:
%here%howm-congrats-count ;; embed value of variable howm-congrats-count
%here%(howm-menu-search \"search\")
;; embarque le résultat de (...), c'est à dire, rechercher \"search\" et embarquer les lines correspondant.
Pour des raisons de sécurité, les fonctions doivent être enregistrées.
(setq howm-menu-allow (append '(foo bar) howm-menu-allow)) ;; allow foo, bar

Action:
%eval%(message (buffer-name))  ;; évalue la S expression
%call%find-file  ;; appelle la fonction de manière intéractive
L'une et l'autre sont évaluées dans le précédent buffer avant de passer sur le menu.

== Camouflage

'%' + '|' active (et desactive) la visibilité
comme: visible%|invisible%|apparait%|disparait  - jusqu'à la fin de la line
(Le caractère Newline est retiré lorsque la fin de ligne est invisible.)

== Menus multiples

Les liens vers %xxx% ouvrent \"<< < %xxx%\" avec le menu menu-mode: >>> %menu%
Lorsque vous ajoutez un nouveau menu, [[%menu%]] peut être plus pratique parce que l'entrée correspondante
est générée automatiquement.

%eval%(howm-menu-open \"00000000-000000.txt\")  -- ouvrir le fichier avec menu-mode
")

(provide 'howm-menu-fr)
