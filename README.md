# howm: Write fragmentarily and read collectively.

Howm is a note-taking tool on Emacs. It is similar to emacs-wiki.el; you can enjoy hyperlinks and full-text search easily. It is not similar to emacs-wiki.el; it can be combined with any format.

* [Home](https://kaorahi.github.io/howm/)
* [Introduction by Leah Neukirchen](https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html) (thx!)
* [Detailed English & Russian tutorials by Andrei Sukhovskii](https://github.com/Emacs101/howm-manual) (thx!)
<!-- * [1-minute introduction on YouTube under Emacs Elements channel](https://www.youtube.com/watch?v=cCflzhDelvg) (unavailable? [2024-09-07]) -->

The following screenshot illustrates the Howm linking system:
![screenshot](doc/screenshot.png)

(Colorscheme: [Modus themes](https://protesilaos.com/emacs/modus-themes#h:7ea8fa66-1cd8-47b0-92b4-9998a3068f85).)

## Quick start

If you're using a recent version of Emacs and have enabled the [MELPA](https://melpa.org/) community package repository, you can simply place the following in your `~/.config/emacs/init.el` configuration file and restart Emacs:

```emacs-lisp
(use-package howm
  :ensure t)
```

After that, you can press e.g. `C-c , ,` to open the main menu, `C-c , a` to see a list of all your notes, or `C-c , c` to capture a new note from anywhere. See the documentation links above for more detailed instructions on how to use Howm.

Alternatively, here is a configurable snippet. If you want to change the note format, delete the menu file `0000-00-00-000000.txt` beforehand to regenerate a new menu in that format.

```emacs-lisp
(use-package howm
  :ensure t
  :init
  ;; 
  ;; Options: Remove the leading ";" in the following lines if you like.
  ;; 
  ;; Format
  ;(require 'howm-markdown) ;; Write notes in markdown-mode. (*1)
  ;(require 'howm-org) ;; Write notes in Org-mode. (*2)
  ;; 
  ;; Preferences
  ;(setq howm-directory "~/Documents/Howm") ;; Where to store the files?
  ;(setq howm-follow-theme t) ;; Use your Emacs theme colors. (*3)
  ;; 
  ;; Performance
  ;(setq howm-menu-expiry-hours 1) ;; Cache menu N hours. (*4)
  ;(setq howm-menu-refresh-after-save nil) ;; Speed up note saving. (*5)
  )
```

* (*1) [Markdown-mode](https://jblevins.org/projects/markdown-mode/) must be installed separately. Howm's wiki link `[[...]]` is disabled for syntax compatibility.
* (*2) Just replace `C-c ,` with `C-c ;` in Howm's documentation, including that `C-c ; ;` opens the menu. Howm's wiki link `[[...]]` is disabled for syntax compatibility.
* (*3) Technically, it inherits the colors used by Org-mode; this requires that you either (i) use a theme that defines the Org faces or (ii) that your `init.el` loads Org itself.
* (*4) Howm caches the menu state, so that opening the menu is instant. But it regenerates the menu if either (i) it's been more than 1 hour (the "expiry hours") since last menu update, or (ii) you saved a file that belongs to Howm (howm-mode was active in the buffer).
* (*5) Howm doesn't regenerate the menu if you save a file either, only if it's been more than 1 hour since the last menu update. If you want to update it more frequently, you have to manually refresh it (keybinding `R`).

For a more extensive overview of how you can customize Howm, you can use the Customize system (`M-x customize-group RET howm RET`). See also [Emacs Wiki](https://www.emacswiki.org/emacs/HowmMode) for various tips.

## Project history

* 2002-05-29 initial release (v0.1) on sourceforge.jp
* ...
* 2023-02-18 v1.5.1-snapshot4 on [OSDN](https://howm.osdn.jp/) (renamed from sourceforge.jp)
* 2023-05-13 moved the repository to [GitHub](https://github.com/kaorahi/howm)
* 2023-07-30 moved the project home to [GitHub](https://kaorahi.github.io/howm/)
