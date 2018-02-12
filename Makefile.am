# shamelessly copied from navi2ch-cvs-0.0.20031209

howm_LISP = \
	howm.el \
	howm-menu.el \
	howm-reminder.el \
	howm-date.el \
	howm-misc.el \
	howm-mode.el \
	howm-view.el \
	howm-backend.el \
	howm-common.el \
	howm-vars.el \
	howm-lang-en.el \
	howm-lang-fr.el \
	howm-lang-ja.el \
	howm-menu-en.el \
	howm-menu-fr.el \
	howm-menu-ja.el \
	honest-report.el \
	action-lock.el \
	riffle.el \
	gfunc.el \
	illusion.el \
	cheat-font-lock.el

EXTRA_DIST = Makefile.old bcomp.el howm-mkmenu.el $(howm_LISP) \
	bcomp.el.in howm.el.in test.bat
BUILT_SOURCES = $(srcdir)/howm.el $(srcdir)/bcomp.el
SUBDIRS = doc ext en fr ja sample

MENU_SRC = en/0000-00-00-000000.txt \
	fr/0000-00-00-000000.txt \
	ja/0000-00-00-000000.txt
MENU_EL = $(srcdir)/howm-menu-en.el \
	$(srcdir)/howm-menu-fr.el \
	$(srcdir)/howm-menu-ja.el
TEST_INIT = sample/dot.emacs

AM_MAKEFLAGS = HOWM_MAKE=t

$(MENU_EL): $(srcdir)/howm-mkmenu.el $(MENU_SRC)
	$(EMACS) -q -batch -l $(srcdir)/howm-mkmenu.el

# snap:
# 	rm -f $(srcdir)/howm-version.el && \
# 	$(MAKE) $(AM_MAKEFLAGS) VERSION=@VERSION@-`date +%y%m%d` dist elcdist && \
# 	rm -f $(srcdir)/howm-version.el && \
# 	$(MAKE) $(AM_MAKEFLAGS) $(srcdir)/howm-version.el

# recompile: $(howm_LISP)
# 	rm -f $(ELCFILES) && \
# 	HOWM_MAKE=t EMACS="$(EMACS)" $(SHELL) $(srcdir)/elisp-comp $(howm_LISP)

# elcdist: $(ELCFILES)
# 	rm -rf $(distdir) && \
# 	mkdir $(distdir) && \
# 	cp -p $(ELCFILES) $(distdir) && \
# 	$(AMTAR) chof - $(distdir) | GZIP=$(GZIP_ENV) gzip -c >$(distdir)-elc.tar.gz && \
# 	rm -rf $(distdir)

$(srcdir)/bcomp.el: $(srcdir)/bcomp.el.in $(srcdir)/Makefile.am
	rm -f $@ && \
	list=`for file in $(howm_LISP); do echo -n "\"$$file\" "; done` && \
	sed -e "s/@\(howm_LISP\)@/$$list/" < $(srcdir)/bcomp.el.in > $@

# $(ELCFILES): $(howm_LISP)
# 	$(MAKE) $(AM_MAKEFLAGS) recompile

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$@ $(SHELL) ./config.status

Makefiles: $(top_builddir)/config.status
	$(SHELL) $(top_builddir)/config.status

update:
	( cd $(srcdir); $(CVS) update -P -d ) && \
	$(MAKE) $(AM_MAKEFLAGS) Makefiles

test:
	HOWM_TEST=t $(EMACS) -q --no-site-file -l $(TEST_INIT)
