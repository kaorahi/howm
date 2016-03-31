EXTRA_DIST = CL-HOWM.ja.rd OLD.rd README.ja.rd README.rd TUTORIAL.ja.rd \
	index-j.html index.html \
	README-j.html README.html \
	s-cat.png s-menu.png s-screen-shot.png s-search.png \
	come.png go.png updown.png todo.png 
MAINTAINERCLEANFILES = README-j.html README.html

doc_DATA = $(EXTRA_DIST)

README-j.html: README.ja.rd
	rd2 README.ja.rd > README-j.html

README.html: README.rd
	rd2 README.rd > README.html

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
