EXTRA_DIST = \
	hcal.rb \
	howm2 \
	howmkara \
	hsplit.rb

ext_DATA = $(EXTRA_DIST)

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
