#                               MAIN MAKEFILE
#                               -------------

ROOT_DIR       = /home/diaz/GP
PKG_NAME       = gprolog-1.5.1

INSTALL_DIR    = $(DESTDIR)/tmp/gprolog-1.5.1
LINKS_DIR      = $(DESTDIR)/tmp/bin
DOC_DIR        = $(DESTDIR)/tmp/gprolog-1.5.1/doc
HTML_DIR       = $(DESTDIR)/tmp/gprolog-1.5.1/doc/html_node
EXAMPLES_DIR   = $(DESTDIR)/tmp/gprolog-1.5.1/examples

TXT_FILES      = README COPYING VERSION NEWS ChangeLog gprolog.ico
BIN_FILES      =  gplc hexgplc gprolog pl2wam wam2ma ma2asm fd2c
OBJ_FILES      =  top_level_main.o top_level.o all_pl_bips.o debugger.o all_fd_bips.o
LIB_FILES      = libengine_pl.a libbips_pl.a liblinedit.a libengine_fd.a libbips_fd.a
INC_FILES      = gprolog.h fd_to_c.h
DOC_FILES      = ???*.dvi ???*.ps ???*.pdf ???*.chm ???*.eps ???*.html ???*.png
HTML_FILES     = ???*.html ???*.css ???*.svg
EXPL_FILES     = Makefile README PROGS ??*.pl
EXC_FILES      = Makefile README ??*.pl ??*_c.c
EXFD_FILES     = Makefile ??*.pl ??*.fd

INSTALL        = /usr/bin/install -c
INSTALL_PROGRAM= ${INSTALL}
INSTALL_DATA   = ${INSTALL} -m 644

TOP_LEVEL      = gprolog

LN_S           = ln -s -f
RANLIB         = ranlib

LE_DIRS        = Linedit
W32GC_DIRS     = W32GUICons
FD_DIRS        = Fd2C EngineFD BipsFD
SUB_DIRS       = EnginePl TopComp Wam2Ma Ma2Asm $(LE_DIRS)  \
                 BipsPl Pl2Wam $(FD_DIRS)
SUB_DIRS_ALL   = EnginePl TopComp Wam2Ma Ma2Asm $(LE_DIRS) $(W32GC_DIRS) \
                 BipsPl Pl2Wam $(FD_DIRS)


# --- COMPILE --- #

all: config
	. ./SETVARS;for i in $(SUB_DIRS);do (cd $$i; $(MAKE)) || exit 1; done;\
	(cd TopComp; $(MAKE) top-level) || exit 1;\
	(cd Pl2Wam; $(MAKE) stage2)

# config mainly creates gplc needed by other Makefiles (so we can use make -j)
config: EnginePl/Makefile
	cd EnginePl; $(MAKE) config

EnginePl/Makefile:
	echo '*** run ./configure before make please (see file INSTALL) ***'
	exit 1

doc:
	(cd ../doc; $(MAKE) all)

# --- INSTALL --- #

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

install: install-system install-doc install-html install-examples install-links

install-system:
	./mkinstalldirs $(INSTALL_DIR) $(INSTALL_DIR)/bin \
	$(INSTALL_DIR)/include $(INSTALL_DIR)/lib
	for i in $(TXT_FILES); do $(INSTALL_DATA) ../$$i $(INSTALL_DIR); done
	for i in $(BIN_FILES); do $(INSTALL_PROGRAM) */$$i $(INSTALL_DIR)/bin; done
	for i in $(OBJ_FILES); do $(INSTALL_DATA) */$$i $(INSTALL_DIR)/lib; done
	for i in $(LIB_FILES); do $(INSTALL_DATA) */$$i $(INSTALL_DIR)/lib; done
	for i in $(INC_FILES); do EnginePl/cpp_headers $$i ./$$i EnginePl EngineFD BipsPl BipsFD; $(INSTALL_DATA) ./$$i $(INSTALL_DIR)/include; rm -f $$i; done
	(cd $(INSTALL_DIR)/lib; for i in *.a; do $(RANLIB) $$i; done)


uninstall: uninstall-links uninstall-html uninstall-doc uninstall-examples
	for i in $(BIN_FILES); do rm -f $(INSTALL_DIR)/bin/$$i; done
	rmdir -p $(INSTALL_DIR)/bin 2>/dev/null || exit 0
	for i in $(LIB_FILES) $(OBJ_FILES); do rm -f $(INSTALL_DIR)/lib/$$i; done
	rmdir -p $(INSTALL_DIR)/lib 2>/dev/null || exit 0
	for i in $(INC_FILES); do rm -f $(INSTALL_DIR)/include/$$i; done
	rmdir -p $(INSTALL_DIR)/include 2>/dev/null || exit 0
	for i in $(TXT_FILES); do rm -f $(INSTALL_DIR)/$$i; done
	rmdir $(INSTALL_DIR) 2>/dev/null || exit 0


# --- Links --- #

install-links: install-system uninstall-links
	if test /tmp/bin != none; then \
	   ./mkinstalldirs $(LINKS_DIR); \
	   (cd $(LINKS_DIR) ; $(LN_S) $(wildcard $(INSTALL_DIR)/bin/*) .); \
	fi

uninstall-links:
	-if test /tmp/bin != none; then \
	   (cd $(LINKS_DIR) 2>/dev/null && rm -f $(BIN_FILES)); \
	   rmdir $(LINKS_DIR) 2>/dev/null; \
	fi || exit 0;


# --- Documentation --- #

install-doc:
	if test /tmp/gprolog-1.5.1/doc != none; then \
	   ./mkinstalldirs $(DOC_DIR); \
	   (F=`cd ../doc; echo $(DOC_FILES)`; \
	   for i in $$F; do $(INSTALL_DATA) ../doc/$$i $(DOC_DIR); done); \
	fi

uninstall-doc:
	-if test /tmp/gprolog-1.5.1/doc != none; then \
	   (cd $(DOC_DIR); rm -f $(DOC_FILES)); \
	   rmdir $(DOC_DIR) 2>/dev/null; \
	fi || exit 0;


# --- HTML --- #

install-html:
	if test /tmp/gprolog-1.5.1/doc/html_node != none; then \
	   ./mkinstalldirs $(HTML_DIR); \
	   (F=`cd ../doc/html_node; echo $(HTML_FILES)`; \
	   for i in $$F; do $(INSTALL_DATA) ../doc/html_node/$$i $(HTML_DIR); done); \
	fi

uninstall-html:
	-if test /tmp/gprolog-1.5.1/doc/html_node != none; then \
	   (cd $(HTML_DIR); rm -f $(HTML_FILES)); \
	   rmdir $(HTML_DIR) 2>/dev/null; \
	fi || exit 0;


# --- Examples --- #

install-examples:
	if test /tmp/gprolog-1.5.1/examples != none; then \
	   ./mkinstalldirs $(EXAMPLES_DIR)/ExamplesPl; \
	   (F=`cd ../examples/ExamplesPl; echo $(EXPL_FILES)`; \
	   for i in $$F; do $(INSTALL_DATA) ../examples/ExamplesPl/$$i $(EXAMPLES_DIR)/ExamplesPl; done); \
	   ./mkinstalldirs $(EXAMPLES_DIR)/ExamplesC; \
	   (F=`cd ../examples/ExamplesC; echo $(EXC_FILES)`; \
	   for i in $$F; do $(INSTALL_DATA) ../examples/ExamplesC/$$i $(EXAMPLES_DIR)/ExamplesC; done); \
	   ./mkinstalldirs $(EXAMPLES_DIR)/ExamplesFD; \
	   (F=`cd ../examples/ExamplesFD; echo $(EXFD_FILES)`; \
	   for i in $$F; do $(INSTALL_DATA) ../examples/ExamplesFD/$$i $(EXAMPLES_DIR)/ExamplesFD; done); \
	fi

uninstall-examples:
	rm -rf $(EXAMPLES_DIR)/ExamplesPl
	rm -rf $(EXAMPLES_DIR)/ExamplesC
	rm -rf $(EXAMPLES_DIR)/ExamplesFD
	-rmdir $(EXAMPLES_DIR) 2>/dev/null || exit 0


# --- CHECKS --- #

check:
	. ./SETVARS; \
	(cd EnginePl; $(MAKE) check) && \
	(cd Ma2Asm; $(MAKE) check) && \
	(cd Pl2Wam; $(MAKE) check) && \
	(cd BipsPl; $(MAKE) check) && \
	echo All tests succeeded

check-i:
	. ./SETVARS; \
	(cd Pl2Wam; $(MAKE) check-i) && \
	(cd BipsPl; $(MAKE) check-i) && \
	echo All tests succeeded

# --- CLEAN --- #

RUN_PL_DIRS = EnginePl BipsPl
RUN_FD_DIRS = EngineFD BipsFD
RUN_SUB_DIRS= $(RUN_PL_DIRS) $(RUN_PL_DIRS)



clean:
	for i in $(SUB_DIRS_ALL); do (cd $$i; $(MAKE) clean); done

clean-pl:
	for i in $(RUN_PL_DIRS); do (cd $$i; $(MAKE) clean); done

clean-fd:
	for i in $(RUN_FD_DIRS); do (cd $$i; $(MAKE) clean); done

clean-pl-fd: clean-pl clean-fd


distclean:
	rm -rf config.status config.log config.cache autom4te.cache
	for i in $(SUB_DIRS_ALL); do (cd $$i; $(MAKE) distclean); done


# --- DISTRIBUTION --- #

distdir=/tmp
distpath=$(distdir)/$(PKG_NAME)

dist: doc $(PKG_NAME).tar.gz


$(PKG_NAME).tar.gz: copy_dist_tree
	(cd $(distdir); tar cf $(ROOT_DIR)/src/$(PKG_NAME).tar $(PKG_NAME))
	gzip -f $(PKG_NAME).tar
	-rm -rf $(distpath)
	@banner="$(PKG_NAME).tar.gz is ready for distribution"; \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"


# below we use 2 cd in case distpath is not an absolute path like '.'
copy_dist_tree:
	-rm -rf $(distpath)
	mkdir -p $(distpath)
	while read f;do files="$$files $$f"; done <DISTRIB_FILES; \
	(cd ..;tar cf - $$files | (cd $(ROOT_DIR)/src;cd $(distpath); tar xf -))


old$(PKG_NAME).tar.gz:
	-rm -rf $(distpath)
	mkdir $(distpath)
	-chmod 777 $(distpath)
	(while read f; do \
	    if [ x$$f != x ]; then \
		subdir=`dirname $$f`; \
		if test "$$subdir" = .; then :; else \
		    test -d $(distpath)/$$subdir \
		    || mkdir -p $(distpath)/$$subdir \
	    	    || exit 1; \
	    	    chmod 777 $(distpath)/$$subdir; \
		fi; \
	        cp -pr ../$$f $(distpath)/$$subdir; \
	    fi; \
	 done <DISTRIB_FILES)
	tar cf $(PKG_NAME).tar $(distpath)
	gzip -f $(PKG_NAME).tar
	-rm -rf $(distpath)
	@banner="$(PKG_NAME).tar.gz is ready for distribution"; \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"
