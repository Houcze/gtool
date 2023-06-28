#= Makefile for gtool5 top directory
#
# Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
# Version::   $Id: Makefile,v 1.17 2010-08-29 16:42:39 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2008. All rights reserved.
# License::   See COPYRIGHT[link:COPYRIGHT]
#
#vi: set ts=8 sw=4:
#
######################################################################
#
#== Settings
#
# [JAPANESE] 各種設定項目

DIRDEPTH=.
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE) # Include file              (optional)
                       # [JA] インクルードファイル (任意)

GuideFiles = INSTALL HISTORY INSTALL_netcdf CREDITS
                       # Documentation located in top directory       (optional)
                       # トップディレクトリに置くドキュメントファイル (任意)

# End Settings  [JA] 設定項目ここまで
######################################################################

######################################################################
#== Set GuideRDFiles
GuideRDFiles = $(GuideFiles:%=%.rd)
GuideJA      = $(GuideFiles:%=%.htm)
GuideEN      = $(GuideFiles:%=%.htm.en)

##########################################################################
#== Rules
#
all:
	cd src; $(MAKE) $@
	cd gt5frt; $(MAKE) $@

doc: guide
	cd doc; $(MAKE)

install: all mkprefix
	cd src;    $(MAKE) $@
	cd gt5frt; $(MAKE) $@

mkprefix:
	test -d $(prefix) || $(INSTALLDIR) $(prefix)
	test -d $(exec_prefix) || $(INSTALLDIR) $(exec_prefix)

install-doc:
	cd doc ; $(MAKE) install
	$(INSTALL) $(GuideJA) $(GuideEN) $(GuideFiles) ChangeLog COPYRIGHT $(prefix)

clean:
	cd src; $(MAKE) $@
	cd gt5frt; $(MAKE) $@
	cd test; $(MAKE) $@

clean.depend:
	cd src; $(MAKE) $@

distclean: distclean.depend
distclean.depend: clean
	cd src; $(MAKE) clean.depend
	cd src; $(MAKE) clean.mkwin
	-@find . -name depend | xargs rm -fr
	-@$(RM) config.status chkfort.cfg chkgmake.cfg chkrps.cfg
	-@$(RM) config.cache config.log Config.mk
	-@$(RM) autom4te.cache/*
	@test -d autom4te.cache && $(RMDIR) autom4te.cache 2> /dev/null || true
	@echo ""
	@echo "    Removing Config.mk,"
	@echo "      you should rerun ./configure to build gtool5."
	@echo ""

clean.doc: clean.guide
	cd doc; $(MAKE) clean.all

test-f: all
	cd test; $(MAKE) $@

test: all
	cd test; $(MAKE) $@

test-c: all
	cd test; $(MAKE) $@

test-installed:
	cd gt5frt; $(MAKE) $@

test-installed-c:
	cd gt5frt; $(MAKE) $@

test-mpi: mpi-check all
	cd test; $(MAKE) $@

test-mpi-c: mpi-check all
	cd test; $(MAKE) $@

test-mpi-installed: mpi-check
	cd gt5frt; $(MAKE) $@

mpi-check:
	@test -n "$(MPIEXEC)" || \
	    eval 'echo ; echo "  Error: \$$MPIEXEC is not specified."; echo "         If you want to test gtool5 with MPI library, "; echo "         specify --with-mpiexec to configure."; echo  ; exit 1'
	@test -n "$(MPIEXECPROCOPT)" || \
	    eval 'echo ; echo "  Error: \$$MPIEXECPROCOPT is not specified."; echo "         \$$MPIEXEC=$(MPIEXEC) is unknown MPI executable command, "; echo "         so edit \$$MPIEXECPROCOPT in Config.mk directly."; echo  ; exit 1'


tags:
	perl src/f90tags src/*.f90

guide:
	@for file in $(GuideFiles) ; do \
	    echo $(CP) $${file} $${file}.rd ;\
	    $(CP) $${file} $${file}.rd ;\
	done
	$(MAKE) -f htmltools/Makefile.rd2html
	$(RM) $(GuideRDFiles)

clean.guide:
	@if [ ! X"$(wildcard $(GuideJA) $(GuideEN) $(GuideRDFiles))" = X"" ]; then \
	    echo $(RM) $(wildcard $(GuideJA) $(GuideEN) $(GuideRDFiles)) ;\
	    $(RM) $(wildcard $(GuideJA) $(GuideEN) $(GuideRDFiles)) ;\
	fi

cl: git2cl
git2cl:
	@git log --date=short --pretty=format:"%ad %an <%ae>%n%n%s%n%b" | \
	sed -e 's/^\(.*\)$$/\t\1/g' | \
	sed -e 's/^\t\([0-9]*-[0-9]*-[0-9]*.*\)$$/\1/g' | \
	sed -e 's/^\t$$//g' | \
	sed -e '/^\tSigned-off-by.*$//d' \
	> ChangeLog

create-configure:
	@if [ ! -x /usr/bin/autoconf ]; then \
		echo -n "autoconf(1) is not valid. "; \
		echo "This is \"configure\" Generater."; \
		exit 1; \
	fi
	@if [ ! -r configure.in ]; then \
		echo -n "configure.in is not exists or not readable. "; \
		echo "This file needed for creating \"configure\"."; \
		exit 1; \
	fi
	@if [ ! -r script/configure/aclocal.m4 ]; then \
		echo -n "aclocal.m4 is not exists or not readable. "; \
		echo "This file needed for creating \"configure\"."; \
		exit 1; \
	fi
	autoconf -B script/configure
	@echo "\"configure\" Generated."
