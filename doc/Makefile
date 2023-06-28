#= Makefile for gtool5 documentation
#
# Authors::   Yasuhiro MORIKAWA, Eizi TOYODA, Masatsugu ODAKA
# Version::   $Id: Makefile,v 1.3 2009-03-21 08:08:31 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#
# in Config.mk, DEST_DOC is defined
#
######################################################################
#
#== Settings

DIRDEPTH=..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

DEST_DOC_THISDIR=$(DEST_DOC)

SUBDIRS=images tutorial

# End Settings
######################################################################

######################################################################
#== Rules
#

all: rd2html
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done
	$(MAKE) rdoc
	$(MAKE) rdoc-dev

rd2html:
	$(MAKE) -f Makefile.rd2html

rdoc:
	$(MAKE) -f Makefile.rdoc

rdoc-dev:
	$(MAKE) -f Makefile.rdoc-dev

install:
	test -d $(DEST_DOC_THISDIR) || $(INSTALLDIR) $(DEST_DOC_THISDIR)
	$(INSTALL) $(wildcard *.htm *.htm.en *.rd) $(DEST_DOC_THISDIR)
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done
	$(MAKE) $@ -f Makefile.rdoc
	$(MAKE) $@ -f Makefile.rdoc-dev

clean:

clean.all: clean.rd2html clean.all.subdirs clean.rdoc clean.rdoc-dev

clean.rd2html:
	$(MAKE) clean.all -f Makefile.rd2html

clean.rdoc:
	$(MAKE) clean -f Makefile.rdoc

clean.rdoc-dev:
	$(MAKE) clean -f Makefile.rdoc-dev

clean.all.subdirs:
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) clean.all || exit 1; \
	  cd .. ; \
	done
