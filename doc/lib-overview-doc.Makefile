#= Makefile for gtool5 documentation
#
# Authors::   <Input your name>
# Version::   $Id: lib-overview-doc.Makefile,v 1.1 2009-03-21 10:32:32 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#
# in Config.mk, DEST_DOC is defined
#
######################################################################
#
#== Settings

DIRDEPTH=<Input relative path to gtool5 top directory. For example, "../..", etc.>
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

DEST_DOC_THISDIR=$(DEST_DOC)/<Input directory name>

SUBDIRS=

# End Settings
######################################################################

######################################################################
#== Set variables
#

all: rd2html
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

rd2html:
	$(MAKE) -f Makefile.rd2html

install:
	test -d $(DEST_DOC_THISDIR) || $(INSTALLDIR) $(DEST_DOC_THISDIR)
	$(INSTALL) $(wildcard *.htm *.htm.en *.rd) $(DEST_DOC_THISDIR)
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean:

clean.all: clean.rd2html

clean.rd2html:
	$(MAKE) clean.all -f Makefile.rd2html
