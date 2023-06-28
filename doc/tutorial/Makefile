#= Makefile for gtool5 documentation
#
# Authors::   Yasuhiro MORIKAWA, Eizi TOYODA, Masatsugu ODAKA
# Version::   $Id: Makefile,v 1.3 2009-03-21 08:08:30 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2008. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#
# in Config.mk, DEST_DOC is defined
#
######################################################################
#
#== Settings

DIRDEPTH=../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

DEST_DOC_THISDIR=$(DEST_DOC)/tutorial

SUBDIRS=dc_args dc_clock dc_date dc_message dc_trace dc_types \
	gthist_attr gthist_f77 gthist_first gthist_gave gthist_multi \
	gtauto_first

# End Settings
######################################################################

######################################################################
#== Set variables
#

all: rd2html

rd2html:
	$(MAKE) -f Makefile.rd2html

install:
	test -d $(DEST_DOC_THISDIR) || $(INSTALLDIR) $(DEST_DOC_THISDIR)
	$(INSTALL) $(wildcard *.htm *.htm.en *.rd *.f90 *.f *.nc) $(DEST_DOC_THISDIR)
	@for i in $(SUBDIRS) ; do \
	  test -d $(DEST_DOC_THISDIR)/$$i || $(INSTALLDIR) $(DEST_DOC_THISDIR)/$$i; \
	  cd $$i || exit 1; \
	  echo copy `ls *.htm *.htm.en *.rd *.f90 *.f *.nc *.png *.nml 2> /dev/null` '->' $(DEST_DOC_THISDIR)/$$i; \
	  $(INSTALL) *.htm *.htm.en *.rd *.f90 *.f *.nc *.png *.nml $(DEST_DOC_THISDIR)/$$i 2> /dev/null || true; \
	  cd .. ; \
	done

clean:

clean.all: clean.rd2html

clean.rd2html:
	$(MAKE) clean.all -f Makefile.rd2html
