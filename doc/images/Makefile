#= Makefile for gtool5 documentation image files
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: Makefile,v 1.2 2009-03-21 08:08:30 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
# in Config.mk, DEST_DOC is defined
#
######################################################################
#
#== Settings

DIRDEPTH=../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

DEST_DOC_THISDIR=$(DEST_DOC)/images

# End Settings
######################################################################

######################################################################
#== Rules
#

all:

clean.all:

install:
	test -d $(DEST_DOC_THISDIR) || $(INSTALLDIR) $(DEST_DOC_THISDIR)
	$(INSTALL) $(wildcard *.png *.ps *.eps) $(DEST_DOC_THISDIR)

eps2png:
	convert -density 300x300 -geometry 800x800 gtool5_detail.eps gtool5_detail.png
	convert -density 300x300 -geometry 1300x1300 gtool5_detail.eps gtool5_detail_big.png
