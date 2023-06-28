#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Eizi TOYODA (toyoda), Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.13 2009-05-06 14:23:13 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

# SUBDIRS	= sysdep dc_utils netcdf gtdata gtool
SUBDIRS	= sysdep dc_utils gtdata gtool

# End Settings
######################################################################

######################################################################
#== Rules
#
all: rb2f90
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

rb2f90: 
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean: 
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.rb2f90: depend.touch
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

depend.subdir:
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.depend: 
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

install: all
	test -d $(DEST_LIB) || $(INSTALLDIR) $(DEST_LIB)
	test -d $(DEST_INC) || $(INSTALLDIR) $(DEST_INC)
	$(INSTALL) $(LIBPATH) $(DEST_LIB)
	$(RANLIB) $(DEST_LIB)/$(LIBNAME)
	$(INSTALL) $(MODDIR)/$(MODS) $(MODINCS) $(DEST_INC)

Makefile.win:
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.mkwin: 
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done
