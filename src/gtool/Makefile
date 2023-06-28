#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.6 2009-05-11 15:15:16 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

SUBDIRS	= gtool_history gtool_history_nmlinfo gtool_historyauto

OBJS = gtool5.o gt4f90io.o

RB2F90 = 

# End Settings
######################################################################


######################################################################
#== Rules
#
all: $(LIBPATH) archive
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

include $(DIRDEPTH)/rules.make

clean: depend.touch clean.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

rb2f90: rb2f90.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.rb2f90: depend.touch clean.rb2f90.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

depend.subdir: depend
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.depend: clean.depend.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

Makefile.win: Makefile.win.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

clean.mkwin: clean.mkwin.default
	@for i in $(SUBDIRS) ; do \
	  cd $$i || exit 1; \
	  $(MAKE) $@ || exit 1; \
	  cd .. ; \
	done

include depend
