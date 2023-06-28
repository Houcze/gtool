#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.2 2009-05-25 09:51:59 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=../../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

SUBDIRS	= gtdata_netcdf_file

OBJS = \
      gtdata_netcdf_types.o gtdata_netcdf_generic.o gtdata_netcdf_internal.o \
      gdncvarputattrchar.o gdncvarputattrint.o gdncvarputattrreal.o \
      gdncvarattr.o gdncvarattrtrue.o gdncvarclose.o gdncvarcreate.o \
      gdncvaropen.o \
      gdncattrgetnum.o \
      gdncattrgetchar.o \
      gdncvardelattr.o gdncvarsearch.o gdncvarsync.o gdncvarcreated.o \
      gdncattrinquire.o gdncvarattrcopy.o \
      gdncvarinquire.o \
      gdncvarinquireia.o \
      gdncvaropenbydimord.o gdncvartostring.o \
      gdncattrinquire.o \
      gdncxtypename.o \
      gdncvargetnum.o \
      gdncvarputnum.o \
      gdncsearchdim.o

RB2F90 = gdncattrgetnum.f90 gdncvargetnum.f90 gdncvarputnum.f90

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

#
#=== Files generated with ruby script
#
gdncattrgetnum.f90: gdncattrgetnum.erb
	erb $< >$@
# gdncattrgetnum.f90: gdncattrgetnum.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
#	    $(RUBY) $< > $@ || \
#	    touch $@

gdncvargetnum.f90: gdncvargetnum.erb
	erb $< >$@
# gdncvargetnum.f90: gdncvargetnum.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< > $@ || \
# 	    touch $@

gdncvarputnum.f90: gdncvarputnum.erb
	erb $< >$@
# gdncvarputnum.f90: gdncvarputnum.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< > $@ || \
# 	    touch $@


include depend
