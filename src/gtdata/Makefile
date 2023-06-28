#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa), HOU
# Version::   $Id: Makefile,v 1.7 2009-05-29 14:41:13 morikawa Exp $
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

SUBDIRS	= gtdata_memory gtdata_netcdf

OBJS = \
      gtdata_internal_vartable.o gtdata_generic.o \
      gtdata_types.o gtdata_internal_map.o \
      gtvartmpnam.o \
      gtvardeldim.o gtvaradddim.o gtvaraddmember.o gtvarexchdim.o \
      gtvardimname2ord.o \
      gtvargetattr.o gtvargetattrsc.o \
      gtvarsync.o \
      gtvargetslice.o \
      gtvarcopyattr.o gtvarcopyattrall.o \
      gtvarattrtrue.o gtvardelattr.o \
      gtvarattrsearch.o \
      gtvarputattrchar.o \
      gtvaropen.o gtvarclose.o \
      gtvaropenbydimord.o \
      gtvarslice.o gtvarslicenext.o gtvarsliceauto.o \
      gtvarcreatecopy.o gtvarcreate.o gtvarcreate.o gtvarcreated.o \
      gtvargetsliceall.o \
      gtvarputline.o \
      gtvarsearch.o \
      gtvargetnum.o gtvargetpointernum.o \
      gtvarputnum.o \
      gtvarinquire.o \
      gtvarlimit.o gtvarlimitbinary.o \
      gtvarequivalent.o

RB2F90 = gtdata_generic.f90 gtvarputnum.f90 gtvargetpointernum.f90 gtvargetnum.f90

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
gtdata_generic.f90: gtdata_generic.erb
	erb $< > $@
# gtdata_generic.f90: gtdata_generic.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< --gtvarget_dim=$(MAXDIM) --gtvarput_dim=$(MAXDIM) > $@ || \
# 	    touch $@

gtvargetpointernum.f90: gtvargetpointernum.erb
	erb $< > $@
# gtvargetpointernum.f90: gtvargetpointernum.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< --gtvarget_dim=$(MAXDIM) > $@ || \
# 	    touch $@

gtvargetnum.f90: gtvargetnum.erb
	erb $< > $@
# gtvargetnum.f90: gtvargetnum.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< > $@ || \
# 	    touch $@

gtvarputnum.f90: gtvarputnum.erb
	erb $< > $@
#gtvarputnum.f90: gtvarputnum.rb2f90
#	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
#	    $(RUBY) $< --gtvarput_dim=$(MAXDIM) > $@ || \
#	    touch $@

include depend
