#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.2 2010-12-28 09:29:09 morikawa Exp $
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

OBJS =  \
	gtool_historyauto.o \
	gtool_historyauto_internal.o gtool_historyauto_generic.o \
	historyautocreate.o historyautoclose.o \
	historyautoaddweight.o \
	historyautoputaxis.o historyautoputaxismpi.o \
	historyautoaddvariable.o historyautoaddattr.o \
	historyautoput.o historyautoallvarfix.o historyautochkoutput.o

RB2F90 = \
	gtool_historyauto_internal.f90 gtool_historyauto_generic.f90 \
	historyautoclose.f90 \
	historyautoaddweight.f90 \
	historyautoputaxis.f90 historyautoputaxismpi.f90 \
	historyautoaddvariable.f90 historyautoaddattr.f90 \
	historyautoput.f90


# End Settings
######################################################################

######################################################################
#== Rules
#
all: $(LIBPATH) archive

include $(DIRDEPTH)/rules.make

clean: depend.touch clean.default
rb2f90: rb2f90.default
clean.rb2f90: depend.touch clean.rb2f90.default
depend.subdir: depend
clean.depend: clean.depend.default
Makefile.win: Makefile.win.default
clean.mkwin: clean.mkwin.default

#
#=== Files generated with ruby script
#
gtool_historyauto_internal.f90: gtool_historyauto_internal.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

gtool_historyauto_generic.f90: gtool_historyauto_generic.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

historyautoclose.f90: historyautoclose.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

historyautoaddweight.f90: historyautoaddweight.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyautoputaxis.f90: historyautoputaxis.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyautoputaxismpi.f90: historyautoputaxismpi.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyautoaddvariable.f90: historyautoaddvariable.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

historyautoaddattr.f90: historyautoaddattr.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyautoput.f90: historyautoput.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

include depend
