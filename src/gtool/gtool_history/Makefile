#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.4 2009-10-12 04:03:12 morikawa Exp $
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
	gtool_history.o \
	gt4_history.o \
	gtool_history_types.o gtool_history_internal.o gtool_history_generic.o \
	historyget.o historygetattr.o \
	historycreate.o historyaxiscreate.o historyvarinfocreate.o \
	historyputline.o historyinitialized.o historycopy.o \
	historyaxiscopy.o historyvarinfocopy.o \
	historyaddattr.o historyaxisaddattr.o historyvarinfoaddattr.o \
	historyaddvariable.o historycopyvariable.o \
	historyinquire.o historyaxisinquire.o historyvarinfoinquire.o \
	historysettime.o historyput.o historyclose.o \
	historyaxisclear.o historyvarinfoclear.o historyvarinfoinitialized.o \
	hscrea.o hsavar.o hsclse.o \
	hsaatc.o hsaatr.o hsaatd.o hsaati.o \
	hsp1r.o hsp2d.o hsp3d.o hsp1d.o hsp2r.o hsp3r.o


RB2F90 = gtool_history_generic.f90 gtool_history_internal.F90 \
	historycreate.F90 historyput.F90 \
	historyget.F90 historygetattr.F90 \
	historyaddattr.F90 \
	historyaxisaddattr.f90 historyvarinfoaddattr.f90 \


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
gtool_history_generic.f90: gtool_history_generic.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) --histget_dim=$(MAXDIM) > $@ || \
	    touch $@

gtool_history_internal.F90: gtool_history_internal.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyget.F90: historyget.erb
	erb $< > $@
# historyget.F90: historyget.rb2f90
# 	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
# 	    $(RUBY) $< --histget_dim=$(MAXDIM) > $@ || \
# 	    touch $@

historyput.F90: historyput.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --histput_dim=$(MAXDIM) > $@ || \
	    touch $@

historygetattr.F90: historygetattr.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historycreate.F90: historycreate.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyaddattr.F90: historyaddattr.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyaxisaddattr.f90: historyaxisaddattr.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

historyvarinfoaddattr.f90: historyvarinfoaddattr.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

include depend
