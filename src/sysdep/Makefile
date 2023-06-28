#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.3 2009-04-28 11:05:58 morikawa Exp $
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

OBJS = \
      sysdepabort-$(SYSDEPABORT).o \
      sysdeparg-$(SYSDEPARG).o \
      sysdepenv-$(SYSDEPENV).o \
      sysdep.o

RB2F90 = sysdepabort-abort.f90 \
	sysdepabort-exit.f90 sysdepabort-stop.f90 \
	sysdepabort-setrcd.f90 sysdepabort-errtra-setrcd.f90 \
	sysdeparg-nostd.f90 sysdeparg-f2003std.f90 sysdeparg-hitachi.f90 \
	sysdepenv-nostd.f90 sysdepenv-f2003std.f90 sysdepenv-nogetenv.f90

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
sysdepabort-abort.f90: sysdepabort.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --aborttype=ABORT > $@ || \
	    touch $@

sysdepabort-exit.f90: sysdepabort.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --aborttype=EXIT > $@ || \
	    touch $@

sysdepabort-stop.f90: sysdepabort.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --aborttype=STOP > $@ || \
	    touch $@

sysdepabort-setrcd.f90: sysdepabort.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --aborttype=SETRCD > $@ || \
	    touch $@

sysdepabort-errtra-setrcd.f90: sysdepabort.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --aborttype=ERRTRA-SETRCD > $@ || \
	    touch $@

sysdeparg-nostd.f90: sysdeparg.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --argtype=NOSTD > $@ || \
	    touch $@

sysdeparg-f2003std.f90: sysdeparg.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --argtype=F2003STD > $@ || \
	    touch $@

sysdeparg-hitachi.f90: sysdeparg.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --argtype=HITACHI > $@ || \
	    touch $@

sysdepenv-nostd.f90: sysdepenv.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --envtype=NOSTD > $@ || \
	    touch $@

sysdepenv-f2003std.f90: sysdepenv.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --envtype=F2003STD > $@ || \
	    touch $@

sysdepenv-nogetenv.f90: sysdepenv.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --envtype=NOGETENV > $@ || \
	    touch $@

include depend
