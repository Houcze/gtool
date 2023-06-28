#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.12 2009-12-29 16:10:03 morikawa Exp $
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
      dc_utils.o \
      dc_test.o \
      dc_regex.o \
      dcstringsprintf.o dcstringfprintf.o dcstringcprintf.o \
      dcstringputline.o \
      dc_trace.o \
      dc_string.o dc_types.o \
      dc_error.o dc_url.o dc_units.o dcunits_com.o \
      dc_message.o dc_present.o \
      dc_scaledsec.o \
      dc_clock.o dc_iounit.o \
      dc_hash.o dc_args.o \
      dc_date_types.o dc_date_internal.o dc_date_generic.o dc_date.o \
      dcdatetimecreate.o dcdatetimeeval.o \
      dcdatetimesetcaltype.o dcdatetimesetsecofday.o \
      dcdatetimevalidcaltype.o dcdatetimezone.o \
      dcdatetimetochar.o dcdatetimeputline.o dc_date.o \
      dcdatetimeadd.o dcdatetimesub.o dcdatetimemul.o dcdatetimediv.o \
      dcdatetimemod.o dcdatetimemax.o dcdatetimemin.o \
      dcdatetimeeq.o dcdatetimegt.o dcdatetimelt.o dcdatetimege.o dcdatetimele.o \
      dcdatetimeparseunits.o \
      dc_calendar_types.o dc_calendar_generic.o dc_calendar_internal.o \
      dc_calendar.o \
      dccalcreate.o dccalinquire.o dccaltochar.o dccaldefault.o \
      dccaldatecreate.o dccaldateparsestr.o \
      dccaldateinquire.o dccaldatetochar.o dccaldateeval.o dccaldatechkleapyear.o \
      dccalconvertbyunit.o dccalconverttosec.o dccalconverttomin.o \
      dccalconverttohour.o dccalconverttoday.o \
      dccaldatedifference.o dccaldatecurrent.o dccalparseunit.o

RB2F90 = dc_test.f90 dc_string.f90 \
	dcstringputline.f90 dc_present.f90

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
dc_test.f90: dc_test.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --max_dim=$(MAXDIM) > $@ || \
	    touch $@

dc_string.f90: dc_string.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --max_dim=$(MAXDIM) > $@ || \
	    touch $@

dcstringputline.f90: dcstringputline.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< --max_dim=$(MAXDIM) > $@ || \
	    touch $@

dc_present.f90: dc_present.rb2f90
	$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
	    $(RUBY) $< > $@ || \
	    touch $@

include depend
