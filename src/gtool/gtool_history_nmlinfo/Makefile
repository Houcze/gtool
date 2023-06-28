#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.1 2009-05-11 15:15:15 morikawa Exp $
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
	gtool_history_nmlinfo_types.o gtool_history_nmlinfo_internal.o \
	gtool_history_nmlinfo_generic.o \
	gtool_history_nmlinfo.o \
	hstnmlinfocreate.o hstnmlinfoclose.o hstnmlinfoputline.o \
	hstnmlinfoinitialized.o hstnmlinfodefinemode.o \
	hstnmlinfoenddefine.o hstnmlinforedefine.o \
	hstnmlinfoadd.o hstnmlinfodelete.o \
	hstnmlinforesetdefault.o hstnmlinfoinquire.o \
	hstnmlinfoassocgthist.o hstnmlinfooutputstepdisable.o \
	hstnmlinfooutputstep.o hstnmlinfooutputvalid.o \
	hstnmlinfonames.o hstnmlinfogetnames.o hstnmlinfoallvarinicheck.o \
	hstnmlinfosetvalidname.o hstnmlinfoallnamevalid.o

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

include depend
