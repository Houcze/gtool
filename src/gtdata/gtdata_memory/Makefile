#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.3 2009-05-25 09:47:27 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=../../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

OBJS = gtdata_memory_types.o gtdata_memory_internal.o \
	gtdata_memory_generic.o \
	gdmemvarattradd.o gdmemvarattrdel.o gdmemvarattrget.o \
	gdmemvarattrnext.o gdmemvarattrrewind.o gdmemvarattrtrue.o \
	gdmemvarclose.o gdmemvarcreated.o

RB2F90 = 

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


include depend

