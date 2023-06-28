#= Makefile for build of gtool5 library in UNIX like platform
#
# Authors::   Yasuhiro MORIKAWA (morikawa)
# Version::   $Id: Makefile,v 1.2 2009-05-25 09:49:24 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
# License::   See COPYRIGHT[link:../../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=../../../..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

OBJS =  gtdata_netcdf_file_types.o \
	gtdata_netcdf_file_generic.o \
	gtdata_netcdf_file_internal.o \
	gdncfileopen.o gdncfileclose.o gdncfilereopen.o \
	gdncfiledatamode.o gdncfiledefinemode.o \
	gdncfilesync.o gdncfileinquirename.o

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
