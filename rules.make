#= Rules for GNU make
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: rules.make,v 1.7 2009-05-31 09:04:55 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
# License::   See COPYRIGHT[link:COPYRIGHT]
#
#== Overview
#
# This file provides general rules for Makefiles.
# To use, "include" this file.
#
# [JAPANESE]
# このファイルには Makefile 群から呼び出される共通ルールが
# 記述されています. このファイルを使用するには, 各 Makefile で
# このファイルを include してください.
#
############################################################

############################################################
#== Suffixes rules
#
# [JAPANESE] サフィックスルール
#
%.o: %.F90
	@test $< -ot depend || eval "rm -fv depend; $(MAKE) depend"
	$(FC) -o $@ -c $(MODPATHFLAG)$(MODDIR)  $(FFLAGS) $(SYSFFLAGS) $(INCLUDES) $(CPPFLAGS) $<
	@if [ -f $(MODFILE) ]; then \
	    test -d $(MODDIR) || $(INSTALLDIR) $(MODDIR) ;\
	    $(CP) $(MODFILE) $(MODDIR) ;\
	    if [ ! `dirname $@` = "." ]; then \
		$(MV) $(MODFILE) `dirname $@` ;\
	    fi ;\
	fi

%.o: %.f90
	@test $< -ot depend || eval "rm -fv depend; $(MAKE) depend"
	$(FC) -o $@ -c $(MODPATHFLAG)$(MODDIR) $(FFLAGS) $(SYSFFLAGS) $(INCLUDES) $<
	@if [ -f $(MODFILE) ]; then \
	    test -d $(MODDIR) || $(INSTALLDIR) $(MODDIR) ;\
	    $(CP) $(MODFILE) $(MODDIR) ;\
	    if [ ! `dirname $@` = "." ]; then \
		$(MV) $(MODFILE) `dirname $@` ;\
	    fi ;\
	fi

%: %.o
	$(LINKF) -o $@ $(MODPATHFLAG)$(MODDIR) $+ $(LIBPATH) $(SYSLDFLAGS) $(SYSLDLIBS)

$(LIBPATH): $(OBJS)

archive:
	$(AR) $(ARFLAGS) $(LIBPATH) $(OBJS)
	$(RANLIB) $(LIBPATH)

rb2f90.default: $(RB2F90)

clean.default:
	-@$(RM) $(wildcard $(MODS) *.o)
	-@$(RM) $(wildcard $(OBJS:%.o=$(MODDIR)/%$(MODSUFFIX)))
	@test -d $(MODDIR) && $(RMDIR) $(MODDIR) 2> /dev/null || true
	-@$(RM) $(wildcard $(LIBPATH))

clean.rb2f90.default:
	-@$(RM) $(wildcard $(RB2F90))

Makefile.win.default: Makefile depend
	$(RUBY) $(SCRIPTWIN)/gnumakefileparser-test.rb
	$(RUBY) $(SCRIPTWIN)/gen-Makefile.win.rb --input $(shell echo $+ | sed -e 's/ /,/') --output $@

depend:
	@echo A file \"depend\" for Fortran 90/95 module dependencies is being updated ...
	@$(F90DEPEND) $(F90DEPENDOPTS) $(wildcard *.erb *.f90 *.F90 */*.erb */*.f90 */*.F90 ../*/*.erb ../*/*.f90 ../*/*.F90 $(DIRDEPTH)/src/*/*.erb $(DIRDEPTH)/src/*/*.f90 $(DIRDEPTH)/src/*/*.F90) > depend || \
	    touch depend

depend.touch:
	@touch depend

clean.depend.default:
	$(RM) depend

clean.mkwin.default:
	$(RM) Makefile.win

#$(OBJS): Makefile $(DIRDEPTH)/Config.mk
#	sh $(SCRIPTF90)/chkpcl.sh $(SYSFFLAGS)

######################################################################
#== Mode setting for Emacs
#Local Variables:
#mode: makefile
#End:
#
