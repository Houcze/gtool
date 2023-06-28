#= Makefile for gtool5/test
#
# Authors::   Yasuhiro MORIKAWA (morikawa), Eizi TOYODA (toyoda)
# Version::   $Id: Makefile,v 1.15 2009-10-12 05:11:52 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2008. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################
#
#== Settings

# in Config.mk, DEST_LIB, DEST_INC is defined
DIRDEPTH=..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE)

TARGETS=histinquire histtest histaxis histget histgetattr histcopy histnmlinfo \
	histauto histauto2

TARGETS_DC=dc_test_test dc_string_test dc_url_test dc_args_test dc_hash_test \
	dc_date_test dc_clock_test dc_iounit_test dc_scaledsec_test \
	dc_calendar_test

BCGT4=histgt4bc

TARGETS_MPI=histtest_mpi histget_mpi histgetattr_mpi histauto_mpi histauto2_mpi

CLEANED=*.o $(MODS) $(TARGETS_DC) $(TARGETS) $(TARGETS_MPI) xdifs.log \
	dc_clock_test.tmp \
	xhisttest/xhisttest?.nc xhisttest/xhisttest??.nc \
	xhisttest/xhisttest*.cdl \
	xhistaxis/xhistaxis?.nc xhistaxis/xhistaxis*.cdl \
	xhistget/xhistget?.nc xhistget/xhistgetdif?.cdl \
	xhistcopy/xhistcopy?.nc xhistcopy/xhistcopy*.cdl \
	xhistinquire?.nc \
	xhistauto/tmp.nc xhistauto/tmp?.nc xhistauto/tmp?_??.nc \
	xhistauto/tmp?_????.nc xhistauto/*.cdl \
	xhistauto2/tmp.nc xhistauto2/tmp?.nc xhistauto2/tmp?_??.nc \
	xhistauto2/tmp?_????.nc xhistauto2/*.cdl \
	xhisttest_mpi/*.cdl \
	xhistget_mpi/xhistget?_rank??????.nc xhistget_mpi/*.cdl \
	xhistnmlinfo/*.cdl \
	xhistnmlinfo/Data?.nc \
	xhistnmlinfo/Data2_3_4.nc

CLEANED_DIR=tmp_dc_iounit_test

# End Settings
######################################################################

######################################################################
#== Rules
#

%: %.sh   # clear an implicit rule

all: $(LIBPATH) $(TARGETS_DC) $(TARGETS)

include $(DIRDEPTH)/rules.make

mpi: mpi-check $(LIBPATH) $(TARGETS_MPI)

mpi-check:
	@test -n "$(MPIEXEC)" || \
	    eval 'echo ; echo "  Error: \$$MPIEXEC is not specified."; echo "         If you want to build gtool5 with MPI library, "; echo "         specify --with-mpiexec to configure."; echo  ; exit 1'
	@test -n "$(MPIEXECPROCOPT)" || \
	    eval 'echo ; echo "  Error: \$$MPIEXECPROCOPT is not specified."; echo "         \$$MPIEXEC=$(MPIEXEC) is unknown MPI executable command, "; echo "         so edit \$$MPIEXECPROCOPT in Config.mk directly."; echo  ; exit 1'

clean: clean-histauto clean-histtest-mpi
	@if [ ! X"$(wildcard $(CLEANED))" = X"" ]; then \
		echo $(RM) $(wildcard $(CLEANED)) ;\
		$(RM) $(wildcard $(CLEANED)) ;\
	fi
	@if [ ! X"$(wildcard $(CLEANED_DIR))" = X"" ]; then \
		echo $(RM) -r $(wildcard $(CLEANED_DIR)) ;\
		$(RM) -r $(wildcard $(CLEANED_DIR)) ;\
	fi

clean-histauto:
	cd xhistauto  ; $(RM) -f `sh ../histauto.sh -l`
	cd xhistauto2 ; $(RM) -f `sh ../histauto2.sh -l`

clean-histtest-mpi:
	cd xhisttest_mpi ; $(RM) -f `sh ../histtest_mpi.sh -l`

test: all
	@test -n "$(NCDUMP)" || \
	    eval 'echo ; echo "  Error: ncdump: command not found."; echo ; exit 1'
	@if [ ! "$(CROSS_COMPILING)" = "yes" ] && [ ! "$(CROSS_COMPILING)" = "maybe" ] ; then \
	    for i in $(TARGETS_DC) $(TARGETS) $(BCGT4) ; do  \
	      CROSS_COMPILING=$(CROSS_COMPILING) NCDUMP=$(NCDUMP) \
	      MKDIR="$(MKDIR)" MAKE="$(MAKE)" \
		 sh $$i.sh || \
	         eval 'echo "  Error: stderr is output to xdifs.log ." ; exit 1';\
	    done ;\
	else \
	    echo "" ;\
	    echo "   Cross compile mode will be used." ;\
	    echo "   After submitting ..." ;\
	    echo "          $(TARGETS_DC) $(TARGETS)," ;\
	    echo "" ;\
	    echo "   do \"make test-c\"" ;\
	    echo "" ;\
	    exit;\
	fi
	@echo ""
	@echo "   **** All Tests were Successful ****"
	@echo ""

test-f: all
	@test -n "$(NCDUMP)" || \
	    eval 'echo ; echo "  Error: ncdump: command not found."; echo ; exit 1'
	-@for i in $(TARGETS_DC) $(TARGETS) $(BCGT4)  ; do  \
	  CROSS_COMPILING=$(CROSS_COMPILING) NCDUMP=$(NCDUMP) \
	      MKDIR="$(MKDIR)" MAKE="$(MAKE)" \
		 sh $$i.sh || \
	     echo "  $$i.sh: Error (continue mandatorily)";\
	done

test-c: all
	@test -n "$(NCDUMP)" || \
	    eval 'echo ; echo "  Error: ncdump: command not found."; echo ; exit 1'
	@for i in $(TARGETS) $(BCGT4) ; do  \
	  CROSS_COMPILING=$(CROSS_COMPILING) NCDUMP=$(NCDUMP) \
	      MKDIR="$(MKDIR)" MAKE="$(MAKE)" \
	      sh $$i.sh || exit 1; \
	done
	@echo ""
	@echo "   **** All Tests were Successful ****"
	@echo ""

test-mpi: mpi
	@test -n "$(NCDUMP)" || \
	    eval 'echo ; echo "  Error: ncdump: command not found."; echo ; exit 1'
	@if [ ! "$(CROSS_COMPILING)" = "yes" ] && [ ! "$(CROSS_COMPILING)" = "maybe" ] ; then \
	    for i in $(TARGETS_MPI) ; do  \
	      CROSS_COMPILING=$(CROSS_COMPILING) NCDUMP=$(NCDUMP) \
	      MPIEXEC=$(MPIEXEC) MPIEXECPROCOPT=$(MPIEXECPROCOPT) \
	      MKDIR="$(MKDIR)" MAKE="$(MAKE)" \
		 sh $$i.sh || \
	         eval 'echo "  Error: stderr is output to xdifs.log ." ; exit 1';\
	    done ;\
	else \
	    echo "" ;\
	    echo "   Cross compile mode will be used." ;\
	    echo "   After submitting ..." ;\
	    echo "          $(TARGETS_DC) $(TARGETS)," ;\
	    echo "" ;\
	    echo "   do \"make test-mpi-c\"" ;\
	    echo "" ;\
	    exit;\
	fi
	@echo ""
	@echo "   **** All Tests (MPI) were Successful ****"
	@echo ""

test-mpi-c: all
	@test -n "$(NCDUMP)" || \
	    eval 'echo ; echo "  Error: ncdump: command not found."; echo ; exit 1'
	@for i in $(TARGETS_MPI) ; do  \
	  CROSS_COMPILING=$(CROSS_COMPILING) NCDUMP=$(NCDUMP) \
	      MKDIR="$(MKDIR)" MAKE="$(MAKE)" \
	      MPIEXEC=$(MPIEXEC) MPIEXECPROCOPT=$(MPIEXECPROCOPT) \
	      sh $$i.sh || exit 1; \
	done
	@echo ""
	@echo "   **** All Tests (MPI) were Successful ****"
	@echo ""
