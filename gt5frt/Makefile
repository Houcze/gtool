#= Makefile for gtool5/gt5frt
#
# Authors::   Eizi TOYODA, Yasuhiro MORIKAWA, Youhei SASAKI
# Copyright:: Copyright (C) GFD Dennou Club, 2004-2012. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#
#vi: set ts=8 sw=4:
#
######################################################################
#
#== Settings
#
# [JAPANESE] 各種設定項目

DIRDEPTH=..
INCLUDEFILE = $(DIRDEPTH)/Config.mk
include $(INCLUDEFILE) # Include file              (optional)
                       # [JA] インクルードファイル (任意)
TARGET=gt5

TESTDIR  = ../test
TESTFILES=dc_test_test dc_string_test dc_url_test dc_args_test dc_hash_test \
	dc_date_test dc_clock_test dc_iounit_test dc_scaledsec_test \
	histinquire \
	histtest histaxis histget histcopy histnmlinfo histauto
TESTS    = $(TESTFILES:%=../test/%)

TESTFILES_MPI = histtest_mpi
TESTS_MPI     = $(TESTFILES_MPI:%=../test/%)


# End Settings  [JA] 設定項目ここまで
######################################################################


all: $(TARGET)frt$(gt5binsuffix) $(TARGET)config$(gt5binsuffix)

install: $(TARGET)frt$(gt5binsuffix)
	test -d $(DEST_BIN) || $(INSTALLDIR) $(DEST_BIN)
	$(INSTALLBIN) $(TARGET)frt$(gt5binsuffix) $(DEST_BIN)
	$(INSTALLBIN) $(TARGET)config$(gt5binsuffix) $(DEST_BIN)

$(TARGET)frt$(gt5binsuffix): gt5frtgen.sh Makefile ../Config.mk
	@echo "Making shell script $(TARGET)frt$(gt5binsuffix)"
	LIBBODY=$(LIBBODY) MODINSTDIR=$(DEST_INC) \
		LIBINSTDIR=$(DEST_LIB) \
		SYSLDFLAGS="$(SYSLDFLAGS)" \
		SYSLDLIBS="$(SYSLDLIBS)" \
		SYSFFLAGS="$(SYSFFLAGS)" \
		F90MODTYPE=$(F90MODTYPE) \
		MODPATHFLAG=$(MODPATHFLAG) \
		FC=$(FC) \
		LINKF="$(LINKF)" HOST_CPU="$(HOST_CPU)"\
		sh ./gt5frtgen.sh $@
	chmod +x $(TARGET)frt$(gt5binsuffix)

$(TARGET)config$(gt5binsuffix): gt5configgen.sh Makefile ../Config.mk
	@echo "Making shell script $(TARGET)config$(gt5binsuffix)"
	LIBBODY=$(LIBBODY) MODINSTDIR=$(DEST_INC) \
		LIBINSTDIR=$(DEST_LIB) \
		SYSLDFLAGS="$(SYSLDFLAGS)" \
		SYSLDLIBS="$(SYSLDLIBS)" \
		SYSFFLAGS="$(SYSFFLAGS)" \
		F90MODTYPE=$(F90MODTYPE) \
		MODPATHFLAG=$(MODPATHFLAG) \
		FC=$(FC) \
		LINKF="$(LINKF)" HOST_CPU="$(HOST_CPU)"\
		sh ./gt5configgen.sh $@
	chmod +x $(TARGET)config$(gt5binsuffix)

clean:
	-rm -f *~ $(TARGET)frt$(gt5binsuffix) $(TARGET)config$(gt5binsuffix)

test-installed: test-build test

test-installed-c: test-c

test-build: 
	@for i in $(TESTS) ; do \
	  $(DEST_BIN)/$(TARGET)frt$(gt5binsuffix) -c $$i.f90 -o $$i.o ;\
	  $(DEST_BIN)/$(TARGET)frt$(gt5binsuffix) -o $$i $$i.o ; \
	done

test-mpi-installed: test-mpi-build test-mpi

test-mpi-build: 
	@for i in $(TESTS_MPI) ; do \
	  $(DEST_BIN)/$(TARGET)frt$(gt5binsuffix) -c $$i.f90 -o $$i.o ;\
	  $(DEST_BIN)/$(TARGET)frt$(gt5binsuffix) -o $$i $$i.o ; \
	done

test:
	@if [ ! "$(CROSS_COMPILING)" = "yes" ] && [ ! "$(CROSS_COMPILING)" = "maybe" ] ; then \
	    echo "cd $(TESTDIR)" ; cd $(TESTDIR) || exit 1 ; \
	    echo "make $@"       ; make $@ || exit 1 ; \
	    echo "make clean"    ; make clean ; \
	else \
	    echo "" ;\
	    echo "   Cross compile mode will be used." ;\
	    echo "   First, change directory to $(DIRDEPTH)/test ." ;\
	    echo "   Secondly, submit ..." ;\
	    echo "          $(TESTFILES)," ;\
	    echo "" ;\
	    echo "   Thirdly, change directory to $(DIRDEPTH) ." ;\
	    echo "   Last \"make test-installed-c\"" ;\
	    echo "" ;\
	    exit ;\
	fi
	@echo ""
	@echo ""
	@echo "   *** Compilation and installation are succeeded !! ***"
	@echo ""
	@echo ""

test-mpi:
	@if [ ! "$(CROSS_COMPILING)" = "yes" ] && [ ! "$(CROSS_COMPILING)" = "maybe" ] ; then \
	    echo "cd $(TESTDIR)" ; cd $(TESTDIR) || exit 1 ; \
	    echo "make $@"       ; make $@ || exit 1 ; \
	    echo "make clean"    ; make clean ; \
	else \
	    echo "" ;\
	    echo "   Cross compile mode will be used." ;\
	    echo "   First, change directory to $(DIRDEPTH)/test ." ;\
	    echo "   Secondly, submit ..." ;\
	    echo "          $(TESTFILES)," ;\
	    echo "" ;\
	    echo "   Thirdly, change directory to $(DIRDEPTH) ." ;\
	    echo "   Last \"make test-installed-c\"" ;\
	    echo "" ;\
	    exit ;\
	fi
	@echo ""
	@echo ""
	@echo "   *** Compilation and installation (MPI) are succeeded !! ***"
	@echo ""
	@echo ""

test-c:
	cd $(TESTDIR) ; make $@ || exit 1 ; make clean
	@echo ""
	@echo ""
	@echo "   *** Compilation and installation are succeeded !! ***"
	@echo ""
	@echo ""
