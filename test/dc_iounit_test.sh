#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} dc_iounit_test

TESTDIR=tmp_dc_iounit_test
test -n "$MKDIR" || MKDIR=mkdir
test -n "$RM" || RM=rm

echo "dc_iounit Test"

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    $RM -rf $TESTDIR
    $MKDIR $TESTDIR
    ./dc_iounit_test 2> xdifs.log

else
    echo ""
    echo "   WARNING: Cross compile mode will be used."
    echo "            Submit ./dc_iounit_test  ."
    echo ""
    exit 1
fi



exit 0

