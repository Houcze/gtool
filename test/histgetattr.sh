#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histgetattr
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d xhistgetattr || $MKDIR xhistgetattr

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    test ! -f xhistgetattr/xhistgetattr1.nc || rm -f xhistgetattr/xhistgetattr1.nc
    ./histgetattr 2> xdifs.log
else
    if [ ! -f xhistgetattr/xhistgetattr1.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histgetattr, execute $0 again."
	echo ""
        exit 1
    fi
fi

rm -f ../xdifs.log xhistgetattr?.* xhistgetattrdif?.cdl

exit 0
