#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histinquire

echo "histinquire Test"

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    ./histinquire 2> xdifs.log
else
    echo ""
    echo "   WARNING: Cross compile mode will be used."
    echo "            Submit ./histinquire  ."
    echo ""
    exit 1
fi




exit 0
