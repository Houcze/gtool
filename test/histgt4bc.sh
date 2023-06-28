#!/bin/sh
#= A script for Backward Compatibility test for gt4_history
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: histgt4bc.sh,v 1.2 2008-09-23 14:09:47 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################

set -e

testsh="histtest histaxis histcopy histget histinquire"
#testsh="histtest"
suffix="_gt4bc"

######################################################################

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

######################################################################

for sh in $testsh; do
    echo $ECHO_N "${sh} testing for backward compatibility for gt4_history ..." $ECHO_C
    cat ${sh}.f90 | sed 's/use gtool_history/use gt4_history/' > ${sh}${suffix}.f90
    cat ${sh}.sh | \
	sed "s/\${MAKE:-make} ${sh}/\${MAKE:-make} ${sh}${suffix}/" | \
	sed "s|\./${sh}|./${sh}${suffix}|" > ${sh}${suffix}.sh
    sh ${sh}${suffix}.sh
    echo "okay"
    rm -f ${sh}${suffix} ${sh}${suffix}.sh ${sh}${suffix}.f90
done

echo ""
echo "   backward compatibility for gt4_history tests are successful"
echo ""

exit 0
