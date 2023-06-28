#!/bin/sh
#
#= Compile and Execute test program of "histnmlinfo"
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: histnmlinfo.sh,v 1.1 2008-09-23 09:56:39 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2007. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
######################################################################
#
#== Settings

test -n "$TEST_BASE" || TEST_BASE="histnmlinfo"
TEST_OBJ="${TEST_BASE}.f90"
TEST_EXEC="${TEST_BASE}"
#TEST_NML00="${TEST_BASE}_test00.nml"
TEST_DIR="xhistnmlinfo"

NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

#test -n "$LINKF" || LINKF=gt4frt
test -n "$MAKE" || MAKE=make

# End Settings
######################################################################

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

echo ""
echo "****** ${TEST_BASE} Test ******"
echo " in \"`pwd`\""

echo " Compiling and Linking ... "
if [ -n "${LINKF}" ]; then
    echo "   ${LINKF} ${TEST_OBJ} -o ${TEST_EXEC}"
    ${LINKF} ${TEST_OBJ} -o ${TEST_EXEC}
else
    ${MAKE} ${TEST_EXEC}
fi
echo " done . "

test -d $TEST_DIR || $MKDIR $TEST_DIR

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    echo "./${TEST_EXEC}"
    ./${TEST_EXEC}
#    echo "./${TEST_EXEC} --namelist=${TEST_NML00}"
#    ./${TEST_EXEC} --namelist=${TEST_NML00}
else
    echo ""
    echo "   WARNING: Cross compile mode will be used."
    echo "            Submit ./${TEST_EXEC}  ."
    echo ""
    exit 1
fi

cd $TEST_DIR
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

###### Basic Output test #####

echo $ECHO_N " [Basic Output test] ... " $ECHO_C

$NCDUMP Data1.nc    | sed "$history_to_unknown" > Data1.cdl
$NCDUMP Data1org.nc | sed "$history_to_unknown" > Data1org.cdl

diff Data1.cdl Data1org.cdl > Data1dif.cdl ||

diff Data1dif.cdl - <<EOF
1c1
< netcdf Data1 {
---
> netcdf Data1org {
EOF

echo "okay"

rm -f Data1.nc Data1*.cdl Data1dif.cdl


###### Multi variables Output test #####

echo $ECHO_N " [Multi variables Output test] ... " $ECHO_C

$NCDUMP Data2_3_4.nc    | sed "$history_to_unknown" > Data2_3_4.cdl
$NCDUMP Data2_3_4org.nc | sed "$history_to_unknown" > Data2_3_4org.cdl

diff Data2_3_4.cdl Data2_3_4org.cdl > Data2_3_4dif.cdl ||

diff Data2_3_4dif.cdl - <<EOF
1c1
< netcdf Data2_3_4 {
---
> netcdf Data2_3_4org {
EOF

echo "okay"

rm -f Data2_3_4.nc Data2_3_4*.cdl Data2_3_4dif.cdl


###### Close error handling test #####

echo $ECHO_N " [Close error handling test 1] ... " $ECHO_C

$NCDUMP Data5.nc    | sed "$history_to_unknown" > Data5.cdl
$NCDUMP Data5org.nc | sed "$history_to_unknown" > Data5org.cdl

diff Data5.cdl Data5org.cdl > Data5dif.cdl ||

diff Data5dif.cdl - <<EOF
1c1
< netcdf Data5 {
---
> netcdf Data5org {
EOF

echo "okay"

rm -f Data5.nc Data5*.cdl Data5dif.cdl


echo $ECHO_N " [Close error handling test 2] ... " $ECHO_C

$NCDUMP Data6.nc    | sed "$history_to_unknown" > Data6.cdl
$NCDUMP Data6org.nc | sed "$history_to_unknown" > Data6org.cdl

diff Data6.cdl Data6org.cdl > Data6dif.cdl ||

diff Data6dif.cdl - <<EOF
1c1
< netcdf Data6 {
---
> netcdf Data6org {
EOF

echo "okay"

rm -f Data6.nc Data6*.cdl Data6dif.cdl


echo "  *** Test program \"${TEST_EXEC}\" becomes successful ***"
exit 0
