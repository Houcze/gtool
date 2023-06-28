#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histaxis
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d xhistaxis || $MKDIR xhistaxis

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    test ! -f xhistaxis/xhistaxis1.nc || rm -f xhistaxis/xhistaxis1.nc
    test ! -f xhistaxis/xhistaxis2.nc || rm -f xhistaxis/xhistaxis2.nc
    test ! -f xhistaxis/xhistaxis3.nc || rm -f xhistaxis/xhistaxis3.nc
    test ! -f xhistaxis/xhistaxis4.nc || rm -f xhistaxis/xhistaxis4.nc

    ./histaxis 2> xdifs.log
else
    if [ ! -f xhistaxis/xhistaxis1.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histaxis, execute $0 again."
	echo ""
        exit 1
    fi
fi

cd xhistaxis
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

###### Basic Create test of HistoryCreate2 and HistoryAddVariable2 #####

echo $ECHO_N "histaxis [Basic Create test of HistoryCreate2 and HistoryAddVariable2] ... " $ECHO_C

$NCDUMP xhistaxis1.nc       | sed "$history_to_unknown" > xhistaxis1.cdl
$NCDUMP xhistaxis1org.nc    | sed "$history_to_unknown" > xhistaxis1org.cdl
diff xhistaxis1.cdl xhistaxis1org.cdl > xhistaxis1dif.cdl ||

diff xhistaxis1dif.cdl - <<EOF
1c1
< netcdf xhistaxis1 {
---
> netcdf xhistaxis1org {
EOF

echo "okay"

rm -f xhistaxis1.nc xhistaxis1*.cdl xhistaxis1dif.cdl

###### Derived Type GT_HISTORY_AXIS and GT_HISTORY_VARINFO test #####

echo $ECHO_N "histaxis [Derived Type GT_HISTORY_AXIS and GT_HISTORY_VARINFO directly arguments test] ... " $ECHO_C

$NCDUMP xhistaxis2.nc       | sed "$history_to_unknown" > xhistaxis2.cdl
$NCDUMP xhistaxis2org.nc    | sed "$history_to_unknown" > xhistaxis2org.cdl
diff xhistaxis2.cdl xhistaxis2org.cdl > xhistaxis2dif.cdl ||

diff xhistaxis2dif.cdl - <<EOF
1c1
< netcdf xhistaxis2 {
---
> netcdf xhistaxis2org {
EOF

echo "okay"

rm -f xhistaxis2.nc xhistaxis2*.cdl xhistaxis2dif.cdl


###### Axes by GT_HISTORY_AXIS test #####

echo $ECHO_N "histaxis [HistorydCreate2 without Argument Keyword test] ... " $ECHO_C

$NCDUMP xhistaxis3.nc       | sed "$history_to_unknown" > xhistaxis3.cdl
$NCDUMP xhistaxis3org.nc    | sed "$history_to_unknown" > xhistaxis3org.cdl
diff xhistaxis3.cdl xhistaxis3org.cdl > xhistaxis3dif.cdl ||

diff xhistaxis3dif.cdl - <<EOF
1c1
< netcdf xhistaxis3 {
---
> netcdf xhistaxis3org {
EOF

echo "okay"

rm -f xhistaxis3.nc xhistaxis3*.cdl xhistaxis3dif.cdl


###### Axes by GT_HISTORY_AXIS test #####

echo $ECHO_N "histaxis [HistorydAddAttr2 test] ... " $ECHO_C

$NCDUMP xhistaxis4.nc       | sed "$history_to_unknown" > xhistaxis4.cdl
$NCDUMP xhistaxis4org.nc    | sed "$history_to_unknown" > xhistaxis4org.cdl
diff xhistaxis4.cdl xhistaxis4org.cdl > xhistaxis4dif.cdl ||

diff xhistaxis4dif.cdl - <<EOF
1c1
< netcdf xhistaxis4 {
---
> netcdf xhistaxis4org {
EOF

echo "okay"

rm -f xhistaxis4.nc xhistaxis4*.cdl xhistaxis4dif.cdl

rm -f xhistaxis?.nc xhistest?*.cdl ../xdifs.log

exit 0
