#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histget
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d xhistget || $MKDIR xhistget

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    test ! -f xhistget/xhistget1.nc || rm -f xhistget/xhistget1.nc
    test ! -f xhistget/xhistget2.nc || rm -f xhistget/xhistget2.nc
    test ! -f xhistget/xhistget3.nc || rm -f xhistget/xhistget3.nc
    test ! -f xhistget/xhistget4.nc || rm -f xhistget/xhistget4.nc
    test ! -f xhistget/xhistget5.nc || rm -f xhistget/xhistget5.nc
    test ! -f xhistget/xhistget6.nc || rm -f xhistget/xhistget6.nc

    ./histget 2> xdifs.log
else
    if [ ! -f xhistget/xhistget1.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histget, execute $0 again."
	echo ""
        exit 1
    fi
fi

cd xhistget
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

$NCDUMP xhistget1.nc | sed "$history_to_unknown" > xhistget1.cdl
$NCDUMP xhistget2.nc | sed "$history_to_unknown" > xhistget2.cdl
$NCDUMP xhistget3.nc | sed "$history_to_unknown" > xhistget3.cdl
$NCDUMP xhistget4.nc | sed "$history_to_unknown" > xhistget4.cdl
$NCDUMP xhistget5.nc | sed "$history_to_unknown" > xhistget5.cdl
$NCDUMP xhistget6.nc | sed "$history_to_unknown" > xhistget6.cdl

##### fixed-length array data input test ######

echo $ECHO_N "histget [fixed-length array data input test] ... " $ECHO_C

diff xhistget1.cdl xhistget2.cdl > xhistgetdif1.cdl ||

diff xhistgetdif1.cdl - <<EOF
1c1
< netcdf xhistget1 {
---
> netcdf xhistget2 {
EOF

echo "okay"


##### pointer array data input test ######

echo $ECHO_N "histget [pointer array data input test] ... " $ECHO_C

diff xhistget1.cdl xhistget3.cdl > xhistgetdif2.cdl ||

diff xhistgetdif2.cdl - <<EOF
1c1
< netcdf xhistget1 {
---
> netcdf xhistget3 {
EOF

echo "okay"


##### dimension array data input test ######

echo $ECHO_N "histget [dimension array data input test] ... " $ECHO_C

diff xhistget1.cdl xhistget4.cdl > xhistgetdif3.cdl ||

diff xhistgetdif3.cdl - <<EOF
1c1
< netcdf xhistget1 {
---
> netcdf xhistget4 {
EOF

echo "okay"


##### range option test ######

echo $ECHO_N "histget [range option test] ... " $ECHO_C

diff xhistget1.cdl xhistget5.cdl > xhistgetdif4.cdl ||

diff xhistgetdif4.cdl - <<EOF
1c1
< netcdf xhistget1 {
---
> netcdf xhistget5 {
EOF

echo "okay"


##### range option test ######

echo $ECHO_N "histget [getting array with different dimsize array test] ... " $ECHO_C

diff xhistget1.cdl xhistget6.cdl > xhistgetdif5.cdl ||

diff xhistgetdif5.cdl - <<EOF
1c1
< netcdf xhistget1 {
---
> netcdf xhistget6 {
EOF

echo "okay"



rm -f ../xdifs.log xhistget?.* xhistgetdif?.cdl

exit 0
