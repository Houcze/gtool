#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histtest
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d xhisttest || $MKDIR xhisttest

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    test ! -f xhisttest/xhisttest1.nc || rm -f xhisttest/xhisttest1.nc
    test ! -f xhisttest/xhisttest2.nc || rm -f xhisttest/xhisttest2.nc
    test ! -f xhisttest/xhisttest3.nc || rm -f xhisttest/xhisttest3.nc
    test ! -f xhisttest/xhisttest4.nc || rm -f xhisttest/xhisttest4.nc
    test ! -f xhisttest/xhisttest5.nc || rm -f xhisttest/xhisttest5.nc
    test ! -f xhisttest/xhisttest6.nc || rm -f xhisttest/xhisttest6.nc
    test ! -f xhisttest/xhisttest7.nc || rm -f xhisttest/xhisttest7.nc
    test ! -f xhisttest/xhisttest8.nc || rm -f xhisttest/xhisttest8.nc
    test ! -f xhisttest/xhisttest8.nc || rm -f xhisttest/xhisttest9.nc
    test ! -f xhisttest/xhisttest10.nc || rm -f xhisttest/xhisttest10.nc
    test ! -f xhisttest/xhisttest11.nc || rm -f xhisttest/xhisttest11.nc
    test ! -f xhisttest/xhisttest12.nc || rm -f xhisttest/xhisttest12.nc
    test ! -f xhisttest/xhisttest13.nc || rm -f xhisttest/xhisttest13.nc
    test ! -f xhisttest/xhisttest14.nc || rm -f xhisttest/xhisttest14.nc

    ./histtest 2> xdifs.log
else
    if [ ! -f xhisttest/xhisttest1.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histtest, execute $0 again."
	echo ""
        exit 1
    fi
fi

cd xhisttest
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

###### Basic Create test (Conventions auto output) #####

echo $ECHO_N "histtest [Basic Create test (Conventions auto output)] ... " $ECHO_C

$NCDUMP xhisttest1.nc    | sed "$history_to_unknown" > xhisttest1.cdl
$NCDUMP xhisttest1org.nc | sed "$history_to_unknown" > xhisttest1org.cdl

diff xhisttest1.cdl xhisttest1org.cdl > xhisttest1dif.cdl ||

diff xhisttest1dif.cdl - <<EOF
1c1
< netcdf xhisttest1 {
---
> netcdf xhisttest1org {
EOF

echo "okay"

rm -f xhisttest1.nc xhisttest1*.cdl xhisttest1dif.cdl

###### Auto Set Time test (gt_version manually output) #####

echo $ECHO_N "histtest [Auto Set Time test (gt_version manually output)] ... " $ECHO_C

$NCDUMP xhisttest2.nc    | sed "$history_to_unknown"    > xhisttest2.cdl
$NCDUMP xhisttest2org.nc | sed "$history_to_unknown"    > xhisttest2org.cdl
diff xhisttest2.cdl xhisttest2org.cdl > xhisttest2dif.cdl ||

diff xhisttest2dif.cdl - <<EOF
1c1
< netcdf xhisttest2 {
---
> netcdf xhisttest2org {
EOF

echo "okay"

rm -f xhisttest2.nc xhisttest2*.cdl xhisttest2dif.cdl

###### Axes by GT_HISTORY_AXIS test #####

echo $ECHO_N "histtest [Set Time by HistoryPut test (Conventions, gt_version manually output) (origin, interval auto setting)] ... " $ECHO_C

$NCDUMP xhisttest3.nc    | sed "$history_to_unknown"    > xhisttest3.cdl
$NCDUMP xhisttest3org.nc | sed "$history_to_unknown"    > xhisttest3org.cdl
diff xhisttest3.cdl xhisttest3org.cdl > xhisttest3dif.cdl ||

diff xhisttest3dif.cdl - <<EOF
1c1
< netcdf xhisttest3 {
---
> netcdf xhisttest3org {
EOF

echo "okay"

rm -f xhisttest3.nc xhisttest3*.cdl xhisttest3dif.cdl


###### HistorySetTime test #####

echo $ECHO_N "histtest [HistorySetTime test] ... " $ECHO_C

$NCDUMP xhisttest4.nc    | sed "$history_to_unknown"    > xhisttest4.cdl
$NCDUMP xhisttest4org.nc | sed "$history_to_unknown"    > xhisttest4org.cdl
diff xhisttest4.cdl xhisttest4org.cdl > xhisttest4dif.cdl ||

diff xhisttest4dif.cdl - <<EOF
1c1
< netcdf xhisttest4 {
---
> netcdf xhisttest4org {
EOF

echo "okay"

rm -f xhisttest4.nc xhisttest4*.cdl xhisttest4dif.cdl



###### HistorySetTime test #####

echo $ECHO_N "histtest [HistoryCreate without Argument Keyword test] ... " $ECHO_C

$NCDUMP xhisttest5.nc    | sed "$history_to_unknown"    > xhisttest5.cdl
$NCDUMP xhisttest5org.nc | sed "$history_to_unknown"    > xhisttest5org.cdl
diff xhisttest5.cdl xhisttest5org.cdl > xhisttest5dif.cdl ||

diff xhisttest5dif.cdl - <<EOF
1c1
< netcdf xhisttest5 {
---
> netcdf xhisttest5org {
EOF

echo "okay"

rm -f xhisttest5.nc xhisttest5*.cdl xhisttest5dif.cdl



###### HistoryAddAttr test #####

echo $ECHO_N "histtest [HistoryAddAttr test] ... " $ECHO_C

$NCDUMP xhisttest6.nc    | sed "$history_to_unknown"    > xhisttest6.cdl
$NCDUMP xhisttest6org.nc | sed "$history_to_unknown"    > xhisttest6org.cdl
diff xhisttest6.cdl xhisttest6org.cdl > xhisttest6dif.cdl ||

diff xhisttest6dif.cdl - <<EOF
1c1
< netcdf xhisttest6 {
---
> netcdf xhisttest6org {
EOF

echo "okay"

rm -f xhisttest6.nc xhisttest6*.cdl xhisttest6dif.cdl


###### HistoryCopy test 1 #####

echo $ECHO_N "histtest [HistoryCopy test 1] ... " $ECHO_C

$NCDUMP xhisttest7.nc    | sed "$history_to_unknown"    > xhisttest7.cdl
$NCDUMP xhisttest7org.nc | sed "$history_to_unknown"    > xhisttest7org.cdl
diff xhisttest7.cdl xhisttest7org.cdl > xhisttest7dif.cdl ||

diff xhisttest7dif.cdl - <<EOF
1c1
< netcdf xhisttest7 {
---
> netcdf xhisttest7org {
EOF

echo "okay"

rm -f xhisttest7.nc xhisttest7*.cdl xhisttest7dif.cdl



###### HistoryCopy test 2 #####

echo $ECHO_N "histtest [HistoryCopy test 2] ... " $ECHO_C

$NCDUMP xhisttest8.nc    | sed "$history_to_unknown"    > xhisttest8.cdl
$NCDUMP xhisttest8org.nc | sed "$history_to_unknown"    > xhisttest8org.cdl
diff xhisttest8.cdl xhisttest8org.cdl > xhisttest8dif.cdl ||

diff xhisttest8dif.cdl - <<EOF
1c1
< netcdf xhisttest8 {
---
> netcdf xhisttest8org {
EOF

echo "okay"

rm -f xhisttest8.nc xhisttest8*.cdl xhisttest8dif.cdl


###### "range" argument test in HistoryPut test #####

# echo $ECHO_N "histtest [\"range\" argument test in HistoryPut test] ... " $ECHO_C

# $NCDUMP xhisttest9.nc    | sed "$history_to_unknown"    > xhisttest9.cdl
# $NCDUMP xhisttest9org.nc | sed "$history_to_unknown"    > xhisttest9org.cdl
# diff xhisttest9.cdl xhisttest9org.cdl > xhisttest9dif.cdl ||

# diff xhisttest9dif.cdl - <<EOF
# 1c1
# < netcdf xhisttest9 {
# ---
# > netcdf xhisttest9org {
# EOF

# echo "okay"

rm -f xhisttest9.nc xhisttest9*.cdl xhisttest9dif.cdl


###### average output test in HistoryPut test #####

echo $ECHO_N "histtest [average output test in HistoryPut test] ... " $ECHO_C

$NCDUMP xhisttest10.nc    | sed "$history_to_unknown"    > xhisttest10.cdl
$NCDUMP xhisttest10org.nc | sed "$history_to_unknown"    > xhisttest10org.cdl
diff xhisttest10.cdl xhisttest10org.cdl > xhisttest10dif.cdl ||

diff xhisttest10dif.cdl - <<EOF
1c1
< netcdf xhisttest10 {
---
> netcdf xhisttest10org {
EOF

echo "okay"

rm -f xhisttest10.nc xhisttest10*.cdl xhisttest10dif.cdl


###### Create with DC_DIFFTIME test #####

echo $ECHO_N "histtest [Create with DC_DIFFTIME test] ... " $ECHO_C

$NCDUMP xhisttest11.nc    | sed "$history_to_unknown"    > xhisttest11.cdl
$NCDUMP xhisttest11org.nc | sed "$history_to_unknown"    > xhisttest11org.cdl
diff xhisttest11.cdl xhisttest11org.cdl > xhisttest11dif.cdl ||

diff xhisttest11dif.cdl - <<EOF
1c1
< netcdf xhisttest11 {
---
> netcdf xhisttest11org {
EOF

echo "okay"

rm -f xhisttest11.nc xhisttest11*.cdl xhisttest11dif.cdl


###### SetTime with DC_DIFFTIME test #####

echo $ECHO_N "histtest [SetTime with DC_DIFFTIME test] ... " $ECHO_C

$NCDUMP xhisttest12.nc    | sed "$history_to_unknown"    > xhisttest12.cdl
$NCDUMP xhisttest12org.nc | sed "$history_to_unknown"    > xhisttest12org.cdl
diff xhisttest12.cdl xhisttest12org.cdl > xhisttest12dif.cdl ||

diff xhisttest12dif.cdl - <<EOF
1c1
< netcdf xhisttest12 {
---
> netcdf xhisttest12org {
EOF

echo "okay"

rm -f xhisttest12.nc xhisttest12*.cdl xhisttest12dif.cdl


###### Output of averaged values with unequally-spaced time test #####

echo $ECHO_N "histtest [Output of averaged values with unequally-spaced time test] ... " $ECHO_C

$NCDUMP xhisttest13.nc    | sed "$history_to_unknown"    > xhisttest13.cdl
$NCDUMP xhisttest13org.nc | sed "$history_to_unknown"    > xhisttest13org.cdl
diff xhisttest13.cdl xhisttest13org.cdl > xhisttest13dif.cdl ||

diff xhisttest13dif.cdl - <<EOF
1c1
< netcdf xhisttest13 {
---
> netcdf xhisttest13org {
EOF

echo "okay"

rm -f xhisttest13.nc xhisttest13*.cdl xhisttest13dif.cdl


###### Char data output test #####

echo $ECHO_N "histtest [Char data output test] ... " $ECHO_C

$NCDUMP xhisttest14.nc    | sed "$history_to_unknown"    > xhisttest14.cdl
$NCDUMP xhisttest14org.nc | sed "$history_to_unknown"    > xhisttest14org.cdl
diff xhisttest14.cdl xhisttest14org.cdl > xhisttest14dif.cdl ||

diff xhisttest14dif.cdl - <<EOF
1c1
< netcdf xhisttest14 {
---
> netcdf xhisttest14org {
EOF

echo "okay"

rm -f xhisttest14.nc xhisttest14*.cdl xhisttest14dif.cdl


rm -f xhisttest?.nc xhisttest??.nc xhistest?*.cdl ../xdifs.log

exit 0
