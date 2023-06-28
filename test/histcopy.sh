#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

${MAKE:-make} histcopy
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d xhistcopy || $MKDIR xhistcopy

##### make CDL file from Original data by ncdump #####
cp xhistcopy/xhistcopy1org.nc xhistcopy1.nc
cp xhistcopy/xhistcopy2org.nc xhistcopy2.nc
cp xhistcopy/xhistcopy3org.nc xhistcopy3.nc
$NCDUMP xhistcopy1.nc > xhistcopy/xhistcopy1org.cdl
$NCDUMP xhistcopy2.nc > xhistcopy/xhistcopy2org.cdl
$NCDUMP xhistcopy3.nc > xhistcopy/xhistcopy3org.cdl
rm -f xhistcopy1.nc
rm -f xhistcopy2.nc
rm -f xhistcopy3.nc

##### Execute histcopy #####
if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    test ! -f xhistcopy/xhistcopy1.nc || rm -f xhistcopy/xhistcopy1.nc
    test ! -f xhistcopy/xhistcopy2.nc || rm -f xhistcopy/xhistcopy2.nc
    test ! -f xhistcopy/xhistcopy3.nc || rm -f xhistcopy/xhistcopy3.nc

    ./histcopy 2> xdifs.log
else
    if [ ! -f xhistcopy/xhistcopy1.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histcopy, execute $0 again."
	echo ""
        exit 1
    fi
fi


cd xhistcopy
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

##### Make CDL file #####
$NCDUMP xhistcopy1.nc | sed "$history_to_unknown" > xhistcopy1.cdl
$NCDUMP xhistcopy2.nc | sed "$history_to_unknown" > xhistcopy2.cdl
$NCDUMP xhistcopy3.nc | sed "$history_to_unknown" > xhistcopy3.cdl

diff xhistcopy1org.cdl xhistcopy2org.cdl > xhistcopydif2org.cdl ||
diff xhistcopy1org.cdl xhistcopy3org.cdl > xhistcopydif3org.cdl ||

diff xhistcopy1.cdl xhistcopy2.cdl > xhistcopydif2.cdl ||
diff xhistcopy1.cdl xhistcopy3.cdl > xhistcopydif3.cdl ||

##### dimension identify test ######

echo $ECHO_N "histcopy [dimension identify test] ... " $ECHO_C

diff xhistcopydif2org.cdl xhistcopydif2.cdl

echo "okay"


##### dimension automatic generation test ######

echo $ECHO_N "histcopy [dimension automatic generation test] ... " $ECHO_C

diff xhistcopydif3org.cdl xhistcopydif3.cdl

echo "okay"

rm -f ../xdifs.log xhistcopy?.nc xhistcopy*.cdl

exit 0
