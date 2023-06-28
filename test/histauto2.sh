#!/bin/sh

set -e

case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac

filelist=
while [ $# -gt 0 ]; do
    case $1 in
        "-l")
            filelist=1
            shift
            ;;
        *)
            break
            ;;
    esac
done

files="test01-u.nc test01-v.nc test01-w.nc"
files="$files test02-u.nc test02-v.nc test03-u.nc "
files="$files test04-u.nc test04-v.nc test04-ww.nc"
files="$files test05-ns.nc test05-v.nc test05-w.nc"
files="$files test06-ns.nc test06-v.nc test06-w.nc"
files="$files test07-u.nc test07-v.nc test07-w.nc"
files="$files test08-u.nc test08-v.nc test08-w.nc"
files="$files test09-u.nc test09-v.nc test09-w.nc"
files="$files test10-u.nc test10-v.nc test10-w.nc"
files="$files test11-u_time00000000.nc test11-u_time00000001.nc test11-u_time00000002.nc test11-u_time00000003.nc test11-u_time00000004.nc test11-v_time00000001.nc test11-v_time00000002.nc test11-v_time00000003.nc test11-w_time00000001.nc test11-w_time00000002.nc test11-w_time00000003.nc"
files="$files test12-u_time00000000.nc test12-u_time00000030.nc test12-u_time00000060.nc test12-u_time00000090.nc test12-v_time00000024.nc test12-v_time00000030.nc test12-v_time00000036.nc test12-w_time00000025.nc test12-w_time00000030.nc test12-w_time00000035.nc"
files="$files test13-u_rank01.nc test13-v_rank01.nc test13-w_rank01_time00000000.nc test13-w_rank01_time00000100.nc test13-w_rank01_time00000200.nc test13-w_rank01_time00000300.nc"
# files="$files test14-u.nc test14-w.nc"
files="$files test15-01-u.nc test15-01-v.nc"
files="$files test15-02-u.nc test15-02-v.nc"
files="$files test16-u.nc test16-v.nc test16-w.nc"
files="$files test17-u.nc"
files="$files test18-u.nc test18-v.nc test18-w.nc"
files="$files test19-gregorian-u.nc test19-noleap-u.nc test19-none-u.nc"
files="$files test20-u.nc"
files="$files test21-u.nc test21-v.nc test21-w.nc"
files="$files test22-u.nc test22-v.nc test22-w1.nc test22-w2.nc"
files="$files test22-w3_time00000000.nc test22-w3_time00000006.nc test22-w3_time00000012.nc"
files="$files test23-u.nc test23-v.nc"

if [ -n "$filelist" ]; then
    echo $files
    exit
fi

${MAKE:-make} histauto2
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}
XDIR=xhistauto2

if ! $NCDUMP ncdump_check.nc > /dev/null 2>&1  ;then
    echo ""
    echo "   Error: $NCDUMP: command not found."
    echo ""
    exit 1
fi

test -d $XDIR || $MKDIR $XDIR

for f in $files ; do
    origfiles="$origfiles `echo $f | sed 's/\.nc/.orig.nc/'`"
done

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then
    for f in $files ; do
	test ! -f $XDIR/$f || rm -f $XDIR/$f
    done

    ./histauto2 2> xdifs.log
else
    if [ ! -f $XDIR/test01-u.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histauto2, execute $0 again."
	echo ""
        exit 1
    fi
fi

##### some definitions #####

history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'
delete_orig='s/\.orig//'


##### change directory #####

cd $XDIR

##### Make CDL files from Original data by ncdump #####
echo $ECHO_N "making CDL files from original data files " $ECHO_C
for f in $origfiles
do
   echo $ECHO_N "." $ECHO_C
   $NCDUMP $f | sed -e "$history_to_unknown" -e $delete_orig > ${f}.cdl
done
echo "  done."

##### Make CDL file from generated data by ncdump #####
echo $ECHO_N "making CDL files from generated data files " $ECHO_C
for f in $files
do
   echo $ECHO_N "." $ECHO_C
    $NCDUMP $f | sed "$history_to_unknown" > ${f}.cdl
done
echo "  done."

##### test ######

for f in $files
do
   echo $ECHO_N "testing ${f} ..." $ECHO_C
   of=`echo $f | sed 's/\.nc$/.orig.nc/'`
   diff ${of}.cdl ${f}.cdl
   echo "okay"
done

f=test04-u3.nc
echo $ECHO_N "testing ${f} ..." $ECHO_C
if [ -f "${f}" ]; then
    echo
    echo "  Error: ${f} must be not found."
else
    echo "okay"
fi

rm -f ../xdifs.log *.cdl $files

echo ""
echo "  gtool_historyauto test 2 is successful"
echo ""

exit 0
