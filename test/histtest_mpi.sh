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

files="xhisttest_mpi1_rank000000.nc xhisttest_mpi1_rank000001.nc xhisttest_mpi1_rank000002.nc xhisttest_mpi1_rank000003.nc"
files="$files xhisttest_mpi2.nc"
files="$files xhisttest_mpi3.nc"
files="$files xhisttest_mpi4_rank000000.nc xhisttest_mpi4_rank000001.nc xhisttest_mpi4_rank000002.nc xhisttest_mpi4_rank000003.nc"
files="$files xhisttest_mpi5.nc"
files="$files xhisttest_mpi6.nc"
files="$files xhisttest_mpi7.nc"
files="$files xhisttest_mpi8.nc"
files="$files xhisttest_mpi9.nc"
files="$files xhisttest_mpi10.nc"
files="$files xhisttest_mpi11.nc"
files="$files xhisttest_mpi12_rank000000.nc xhisttest_mpi12_rank000001.nc xhisttest_mpi12_rank000002.nc xhisttest_mpi12_rank000003.nc"

if [ -n "$filelist" ]; then
    echo $files
    exit
fi

${MAKE:-make} histtest_mpi
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}
XDIR=xhisttest_mpi
MPIEXEC=${MPIEXEC:-mpiexec}
MPIEXECPROCOPT=${MPIEXECPROCOPT:--n}
MPIEXECNPNUM=4

MPIEXECALL="${MPIEXEC} ${MPIEXECPROCOPT} ${MPIEXECNPNUM}"

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

    ${MPIEXECALL} ./histtest_mpi 2> xdifs.log
else
    if [ ! -f $XDIR/test01-u.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ${MPIEXECALL} ./histtest_mpi, execute $0 again."
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

rm -f ../xdifs.log *.cdl $files

echo ""
echo "  gtool_history MPI test is successful"
echo ""

exit 0
