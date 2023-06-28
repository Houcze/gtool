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

files="       xhistget1_rank000000.nc xhistget1_rank000001.nc"
files="$files xhistget1_rank000002.nc xhistget1_rank000003.nc"
files="$files xhistget2_rank000000.nc xhistget2_rank000001.nc"
files="$files xhistget2_rank000002.nc xhistget2_rank000003.nc"
files="$files xhistget3_rank000000.nc xhistget3_rank000001.nc"
files="$files xhistget3_rank000002.nc xhistget3_rank000003.nc"
files="$files xhistget4_rank000000.nc xhistget4_rank000001.nc"
files="$files xhistget4_rank000002.nc xhistget4_rank000003.nc"

if [ -n "$filelist" ]; then
    echo $files
    exit
fi

${MAKE:-make} histget_mpi
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}
XDIR=xhistget_mpi
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

if [ ! "$CROSS_COMPILING" = "yes" ] && [ ! "$CROSS_COMPILING" = "maybe" ] ; then

    for f in $files ; do
        test ! -f $XDIR/$f || rm -f $XDIR/$f
    done

    ${MPIEXECALL} ./histget_mpi 2> xdifs.log
else
    if [ ! -f $XDIR/xhistget1_rank000000.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histget, execute $0 again."
	echo ""
        exit 1
    fi
fi

cd $XDIR
history_to_unknown='s/:history = ".*>/:history = "unknown unknown>/'
#date_to_unknown='s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][+-][0-9][0-9]:[0-9][0-9]/unknown/'

for f in $files ; do
    $NCDUMP $f | sed "$history_to_unknown" > ${f}.cdl
done

##### fixed-length array data input test ######

echo $ECHO_N "histget_mpi [fixed-length array data input test] ... " $ECHO_C

for rank in 000000 000001 000002 000003 ; do
    diff xhistget1_rank${rank}.nc.cdl xhistget2_rank${rank}.nc.cdl > xhistgetdif1_rank${rank}.cdl ||

    diff xhistgetdif1_rank${rank}.cdl - <<EOF
1c1
< netcdf xhistget1_rank${rank} {
---
> netcdf xhistget2_rank${rank} {
EOF
    
done

echo "okay"


##### pointer array data input test ######

echo $ECHO_N "histget_mpi [pointer array data input test] ... " $ECHO_C

for rank in 000000 000001 000002 000003 ; do
    diff xhistget1_rank${rank}.nc.cdl xhistget3_rank${rank}.nc.cdl > xhistgetdif2_rank${rank}.cdl ||

    diff xhistgetdif2_rank${rank}.cdl - <<EOF
1c1
< netcdf xhistget1_rank${rank} {
---
> netcdf xhistget3_rank${rank} {
EOF
    
done

echo "okay"


##### dimension array data input test ######

echo $ECHO_N "histget_mpi [dimension array data input test] ... " $ECHO_C

for rank in 000000 000001 000002 000003 ; do
    diff xhistget1_rank${rank}.nc.cdl xhistget4_rank${rank}.nc.cdl > xhistgetdif3_rank${rank}.cdl ||

    diff xhistgetdif3_rank${rank}.cdl - <<EOF
1c1
< netcdf xhistget1_rank${rank} {
---
> netcdf xhistget4_rank${rank} {
EOF
    
done

echo "okay"


rm -f ../xdifs.log xhistget?_rank??????.nc *.cdl

exit 0
