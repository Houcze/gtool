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

files="       xhistgetattr1_rank000000.nc xhistgetattr1_rank000001.nc"
files="$files xhistgetattr1_rank000002.nc xhistgetattr1_rank000003.nc"

if [ -n "$filelist" ]; then
    echo $files
    exit
fi

${MAKE:-make} histgetattr_mpi
NCDUMP=${NCDUMP:-ncdump}
MKDIR=${MKDIR:-mkdir}
XDIR=xhistgetattr_mpi
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

    ${MPIEXECALL} ./histgetattr_mpi 2> xdifs.log
else
    if [ ! -f $XDIR//xhistgetattr1_rank000000.nc ] ; then
	echo ""
	echo "   WARNING: Cross compile mode will be used."
	echo "            After submitting ./histgetattr_mpi, execute $0 again."
	echo ""
        exit 1
    fi
fi

cd $XDIR
rm -f ../xdifs.log $files

exit 0
