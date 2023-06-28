#!/bin/sh
#= Generate shell script "gt5config".
#
# Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
# Version::   $Id: gt5configgen.sh,v 1.2 2009-03-20 09:26:49 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2000-2008. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#
fc="$FC"
linker="$LINKF ${MODPATHFLAG}${MODINSTDIR:?}"
fflags="${MODPATHFLAG}${MODINSTDIR:?} $SYSFFLAGS"
ldflags="$SYSLDFLAGS -L$LIBINSTDIR"
ldlibs="-l${LIBBODY} $SYSLDLIBS"
out=${1:-gt5config}

if [ $F90MODTYPE = intel.d ]; then
	ldlibs=`echo $ldlibs | sed s/-lm//`
fi

LINKF_HEAD=`echo "$LINKF" | sed s'/ .*//'`
if [ X"$LINKF_HEAD" = X"$FC" ]; then
    linker_is_not_fortran=0
else
    linker_is_not_fortran=1
fi

cat > $out <<END_OF_HEADER
#!/bin/sh

fc="$fc"
linker="$linker"
fflags="$fflags"
ldflags="$ldflags"
ldlibs="$ldlibs"
END_OF_HEADER


if [ $linker_is_not_fortran = 0 ]; then
    cat >> $out <<END_OF_SCRIPT
usage() {
	cat <<EOF
usage: gt5config [OPTIONS]
options: one of --fc --fflags --ldflags --ldlibs
EOF
	exit 1
}

if test \$# -eq 0; then
	usage
fi

while test \$# -gt 0; do
    case \$1 in
	--fc)		echo \$fc ;;
	--fflags)	echo \$fflags ;;
	--ldflags)	echo \$ldflags ;;
	--ldlibs)	echo \$ldlibs ;;
	*)		usage ;;
    esac
    shift
done

END_OF_SCRIPT

else
    cat >> $out <<END_OF_SCRIPT
usage() {
	cat <<EOF
usage: gt5config [OPTIONS]
options: one of --fc --linker --fflags --ldflags --ldlibs
EOF
	exit 1
}

if test \$# -eq 0; then
	usage
fi

while test \$# -gt 0; do
    case \$1 in
	--fc)		echo \$fc ;;
	--linker)	echo \$linker ;;
	--fflags)	echo \$fflags ;;
	--ldflags)	echo \$ldflags ;;
	--ldlibs)	echo \$ldlibs ;;
	*)		usage ;;
    esac
    shift
done

END_OF_SCRIPT

fi