#!/bin/sh
#= Generate shell script "gt5frt".
#
# Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
# Version::   $Id: gt5frtgen.sh,v 1.2 2009-03-20 09:26:49 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2000-2008. All rights reserved.
# License::   See COPYRIGHT[link:../COPYRIGHT]
#

fc="$FC"
linker="$LINKF"
mods="${MODPATHFLAG}${MODINSTDIR:?}"
fflags="${SYSFFLAGS}"
libs="-L${LIBINSTDIR:?} -l${LIBBODY:?}"

ldflags=${SYSLDFLAGS}
ldlibs=${SYSLDLIBS:?}
out=${1:-gt5frt}

if [ ${F90MODTYPE:?} = intel.d ]; then
    ldlibs=`echo $ldlibs | sed s/-lm//`
fi

cat > $out <<EOF
#!/bin/sh

fc="$fc"
linker="$linker"
mods="$mods"
fflags="$fflags"
libs="$libs"
ldflags="$ldflags"
ldlibs="$ldlibs"

EOF

LINKF_HEAD=`echo "$LINKF" | sed s'/ .*//'`
if [ X"$LINKF_HEAD" = X"$FC" ]; then
    linker_is_not_fortran=0
else
    linker_is_not_fortran=1
fi

if [ $linker_is_not_fortran = 0 ]; then
    cat >> $out <<EOF

echo \$fc \$fflags \$mods "\$@" \$libs \$ldflags \$ldlibs
exec \$fc \$fflags \$mods "\$@" \$libs \$ldflags \$ldlibs
EOF

else
    cat >> $out <<EOF

allopts=
outputflag=0
outputfile=
srcfile=
objectfiles=
nolink=
for opt in "\$@"
do
    if [ \$opt = "-c" ]; then
        nolink=1
        continue
    fi
    if [ \$outputflag = 1 ]; then
        outputfile=\$opt
        outputflag=0
        continue
    fi
    if [ \$opt = "-o" ]; then
        outputflag=1
        continue
    fi
    case "\$opt" in
        *?.f95|*?.f90|*?.f|*?.F95|*?.F90|*?.F)
        srcfile="\$opt"
#        srcbase=\`echo \$opt | sed 's/\.[\.]*$//'\`
        continue
        ;;
    esac
    case "\$opt" in
        *?.o)
        objectfiles="\$objectfiles \$opt"
        continue
        ;;
    esac
    allopts="\$allopts \$opt"
done

if test X"\$srcfile" = X"" && test -n "\$nolink"; then
    echo "\`basename \$0\`: Error: Specify fortran source file with suffix *.f95|*.f90|*.f|*.F95|*.F90|*.F"
    exit 1
fi

if test X"\$objectfiles" = X"" && test X"\$srcfile" = X"" ; then
    echo "\`basename \$0\`: Error: Specify fortran source file with suffix *.f95|*.f90|*.f|*.F95|*.F90|*.F, or object file with suffix *.o"
    exit 1
fi

if test X"\$outputfile" = X""; then
    outputfile=a.out
fi
#echo \$srcfile
#echo \$srcbase
#echo \$outputfile
#echo \$allopts
#
#exit


if test -n "\$nolink"; then
    echo \$fc \$fflags \$mods -c \$srcfile -o \$outputfile \$allopts
    \$fc \$fflags \$mods -c \$srcfile -o \$outputfile \$allopts || exit 1
else

    if test -n "\$objectfiles"; then
        echo \$linker \$mods \$allopts \$objectfiles -o \$outputfile \$libs \$ldflags \$ldlibs
        \$linker \$mods \$allopts \$objectfiles -o \$outputfile \$libs \$ldflags \$ldlibs  || exit 1

    else
        echo \$fc \$fflags \$mods -c \$srcfile -o \$srcfile.o \$allopts
        \$fc \$fflags \$mods -c \$srcfile -o \$srcfile.o \$allopts || exit 1

        echo \$linker \$mods \$allopts \$srcfile.o -o \$outputfile \$libs \$ldflags \$ldlibs
        \$linker \$mods \$allopts \$srcfile.o -o \$outputfile \$libs \$ldflags \$ldlibs  || exit 1

        echo rm \$srcfile.o
        rm \$srcfile.o  || exit 1
    fi
fi

EOF

fi
