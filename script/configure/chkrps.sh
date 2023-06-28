#!/bin/sh
#
#= Script of checking Ruby, Perl, Bash
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: chkrps.sh,v 1.1 2009-03-25 08:17:37 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
# License::   See COPYRIGHT[link:COPYRIGHT]
#
#= Overview
#
# This script checks whether "make"s in PATH are GNU make.
#

report_me() {
	echo Error: $*
	echo please report it to GFD-Dennou Club.
	exit 1
}

help() {
	cat <<'END_OF_HELP'
chkrps.sh - Ruby, Perl, Bash auto-detector

 --- input ---
    PATH       - $PATH environment variable
    CHKRUBYVER - path for chkrubyver.rb
 --- output ---
    output is written on 'chkrps.cfg' (or \$OUT if set)
    RUBY       - Ruby (1.8 or more) executable file
    PERL       - Perl executable file
    BASH       - Bash executable file

       If the file is not found, blank is specified. 

END_OF_HELP
	exit 1
}

#
# Step 1: initialization; read chkrps.cfg if exists
#

output=${OUT:-chkrps.cfg}

killcache=no
for arg in "$@"; do
	case $arg in
	-reinit|-nocache)
		killcache=yes
		;;
	-help)
		help
		;;
	-h)
		help
		;;
	esac
done
[ X"$killcache" = X"yes" ] && [ -f $output ] && rm -f $output

if [ -f $output ] && [ X"$FC" = X"" ]; then
	echo previous configuration result \`$output\' used.
	. ./$output
fi

#
# Step 2: check echo options
#
case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ECHO_T='      ' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ECHO_T= ;;
  *)       ECHO_N= ECHO_C='\c' ECHO_T= ;;
esac


#
# Step 2: print PATH list
#
path_list=`echo $PATH | sed 's/:/ /g'`
echo "Ruby, Perl, Bash are searched in"
echo "  $path_list"

#
# Step 3: search "ruby" in PATH;
#
ruby_list=""

for p in $path_list; do
    for ex in ruby ; do
	if [ -x "$p/$ex" ]; then
	    ruby_list="$ruby_list $p/$ex"
	fi
    done
done

echo "Ruby is searched in ... $ruby_list"

#
# Step 3-1: "ruby" version check;
#
CHKRUBYVER=${CHKRUBYVER:-chkrubyver.rb}

if [ ! -f "$CHKRUBYVER" ]; then
    echo 
    echo "  Error: ruby version checker \"$CHKRUBYVER\" is not found."
    echo
    exit 1
fi

checked_ruby=
for ex in $ruby_list ; do
    if $ex $CHKRUBYVER > /dev/null 2>&1 ; then
	checked_ruby=$ex
	break
    fi
done

#
# Step 4: search "perl" in PATH;
#
checked_perl=

for p in $path_list; do
    for ex in perl ; do
	if [ -x "$p/$ex" ]; then
	    checked_perl="$p/$ex"
	    break
	fi
	
    done
    if [ -n "$checked_perl" ]; then
	break
    fi
done

#
# Step 5: search "bash" in PATH;
#
checked_bash=

for p in $path_list; do
    for ex in bash ; do
	if [ -x "$p/$ex" ]; then
	    checked_bash="$p/$ex"
	    break
	fi
    done
done

#
# Step 6: set environment variables
#

if [ -n "$checked_ruby" ]; then
    echo "Ruby (1.8 or more) is $checked_ruby"
else
    echo "Ruby (1.8 or more) is not found."
fi

if [ -n "$checked_perl" ]; then
    echo "Perl is $checked_perl"
else
    echo "Perl is not found."
fi

if [ -n "$checked_bash" ]; then
    echo "Bash is $checked_bash"
else
    echo "Bash is not found."
fi

cat <<END_OF_REPORT > $output
 RUBY=$checked_ruby     ; export RUBY
 PERL=$checked_perl     ; export PERL
 BASH=$checked_bash     ; export BASH
END_OF_REPORT
echo my guess about Ruby, Perl, Bash are written onto $output.
exit 0
