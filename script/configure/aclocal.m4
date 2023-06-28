dnl= Definitions of Functions for configure
dnl
dnl  Authors::   Eizi TOYODA, Yasuhiro Morikawa
dnl  Version::   $Id: aclocal.m4,v 1.1 2009-03-25 08:17:37 morikawa Exp $
dnl  Tag Name::  $Name:  $
dnl  Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
dnl  License::   See COPYRIGHT[link:COPYRIGHT]
dnl
dnl== Overview
dnl
dnl These functions are used by configure script.
dnl

dnl
dnl == DC_ARG_WITH(withname, description, varname, ifnot)
dnl
AC_DEFUN(DC_ARG_WITH, [
	AC_ARG_WITH($1, [  --with-$1=ARG: $2], [
		$3=$withval
	], [
		AC_CACHE_CHECK([$2], $3, [$4])
	])
])

dnl
dnl == DC_ARG_ENABLE(feature, description, varname, ifnot)
dnl
AC_DEFUN(DC_ARG_ENABLE, [
	AC_ARG_ENABLE($1, [  --enable-$1: $2], [
		$3=$enableval
	], [
		AC_CACHE_CHECK([$2], $3, [$4])
	])
])

dnl
dnl == Check libfile and set LIBDIR and  LIBNAME
dnl
dnl  Check existence of "libfile" file, and set LIBDIR, LIBNAME
dnl  from "libfile". "libfile" file must have suffixes ".a" or ".so"
dnl
dnl  usage: DC_SET_LIBDIR_LIBNAME(libfile, LIBDIR, LIBNAME)
dnl
AC_DEFUN(DC_SET_LIBDIR_LIBNAME, [
	if test ! -f $1 ; then
		AC_MSG_ERROR(specified library file \"$1\" is not exist)
	fi
	$2=`dirname $1`
	case "$1" in
	    *.a)
		$3=`basename $1 .a | sed 's/^lib//'`
		;;
	    *.so)
		$3=`basename $1 .so | sed 's/^lib//'`
		;;
	    *)
		AC_MSG_ERROR(specified library file \"$1\" have invalid suffix. Valid suffixes are \".a\" or \".so\")
		;;
	esac
])

dnl
dnl == Modify INSTALL (if "./install-sh", set absolute path)
dnl
dnl  usage: DC_MOD_INSTALL
dnl
AC_DEFUN(DC_MOD_INSTALL,
[
dnl     AC_PATH_PROG(PWD, pwd)
    case "${INSTALL}" in
        './'*)
            case "${PWD-}" in
                '')
                    AC_MSG_WARN(*** WARN *** Please set environment variable INSTALL with absolute path by yourself)
                    ;;
                *)
                    INSTALL=${PWD}/$INSTALL
                    AC_SUBST(INSTALL)
                    AC_MSG_NOTICE(*** MSG *** environment variable INSTALL is reconfigured with absolute path)
            esac
            ;;
        *) ;;
    esac
])
