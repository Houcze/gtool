#!/usr/bin/env ruby
# coding: euc-jp
#
#= Ruby version check
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: chkrubyver.rb,v 1.1 2009-03-25 08:17:37 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2006. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
#
require 'rbconfig' unless defined? RbConfig
exit 1 unless (RbConfig::CONFIG['MAJOR'] + "." + RbConfig::CONFIG['MINOR']).to_f >= 1.8
exit 0


# このバージョンより大きいか, 等しい場合に終了コード 0 を返す.
# それ以外の場合は 1 を返す.
# check_ver = "1.8"

# script_ver = VERSION

# check_ver_ary = check_ver.split('.')
# script_ver_ary = script_ver.split('.')

# ary_size = check_ver_ary.size
# ary_size = script_ver_ary.size if script_ver_ary.size < ary_size

# ary_size.times{ |i|
#   exit 1 if script_ver_ary[i] < check_ver_ary[i]
# }
