#!/usr/bin/env ruby
#
#== Recursive sed for netCDF file
#
#Authors::   Yasuhiro MORIKAWA
#Version::   $Id: nc_rsed.rb,v 1.1 2008-09-23 09:56:38 morikawa Exp $
#Tag Name::  $Name:  $
#Copyright:: Copyright (C) GFD Dennou Club, 2005. All rights reserved.
#License::   See link:../COPYRIGHT
#
#複数の netCDF ファイルに対して sed をかける為の Ruby スクリプトです.
#
#単純に ncdump して sed して ncgen するというだけの代物です.
#
#== USAGE
USAGE = "
      nc_rsed.rb 対象文字列 変更後文字列  nc_files [nc_file...]"

#
#
############################################################
## config part

NCDUMP = "/usr/bin/env ncdump"
NCGEN  = "/usr/bin/env ncgen"

TMPFILE = "/tmp/nc_rsed.tmp-#{$$}"

## config part end
############################################################

## required libraries

require 'kconv'
require "optparse"

## options are parsed

opt = OptionParser.new
OPTS = {}
ARGV.options{|opt|

  opt.summary_width = 23
  opt.summary_indent = ' '*6

  opt.on('-n', '--dry-run',
                             "実際には置換を行わず、置換される文字列
                              を表示します.\n"
         ){|v| OPTS[:dryrun] = v}

  opt.on_tail('-h', '-H', '--help', 
                             "詳細なヘルプを出力します.\n"
              ){|v| OPTS[:help] = v}

  opt.parse!
}

## main part

if ARGV.size < 3 || OPTS[:help] then
  print <<-"EOF"

  nc_rsed.rb:

    USAGE: #{USAGE}

    OPTION: \n#{opt.to_a[1..-1].join("")}
    VERSION: 
      Version: $Id: nc_rsed.rb,v 1.1 2008-09-23 09:56:38 morikawa Exp $

  EOF
  exit 1
end

# 1 つ目の要素をパターンとして取り出す
#   - 読み込んだ文字列はとりあえず euc に変換
from = /#{Kconv::toeuc(ARGV.shift)}/e

# 2 つ目の要素はそのまま文字列として取り出す
to   = Kconv::toeuc(ARGV.shift)

# コマンドのチェック
if !system(NCDUMP + " &> /dev/null")
  raise ArgumentError, "\n\n  Error : \"#{NCDUMP}\" is not found.\n\n"
end

ncgen_test_input = <<-'EOF'
netcdf foo {  // an example netCDF specification in CDL

dimensions:
     lat = 10, lon = 5, time = unlimited ;

variables:
     long    lat(lat), lon(lon), time(time);
     float   Z(time,lat,lon), t(time,lat,lon);
     double  p(time,lat,lon);
     long    rh(time,lat,lon);

     // variable attributes
     lat:long_name = "latitude";
     lat:units = "degrees_north";
     lon:long_name = "longitude";
     lon:units = "degrees_east";
     time:units = "seconds since 1992-1-1 00:00:00";
     Z:units = "geopotential meters";
     Z:valid_range = 0., 5000.;
     p:_FillValue = -9999.;
     rh:_FillValue = -1;

data:
     lat   = 0, 10, 20, 30, 40, 50, 60, 70, 80, 90;
     lon   = -140, -118, -96, -84, -52;
}
EOF

ncgen_check = "echo '" + ncgen_test_input + "' | " + NCGEN + " - "

if !system(ncgen_check)
  raise ArgumentError, "\n\n  Error : \"#{NCGEN}\" is not found.\n\n"
end

# 残りの引数はファイル名として処理。先に全ファイルが存在し、
# 且つ netCDF ファイルかチェックする。
ARGV.each{|file|
  if !File.exist?(file)
    raise ArgumentError, "\n\n  Error : \"#{file}\" is not found.\n\n"
  end
  ncfile_check = NCDUMP + " #{file} &> /dev/null"
  if !system(ncfile_check)
    raise ArgumentError, "\n\n  Error : \"#{file}\" is not a netCDF file.\n\n"
  end
}

# 各ファイルに関して sed をかける. ncdump で cdl ファイルを作り,
# そのファイルの中身を sed した後, 再び ncgen で元に戻す.
ARGV.each{|file|
  ncdump_cmd = "#{NCDUMP} #{file}"
  cdl_string = IO.popen(ncdump_cmd).read
  modified_flag = false
  line_num = 0
  print file
  print "\n"
  print " +===========+\n"
  cdl_lines = cdl_string.split("\n")
  cdl_lines.collect!{ |line|
    line_num += 1
    next line if line == line.gsub(from, to)
    modified_flag = true
    print " |", line_num, ": --- ", line, "\n"
    print " |", line_num, ": +++ ", line.gsub(from, to), "\n"
    line.gsub(from, to)
  }
  print " +------------------------------------------+\n"
  new_cdl_string = cdl_lines.join("\n")
  io = open(TMPFILE, 'w')
  io.puts new_cdl_string
  io.close

  next if !modified_flag

#  ncgen_cmd = "echo '" + new_cdl_string + "' | " + NCGEN + " - -o #{file}"
  ncgen_cmd = NCGEN + " #{TMPFILE} -o #{file}"

  system(ncgen_cmd) if !OPTS[:dryrun]
  File.unlink(TMPFILE)
}
