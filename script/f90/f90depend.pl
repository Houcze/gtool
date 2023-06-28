#!/usr/bin/env perl
#
#= Fortran 90/95 dependency lister
#
$AUTHORS="Yasuhiro MORIKAWA";
$VERSION='$Id: f90depend.pl,v 1.3 2009-05-31 11:26:02 morikawa Exp $';
$COPYRIGHT='Copyright (C) GFD Dennou Club, 2008. All rights reserved.';
#
# 引数として受け取る Fortran 90/95 ファイル内の use 文および
# module 文を解析し, 依存関係を記したファイルを Makefile 形式
# で標準出力に書き出す.
#
# Ruby がインストールされていない環境での
# f90depend.rb の代替ツール
# (ソースの検索が Ruby に比べて甘い)
#

######################################################################
# Settings

# End Settings
######################################################################

sub f90dependhelp() {
    print STDOUT <<EOF;

  Usage: 
    f90depend.pl [OPTIONS] f90_file [f90_file [f90_file] .. ]

  Options
       -v:       verbose mode
       -q:       quiet mode
       -h:       show help messages

EOF

  &f90dependprintversion;
}

sub f90dependprintversion() {
    print STDOUT <<EOF;
  f90depend.pl Version ${VERSION}

  ${AUTHORS}
    ${COPYRIGHT}

EOF
}

#
# 重複する項目を削除
#
sub uniqArray{
    my $array = shift;
    my $num = 0;
    my @uniqary = ();
    
    foreach my $af ( @$array ) {
        $new = 1;
        foreach (@uniqary) {
	    if ( $af eq $_ ) {
		$new = 0;
	    }
        }
        if ( $new ) {
	    $uniqary[$num] = $af;
	    $num = $num + 1;
        }
    }

    return @uniqary;
}

#
# 引数取得
#
$verbose = 0;
$num = 0;


while ( $#ARGV > -1 ) {
    $argv0 = shift(@ARGV);
    if ( $argv0 =~ /^\-v/ ){
	$verbose = 1;
    } elsif ( $argv0 =~ /^\-q/ ) {
	$verbose = 0;
    } elsif ( $argv0 =~ /^\-h/ ) {
	&f90dependhelp;
	exit 1
    } elsif ( $argv0 =~ /^\-/ ) {
	print STDERR "Error: \"$argv0\" is invalid option.\n";
	exit 1;
    } else {
	@ArgvFiles[$num] = $argv0;
	$num = $num + 1;
	if ( not -f $argv0 ) {
	    print STDERR "\n  Error: \"$argv0\" is not found or not a regular file.\n\n";
	    exit 1;
	}
    }
}

#
# 引数が 2 つ以上ない場合はヘルプ表示
#
if ( $#ArgvFiles < 1 ) {
    f90dependhelp;
    exit 1;
}

#
# 重複するファイルを削除
#
@Files = uniqArray(\@ArgvFiles);

if ( $verbose ) {
    print "#--- File list --- \n";
    foreach (@Files) {
	print "#  $_\n";
    }
}

##
## 引数が 2 つ以上ない場合はヘルプ表示 (改めて)
##
if ( $#Files < 1 ) {
    f90dependhelp;
    exit 1;
}

#
# 各 f90 ファイル読み込み USE 文検索
#
$num=0;
foreach $file (@Files) {
    open( F90FILE, $file ) || die "\"$file\" can not read.";
    @textary = <F90FILE>;
    close(F90FILE);
    @FileContent[$num] = join('', @textary);
    $num = $num + 1;
}

$num = 0;
if ( $verbose ) {
    print "#--- Cont list --- \n";
    foreach $cont (@FileContent) {
	@lines = split( /\n/, $cont );
	print "#  $Files[$num]: $#lines lines \n";
#	print $cont;
#	print "\n";
	$num = $num + 1;
    }
}

#
# 各 f90 ファイルから USE 文検索
#
$num = 0;
foreach $cont (@FileContent) {
    @lines = split( /\n/, $cont );
    @uses = ();
    foreach $line (@lines) {
	if ( $line =~ /^\s*USE\s+/i ) {
	    $line =~ s/^.*USE\s+//i;
	    $line =~ s/\,.*$//i;
	    $line =~ s/\!.*$//i;
	    $line =~ s/\s*$//i;

	    $new = 1;
	    foreach $alr (@uses) {
		if ( $alr eq $line ) {
		    $new = 0;
		}
	    }
	    if ( $new ) {
		push (@uses, $line);
	    }
	}
    }
    $UseList[$num] = join(" ", @uses);
    $num = $num + 1;
}

$num = 0;
if ( $verbose ) {
    print "#--- Uses list --- \n";
    foreach (@Files) {
	print "#  $_: $UseList[$num] \n";
	$num = $num + 1;
    }
}


#
# 各 f90 ファイルから MODULE 文検索
#
$num = 0;
foreach $cont (@FileContent) {
    @lines = split( /\n/, $cont );
    @modules = ();
    foreach $line (@lines) {
	if ( $line =~ /^\s*MODULE\s+/i ) {
	    next if ( $line =~ /^\s*MODULE\s+PROCEDURE\s+/i );
	    $line =~ s/^.*MODULE\s+//i;
	    $line =~ s/\!.*$//i;
	    $line =~ s/\s*$//i;

	    $new = 1;
	    foreach $alr (@modules) {
		if ( $alr eq $line ) {
		    $new = 0;
		}
	    }
	    if ( $new ) {
		push (@modules, $line);
	    }
	}
    }
    $ModuleList[$num] = join(" ", @modules);
    $num = $num + 1;
}

$num = 0;
if ( $verbose ) {
    print "#--- Modules list --- \n";
    foreach (@Files) {
	print "#  $_: $ModuleList[$num] \n";
	$num = $num + 1;
    }
}


#
# 各 f90 ファイルからメインファイル検索
#
$num = 0;
foreach $cont (@FileContent) {
    @lines = split( /\n/, $cont );
    @mains = ();
    $MainList[$num] = "";
    foreach $line (@lines) {
	if ( $line =~ /^\s*PROGRAM\s+/i ) {
	    $file = $Files[$num];
	    $file =~ s/\.f9[05]$//i;
	    push (@mains, $file);
	}
    }
    $MainList[$num] = join(" ", @mains);
    $num = $num + 1;
}

$num = 0;
if ( $verbose ) {
    print "#--- Mains list --- \n";
    foreach (@Files) {
	print "#  $_: $MainList[$num] \n";
	$num = $num + 1;
    }
}

#
# オブジェクトファイル名の作成
#
$num = 0;
foreach (@Files) {
    @ObjFiles[$num] = $_;
    @ObjFiles[$num] =~ s/\.f9[05]$/.o/i;
    $num = $num + 1;
}

#
# USE エントリ情報をファイル名に変換 (存在しないものは削除)
#
for ($num = 0; $num <= $#Files; $num++) {
    $DependList[$num] = '';
    @uses = split( / /, $UseList[$num] );
    UL: foreach $ul (@uses) {
	next if ( $ul eq $ModuleList[$num] );
	$fnum = 0;
	foreach $file (@Files) {
	    @modules = split( / /, $ModuleList[$fnum] );
	    foreach $ml (@modules) {
		if ( $ul eq $ml ) {
		    $DependList[$num] = $DependList[$num] . ' ' . $ObjFiles[$fnum];
		    next UL;
		}
	    }
	    $fnum = $fnum + 1;
	}
    }
}

$num = 0;
if ( $verbose ) {
    print "#--- Depend list --- \n";
    foreach (@Files) {
	print "#  $_: \n";
	print "#    $DependList[$num] \n";
	$num = $num + 1;
    }
}



#
# 主プログラムに関する依存リスト作成
#

for ($num = 0; $num <= $#Files; $num++) {
    next if ( $MainList[$num] eq '' );
    $MainDepList[$num] = $DependList[$num];
    $PrevMainDepList = $MainDepList[$num];

    while ( 1 ) {
	@depends = split( / /, $MainDepList[$num] );
	foreach $dl (@depends) {
	    $fnum = 0;
	    foreach $ob (@ObjFiles) {
		if ( $dl eq $ob ) {
		    $MainDepList[$num] =
			$MainDepList[$num] . ' ' . $ob . ' ' . $DependList[$fnum]
		    }
		$fnum = $fnum + 1;
	    }
	}
	@mdepl = split( / /, $MainDepList[$num] );
	@mdepl = uniqArray(\@mdepl);
	@mdepl = sort @mdepl;
	$MainDepList[$num] = join( " ", @mdepl );

	if ( $PrevMainDepList eq $MainDepList[$num] ) {
	    last;
	} else {
	    $PrevMainDepList = $MainDepList[$num];
	}
    }

}

$num = 0;
if ( $verbose ) {
    print "#--- MainDep list --- \n";
    foreach (@Files) {
	print "#  $_: \n";
	print "#    $MainDepList[$num] \n";
	$num = $num + 1;
    }
}

#
# 出力
#
for ($num = 0; $num <= $#Files; $num++) {
    print "$ObjFiles[$num]: $DependList[$num]\n"
}

for ($num = 0; $num <= $#Files; $num++) {
    next if ( $MainList[$num] eq '' );
    print "$MainList[$num]: $MainDepList[$num]\n"
}
