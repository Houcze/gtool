#!/usr/bin/perl

print <<EOF if ($ARGV[0] eq '--help');
$0: Fortran 90 簡易デバッグツール

Fortran 90 のソースコード (ただし下記スタイルに限る) をこのフィルタに
通すと実行文の前に PRINT 文を挿入し、ファイル名、行番号、ソースコードの
行自体を印字する。これによりデバッガがないか信頼できない環境でも
「落ちる場所の探知」くらいはできるようになる。

スタイルの条件は以下の通り:
  ・副プログラムの最初の実行文の前に文番号なし continue 文を置く
  ・副プログラムを終えるためには end function/end subroutine 文を使用
  ・継続行は & ではじめる

vi エディタを使用している場合は適用に 1G!Gperl $0 \%^M とし、
解除に :g/!DBG\$/d^M とすればよい。ただしここで ^M は改行である。
EOF

while(<>) {
    chomp;
    $line = $_;
    $line =~ s/'/''/g;
    $line =~ s/^\s*//;
    $run = 1 if ($line =~ /^continue/);
    $run = 0 if ($line =~ /^contains/);
    $run = 0 if ($line =~ /^end [fsp]/);
    $tmprun = $run;
    $tmprun = 0 if ($line =~ /^&/);
    $tmprun = 0 if ($line =~ /^case/);
    print "PRINT '(A)', '\&$ARGV $.: $line' !DBG\n" if $tmprun;
    print "$_\n";
}
