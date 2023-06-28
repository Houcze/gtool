#
# gtool5 Installation Guide in RD (Ruby Document) format
#
#   For instruction of installing gtool5, see "INSTALL.htm.en"
#   (written in English) or  "INSTALL.htm" (written in Japanese)
#   included in "gtool5" TGZ package available from
#   <http://www.gfd-dennou.org/library/gtool>.
#   Otherwise, see this file directly, or generate
#   above mentioned html files with "make guide" 
#   in current directory after installing  rdtool
#   <http://raa.ruby-lang.org/project/rdtool/>.

=begin JA

= gtool5 インストールガイド

# * 森川 靖大 (morikawa), 豊田 英司 (toyoda), 小高 正嗣 (odakker)
#   * $Id: INSTALL,v 1.11 2010-04-13 05:22:42 morikawa Exp $

=end JA
=begin EN

= Gtool5 Installation Guide

# * Yasuhiro MORIKAWA (morikawa), Eizi TOYODA (toyoda), Masatsugu ODAKA (odakker)
#   * $Id: INSTALL,v 1.11 2010-04-13 05:22:42 morikawa Exp $

=end EN

=begin JA
== 動作環境

gtool5 ライブラリは以下の環境での動作を確認しています.

=end JA

=begin EN
== Operation Environment

Gtool5 library is operated by following environments.

=end EN

=begin

  * ((<"Debian GNU/Linux 5.0"|URL:http://www.debian.org/>)) +
    ((<G95 Fortran 0.91      |URL:http://www.g95.org/>))

  * ((<"Debian GNU/Linux 5.0 "|URL:http://www.debian.org/>)) +
    ((<Fujitsu Fortran Version 5.0 (in JAPANESE)|URL:http://www.fqs.fujitsu.com/fort-c/>))

  * ((<"Debian GNU/Linux 5.0"|URL:http://www.debian.org/>)) +
    ((<G95 Fortran 0.91      |URL:http://www.g95.org/>)) +
    ((<MPICH2 1.0.7|URL:http://www.mcs.anl.gov/research/projects/mpich2/>))

  * ((<"Debian GNU/Linux 5.0"|URL:http://www.debian.org/>)) +
    ((<Fujitsu Fortran Version 5.0 (in JAPANESE)|URL:http://www.fqs.fujitsu.com/fort-c/>)) + 
    ((<MPICH2 1.0.7|URL:http://www.mcs.anl.gov/research/projects/mpich2/>))

=end

=begin JA
過去には以下の環境でも動作したことが確認されています.
確認はしていませんが, 現在のバージョンでもおそらく動作すると期待されます.
=end JA
=begin EN
Gtool5 library was operated by following environments in the past.
Latest version may be operated (unconfirmed).
=end EN

=begin
  * Debian GNU/Linux squeeze + gfortran 4.4.3
  * Cray XT4 + PGI Fortran
  * Debian GNU/Linux 5.0 + Intel Fortran 11.0
  * Debian GNU/Linux 4.0 + Intel Fortran 10.0
  * Linux + PGI Fortran
  * NEC SX-6 + FORTRAN90/SX
  * NEC SX-8 + FORTRAN90/SX
  * Windows Vista + Intel Visual Fortran 9.1.032

# gt4f90io で動作確認されたもの
#  * Debian GNU/Linux 4.0 + gfortran 4.4.0
#  * NEC SX-6 + FORTRAN90/SX
#  * Debian GNU/Linux 3.1 + G95 Fortran 0.9
#  * Debian GNU/Linux 3.1 + Fujitsu Fortran Version 5.0
#  * Debian GNU/Linux 3.1 + Intel Fortran 9.0
#  * Windows Vista + G95 Fortran
#  * Windows Vista + Intel Visual Fortran 9.1.032
#  * Windows Vista + Compaq Visual Fortran 6.6
#  * HITACHI SR11000 + IBM XL Fortran Enterprise Edition for AIX5L
#  * HITACHI SR11000 + Optimizing FORTRAN90 Compiler
#  * FUJITSU VPP-5000 + VPP Fortran
=end


=begin JA
== 入出力ファイル形式

gtool5 は以下のファイル形式をサポートします.

  * ((<gtool4 netCDF 規約|URL:doc/xref.htm#label-6>))
    に従う netCDF ファイル

また, 今後以下のファイル形式がサポートされるといいなと考えています.

  * GrADS 格子点データ
  * GRIB
  * その他一般のべた書き系データ

=end JA

=begin EN
== Input Output Data Format

gtool5 support following data format.

  * NetCDF file conformed with
    ((<gtool4 netCDF Conventions|URL:doc/xref.htm.en#label-6>))

=end EN

=begin JA
== インストールの流れ

gtool5 のインストールは以下のように行います.
詳しくは各項目を参照してください.

  (1) ((<必要なソフトウェア>)) をインストールします.
  (2) ((<ビルドの手引き>)) に従い, ソースからライブラリをビルドします.
  (3) ((<インストールの手引き>)) に従い, ライブラリをインストールします.
  (4) ((<テストプログラム実行の手順>)) に従い, インストールされた
      ライブラリが正常に機能するかどうか確認してください.
  (5) ((<gt5frt へのパスの設定>)) を行ってください.

=== インストールの例

((<gtool プロジェクト|URL:http://www.gfd-dennou.org/library/gtool/>))
の「チュートリアル」では, インストールの実例も紹介されています.
インストールがうまくいかない場合は参照してください.


=== Debian GNU/Linux ユーザのみなさまへ

Debian GNU/Linux を使用している場合は,
((<URL:http://www.gfd-dennou.org/library/gtool/debian.htm>))
にて配布しているバイナリパッケージも利用できます.

=== Windows ユーザのみなさまへ

((<乙部 直人さん|URL:http://www.se.fukuoka-u.ac.jp/otobe/>))が
((<gtool5 Windows バイナリ|URL:http://www.se.fukuoka-u.ac.jp/otobe/gtool5.html>)) にて
((<Intel Visual Fortran|URL:http://www.xlsoft.com/jp/products/intel/compilers/fcw/index.html>)) 用
#および((<G95 Fortran|URL:http://g95.org/>)) 用
の Windows バイナリを作成・公開しています.

=end JA

=begin EN
== General outline

Install gtool5 as follows. Refer each items for details.

  (1) Satisfy ((<Software Requirements>)).
  (2) Build the library following ((<How to build>)).
  (3) Install the library following ((<How to install>)).
  (4) Check whether the installed library functions normally
      following ((<Execute test programs>)).
  (5) ((<Set PATH to gt5frt>)).

=== Install example

There are examples of installation in "Tutorial" on
((<Gtool Project|URL:http://www.gfd-dennou.org/library/gtool/>)).
Refer them if the installation is not successful.

=== For Debian GNU/Linux users

When you are using Debian GNU/Linux, binary packages distributed with
((<URL:http://www.gfd-dennou.org/library/gtool/debian.htm.en>))
can be used.

=== For Windows users

((<Naohito OTOBE (in JAPANESE)|URL:http://www.se.fukuoka-u.ac.jp/otobe/>))
creates and releases
((<gtool5 Windows binaries (in JAPANESE)|URL:http://www.se.fukuoka-u.ac.jp/otobe/gtool5.html>)) for 
((<Intel Visual Fortran|URL:http://www.xlsoft.com/en/products/intel/compilers/reseller_productpage_fortran_compilers_windows.html>))
#and ((<G95 Fortran|URL:http://g95.org/>)).

=end EN

=begin JA
== 必要なソフトウェア

gtool5 を利用するためには, 以下のソフトウェアを
事前にインストールしておく必要があります.

: ((<netCDF|URL:http://www.gfd-dennou.org/library/netcdf>)) (バージョン 3.6)
  Debian GNU/Linux を使用しており, バイナリパッケージを利用する場合

  * debian パッケージ
    [((<Fujitsu ver5|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf/sarge/netcdf-ffc5_3.6.2-1_i386.deb>)) |
    ((<Intel ver9.0|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf/sarge/netcdf-ifc9_3.6.2-1_i386.deb>)) |
    ((<G95|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf-g95/sarge/>))]

    上記 deb パッケージの他に netCDF の Development kit
    (((*netcdf-bin*)) パッケージ) もインストールする必要があります.

  ソースからビルドする場合
  * ((<3.6.2 版 ソースのTGZ|URL:http://www.gfd-dennou.org/library/netcdf/unidata-mirror/netcdf-3.6.2.tar.gz>))

    [((<Unidata 本家 NetCDFクイックインストールガイド (英語) |URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/Quick-Instructions-for-Installing-NetCDF-on-Unix.html#Quick-Instructions-for-Installing-NetCDF-on-Unix>))
    |
    ((<(日本語訳)|URL:INSTALL_netcdf.htm>)) ]

    [((<Unidata 本家のインストールガイドの目次|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/index.html>))]
  

ドキュメントを生成するためには以下のソフトウェアを
事前にインストールしておく必要があります. ただし,
((<gtool5 の TGZ パッケージ|URL:http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current.tgz>))
から入手する場合には既に生成済みです.

* ((<ruby|URL:http://www.ruby-lang.org/>))
* ((<rdtool|URL:http://raa.ruby-lang.org/project/rdtool/>))
* ((<"RDoc Fortran90/95 ソースコード解析機能強化版"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95>))
#* ((<rd2latex-lib|URL:http://shugo.net/archive/rd2latex/>))

=end JA
=begin EN

== Software Requirements

The following software needs to use gtool5.

: ((<netCDF|URL:http://my.unidata.ucar.edu/content/software/netcdf/index.html>)) (version 3.6)

  Debian GNU/Linux (use binary packages)

  * debian packages
    [((<Fujitsu ver5|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf/sarge/netcdf-ffc5_3.6.2-1_i386.deb>)) |
    ((<Intel ver9.0|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf/sarge/netcdf-ifc9_3.6.2-1_i386.deb>)) |
    ((<G95|URL:http://www.gfd-dennou.org/library/cc-env/Linux/debian-dennou/netcdf-g95/sarge/>))]

    If you use above Debian package to install netCDF, you need
    ((* "netcdf-bin" *)) package too. (Parhaps you can get by "apt-get").

  Build from source codes

  * ((<Version 3.6.2 TGZ|URL:http://www.gfd-dennou.org/library/netcdf/unidata-mirror/netcdf-3.6.2.tar.gz>)),

    [((<Quick Instructions for Installing (Unidata)|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/Quick-Instructions-for-Installing-NetCDF-on-Unix.html#Quick-Instructions-for-Installing-NetCDF-on-Unix>))]
    * [((<NetCDF Installation and Porting Guide (Unidata)|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/index.html>))]


Following softwares should be installed for generating documentations.
But, if you get from
((<gtool5 TGZ package|URL:http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current.tgz>)),
documentations are already generated.

* ((<ruby|URL:http://www.ruby-lang.org/>))
* ((<rdtool|URL:http://raa.ruby-lang.org/project/rdtool/>))
* ((<"Enhanced version of RDoc Fortran90/95 parser"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95>))
#* ((<rd2latex-lib|URL:http://shugo.net/archive/rd2latex/>))

=end EN


=begin JA
== ビルドの手引き
=end JA
=begin EN
== How to build
=end EN

=begin JA
=== TGZ パッケージの展開

適当な作業ディレクトリでソースアーカイブを展開します.
ソースは gtool5-((|バージョン|)) というディレクトリに展開されます.

	$ tar xvzf gtool5_current.tgz

または

	$ zcat gtool5_current.tar.gz | tar -xvf -

=end JA
=begin EN
=== Extract TGZ Package

Make an empty directory, and extract archive.
A directory `gtool5-((|version|))'
created at the current working directory.

	$ tar xvzf gtool5_current.tgz

or

	$ zcat gtool5_current.tar.gz | tar -xvf -

=end EN


=begin JA
=== Fortran コンパイラの指定

環境変数 ((* FC *)) に使用する Fortran コンパイラを指定してください.
以下は, 利用するコンパイラが frt の場合です.

* sh, bash の場合

	$ FC=frt ; export FC

* csh, tcsh の場合

	$ setenv FC frt

最適化やデバッグのためのオプションは環境変数 ((* FFLAGS *))
に設定してください. 以下の例は frt の高速化と
並列化のためのオプションです.

* sh, bash の場合

	$ FFLAGS="-Kfast,parallel" ; export FFLAGS

* csh, tcsh の場合

	$ setenv FFLAGS "-Kfast,parallel"

=end JA
=begin EN
=== Specify Fortran Compiler

Specify Fortran compiler to environment variable ((* FC *)).
For example, if you use "frt",

* sh, bash

	$ FC=frt ; export FC

* csh, tcsh

	$ setenv FC frt

Specify Fortran compiler options for optimization and debug to
environment variable ((* FFLAGS *)).
For example, if you set options for automatic optimization and
automatic parallelization to "frt",

* sh, bash

	$ FFLAGS="-Kfast,parallel" ; export FFLAGS

* csh, tcsh

	$ setenv FFLAGS "-Kfast,parallel"

=end EN


=begin JA
=== Config.mk の作成

展開されたディレクトリに移動し, (({ ./configure }))を実行します. 
(({ --with-netcdf= })) には netCDF ライブラリのパスを指定します. 
(以下の例は /usr/local/netcdf/lib/libnetcdf.a にライブラリがある 
場合のものです). 
このコマンドによって (({ Config.mk })) ファイルが生成されます. 
netCDF ライブラリが共有ライブラリである場合, 
(({ --with-netcdff= })) も指定する必要があるかもしれません. 
詳しくは下記のオプションの詳細を参照してください. 

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a

gtool5 を MPI 用にビルドする場合には,
((<MPI 用にビルドする場合には>)) を参照してください. 

ビルドには GNU make が必要となるため, configure は PATH 内から
自動で GNU make を探査しようとしますが, もし見つからない場合,
エラーを返します. その場合には環境変数 ((* MAKE *)) に
GNU make コマンドを指定して再度 configure を実行してください.

インストール先などを変更したい場合は, 以下のように (({ --help })) オプ
ションをつけることで, 指定可能なオプションリストが表示されます.

	$ ./configure --help

主なオプションに関しての説明です.

:(({--with-netcdf=}))((|ARG|))
  ((|ARG|)) に ((<ビルドに必要な netCDF ライブラリ>)) 
  を指定します. 必ず明示的に指定する必要があります.

:(({--with-netcdff=}))((|ARG|))
  netCDF ライブラリが共有ライブラリである場合, C 用ライブラリと
  Fortran 用ライブラリとに分かれてビルドされている場合があります.
  その際は, 上記オプションに C 用ライブラリを指定し, 本オプションの
  ((|ARG|)) に ((<Fortran 用ライブラリ>)) を
  指定します.

:(({--prefix=}))((|ARG|))
  ((|ARG|)) にライブラリやモジュール, 実行ファイルのインストール先の
  ディレクトリのプレフィックスを指定します.
  デフォルトは (({ /usr/local/gtool5 })) です.

:(({--host=}))((|ARG|))
  クロスコンパイルを行う場合には, パッケージが実行されるシステムタイプ名
  を ((|ARG|)) に指定します.

:(({--libdir=}))((|ARG|))
  ((|ARG|)) にライブラリのインストール先のディレクトリを指定します.
  デフォルトは (({ /usr/local/gtool5/lib })) です.

:(({--includedir=}))((|ARG|))
  ((|ARG|)) にモジュール情報ファイルのインストール先のディレクトリ
  を指定します. デフォルトは (({ /usr/local/gtool5/include })) です.

:(({--bindir=}))((|ARG|))
  ((|ARG|)) に実行ファイルのインストール先のディレクトリを指定します.
  デフォルトは (({ /usr/local/gtool5/bin })) です.

:(({--with-docdir=}))((|ARG|))
  ((|ARG|)) にドキュメントファイルのインストール先を指定します.
  デフォルトは (({ /usr/local/gtool5/doc })) です.

:(({--with-gt5libname=}))((|ARG|))
  ((|ARG|)) に gtool5 のライブラリ名を指定します.
  ライブラリ名は (({lib}))((|<ARG>|))(({.a})) となります.
  デフォルトは (({ gtool5 })) であり, その際のライブラリ名は
  (({ libgtool5.a })) となります.

:(({--with-gt5suffix=}))((|ARG|))
  ((|ARG|)) にはインストールディレクトリ, ライブラリ名,
  実行ファイルの末尾につける接尾語を指定します.
  例えば, 他が全てデフォルトの設定で (({ ffc5 })) と指定すると,
  インストール先のディレクトリのプレフィックスが
  (({ /usr/local/gtool5-ffc5 })), ライブラリ名が
  (({ libgtool5-ffc5.a })), インストールする実行ファイル名は
  (({ gt5frt.ffc5 })), (({ gt5config.ffc5 })) 等のように
  なります.

:(({--with-lang_conform_check=}))((|ARG|))
  ((|ARG|)) に言語規格を指定することで, コンパイル時に言語規格の
  チェックを行うよう, オプションを追加します. 現在 "(({95}))"
  のみ指定可能です. コンパイラによっては指定できません.

:(({--enable-debug}))
  このオプションを指定することで, コンパイル時や実行時にデバッグメッセージ
  を出力するオプションを追加します.
  大抵の場合, プログラムの実行速度が低下するため, デバッグ時のみ
  使用します. コンパイラによっては無効です.

:(({--with-abort=}))((|ARG|))
  ((|ARG|)) に (({abort, errtra-setrcd, exit, setrcd, stop})) のいずれか
  を指定することで, 終了時に用いる Fortran の内部関数を変更することが
  可能です. デフォルトは (({ abort })) です.

  : abort
    Fortran の内部サブルーチン abort で終了します.

  : errtra-setrcd
    Fujitsu Fortran の ERRTRA サービスサブルーチンを呼び出し,
    現在実行中のプログラム単位までのトレースバックマップを出力し,
    終了します. Fujitsu Fortran の SETRCD サービスサブルーチンにて,
    復帰コード 13 を設定し, 終了コードとして 3 を設定します.

  : exit
    Fortran の内部サブルーチン exit で終了します.

  : setrcd
    Fujitsu Fortran の SETRCD サービスサブルーチンを呼び出し,
    Fortran の復帰コードとして 3 を設定して終了します.

  : stop
    Fortran の内部関数 stop で終了します.

:(({--config-cache})) または (({-C}))

  (({ Config.mk })) ファイルが生成されると同時に, (({config.cache}))
  ファイルが作成され, (({ ./configure })) の引数に指定された netCDF
  ライブラリの位置などの情報が保持されます.

  再度 (({ ./configure })) を実行する際にもこのオプションを指定することで,
  (({config.cache})) が読み込まれ, 前回指定したオプション等が引き継がれます.
  既に存在する (({config.cache})) を無視する場合はこのオプションを
  指定せずに (({ ./configure })) を実行してください.

  例えば下記のように (({ ./configure })) を実行するとします.

      $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --enable-debug

  すると次回以降, 下記のように (({ ./configure })) を実行することで
  netCDF ライブラリの位置や, デバッグオプションを付加する情報が
  引き継がれます.

      $ ./configure -C

  Debian GNU/Linux で Fujitsu Fortran を利用している場合には,
  Config.cache.debian-ffc* を (({config.cache})) に移動して
  (({ ./configure })) コマンドを実行するだけで自動的に
  ライブラリの位置が設定されます.

      $ cp Config.cache.debian-ffc5 config.cache
      $ ./configure -C


=end JA
=begin EN

=== Create `Config.mk'

Move created directroy, and excute `(({ ./configure }))'.

If your path of netCDF library is `/usr/local/netcdf/lib/libnetcdf.a',
you should set options as follow.
Then a configure file `Config.mk' will be created at
the current working directory.
If the netCDF library is a shared library, (({ --with-netcdff= }))
option may be needed.
See details of options as follows.

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a

If gtool5 is built for MPI, 
see ((<How to build for MPI>)).

GNU make is needed to build, so configure automatically inquires
into GNU make in PATH. However, it returns error when GNU make is
not found. In that case, please set the GNU make command for
environment variable ((* MAKE *)). And rerun execute `(({ ./configure }))'.

If you want to change directory to which the library and the module, etc.
are installed, please set (({ --help })) option as follow. Available
options are showed.

	$ ./configure --help

Descriptions about principal options are listed below.

:(({--with-netcdf=}))((|ARG|))
  Specify ((<netCDF library needed for build>)) to ((|ARG|)).
  You must specify explicitly.

:(({--with-netcdff=}))((|ARG|))
  If the netCDF library is a shared library, the library may be divided
  C library from Fortran library. In the case, specify the C library
  to above option, and specify 
  ((<the Fortran library>)) to ((|ARG|)) in this option.

:(({--prefix=}))((|ARG|))
  Specify prefix to ((|ARG|)).
  Default value is (({ /usr/local/gtool5 })).

:(({--host=}))((|ARG|))
  When cross-compiling, specify
  the type of system on which the package will run to
  ((|ARG|)).

:(({--libdir=}))((|ARG|))
  Specify directory to which the library is installed to ((|ARG|)).
  Default value is (({ /usr/local/gtool5/lib })).

:(({--includedir=}))((|ARG|))
  Specify directory to which the module is installed to ((|ARG|)).
  Default value is (({ /usr/local/gtool5/include })).

:(({--bindir=}))((|ARG|))
  Specify directory to which the executable file is installed to ((|ARG|)).
  Default value is (({ /usr/local/gtool5/bin })).

:(({--with-docdir=}))((|ARG|))
  Specify directory to which the documentation file is installed to ((|ARG|)).
  Default value is (({ /usr/local/gtool5/doc })).

:(({--with-gt5libname=}))((|ARG|))
  Specify gtool5 library name to ((|ARG|)).
  Library name becomes (({lib}))((|<ARG>|))(({.a})) .
  Default value is (({ gtool5 })), so library name becomes
  (({ libgtool5.a })) .

:(({--with-gt5suffix=}))((|ARG|))
  Specify suffix of installdir, library name, executable files to((|ARG|)).
  For example, you specify (({ ffc5 })) when others are default,
  prefix of installdir becomes (({ /usr/local/gtool5-ffc5 })),
  library name becomes (({ libgtool5-ffc5.a })),
  executable files become (({ gt5frt.ffc5 })), (({ gt5config.ffc5 })).

:(({--with-lang_conform_check=}))((|ARG|))
  Specify language standard to ((|ARG|)).
  And when you compile source code, check the language standard conformance.
  Now, "(({95}))" is valid.
  The compiler that can be used is limited.

:(({--enable-debug}))
  When you compile source code and execute binary file, output debug
  messages. This will slow down your program.
  This option is valid at some compilers (ex. g95, frt, ifort).

:(({--with-abort=}))((|ARG|))
  Specify one of (({abort, errtra-setrcd, exit, setrcd, stop})) to
  ((|ARG|)).
  Default value is (({ abort })).

  : abort
    Stop by intrinsic subroutine "abort".

  : errtra-setrcd
    Stop by Fujitsu Fortran service subroutine "ERRTRA".
    And outputs error trace back map.

  : exit
    Stop by intrinsic subroutine "exit".

  : setrcd
    Stop by Fujitsu Fortran service subroutine "SETRCD".
    And outputs error trace back map.

  : stop
    Stop by intrinsic subroutine "stop".

:(({--config-cache or -C}))
  (({ config.cache })) is created at the same time as (({ Config.mk }))'s
  being generated.
  (({ config.cache })) stores information investigated with
  (({ ./configure })).

  If you set this option, when you execute (({ ./configure })) again,
  (({config.cache})) is loaded.
  If you want to ignore (({config.cache})), don't set this option

  For example, execute (({ ./configure })) as follows.

      $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --enable-debug

  Then, information such as the locations of the netCDF library is
  succeeded by executing (({ ./configure })) as follows after next time.

      $ ./configure -C

  If you use Fujitsu Fortran compiler in Debian GNU/Linux,
  copy "Config.cache.debian-ffc*" to (({config.cache})) and
  execute (({ ./configure })). So, locations of libraries are
  specified automatically.

      $ cp Config.cache.debian-ffc5 config.cache
      $ ./configure



=end EN

=begin JA
=== MPI 用にビルドする場合には

gtool5 を MPI 用にビルドする場合にはまず MPI ライブラリをシステムに
インストールしてください.

Config.mk を作成する場合には, 環境変数 FC には mpif90 などの
MPI 用コンパイルコマンドを指定してください.
そして, 以下のように configure にはオプション --with-mpiexec
に MPI 実行コマンドを指定してください. 

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --with-mpiexec=mpirun

環境によっては MPI 実行コマンドが特殊で, configure
がうまく動作しない場合があります.
その場合には以下のように --enable-mpi オプションを指定してください. 

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --enable-mpi

その他のオプションなどについては, ((<Config.mk の作成>)) を参照してください. 

=end JA

=begin EN
=== How to build for MPI

If gtool5 is built for MPI, install MPI library to a system firstly.

When "Config.mk" is edited, specify a compile commend like as "mpif90"
to environment variable ((* FC *)).
And specify a MPI executable command to "--with-mpiexec" option of "configure".

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --with-mpiexec=mpirun

"configure" may not be performed correctly if a MPI executable command
is particular.
In such cases, specify "--enable-mpi" option as follows. 

	$ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a --enable-mpi

See ((<Create `Config.mk'>)) about other options. 

=end EN



=begin JA
=== Config.mk の編集

環境に合わせ (({ Config.mk })) を手動で編集してください. 
下記の設定についてよく分からない場合にはとりあえず
((<ソースコードのコンパイル>)) へ進んでください. 

   FC          : Fortran コンパイラ

   SYSFFLAGS   : コンパイル時・リンク時に必要なフラグ
                 (Fortran コンパイラ実行時に必要なオプション)

   SYSLDFLAGS  : リンク時に必要なフラグ

   SYSLDLIBS   : リンク時に必要なライブラリ

   F90MODTYPE  : モジュール情報の渡し方

   DEST_LIB    : gtool5 ライブラリインストールディレクトリ

   DEST_INC    : gtool5 モジュールインストールディレクトリ

   DEST_BIN    : gtool5 実行ファイルインストールディレクトリ

   DEST_DOC    : gtool5 ドキュメントファイルインストールディレクトリ



   MODS        : モジュールファイル拡張子

   MAKE        : GNU make コマンド

   AR          : アーカイブコマンド

   ARFLAGS     : アーカイブ時に必要なフラグ

   RANLIB      : アーカイブのインデックスを作成するコマンド

=end JA
=begin EN
=== Edit `Config.mk'

Edit `(({Config.mk}))' manually, if you want to change. 
If you do not understand following settings, 
go forward to ((<Compile source code>)) tentatively. 

   FC          : Fortran Compiler

   SYSFFLAGS   : Flags needed when compiled and linked

   SYSLDFLAGS  : Flags needed when linked

   SYSLDLIBS   : Libraries needed when linked

   F90MODTYPE  : Information of Modules
                 (std.mod, HP.mod, fqs.mod, intel.d, hitachi.f90)

   DEST_LIB    : Directory to which the library file is installed

   DEST_INC    : directory to which the module files are installed

   DEST_BIN    : directory to which the executable files are installed

   DEST_DOC    : directory to which the documantation files are installed


   MODS        : Extensions of Module Files used when "make clean"

   MAKE        : GNU make command

   AR          : Archive command

   ARFLAGS     : Flags of AR

   RANLIB      : Generate index to archive


=end EN

=begin JA
=== ソースコードのコンパイル

ビルドには必ず GNU make を使用してください. 他の "make" プログラムを使
用すると, 正しくビルドが行われません.
以降 GNU make のコマンド名を "make" と表記しますが,
これらはシステムの GNU make コマンドの名前に置き換えてください.

./configure を実行すると, 以下のように GNU make のコマンド名が
表示されます. このメッセージに従って GNU make を実行してください.

  Execute GNU make in the current directory, as follows.

    /usr/bin/make

=end JA
=begin EN
=== Compile source code

You must use GNU make to build. No other "make" program is acceptable.
"make" tentatively means GNU make at the following.
Replace them with GNU make of your system.

When ./configure is executed, the command name of GNU make is
displayed as follows. Execute GNU make according to this
message.

  Execute GNU make in the current directory, as follows.

    /usr/bin/make

=end EN

=begin JA
=== ドキュメントの生成

マニュアルとコードリファレンスのコンパイルはカレントディレクトリ
において, 以下のコマンドを実行してください.
((<gtool5 の TGZ パッケージ|URL:http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current.tgz>))
から入手する場合には既に生成済みです.

	$ make doc
=end JA
=begin EN
=== Generate documentations

To generate documentations, execute the following command
in current directory. If you get from
((<gtool5 TGZ package|URL:http://www.gfd-dennou.org/library/gtool/gtool5/gtool5_current.tgz>)),
documentations are already generated.

	$ make doc
=end EN

=begin JA
== インストールの手引き

カレントディレクトリで以下のコマンドを実行してください.
システム領域にインストールする場合には管理者権限が
必要です. (デフォルトの場合はシステム領域にインストールします).

	# make install

ドキュメントをインストールする場合には以下のコマンドを実行します.

	# make install-doc

=end JA
=begin EN
== How to install

In current directory, execute following command.
If you install to system, you need to be administrator.
(By default, you install to system).

	# make install

If you want to install documentation files, execute following command.

	# make install-doc

=end EN



=begin JA
== テストプログラム実行の手順

カレントディレクトリにおいて, 以下のコマンドを実行してください.
エラーが生じずに
"(({ *** Compilation and installation are succeeded !! *** }))"
というメッセージが表示されればインストールは完了です.

	$ make test-installed

=== クロスコンパイルを行った場合

ライブラリをクロスコンパイルした場合は, 下記のようなメッセージが
表示されます.

   Cross compile mode will be used.
   First, change directory to ./test .
   Secondly, submit ...
          dc_string_test ... histtest ...,

   Thirdly, change directory to ./ .
   Last "make test-installed-c"

このメッセージに従い, 以下のようにテストを行ってください.

  (1) test ディレクトリに移動します.
  (2) リストされる実行ファイルを実行します.
      実行方法はそれぞれの環境に依存するので, 別途調べてください.
  (3) 元のディレクトリに戻ります.
  (4) make test-installed-c を実行します.

エラーが生じずに
"(({ *** Compilation and installation are succeeded !! *** }))"
というメッセージが表示されればインストールは完了です.

=end JA
=begin EN
== Execute test programs

In current directry, execute following command.
If message "(({ *** Compilation and installation are succeeded !! *** }))"
are showed without error, installation is completed.

	$ make test-installed

=== When cross-compiling ...

If you are cross-compiling, following messages will be displayed.

   Cross compile mode will be used.
   First, change directory to ./test .
   Secondly, submit ...
          dc_string_test ... histtest ...,

   Thirdly, change directory to ./ .
   Last "make test-installed-c"

According to above message, test as follows.

  (1) Change directory to test .
  (2) Run listed executable files.
      The way of run depends on each environment.
      Therefore, you should examine the way by yourself.
  (3) Change former directory.
  (4) Do "make test-installed-c".

If message "(({ *** Compilation and installation are succeeded !! *** }))"
are showed without error, installation is completed.

=end EN


=begin JA
== gt5frt へのパスの設定

上記のように正しくインストールが行われたら,
((*gt5frt*)) というシェルスクリプトが (({--prefix=}))((|ARG|))
で指定されたディレクトリ以下の bin ディレクトリ ((|ARG|))/bin
に作成されているはずです.
( (({--prefix=})) を指定しなかった場合は
(({ /usr/local/gtool5/bin/ })) 以下).

このディレクトリへのパスを通してください.
以下は (({ /usr/local/gtool5/bin/ })) 以下に ((*gt5frt*))
がインストールされた場合の例です.

* sh, bash

	$ PATH=$PATH:/usr/local/gtool5/bin ; export PATH

* csh, tcsh

	$ setenv PATH $PATH:/usr/local/gtool5/bin

((*gt5frt*)) は gtool5 ライブラリを利用した Fortran プログラムを
簡単にコンパイル, リンクするためのシェルスクリプトです.
これまで利用していた Fortran コンパイラのコマンドの代わりに
gt5frt を用いることで, 自動的に gtool5 ライブラリへの
リンク, モジュール群へのディレクトリ指定を行ってくれます.

	$ gt5frt test.f90

	/usr/bin/g95 -I/usr/local/gtool5/include -O test.f90 \
	    -L/usr/local/gtool5/lib -lgtool5 -L/usr/local/lib -lnetcdf

=end JA
=begin EN
== Set PATH to gt5frt

If the installation is correctly done as stated above,
shell script ((*gt5frt*)) is made under the directory
(({--prefix=}))((|ARG|))/bin
(By default, (({ /usr/local/gtool5/bin/ })) ).

Please specify PATH to this directory.
It is an example as follows when ((*gt5frt*)) is installed in
(({/usr/local/gtool5/bin/})) .

* sh, bash

	$ PATH=$PATH:/usr/local/gtool5/bin ; export PATH

* csh, tcsh

	$ setenv PATH $PATH:/usr/local/gtool5/bin

((*gt5frt*)) is a shell script in order to easily compile and link
Fortran programs which utilizes the gtool5 library.
Link to the gtool5 library and directory appointment to the modules
are done automatically by using gt5frt in place of command of the
Fortran compiler.

	$ gt5frt test.f90

	/usr/bin/g95 -I/usr/local/gtool5/include -O test.f90 \
	    -L/usr/local/gtool5/lib -lgtool5 -L/usr/local/lib -lnetcdf


=end EN


=begin HTML
<hr />
<small>
  $Id: INSTALL,v 1.11 2010-04-13 05:22:42 morikawa Exp $
</small>
=end HTML

#== Mode setting for Emacs
#Local Variables:
#mode: rd
#End:
#
