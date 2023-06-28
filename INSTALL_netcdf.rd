#
# netCDF Quick Installation Guide in RD (Ruby Document) format
#
#   For instruction of installing gtool5, see "INSTALL_netcdf.htm.en"
#   (written in English) or  "INSTALL_netcdf.htm" (written in Japanese)
#   included in "gtool5" TGZ package available from
#   <http://www.gfd-dennou.org/library/gtool>.
#   Otherwise, see this file directly, or generate
#   above mentioned html files with "make guide" 
#   in current directory after installing  rdtool
#   <http://raa.ruby-lang.org/project/rdtool/>.

=begin TOPLINKJA
<small>[ <a href="INSTALL.htm#label-6">gtool5 インストールガイド</a> ]</small>
=end TOPLINKJA
=begin TOPLINKEN
<small>[ <a href="INSTALL.htm.en#label-6">Gtool5 Installation Guide</a> ]</small>
=end TOPLINKEN

=begin JA

= netCDF インストールガイド

#* 森川 靖大 (morikawa)
#  * $Id: INSTALL_netcdf,v 1.2 2008-10-05 17:28:35 morikawa Exp $

=end JA
=begin EN

= netCDF Installation Guide
#* Yasuhiro MORIKAWA (morikawa)
#  * $Id: INSTALL_netcdf,v 1.2 2008-10-05 17:28:35 morikawa Exp $

Please see following documents provided by ((<Unidata|URL:http://www.unidata.ucar.edu/>)).

[((<Quick Instructions for Installing (Unidata)|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/Quick-Instructions-for-Installing-NetCDF-on-Unix.html#Quick-Instructions-for-Installing-NetCDF-on-Unix>))]
* [((<NetCDF Installation and Porting Guide (Unidata)|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/index.html>))]

=end EN

=begin JA

== 概要

以下は Unix システムに netCDF をインストールする際の簡単な手引きです.
詳しいことは ((<unidata NetCDF|URL:http://my.unidata.ucar.edu/content/software/netcdf/index.html>))
の((<NetCDF Installation and Porting Guide|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/index.html>))
を参照してください.

== tar.gz パッケージの取得

unidata から ((<netCDF 3.6.2|URL:ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-3.6.2.tar.gz>)) を取得します.
((<ミラーサイト (電脳サーバ)|URL:http://www.gfd-dennou.org/library/netcdf/unidata-mirror/netcdf-3.6.2.tar.gz>)) からも取得できます.

== 展開

tar.gz を展開し, 展開したディレクトリに移動します.

     $ tar xvfz netcdf-3.6.2.tar.gz
     $ cd netcdf-3.6.2


== Fortran コンパイラの指定

環境変数 FC に使用するコンパイラのコマンドを指定してください.
例えば, g95 を利用する場合は以下のように指定します.

* sh, bash の場合

     $ FC=g95 ; export FC

* csh, tcsh の場合

     $ setenv FC g95

特定のコンパイラにおいてはその他の環境変数の指定も必要になります.

: G95 Fortran の場合

  以下のように環境変数を指定してください.
  g95 に関する情報はこちら [((<The G95 project|URL:http://www.g95.org/>))]

       FC=g95
       CC=gcc
       CPPFLAGS=-Df2cFortran

: Fujitsu Fortran の場合

  以下のように環境変数を指定してください.

       FC=frt
       CC=fcc
       CXX=FCC
       CPPFLAGS=-DNAGf90Fortran
       FCFLAGS=-Am
       F90FLAGS=-Am
       am_cv_CC_dependencies_compiler_type=none

: Intel Fortran の場合

  以下のように環境変数を指定してください.

       FC=ifort
       CPPFLAGS="-DNDEBUG -DpgiFortran"

: IBM XL Fortran Enterprise Edition for AIX5L

  以下のように環境変数を指定してください. なお, 3.6.1 でのビルドは
  確認されていますが, 3.6.2 では確認されていません.
  IBM XL Fortran に関する情報はこちら
  [((<ライブラリ|URL:http://www.ibm.com/software/awdtools/fortran/xlfortran/library/>)) |
  ((<サポート|URL:http://www.ibm.com/software/awdtools/fortran/xlfortran/support/>))].

       F90=xlf90
       F90FLAGS=-qsuffix=f=f90
       ARFLAGS="-X64 cru"

  IBM XL Fortran を AIX 上で動かす際には
  ((<Known Problems with the netCDF 3.6.0 Distribution|URL:http://www.unidata.ucar.edu/software/netcdf/docs/known_problems.html>))
  の情報が役に立つかもしれません.


: SR11000 HITACHI 最適化 Fortran

  以下のように環境変数を指定してください.
  (コンパイラのコマンド名は f90 だとします).
  なお, 3.6.1 でのビルドは確認されていますが, 3.6.2 では確認されていません.

       FC=f90
       F90=f90
       CC=cc
       CPP=cpp
       CXX=
       F90FLAGS="-nohugeary -i,L"
       ARFLAGS="-X64 cru"

: PGI Fortran

  以下のように環境変数を指定してください. 
  3.6.3 でのビルドが確認されています. 
  PGI Fortran に関する情報はこちら
  [((<The Portland Group|URL:http://www.pgroup.com/>))]

       FC=pgf90
       CC=pgcc
       CPP=cpp
       CXX=
       CPPFLAGS='-DpgiFortran'
       FFLAGS="-O -w"

: GNU Fortran

  以下のように環境変数を指定してください.
  (コンパイラのコマンド名は gfortran だとします). 
  3.6.3 でのビルドが確認されています.
  GNU Fortran に関する情報はこちら
  [((<GCC, the GNU Compiler Collection|URL:http://gcc.gnu.org/>))]

       FC=gfortran


== インストール

configure を実行し, make check, make install を順に実行します.

     $ ./configure
     $ make check
     $ make install

configure スクリプトはインストールに必要なツールをパスから
検索します. インストール場所を変えたい場合は (({ --prefix }))
オプションを使ってください. 例えば, ライブラリを (({ /usr/local/lib }))
以下に, ヘッダーファイルを (({ /usr/local/include})) に,
ユーティリティを (({ /usr/local/bin })) 以下にインストールする
場合には, 以下のようにオプションを追加してください.

     $ ./configure --prefix=/usr/local

デフォルトのインストール先は, configure を実行するディレクトリの
親ディレクトリです.

共有ライブラリを作成する場合は, 以下のように --enable-shared オプションを
指定します. ただし, この場合にはライブラリファイルが C 用 (libnetcdf.a)
と Fortran 用 (libnetcdff.a) と 2 つに分かれてビルドされます. 従って
ビルドした netCDF Fortran ライブラリを使用する際には, libnetcdf.a と
libnetcdff.a の両方のライブラリにリンクする必要があります.

     $ ./configure --enable-shared

なお, configure のオプションは (({ --help })) オプションで確認できます.

     $ ./configure --help

もしもこれでインストールできない場合には,
((<NetCDF Installation and Porting Guide|URL:http://my.unidata.ucar.edu/content/software/netcdf/docs/netcdf-install/index.html>))
を参照してください.

=end JA

=begin HTML
<hr />
<small>
$Id: INSTALL_netcdf,v 1.2 2008-10-05 17:28:35 morikawa Exp $
</small>
=end HTML

#== Mode setting for Emacs
#Local Variables:
#mode: rd
#End:
#
