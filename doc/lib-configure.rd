=begin JA

= configure と Config.mk.in の保守管理

# * 森川 靖大 (morikawa)
#   * $Id: lib-configure.rd,v 1.4 2009-04-01 12:56:12 morikawa Exp $

=end JA

=begin JA

本文書は configure、Config.mk.in の保守管理について記します。

== 概説

gtool5 では、利用者の環境に応じた設定を
configure によって自動的に行います。
実際には、元々ソースコードに同梱される Config.mk.in を元に、
configure から Config.mk ファイルを作成します。
(configure での設定が不十分な場合には、Config.mk
を利用者に直接編集してもらう必要がある場合もあります)。
各ディレクトリの Makefile はその Config.mk
を include 文によって読み込み、ビルドを行います。

   ./configure 実行 --+
                      |
                      +---> Config.mk
                      |
   Config.mk.in ------+


configure 自体は開発者側で作成した configure.in
から autoconf を用いて作成します。
以下では、gtool5 における configure
の保守管理に必要なソフトウェアおよび、
その具体的な方法を記します。


== 保守管理に必要なソフトウェア

configure を保守管理する際には、
以下のソフトウェアが必要となります。

* ((<Autoconf|URL:http://www.gnu.org/software/autoconf/>))

以降では、autoconf を以下のように実行できることを想定して解説します。
システムへのインストールやパスの設定を適切に行ってください。

    $ autoconf --version
    autoconf (GNU Autoconf) X.XX ...

== 保守管理の手順

=== configure.in の編集

編集は ((<configure|URL:../configure>)) 本体ではなく、
((<configure.in|URL:../configure.in>)) に対して行って下さい。

configure.in 自体は基本的に Bourne Shell
のシェルスクリプトとして記述できますが、
((*AC_*)) で始まる関数は configure (Autoconf) 特有の関数です。
以下に、いくつかの関数について紹介します。
詳細については、Autoconf や Autotools
等に関する文書を別途参照してください。

* ソースファイルの指定

  ソースディレクトリの簡単なチェックを行うため、
  ソースパッケージに含まれるファイルの 1 つを指定して下さい。
  (ファイルが存在しない場合はエラーとなるため、
  当該ファイルを移動する場合には書き換えてください。)

    AC_CONFIG_SRCDIR([src/gtool/gtool5.f90])

* シェル変数を外部変数へ渡す

  configure 内の変数の設定値で Config.mk.in 内の ((*@*))((|<変数名>|))((*@*))
  を置換します。以下の場合、configure 内のシェル変数 FC
  に設定されている値で、Config.mk.in 内の ((*@FC@*)) を置換します。

    AC_SUBST(FC)

* 出力ファイルと入力ファイルの指定

  configure で入力するファイルと出力するファイルを指定します。
  以下の場合には、Config.mk.in を入力し、Config.mk を出力します。

    AC_OUTPUT(Config.mk:Config.mk.in)


なお、以下のように ((*DC_*)) で始まる関数もありますが、
これは下記のマクロファイル aclocal.m4 内で定義される関数です。

    DC_ARG_WITH(netcdf, [netcdf library filename], ac_cv_lib_netcdf, [
      AC_MSG_ERROR(specify netcdf library filename (like libnetcdf.a or libnetcdf.so) with --with-netcdf=)
    ])


=== マクロファイル aclocal.m4 の編集

Autoconf として用意する関数を組み合わせたマクロを記述するファイルとして
((<"script/configure/aclocal.m4"|URL:../script/configure/aclocal.m4>)) が用意されています。
ここでは、ライブラリファイルを指定するためのオプションを手軽に指定するための関数などが用意されています。
ローカルに定義されていることを明示するため、関数名の冒頭に
((*DC_*)) を付与しています。

書式は m4 となっています。
(m4 の書法については、ここでは触れません)。


=== configure の作成

configure.in と aclocal.m4 の準備が整ったら、
その 2 つが置かれているディレクトリで autoconf
コマンドを実行してください。
-B オプションは aclocal.m4 が置かれているディレクトリを指定します。
(もしも aclocal.m4 が configure.in と同じディレクトリに置かれている場合には、
オプションの指定は不要です。)

  $ autoconf -B script/configure

これにより configure コマンドが作成されます。
また、キャッシュファイル置き場として autom4te.cache
ディレクトリが作成されますが、これは作業後削除してください。


=== configure 実行時に使用される下請けスクリプト群

configure が実行される際には、
その下請けとしていくつかのスクリプトが呼ばれます。
これらはディレクトリ ((<"script/configure"|URL:../script/configure>))
および gtool5 トップディレクトリ以下に置かれています。

: ((<"script/configure/chkfort.sh"|URL:../script/configure/chkfort.sh>))

  Fortran コンパイラチェック

: ((<"script/configure/chkgmake.sh"|URL:../script/configure/chkgmake.sh>))

  GNU Make コマンドチェック

: ((<"script/configure/chkrps.sh"|URL:../script/configure/chkrps.sh>))

  Ruby, Perl, Shell のパスチェック

: ((<"script/configure/chkrubyver.rb"|URL:../script/configure/chkrubyver.rb>))

  Ruby バージョンチェック

: ((<config.guess|URL:../config.guess>))、((<config.sub|URL:../config.sub>))

  システムタイプの判別。それぞれ
  ((<URL:http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD>))と
  ((<URL:http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD>))
  よりダウンロードしたもの。


=== Config.mk.in の編集

((<Config.mk.in|URL:../Config.mk.in>)) は、GNU Make の書式で記述します。
ただし、((<configure.in の編集>)) でも触れた通り、
configure.in の AC_SUBST 関数で置換される部分のみ、
((*@*))((|<変数名>|))((*@*)) と記述します。
例えば、

  FC=@FC@

と記述しておくと、configure.in 内の

  AC_SUBST(FC)

に応じて、((*@FC@*)) が置換され、Config.mk ファイルが作成されます。



=== CVS コミットに関する注意

CVS リポジトリのコミットは
((*configure.in、configure の両方*)) に対して行ってください。
これは、利用者が Autoconf をインストールをせずに gtool5 
をインストール可能とするためです。
configure 自体は UNIX 系 OS で用意される基本的なコマンドのみで構成されるため、
大半の環境で動作します。

また、Config.mk.in は CVS コミットしますが、
((*Config.mk はコミットしない*))でください。
上記で述べたとおり、Config.mk
は利用者の環境に応じて自動生成されるものであるためです。

=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-configure.rd,v 1.4 2009-04-01 12:56:12 morikawa Exp $
</small>
=end HTMLJA
