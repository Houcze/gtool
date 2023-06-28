=begin JA

= gtool5 ドキュメントの概説と保守管理の解説

# * 森川 靖大 (morikawa)
#   * $Id: lib-overview-doc.rd,v 1.5 2009-03-31 12:46:30 morikawa Exp $

=end JA

=begin JA

本文書は gtool5 ライブラリのドキュメントを概観し、
その保守管理について記します。

== 概説

gtool5 では以下の種類のドキュメントを用意しています。

* ((<インストールガイド|URL:../INSTALL.htm>))、
  ((<開発履歴|URL:../HISTORY.htm>))、
  ((<使用上の注意とライセンス規程|URL:../CREDITS.htm>)) ((*[rdtool]*))
* ((<チュートリアル|URL:tutorial>)) ((*[rdtool]*))
* 開発者・メンテナ向け各種文書 ((*[rdtool]*))
  * 本文書、
    ((<gtool5 ライブラリ概説|URL:lib-overview.htm>))、
    ((<オブジェクト指向スタイル|URL:lib-oop.htm>))…
* コードリファレンス ((*[RDoc]*))
  * [ ((<利用者向け|URL:code_reference>))
    | ((<開発者向け|URL:develop_reference>)) ]
* ((<ChangeLog|URL:../ChangeLog>)) ((*[cvs2cl]*))

これらの文書は HTML (または XHTML) となっており、
ブラウザから参照することが想定されています。

しかし、保守管理を容易にするため、それぞれの文書は HTML
を直接編集するのでは無く、
別途ソースとなるファイルを用意し、そのファイルから HTML
を自動生成するようにしています。
上記文書のうち、末尾に ((*[rdtool]*)) と記述されているものは、
RD (Ruby Document) 形式のテキストをソースファイルとしており、
((*[RDoc]*)) と記述されているものは、Fortran 90/95
コードをソースファイルとしています。
((*[cvs2cl]*)) と記述されているものは、CVS
で管理されているバージョン情報やログ情報をソースとしています
(例外的に「ソースファイル」という形態では管理していません)。


以下ではその保守管理のために必要なソフトウェア、
HTML 生成のための具体的な手順について記します。


== 保守管理に必要なソフトウェア

上記ドキュメントを保守管理する際には、
以下のソフトウェアが必要となります。

* ((<Ruby|URL:http://www.ruby-lang.org/>))
* ((<Perl|URL:http://www.perl.org/>))
* ((<GNU Make|URL:http://www.gnu.org/software/make/>))
* ((<rdtool|URL:http://raa.ruby-lang.org/project/rdtool/>))
* ((<rd2html-ext|URL:http://raa.ruby-lang.org/project/rd2html-ext>))
* ((<"RDoc Fortran90/95 ソースコード解析機能強化版"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95>))

以降では、これらを以下のように実行できることを想定して解説します。
システムへのインストールやパスの設定を適切に行ってください。

* Ruby

    $ ruby -v
    ruby 1.X.X (20XX-XX-XX) ...

* Perl

    $ perl -v
    This is perl, vX.X.X built ...

* GNU Make

    $ make -v  (もしくは gmake -v)
    GNU Make X.XX ..

  以下では、make コマンドが GNU Make であると想定しています。
  gmake が GNU Make である環境では、make を gmake に置き換えてください。

* rdtool

    $ rd2 -v
    RDtool -- rd2 X.X.XX ..

* rd2html-ext

    $ rd2 -r rd/rd2html-ext-lib --version
    ... RDtool -- RD2HTMLVisitor X.X.XX ...

* RDoc Fortran90/95 ソースコード解析機能強化版

    $ rdoc --version
    rdoc RDoc modified by GFD Dennou Club ...

  もしくは

    $ rdoc-f95 --version
    rdoc-f95 X.X.X ...


== rdtool を用いたドキュメントの保守管理

コードリファレンスや ChangeLog などの一部の文書を除き、
ドキュメントのほとんどは、
RD 形式のテキストファイル (以降は RD ファイルと呼びます)
として管理され、これを rdtool を用いて
HTML に変換して公開されます。

以下では、それぞれ

  (1) ((<RD ファイルの編集>))
  (2) ((<新規のディレクトリの作成と RD ファイル管理に必要な作業>))

を行うための手順を記します。

また、例外として
((<doc 以下に置いていない RD 形式のドキュメントの管理>))
についても記します。

=== RD ファイルの編集

一連の手順は以下のようになります。

  (1) RD 形式のテキストファイルの作成もしくは編集
  (2) rdtool と GNU Make による HTML の作成

以下では、この 2 つの手順の詳細を記します。

==== 1. RD 形式のテキストファイルの作成もしくは編集

RD 形式のテキストファイルの書き方については、
((<地球流体電脳倶楽部 dcmodel プロジェクト: モデルプロジェクトのための最低限 rd -- rd ファイルの作成|URL:http://www.gfd-dennou.org/library/dcmodel/doc/TEBIKI.dcmodel-rd-guide.htm#label-10>))
を参考にしてください。

ファイルを新規作成した場合には、そのファイルの拡張子を ((*rd*)) とし、
doc ディレクトリに置いてください。

==== 2. rdtool と GNU Make による HTML の作成

HTML を作成する際には、doc ディレクトリ以下で
以下のように make を実行してください。

  $ make rd2html

これにより、拡張子 ((*rd*)) のファイルから、その拡張子を
((*htm*)) および ((*htm.en*)) に置き換えたファイルが作成されます。

  |****.rd  =====[make]=====>  ****.htm, ****.htm.en 

直接 rd2 コマンドを入力する必要はありません。
事前に用意されている Makefile および Makefile.rd2html
内で rd2 コマンドが実行されるとともに、その他のいくつかの処理が行われます。


=== 新規のディレクトリの作成と RD ファイル管理に必要な作業

((<RD ファイルの編集>)) において、
rdtool を用いたドキュメントの日常的な保守管理方法を記しました。

以下では、doc 以下に新たなディレクトリを作成してそこにドキュメントを作成する場合について記します。
ここでは、doc 以下に users というディレクトリを作成し、
そこでドキュメントを管理することを想定して解説します。

==== 1. Makefile.rd2html のダウンロード

users ディレクトリを作成し、そこに移動した後、
((<地球流体電脳倶楽部 dcmodel プロジェクト: モデルプロジェクトのための最低限 rd|URL:http://www.gfd-dennou.org/library/dcmodel/doc/TEBIKI.dcmodel-rd-guide.htm>))
より ((<Makefile.rd2html|URL:http://www.gfd-dennou.org/library/dcmodel/doc/sample_Makefile/Makefile.rd2html>))
をダウンロードします。

  $ mkdir users
  $ cd users
  $ wget http://www.gfd-dennou.org/library/dcmodel/doc/sample_Makefile/Makefile.rd2html

==== 2. Makefile の作成

users ディレクトリ以下に Makefile を作成します。
((<サンプル Makefile|URL:lib-overview-doc.Makefile>))
をダウンロードし、Makefile にリネームしてください。

  $ mv lib-overview-doc.Makefile Makefile

Makefile について、以下の項目について編集を行ってください。
編集が必須となる項目は "< >" で括られています。

* 作者

  Makefile を編集者の名前を記してください。

    # Authors::   <Input your name>

* ディレクトリの深さ DIRDEPTH

  今回の場合, doc/users ディレクトリを用意しているため、
  "((*../..*))" を与えてください。

    DIRDEPTH=<Input relative path to gtool5 top directory. For example, "../..", etc.>

* インストール先ディレクトリ DEST_DOC_THISDIR

  今回の場合、((*doc/users*)) を与えてください。
  ($(DEST_DOC) は消さないようにしてください。)

    DEST_DOC_THISDIR=$(DEST_DOC)/<Input directory name>


==== 3. 動作テスト

以上でとりあえずの準備は完了です。
RD ファイルを users ディレクトリに置いた後、
HTML ファイルの作成と削除を試してください。

* HTML ファイルの作成

  doc/users ディレクトリで、以下のようにコマンドを実行してください。

    $ make rd2html

  htm, htm.en ファイルが作成されれば OK です。


* HTML ファイルの削除

  doc/users ディレクトリで、以下のようにコマンドを実行してください。

    $ make clean.rd2html

  htm, htm.en ファイルが削除されれば OK です。


==== 4. Makefile.rd2html の編集

ダウンロードした Makefile.rd2html の冒頭には、
HTML 生成に関する各種設定項目が並んでいます。
スタイルシートファイルやヘッダ、フッタなど、
必要に応じて適宜修正を行ってください。

既に作成されている doc や doc/tutorial 以下の Makefile.rd2html
を参考にして下さい。


==== 5. doc ディレクトリの Makefile の編集

トップディレクトリで make doc や make install-doc コマンドを入力した際に、
doc/users 以下でも HTML の生成もしくはインストールが為されるよう、
doc 直下の Makefile の編集を行います。
以下のように SUBDIRS 変数に、作成したディレクトリを追記してください。

  SUBDIRS=images tutorial users


=== doc 以下に置いていない RD 形式のドキュメントの管理

例外的に、doc 以下に置いていない文書として、
((<インストールガイド|URL:../INSTALL.htm>))、
((<開発履歴|URL:../HISTORY.htm>))、
((<使用上の注意とライセンス規程|URL:../CREDITS.htm>))
といったものがあります。

これらのソースファイルには拡張子 rd を付加せず、
それぞれ ((<INSTALL|URL:../INSTALL>))、
((<HISTORY|URL:../HISTORY>))、((<CREDITS|URL:../CREDITS>))
がソースファイルとなっています。

これらから htm, htm.en ファイルを作成する際には、
gtool5 トップディレクトリにおいて以下のコマンドを入力します。

  $ make guide


== RDoc を用いたドキュメントの保守管理

コードリファレンスは、
((<"RDoc Fortran90/95 ソースコード解析機能強化版"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95>))
を用いて、Fortran 90/95 ソースコードから自動生成されています。
以下では、

  (1) ((<RDoc を用いたドキュメントの日常的な保守管理>))
  (2) ((<RDoc によるドキュメント生成の設定変更>))

について記します。


=== RDoc を用いたドキュメントの日常的な保守管理

一連の手順は以下のようになります。

  (1) Fortran ソースコードの編集
  (2) RDoc と GNU Make による HTML の作成

以下では、この 2 つの手順の詳細を記します。

==== 1. Fortran ソースコードの編集

Fortran ソースコード中に既定の書法でコメントを記述することで、
これをコードリファレンスに反映することが可能です。
書法については、
((<チュートリアル: RDoc による数値モデルの自動ドキュメント生成|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95/tutorial/>))
の
((<7.2 RDoc によるドキュメント生成 -- コメントを書き込んでみる|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95/tutorial/#label-13>))
および
((<8.1 RDoc の便利な機能を使ってみる -- コメント部の修飾|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95/tutorial/#label-16>))
を参考にして下さい。
詳細については
((<"RDoc Fortran 90/95 解析機能強化版におけるコメントの書き方"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95/rdoc-f95/doc/files/parsers/parse_f95_rb.html>))
を参照してください。


==== 2. RDoc と GNU Make による HTML の作成

doc ディレクトリ以下で以下のコマンドを入力することで、
Makefile が rdoc コマンドを呼び出し、コードリファレンスが生成されます。

  $ make rdoc rdoc-dev

直接 rdoc コマンドを入力する必要はありません。
事前に用意されている Makefile、Makefile.rdoc、Makefile.rdoc-dev
内で rdoc が実行されるとともに、その他のいくつかの処理が行われます。


=== RDoc によるドキュメント生成の設定変更

((<RDoc を用いたドキュメントの日常的な保守管理>)) において、
RDoc を用いたドキュメントの日常的な保守管理方法を記しました。

以下では、RDoc におけるドキュメント生成の際の設定の変更について記します。

==== RDoc 生成用 Makefile の設定項目

doc ディレクトリ以下の ((<Makefile.rdoc|URL:Makefile.rdoc>))、
((<Makefile.rdoc-dev|URL:Makefile.rdoc-dev>)) が、
それぞれ利用者用、開発者用コードリファレンス作成の Makefile です。
以下では利用者用コードリファレンス作成用 Makefile をサンプルに、
設定項目を挙げます。

* 出力先ディレクトリ OUTPUTDIR

    OUTPUTDIR       = code_reference
                           # Document directory        (necessary)
                           # [JA] 出力先ディレクトリ   (必須)

* ソースコード置場ディレクトリ SRCDIR

    SRCDIR          = $(DIRDEPTH)/src/
                           # Source code directory               (necessary)
                           # [JA] ソースコード置き場ディレクトリ (必須)

* ソースファイル SRCFILES

  RDoc は引数を指定しない場合、RDoc が解釈可能なソースコード
  (Ruby, C, Fortran 等) を自動的に解釈しますが、
  ここでは以下のように明示的にファイルを指定しています。
  これは上記 SRCDIR からの相対パスとして記述しています。

    SRCFILES        = \
                      *.rdoc \
                      netcdf/*.f90 \
                      gtdata/*.f90 history/*.f90 history/*.F90 \
                      gtool/*.f90 \
                      dc_utils/*.f90 dc_utils/*.F90
                           # Source code files    (optional)
                           # [JA] ソースファイル  (任意)

* RDoc ドキュメントのメインページ MAINPAGE

  code_reference ディレクトリ以下の index.html
  を閲覧した際に表示するページを指定します。
  以下の例であれば、gtool5 モジュールに関するページがメインとなります。

    MAINPAGE        = --main gtool5
                           # Main pages         (optional)
                           # [JA] メインページ  (任意)

* RDoc ドキュメントのタイトル TITLE

    TITLE           = --title "gtool5 Reference Manual"
                           # Title          (optional)
                           # [JA] タイトル  (任意)

* RDoc ドキュメントのオプション RDOCOPTS

  オプションに関しては、
  ((<"RDoc Fortran 90/95 ソースコード解析機能強化版"|URL:http://www.gfd-dennou.org/library/dcmodel/rdoc-f95/>))
  を参照ください。

    RDOCOPTS        = -U --charset euc-jp --inline-source --line-numbers \
                      --ignore-case #--all
                           # Options of RDoc                 (optional)
                           # [JA] RDoc コマンドのオプション  (任意)

* RDoc ドキュメントのスタイルシート

    RDOCCSS         = rdoc-style-gtool5.css
                           # Cascade Style Sheet  (optional)
                           # [JA] スタイルシート  (任意)


== ChangeLog の保守管理

ChangeLog は、CVS
で管理されているバージョン情報やログ情報をソースとしています。
更新のためには、CVS リポジトリへアクセスする必要があります。

更新は gtool5 トップディレクトリで以下のコマンドを入力します。

  $ make cl

これにより、ChangeLog が更新されます。


== 一括でのドキュメント生成

rdtool や RDoc を用いたドキュメントについて、
その生成を一括で行うには、gtool5 トップディレクトリで

  $ make doc

とするか、doc ディレクトリ以下で

  $ make

として下さい。

ただし、ChangeLog のみ別途生成する必要があります。


=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-overview-doc.rd,v 1.5 2009-03-31 12:46:30 morikawa Exp $
</small>
=end HTMLJA
