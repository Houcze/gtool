=begin JA
= Ruby による Fortran コード自動生成システム

# * 森川 靖大 (morikawa)
#   * $Id: lib-rb2f90.rd,v 1.6 2009-05-29 16:08:40 morikawa Exp $

=end JA

=begin JA

本文書は、Ruby による Fortran コード自動生成システムを概観し、
その具体的な利用方法について記します。

== 概説

Fortran 90 以降では、総称名称 (INTERFACE 文) を用いることで、
ある名称の手続きに対して異なる引数を与えることで、
複数の手続きを呼び出すことが可能です。
gtool4 データ出力用の HisoryPut を例にあげると、
HistoryPut では整数、単精度実数、倍精度実数のデータ型を持つ、
0 〜 7 次元
((-
次元数の 7 は Fortran 90/95 の規格で定められている、
配列の次元数の最大値です。
ただし、処理系によってはそれ以上の次元数に設定することも可能です。
もしもこれよりも大きい次元を設定したい場合には、
((<次元数の最大値を変更するには>)) を参照してください。
-))
の配列を引数として受け取ることが可能です。
これは、以下のように 24 (= 3 (型) × 8 (次元数) )
の実体となるサブルーチン (HistoryPut((|<型>|))((|<次元数>|))) を
HistoryPut という総称名に設定してるためです。

  　                      (個別名)
                 +---- HistoryPutInt0   ( ..., array, ... )  ! integer:: array
                 |---- HistoryPutInt1   ( ..., array(:), ... )
                 |----       :
                 |
                 |----       :
   (総称名)      |---- HistoryPutInt7   ( ..., array(:,:,:,:,:,:,:), ... )
  HistoryPut <---+---- HistoryPutReal0  ( ..., array, ... )  ! real:: array
                 |---- HistoryPutReal1  ( ..., array(:), ... )
                 |----       :
                 |
                 |----       :
                 |---- HistoryPutReal7  ( ..., array(:,:,:,:,:,:,:), ... )
                 |---- HistoryPutDouble0( ..., array, ... )  ! real(8):: array
                 |----       :
                 +---- HistoryPutDouble7( ..., array(:,:,:,:,:,:,:), ... )

しかしながら、
この 24 種のサブルーチンを個別に改良・メンテナンスするのは非常に手間がかかります。
これらサブルーチン群の内部はほぼ同様である
(サブルーチンの名称、引数の型、引数の次元数ぐらいしか違わない) 
ため、これらを自動生成することによって、
メンテナンスコストの低減を図っています。

具体的には、Ruby によって Fortran コードの生成を行っています。
一方で、複雑な Ruby コードから Fortran コードを作成するようにすると、
その Ruby コードのメンテナンスのためのコストを増加させることとなるため、
「Fortran コードとしてもそれなりに読める Ruby コード」
から Fotrtran コードを自動生成できるよう試みています。
そのために Ruby で、いわばマクロとして動作するメソッドを用意し、
それをもちいて
「Fortran コードとしてもそれなりに読める Ruby コード」を作成しています。

なお、このようなマクロを実装するための
UNIX 標準ツールとしては C プリプロセッサや M4
マクロプロセッサが良く知られていますが、
上記のような使用に際しては複雑な動作を容易に実装できる点、
そして文法エラー時のエラーメッセージが分かりやすい点などから
Ruby を用いています。


== 保守管理に必要なソフトウェア

上記ドキュメントを保守管理する際には、
以下のソフトウェアが必要となります。

* ((<Ruby|URL:http://www.ruby-lang.org/>))

以降では、Ruby を以下のように実行できることを想定して解説します。
システムへのインストールやパスの設定を適切に行ってください。

    $ ruby -v
    ruby 1.X.X (20XX-XX-XX) ...


== 関連ファイル

* ((<lib-rb2f90-macro.rb|URL:../script/rb2f90/lib-rb2f90-macro.rb>))、
  ((<lib-rb2f90-macro-intrinsic_types.rb|URL:../script/rb2f90/lib-rb2f90-macro-intrinsic_types.rb>))

  Ruby コードを Fortran コードに近い形で記述するためのマクロファイル

* src 以下の各ディレクトリの Makefile

  Ruby コードから Fortran コードを make コマンド一つで生成するためのルールが記述された Makefile

これらのファイルと、Ruby コードによって Fortran
コードを生成します。この Ruby コードは、Fortran
コードの元となるという意味を込め、拡張子は rb2f90 としています。
一つの rb2f90 ファイルにつき、一つの f90 ファイルが生成されます。

#rules.make を含む Makefile 間の関係については、
#((<Makefile、Config.mk、rules.make の関係について|URL:lib-makefiles.rb>))
#を参照ください。


== rb2f90 ファイルの具体例

以下では、rb2f90 の具体例として、整数型、単精度実数型、倍精度実数型の
データ型を持つ 0 〜 7 次元の配列を引数として受け取るサブルーチン
Polymor を提供する polymorphism モジュール (ファイル名 polymorphism.f90) 
を生成する Ruby コード polymorphism.rb2f90 をサンプルとして紹介します。

まず、Ruby コードと、そのファイルから得られる Fortran コード、
およびその利用例を紹介し、その後に解説を行います。

=== Ruby コード、Fortran コード、利用例

* ((<polymorphism.rb2f90|URL:polymorphism.rb2f90>))

    01  #!/usr/bin/env ruby
    02  # -*- f90 -*-
    03  # vi: set sw=4 ts=8:
    04  require("lib-rb2f90-macro") 
    05  require("optparse")
    06  #
    07  # "polymorphism.f90" Generator with Ruby.
    08  #
    09  opt = OptionParser.new
    10  opt.on('--max_dim=VAL') {|v| $max_dim = v.to_i}
    11  opt.parse!(ARGV)
    12  $max_dim = 7 unless $max_dim
    13  
    14  
    15  print <<"__EndOfFortran90Code__"
    16  !--
    17  #{rb2f90_header_comment}!
    18  !++
    19  !
    20  != polymorphism: rb2f90 サンプルコード
    21  !
    22  module polymorphism
    23  
    24    interface Polymor
    25                      #{foreach("\\$type\\$", "Int", "Real", "Double", %Q{
    26                      #{forloop("\\$num\\$", 0, $max_dim, %Q{
    27      module procedure Polymor$type$$num$
    28                      })}
    29                      })}
    30    end interface
    31  
    32  contains
    33  
    34  __EndOfFortran90Code__
    35  
    36  
    37  types = ["Int", "Real", "Double"]
    38  types.each{ |type|
    39  for num in 0..$max_dim
    40  print <<"__EndOfFortran90Code__"
    41  
    42      subroutine Polymor#{type}#{num}(array)
    43        use dc_types, only: DP
    44        #{$type_intent_inout[type]}, intent(inout) :: array#{array_colon("#{num}")}
    45        character(*), parameter:: subname = 'Polymor#{type}#{num}'
    46  
    47        write(*,*) subname, ' is called'
    48  
    49        array = array + 1#{$type_numsuf[type]}
    50  
    51                      #{ifelse(num, 0, %Q{
    52        write(*,*) 'sum=', array
    53                      }, %Q{
    54        write(*,*) 'sum=', sum(array, .true.)
    55                      })}
    56  
    57      end subroutine Polymor#{type}#{num}
    58  
    59  __EndOfFortran90Code__
    60  end
    61  }
    62  
    63  
    64  print <<"__EndOfFortran90Code__"
    65  
    66  end module polymorphism
    67  
    68  __EndOfFortran90Code__
    69  
    70  
    71  print <<"__EndOfFooter__"
    72  !--
    73  ! vi:set readonly sw=4 ts=8:
    74  !
    75  #{rb2f90_emacs_readonly}!
    76  !++
    77  __EndOfFooter__
  
このコードが記述されたファイルを実行することで、
Fortran コードが標準出力へと出力されます。
ただし、((<関連ファイル>)) の
((<lib-rb2f90-macro.rb|URL:../script/rb2f90/lib-rb2f90-macro.rb>))、
((<lib-rb2f90-macro-intrinsic_types.rb|URL:../script/rb2f90/lib-rb2f90-macro-intrinsic_types.rb>))
が置かれているディレクトリにパスが通っている必要があります。
例えば、/home/user/gtool5/script/rb2f90 に上記の 2 ファイルが置かれているのであれば、
以下のように実行してください。

  $ RUBYLIB=/home/user/gtool5/script/rb2f90  ruby  polymorphism.rb2f90 > polymorphism.f90

結果として得られる Fortran コードは以下のようになります。
(400 行強となるため、一部を抜粋しています)。

* ((<polymorphism.f90|URL:polymorphism.f90>))

    001  !--
    002  ! *** Caution!! ***
    003  ! 
    004  ! This file is generated from "polymorphism.rb2f90" by Ruby 1.8.5.
    005  ! Please do not edit this file directly.
    006  !
    007  ! [JAPANESE]
    008  !
    009  ! ※※※ 注意!!! ※※※
    010  !
    011  ! このファイルは "polymorphism.rb2f90" から Ruby 1.8.5
    012  ! によって自動生成されたファイルです.
    013  ! このファイルを直接編集しませんようお願い致します.
    014  !
    015  !
    016  !++
    017  !
    018  != polymorphism: rb2f90 サンプルコード
    019  !
    020  module polymorphism
    021  
    022    interface Polymor
    023                                              module procedure PolymorInt0
    024                      
    025      module procedure PolymorInt1
    026                      
                            :
    036                      
    037      module procedure PolymorInt7
    038                      
    039                      
    040                          module procedure PolymorReal0
    041                      
    042      module procedure PolymorReal1
    043
                            :
    070                      
    071      module procedure PolymorDouble7
    072                      
    073                      
    074    end interface
    075  
    076  contains
    077  
    078  
    079      subroutine PolymorInt0(array)
    080        use dc_types, only: DP
    081        integer, intent(inout) :: array
    082        character(*), parameter:: subname = 'PolymorInt0'
    083  
    084        write(*,*) subname, ' is called'
    085  
    086        array = array + 1
    087  
    088                            write(*,*) 'sum=', array
    089                      
    090  
    091      end subroutine PolymorInt0
    092  
    093  
    094      subroutine PolymorInt1(array)
                            :
    423  
    424      subroutine PolymorDouble7(array)
    425        use dc_types, only: DP
    426        real(DP), intent(inout) :: array(:,:,:,:,:,:,:)
    427        character(*), parameter:: subname = 'PolymorDouble7'
    428  
    429        write(*,*) subname, ' is called'
    430  
    431        array = array + 1.0_DP
    432  
    433                            write(*,*) 'sum=', sum(array, .true.)
    434                      
    435  
    436      end subroutine PolymorDouble7
    437  
    438  
    439  end module polymorphism
    440  
    441  !--
    442  ! vi:set readonly sw=4 ts=8:
    443  !
    444  !Local Variables:
    445  !mode: f90
    446  !buffer-read-only: t
    447  !End:
    448  !
    449  !++

このファイルを実際に使用してみます。
以下のような主プログラムファイルを用意します。

* ((<polymorphism-main.f90|URL:polymorphism-main.f90>))

    01  program main
    02    use dc_types, only: DP
    03    use polymorphism
    04  
    05    integer:: aryi0
    06    real:: aryr5(2,2,2,2,2)
    07    real(DP):: aryd7(2,2,2,2,2,2,2)
    08  
    09    aryi0 = 10
    10    aryr5 = 100
    11    aryd7 = 1000
    12  
    13    call Polymor(aryi0)
    14    call Polymor(aryr5)
    15    call Polymor(aryd7)
    16  
    17  end program main

上記の ((<polymorphism.f90|URL:polymorphism.f90>)) と
((<polymorphism-main.f90|URL:polymorphism-main.f90>))
を以下のようにコンパイル、実行してみます。

  $ gt5frt polymorphism.f90 polymorphism-main.f90
  $ ./a.out

その結果、以下のような結果が表示されます。

  PolymorInt0 is called
  sum=          11
  PolymorReal5 is called
  sum=   3232.000    
  PolymorDouble7 is called
  sum=   128128.000000000     


=== 解説

* ((<polymorphism.rb2f90|URL:polymorphism.rb2f90>))

  * 4 行目

    マクロライブラリ
    ((<lib-rb2f90-macro.rb|URL:../script/rb2f90/lib-rb2f90-macro.rb>))
    を読み込みます。これにより、
    マクロライブラリに登録された関数を使用可能になります。

  * 5, 09-12 行目

    次元数の最大値をオプション --max_dim から設定可能としています。
    デフォルトは 7 と設定しています。

  * 15 行目

    ここから Fortran コードを記述します。
    ヒアドキュメントを利用しており、34 行目が終了部分となっています。

  * 17 行目

    Ruby コードから Fortran コードを生成した際、
    Fortran コードがどのように生成されたか、
    また直接編集してはいけないことなどの定型文を出力します。
    これは ((<polymorphism.f90|URL:polymorphism.f90>))
    の 2-15 行目として出力されます。
    (そこに記載される rb2f90 や f90 ファイルの名称、
    Ruby のバージョンなどは自動的に設定されます)。

  * 25-26、28-29 行目

    マクロライブラリ
    ((<lib-rb2f90-macro.rb|URL:../script/rb2f90/lib-rb2f90-macro.rb>))
    で定義される関数 foreach、forloop が使用されています。

    foreach では最後の引数の文字列中の、最初の引数に該当する文字列を、
    2〜最後から一つ前の引数で繰り返して表示します。

    forloop では、
    4 つ目 (最後) の引数の文字列中の、最初の引数に該当する文字列を、
    2 つ目の引数の数値から 1 つずつ増加させ、3 つ目の引数の数値まで
    繰り返して表示します。

    これらの結果得られるのが、
    ((<polymorphism.f90|URL:polymorphism.f90>))
    の 23-71 行目になります。

  * 37-39 行目

    関数やサブルーチンに関しては、Ruby 組み込みの each
    メソッドや for 文を使用しています。
    each メソッドで型に関するループを、
    for 文で次元数に関するループを回しています。

  * 40 行目

    ここから Fortran コードを記述します。
    ヒアドキュメントを利用しており、59 行目が終了部分となっています。

  * 42 行目

    サブルーチン名のサフィックスとして型と次元数を記述すべく、
    #{式} の形で型の名称と次元数を展開しています。
    例えば、 PolymorInt0 などに展開されます。

  * 44 行目

    (({#{$type_intent_inout[type]}})) の (({$type_intent_inout}))
    は INOUT の INTENT 属性を持つ、
    type に応じた型宣言を行うマクロです。
    これが (({integer, intent(inout)})) などに展開されます。
    他にも、(({$type_intent_in}))、(({$type_intent_out}))
    といったマクロが用意されています。

    (({#{array_colon("#{num}")}})) の (({arry_colon}))
    は、引数の数値に応じた、次元数の宣言に展開されます。
    例えば変数 num が 0 であれば何も表示せず、1 であれば
    (:) に展開されます。

  * 49 行目

    (({#{$type_numsuf[type]}})) の (({$type_numsuf})) は、
    数値のサフィックスを展開するマクロです。

  * 51-55 行目

    ifelse は if 文と同様なマクロです。
    この場合には、変数 num が 0 である場合のみ
    (({write(*,*) 'sum=', array})) が表示され、
    それ以外の場合には
    (({write(*,*) 'sum=', sum(array, .true.)}))
    が表示されます。

  * 37-61 行目

    ここまでに述べた部分が、
    ((<polymorphism.f90|URL:polymorphism.f90>))
    の 79-436 行目になります。


このように、マクロライブラリの使用によって、
様々な型を受け取ることが可能な関数やサブルーチンの作成やメンテナンスを容易に行うことが可能となります。
特に、型の違いに拠らない部分については Fortran
コードと全く同様に記述できます。


== 既存の rb2f90 ファイルと自動生成 f90 ファイル編集の手順

既存の rb2f90 が存在する場合には、
((*f90 ファイルではなく、rb2f90 ファイルを編集*))
して下さい。
make コマンドにより、自動的に rb2f90 から f90 ファイルが生成され、
その f90 ファイルのコンパイルを行います。

=== CVS コミットの注意 (1)

利用者が gtool5 インストール時に Ruby が不要であるよう、
rb2f90 ファイルだけでなく、f90 ファイルもコミットしてください。

== 新規の rb2f90 ファイルの作成手順

新規に rb2f90 ファイルを作成する場合には、以下のように行ってください。

=== rb2f90 ファイルの作成

((<rb2f90 ファイルの具体例>)) を参考に、rb2f90 ファイルを作成してください。

=== Makefile の設定

Makefile の以下の部分を編集してください。
(今回は、追加するファイルが new.rb2f90 とします)。

  OBJS = new.o

  RB2F90 = new.f90

  new.f90:$(RUBY) $(RUBYVERCHECK) > /dev/null 2>&1 && \
              $(RUBY) $< --max_dim=$(MAXDIM) > $@ || \
              touch $@

この設定により、make コマンドによって new.rb2f90 -> new.f90 -> new.o
が生成されることを確認してください。


=== CVS コミットの注意 (2)

((<CVS コミットの注意 (1)>)) を参照ください。



== 次元数の最大値を変更するには

次元数の 7 は Fortran 90/95 の規格で定められている、
配列の次元数の最大値です。
ただし、処理系によってはそれ以上の次元数に設定することも可能です。
もしもこれよりも大きい次元を設定したい場合には、
configure の際に --with-maxdim に次元数の最大値となる数値を与えてください。


=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-rb2f90.rd,v 1.6 2009-05-29 16:08:40 morikawa Exp $
</small>
=end HTMLJA
