=begin JA

= オブジェクト指向構成

# * 豊田 英司 (toyoda), 森川 靖大 (morikawa)
#   * $Id: lib-oop.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $

=end JA

=begin JA

== オブジェクト指向スタイル

gtool5 ライブラリは独自のオブジェクト指向スタイルで記述されています。

  * クラスは構造型として表現されます。

  * メソッドはサブルーチンで、 名前の衝突を避けるために総称宣言されています。

  * 動的多相性 (polymorphism) はスーパークラスをサブクラス構造型への
    ポインタ群をまとめた構造型とすることで実現しています。

実例を示しましょう。

gtool5 内部の各プログラムは以下のような構成を為しています。

データアクセスをするには必ず変数を開かなくてはなりません。
変数を開くというのを Fortran ではどう書くかというと、 たとえば

    use gtool5
    type(GT_VARIABLE):: var
    charcter(TOKEN) :: filename
      ...

    filename = "gtool4.nc"
    call Open(var, filename)

のようになります。ある gtool 変数というのは type(GT_VARIABLE) 型の 
(Fortran の) 変数であらわされます。これを Open というサブルーチンに
突っ込むと、変数が開かれるわけです。ではこの Open の実体は
どこにあるかというと、gtdata_generic モジュールに
その手がかりが書かれています。

    ! 一部省略があります

    interface Open

        subroutine GTVarOpen(var, url, writable, err)
            type(GT_VARIABLE), intent(inout):: var
            character(len = *), intent(in):: url
            logical, intent(in), optional:: writable
            logical, intent(out), optional:: err
        end subroutine

        subroutine GTVarOpenByDimOrd(dimvar, var, dimord, ount_compact, err)
            type(GT_VARIABLE), intent(inout):: dimvar
            type(GT_VARIABLE), intent(in):: var
            integer, intent(in):: dimord
            logical, intent(in), optional:: count_compact
            logical, intent(out), optional:: err
        end subroutine

    end interface

ここで Open というインターフェイスは GTVarOpen と GTVarOpenByDimOrd と
いう 2 つのサブルーチンが提供するのであるといっています。
上の例をコンパイルすると、単に Open と書いてあるところで
コンパイラが引数リストに合う 
GTVarOpen を選択してくれるのです。GTVarOpen については別に書いたので
そちらを見てくださいね。

Fortran 90 言語では Open のような interface 文に書かれている名前を総称名、
GTVarOpen などのような本当の名前を個別名といいます。
引数の型の違うサブルーチンは同じ総称名を持つことができます。
同様に、引数の型の違う関数は同じ総称名を持つことができます。
gtool5 ではこの総称名というメカニズムを使って多数の
サブルーチンや引数を整理しています。

構造型名や総称名のリストは別のページにしました。

動的多相性をポインタで実現するアイデアは
((<V.K.Decyk et al. (1998)|URL:xref.htm#label-2>)) から
得たものです。これによって gtgraph 層はすべて GT_DEVICE と GT_OBJECT 
の操作であるかのように利用できますし、gtdata 層はすべて GT_VARIABLE の
操作であるかのように利用できます。

=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-oop.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $
</small>
=end HTMLJA
