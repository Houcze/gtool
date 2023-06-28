=begin JA
= エラーの取り扱い
=end JA
=begin EN
= Error handling
=end EN
=begin
# * 森川 靖大 (morikawa), 豊田 英司 (toyoda)
#   * $Id: lib-error.rd,v 1.1 2009-03-24 00:12:46 morikawa Exp $
=end

=begin JA

本文書は gtool5 ライブラリにおけるエラーの取り扱いについて記します。

プログラムの部品は必ずエラーの取り扱いを明確に規定すべきものです。
エラーとは当該部品への入力が不適切であるとか、
期待される動作をすることができないといった事態を指します。

gtool5 ライブラリがユーザに提供する手続
(手続とはサブルーチンまたは関数の総称) はほとんどの場合、
以下の 2 つの方式のいずれかで呼び出し元にエラーを報告します。

* エラーが発生すると適切なメッセージを表示してプログラム終了
* 論理型の省略できる引数 ((*err*)) が与えられた場合は、
  エラー時にはそれを ((*.true.*)) にします。
  ((*err*)) が省略された場合は上に同じ。

これらの処理はすべて ((*dc_error*))
モジュールの ((*StoreError*)) サブルーチンで行っています。

((*StoreError*)) の引用仕様などに関しては、
((<コードリファレンス: dc_error|URL:code_reference/classes/dc_error.html>))
を参照してください。

=end JA

=begin EN

In this document, error handling in gtool5 library is described. 

Error handling about parts of programs should be regulated definitely.
Error means that input to the part of program is invalid, or
expected operation can not be done, etc.

Procedures (procedures is generic name of subroutines and functions)
provided to users by gtool5 library almost report error to invoker
in the following two manner.

* When error occurs, the program display appropriate messages and 
  aborts.
* If logical optional argument ((*err*)) is given, the argument *err* become
  ((*.true.*)) when error occurs. If ((*err*)) is abbreviated, 
  the operation is same as above.

((*StoreError*)) subroutine in ((*dc_error*))
module handle above all operations.

See 
((<Code references: dc_error|URL:code_reference/classes/dc_error.html>))
for detail about interface of ((*StoreError*)), etc.

=end EN

=begin HTML
<hr />
<small>
  $Id: lib-error.rd,v 1.1 2009-03-24 00:12:46 morikawa Exp $
</small>
=end HTML
