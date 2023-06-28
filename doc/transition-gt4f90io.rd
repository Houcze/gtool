=begin JA

= gt4f90ioからの移行について

# * 森川 靖大 (morikawa)
#   * $Id: transition-gt4f90io.rd,v 1.1 2008-09-23 13:19:24 morikawa Exp $

=end JA
=begin EN

= Transition from gt4f90io

# * Yasuhiro MORIKAWA (morikawa)
#   * $Id: transition-gt4f90io.rd,v 1.1 2008-09-23 13:19:24 morikawa Exp $

=end EN

=begin JA
== インストールについて

インストールについては ((*gt4f90io と同様です. *))
詳細は ((<gtool5 インストールガイド|URL:../INSTALL.htm>))
を参照して下さい.

ただし, デフォルトのインストール先が /usr/local/gt4f90io から
/usr/local/gtool5 に変更になります. 

== 使用の際には

=== Fortran プログラムの変更について

gt4f90io から gtool5 に移行するにあたり, 以下のように
一部のモジュールの名称が変更になりました. ただし, 
gtool5 では旧名称のものも使用可能であるため, 
((*特に利用者側でプログラムを変更する必要はありません.*))

=end JA

=begin HTMLJA

<div align="center">
<table align="center" width="40%">
<tr align="center">
    <th>gt4f90io での<br>モジュールの名称</th><th></th>
    <th>gtool5 での<br>モジュールの名称  </th></tr>
<tr align="center"><td>gt4f90io   </td><td>-&gt;</td><td>gtool5</td></tr>
<tr align="center"><td>gt4_history</td><td>-&gt;</td><td>gtool_history</td></tr>
</table>
</div>

=end HTMLEN

=begin JA

=== プログラムのコンパイルについて

gt4f90io ではプログラムのコンパイル, およびコンパイルオプションを
表示するためのするためのスクリプトとして
((*gt4frt*)), ((*gt4config*))を提供していましたが, 
gtool5 ではこれらが ((*gt5frt*)), ((*gt5config*)) に変更になります. 

また, ライブラリの名称が ((*libgt4f90io.a*)) から
((*libgtool5.a*)) に変更したため,
ライブラリの名称を明示的に指定してコンパイルする場合には, 
この点についても変更してください. 

=end JA

###############################################

=begin EN
== Installation

Installation is ((*same as gt4f90io.*))
See ((<gtool5 Installation Guide|URL:../INSTALL.htm.en>))
for details. 

Note that a default install destination is changed 
from "/usr/local/gt4f90io" to "/usr/local/gtool5". 

== Usage

=== About your Fortran programs

Names of some of modules are changed as follows 
along with transition from "gt4f90io" to "gtool5". 
However, ((*you need not change your programs*)) 
because old names are available in "gtool5". 

=end EN

=begin HTMLEN

<div align="center">
<table align="center" width="50%">
<tr align="center">
    <th>Names of modules in "gt4f90io"</th><th></th>
    <th>Names of modules in "gtool5"</th></tr>
<tr align="center"><td>gt4f90io   </td><td>-&gt;</td><td>gtool5</td></tr>
<tr align="center"><td>gt4_history</td><td>-&gt;</td><td>gtool_history</td></tr>
</table>
</div>

=end HTMLEN

=begin EN

=== Compilation of programs

((*gt4frt*)), ((*gt4config*)) are provided
by "gt4f90io" for compilation of programs, and
display of options for compilation. 
These are changed to ((*gt5frt*)), ((*gt5config*))
in "gtool5".

A library name is changed from ((*libgt4f90io.a*))
to ((*libgtool5.a*)), so if you specified a library name explicitly
for complication, change the name.

=end EN



=begin HTML
<hr />
<small>
  $Id: transition-gt4f90io.rd,v 1.1 2008-09-23 13:19:24 morikawa Exp $
</small>
=end HTML
