!--
! *** Caution!! ***
! 
! This file is generated from "sysdepabort.rb2f90" by Ruby 1.8.5.
! Please do not edit this file directly.
!
! [JAPANESE]
!
! ※※※ 注意!!! ※※※
!
! このファイルは "sysdepabort.rb2f90" から Ruby 1.8.5
! によって自動生成されたファイルです.
! このファイルを直接編集しませんようお願い致します.
!
!
!++
!
!== SysdepAbort - 環境依存性ルーチン (プログラム停止)
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: sysdepabort-stop.f90,v 1.1 2009-03-26 02:28:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! メッセージを表示してプログラムを停止します。
! 通常の処理系では abort() があるのでこの組み込み手続きを利用します。
! たいていは異常終了します。
! abort() が実装されていない処理系では何もしない abort() を造ると
! あとの stop でとめることが可能です。
!

subroutine SysdepAbort(string)
  !
  ! この手続きは、引数 _string_ を装置 * に出力後、
  ! プログラムを強制終了させます。
  ! *AbortProgram* というのは総称名です。
  ! 実際にはソースコードを簡単に見つけるため、
  ! *SysdepAbort* というサブルーチン名を与えられています。
  ! 多くの実装では終了コードをゼロ以外にしようと試みていますが、
  ! それをあまり当てにするべきではありません。
  !
  ! This procedure terminates program after _string_ is
  ! outputted to unit "*".
  ! *AbortProgram* is a generic name of a subroutine.
  ! In fact, the subroutine was given another name *SysdepAbort*,
  ! in order to make it easier to find the source code.
  ! Though many implementations are trying to set exit code
  ! other than zero, that should *not* be relied upon.
  !
  use gtdata_generic, only: gtvarsync
  use dc_trace, only: dbg_scratch
  implicit none
  character(len = *), intent(in):: string
  integer  :: stat
continue
  write(*, *) trim(string)
  call dbg_scratch(.false.)
  call gtvarsync(stat=stat)
  !
  ! Selected by Makefile using m4
  !
  ! --- hitachi ---
  stop 3
  stop 'failsafe'
end subroutine
!--
! vi:set readonly sw=4 ts=8:
!
!Local Variables:
!mode: f90
!buffer-read-only: t
!End:
!
!++
