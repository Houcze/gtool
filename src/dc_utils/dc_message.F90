!= メッセージの出力
!
! Authors::   Yasuhiro MORIKAWA, Masatsugu ODAKA
! Version::   $Id: dc_message.F90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_message
  !
  != メッセージの出力
  !
  !メッセージの出力を行うためのサブルーチン群を持つモジュールです。
  !{dcl の MSGDMP.f}[http://www.gfd-dennou.org/arch/dcl/dcl-f77doc/rc1/math1/node26.html]
  !の上位互換としても利用することを想定しています。
  !現在、出力装置は標準出力に固定されています。
  !
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   {メッセージの出力}[link:../tutorial/dc_message.htm]
  !
  !== Output Form
  !
  ! 本モジュールのサブルーチンによって以下のような形式のメッセージ
  ! が出力されます。
  !
  !    　
  !    *** MESSAGE [where] ***  message
  !    　
  !    *** WARNING [where] ***  message
  !    　
  !    *** ERROR (Code number) [where] *** message
  !
#ifdef LIB_MPI
  ! MPI ライブラリ
  ! MPI library
  !
  use mpi
#endif
  implicit none

  private
  public:: MessageNotify
  public:: MessageSuppressMPI

  interface MessageNotify
    module procedure MessageNotifyC
    module procedure MessageNotifyI
  end interface

  interface MessageSuppressMPI
    module procedure MessageSuppressMPI0
  end interface

  integer, save:: output_rank = -1
#ifdef LIB_MPI
  integer, save:: save_myrank = -1
#endif

contains

  subroutine MessageNotifyC(level, where, message, &
    & i, r, d, L, n, c1, c2, c3, ca, rank_mpi )
    !
    !=== メッセージの出力およびエラーによる終了
    !
    ! メッセージを標準出力へ出力したい場合に用います。
    !
    ! 文字型変数 where にはプログラム名 (サブルーチン名) など、
    ! プログラム内のどこでメッセージを出力するのかを示すものを与えます。
    !
    ! 文字型変数 message には、出力したい文字列を与えます。
    ! オプション変数 i, r, d, L, s, n, c1, c2, c3 を付加する事も出来ます。
    ! 詳細に関しては dc_string#CPrintf を参照して下さい。
    !
    ! 文字型変数 level は出力するメッセージの種類を決める引数で、
    ! <b><tt>"W"</tt></b> (または<b><tt>"Warning"</tt></b>
    ! など <b><tt>"W"</tt></b> で始まる文字)
    ! を与える事で<b>警告</b>であることを、
    ! <b><tt>"E"</tt></b> (または<b><tt>"Error"</tt></b>
    ! など <b><tt>"E"</tt></b> で始まる文字) を与える事で
    ! <b>エラー (メッセージ出力後プログラムを終了) </b>であることを、
    ! それ以外の文字 (大抵は <b><tt>"M"</tt></b>
    ! を与えることを想定しています)
    ! を与える事で<b>通常のメッセージ</b>であることを指定します。
    ! <b><tt>"E"</tt></b>を与えた場合はメッセージ出力後、プログラムを
    ! 強制終了させます。エラーコードは dc_error#USR_ERRNO となります。
    !

    use dc_types  ,only: STRING, DP
    use dc_string ,only: UChar, StrHead, Printf, CPrintf
    use dc_error  ,only: StoreError, USR_ERRNO

    implicit none

    character(*), intent(in)          :: level ! "E", "W", "M" のどれかを与える。
    character(*), intent(in)          :: where ! プログラム名、手続き名
    character(*), intent(in)          :: message ! メッセージ
    integer     , intent(in), optional:: i(:), n(:)
    real        , intent(in), optional:: r(:)
    real(DP)    , intent(in), optional:: d(:)
    logical     , intent(in), optional:: L(:)
    character(*), intent(in), optional:: c1, c2, c3
    character(*), intent(in), optional:: ca(:)
    integer     , intent(in), optional:: rank_mpi
                                        ! MPI 使用時に, ここで指定された
                                        ! ランク数のノードでのみ
                                        ! メッセージ出力を行います. 
                                        ! 負の値を与えた場合には, 
                                        ! 全てのノードで出力を行います. 
                                        ! 
                                        ! MPI を使用していない場合には
                                        ! このオプションは無視されます. 
                                        ! 
                                        ! When MPI is used, messages are 
                                        ! output in only node that has
                                        ! this runk number. 
                                        ! If negative value is given, 
                                        ! output is done on all nodes
                                        ! 
                                        ! This option is ignored 
                                        ! if MPI is not used. 
                                        ! 

    character(string)        :: msg
  continue

    if ( invalid_rank_number( rank_mpi ) ) return

    if (   StrHead(  'ERROR', trim( UChar(level) )  )   ) then
      msg = Cprintf(message, &
        &           i=i, r=r, d=d, L=L, n=n, c1=c1, c2=c2, c3=c3, ca=ca)
      call StoreError(USR_ERRNO, where, cause_c=msg)

    elseif (   StrHead(  'WARNING', trim( UChar(level) )  )   ) then
      msg = Cprintf(message, &
        &           i=i, r=r, d=d, L=L, n=n, c1=c1, c2=c2, c3=c3, ca=ca)
      msg=' *** WARNING [' // trim(where) // '] ***  '// trim(msg)
      call Printf(fmt='%c', c1=msg)

    else
      msg = Cprintf(message, &
        &           i=i, r=r, d=d, L=L, n=n, c1=c1, c2=c2, c3=c3, ca=ca)
      msg=' *** MESSAGE [' // trim(where) // '] ***  ' // trim(msg)
      call Printf(fmt='%c', c1=msg)

    endif

    return
  end subroutine MessageNotifyC

  subroutine MessageNotifyI(number, where, message, &
    & i, r, d, L, n, c1, c2, c3, ca, rank_mpi )
    !
    !=== メッセージの出力およびエラーによる終了
    !
    ! 基本的にもう一方の MessageNotify (または dc_message#MessageNotifyC)
    ! と同様ですが、こちらは第１引数に数値型変数
    ! number をとります。この number はエラーコードとして、
    ! そのまま dc_error#StoreError に引き渡されます。
    ! エラーコードに関しては (dc_error を参照ください)
    !
    use dc_types  ,only: DP
    use dc_string ,only: CPrintf
    use dc_error  ,only: StoreError, USR_ERRNO

    implicit none

    integer,      intent(in)          :: number    ! エラーコード (dc_error 参照)
    character(*), intent(in)          :: where
    character(*), intent(in), optional:: message
    integer     , intent(in), optional:: i(:), n(:)
    real        , intent(in), optional:: r(:)
    real(DP)    , intent(in), optional:: d(:)
    logical     , intent(in), optional:: L(:)
    character(*), intent(in), optional:: c1, c2, c3
    character(*), intent(in), optional:: ca(:)
    integer     , intent(in), optional:: rank_mpi
                                        ! MPI 使用時に, ここで指定された
                                        ! ランク数のノードでのみ
                                        ! メッセージ出力を行います. 
                                        ! 負の値を与えた場合には, 
                                        ! 全てのノードで出力を行います. 
                                        ! 
                                        ! MPI を使用していない場合には
                                        ! このオプションは無視されます. 
                                        ! 
                                        ! When MPI is used, messages are 
                                        ! output in only node that has
                                        ! this runk number. 
                                        ! If negative value is given, 
                                        ! output is done on all nodes
                                        ! 
                                        ! This option is ignored 
                                        ! if MPI is not used. 
                                        ! 

  continue

    if ( invalid_rank_number( rank_mpi ) ) return

    if (.not. present(message)) then
      call StoreError(number, where)

    else
      call StoreError(number, where,  &
        &             cause_c=CPrintf( message, &
        &             i=i, r=r, d=d, L=L, n=n, c1=c1, c2=c2, c3=c3, ca=ca )  )
    endif

    return
  end subroutine MessageNotifyI

  subroutine MessageSuppressMPI0( rank )
    implicit none
    integer, intent(in):: rank
                                        ! 出力するノードのランク数. 
                                        ! 
                                        ! ここに指定されたランク数以外の
                                        ! ノードでの出力は抑止されます. 
                                        ! 
                                        ! MPI を使用していない場合には
                                        ! サブルーチンは無効です. 
                                        ! 
                                        ! Number of rank of an node that output. 
                                        ! 
                                        ! Output on nodes that do not have 
                                        ! this rank number is suppressed. 
                                        ! 
                                        ! This subroutine is ignored, 
                                        ! if MPI is not used. 
                                        ! 
  continue
    output_rank = rank
  end subroutine MessageSuppressMPI0


  logical function invalid_rank_number( rank_mpi ) result(result)
    implicit none
    integer     , intent(in), optional:: rank_mpi
                                        ! MPI 使用時に, ここで指定された
                                        ! ランク数のノードでのみ
                                        ! メッセージ出力を行います. 
                                        ! 負の値を与えた場合には, 
                                        ! 全てのノードで出力を行います. 
                                        ! 
                                        ! MPI を使用していない場合には
                                        ! このオプションは無視されます. 
                                        ! 
                                        ! When MPI is used, messages are 
                                        ! output in only node that has
                                        ! this runk number. 
                                        ! If negative value is given, 
                                        ! output is done on all nodes
                                        ! 
                                        ! This option is ignored 
                                        ! if MPI is not used. 
                                        ! 
#ifdef LIB_MPI
    logical:: initflag_mpi
    integer:: err_mpi
#endif
  continue
  
#ifndef LIB_MPI
    result = .false.
    return
#else
    if ( save_myrank < 0 ) then
      call MPI_Initialized(initflag_mpi, err_mpi)
      if ( initflag_mpi ) then
        call MPI_Comm_Rank(MPI_COMM_WORLD, save_myrank, err_mpi)
      else
        result = .false.
        return
      end if
    end if

    if ( .not. present(rank_mpi) ) then
      if ( output_rank > -1 ) then
        if ( output_rank == save_myrank ) then
          result = .false.
          return
        else
          result = .true.
          return
        end if
      end if

      result = .false.
      return
    end if

    if ( rank_mpi < 0 ) then
      result = .false.
      return
    end if

    if ( rank_mpi == save_myrank ) then
      result = .false.
    else
      result = .true.
    end if
#endif
  end function invalid_rank_number

end module dc_message
