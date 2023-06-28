!= コマンドライン引数の解析
!= Command line arguments parser
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_args.f90,v 1.2 2009-08-09 06:53:11 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_args
  !
  != コマンドライン引数の解析
  != Command line arguments parser
  !
  ! コマンドライン引数の解析を行います.
  !
  ! 加えて, ヘルプメッセージの表示に関して便利なサブルーチンも
  ! 用意しています.
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   {コマンドライン引数の解析}[link:../tutorial/dc_args.htm]
  !
  !== Procedures list
  !
  ! DCArgsOpen     :: 構造型 ARGS 変数の初期化
  ! DCArgsClose    :: 構造型 ARGS 変数の終了処理
  ! DCArgsGet      :: コマンドライン引数の取得
  ! DCArgsNumber   :: コマンドライン引数の数を返す
  ! DCArgsOption   :: コマンドライン引数オプションを取得するための設定
  ! DCArgsDebug    :: デバッグオプションの自動設定
  ! DCArgsHelp     :: ヘルプオプションの自動設定
  ! DCArgsHelpMsg  :: ヘルプメッセージの設定
  ! DCArgsStrict   :: 無効なオプションが指定された時に警告を表示するよう設定
  ! DCArgsPutLine  :: 構造型 ARGS 変数の内容を印字
  !
  !
  !== Usage
  !
  ! 構造型 ARGS の変数を定義し, Open, Get を利用することで
  ! コマンドライン引数を取得することができます.
  !
  !     program dc_args_sample1
  !       use dc_types
  !       use dc_string, only: StoA
  !       use dc_args
  !       implicit none
  !       type(ARGS) :: arg
  !       character(STRING), pointer :: argv(:) => null()
  !       integer :: i
  !
  !       call DCArgsOpen( arg = arg )   ! (out)
  !       call DCArgsDebug( arg = arg )  ! (inout)
  !       call DCArgsHelp( arg = arg )   ! (inout)
  !       call DCArgsStrict( arg = arg ) ! (inout)
  !       call DCArgsGet( arg = arg, &   ! (inout)
  !         & argv = argv )              ! (out)
  !       do i = 1, size( argv )
  !         write(*,*) argv(i)
  !       end do
  !       deallocate( argv )
  !       call DCArgsClose( arg = arg )  ! (inout)
  !     end program dc_args_sample1
  !
  ! 引数にオプションを指定したい場合には, DCArgsOption サブルーチンを
  ! 利用してください. オプションの書式に関しては DCArgsOption の
  ! 「オプションの書式」を参照してください.
  !
  !     program dc_args_sample2
  !       use dc_types
  !       use dc_string, only: StoA
  !       use dc_args
  !       implicit none
  !       type(ARGS) :: arg
  !       logical :: OPT_size
  !       logical :: OPT_namelist
  !       character(STRING) :: VAL_namelist
  !
  !       call DCArgsOpen( arg = arg )              ! (out)
  !       call DCArgsOption( arg = arg, &           ! (inout)
  !         & options = StoA('-s', '--size'), &     ! (in)
  !         & flag = OPT_size, &                    ! (out)
  !         & help = "Return number of arguments")  ! (in)
  !       call DCArgsOption( arg = arg, &           ! (inout)
  !         & options = StoA('-N', '--namelist'), & ! (in)
  !         & flag = OPT_namelist, &                ! (out)
  !         & value = VAL_namelist, &               ! (out)
  !         & help = "Namelist filename")           ! (in)
  !
  !       call DCArgsDebug( arg = arg )  ! (inout)
  !       call DCArgsHelp( arg = arg )   ! (inout)
  !       call DCArgsStrict( arg = arg ) ! (inout)
  !
  !       if (OPT_size) then
  !         write(*,*) 'number of arguments :: ', DCArgsNumber(arg)
  !       end if
  !       if (OPT_namelist) then
  !         write(*,*) '--namelist=', trim(VAL_namelist)
  !       else
  !         write(*,*) '--namelist is not found'
  !       end if
  !       call DCArgsClose( arg = arg ) ! (inout)
  !     end program dc_args_sample2
  !
  ! コマンドライン引数に '-h', '-H', '--help' のいづれかのオプションを
  ! 指定することで, オプションの一覧が標準出力に表示されます.
  !
  ! ヘルプメッセージの内容を充実させたい場合には DCArgsHelpMsg を
  ! 参照してください.
  !
  !
  !== Note
  !
  !=== 後方互換
  !
  ! バージョン 20071009 以前に利用可能だった以下の手続きは, 
  ! 後方互換のため, しばらくは利用可能です. 
  ! 
  ! * Open, Close, Option, PutLine, Debug, Help, HelpMsg, Strict, Get
  !   Number
  !
  !=== dc_args モジュールを作成した理由について
  !
  ! Fortran コンパイラのほとんどには IARGC, GETARG といった
  ! コマンドライン引数取得のための副プログラムが用意されている.
  ! これらの副プログラムの利用によって, コマンドラインの引数を
  ! 単に取得することは簡単である.
  !
  ! しかしこの IARGC, GETARG の使用に際し, 2 つほど面倒な点がある.
  !
  ! 1 つはコンパイラ依存による IARGC, GETARG の仕様の違いである.
  ! これらの副プログラムは Fortran 規格に含まれないサービスルーチン
  ! であるため, たいていのコンパイラにはこの副プログラムは
  ! 存在するものの, 仕様が微妙に異なる場合がある. (大抵のコンパイラは
  ! GETARG の第一引数を 1 にすると一つ目の引数を取得するが, 
  ! 古い HITACHI のコンパイラは第一引数を 2 にしないと一つ目の
  ! 引数を取得できない, など). そこで gtool5 ライブラリでは
  ! これらのコンパイラ依存性を吸収する設計を行っている.
  ! dc_args モジュールを使用する際には, これらのコンパイラ依存は
  ! 気にしなくてよい. (なお, コンパイラ依存性を実際に
  ! 吸収しているのは sysdep モジュールである).
  !
  ! 2 つ目は, コマンドライン引数におけるオプション引数
  ! (-h や --version など) の取り扱いの不便さである.
  ! IARGC や GETARG は単に引数を取得するための副プログラムであり,
  ! Perl や Ruby などのインタプリタ言語のように,
  ! コマンドライン引数にオプション引数を処理するための
  ! ライブラリ (Getopt や OptionParser など) が用意されていない.
  ! dc_args モジュールは, Fortran プログラムでもオプション引数を
  ! 手軽に扱えるよう, オプション引数処理の
  ! ためのコーディングをできるだけ簡素にするべく整備したプログラムである.
  !
  ! 設計思想は, {オブジェクト指向スクリプト言語 Ruby}[http://www.ruby-lang.org/]
  ! の OptionParser[http://www.ruby-lang.org/ja/man/index.cgi?cmd=view;name=OptionParser]
  ! を真似ており, OptionParser クラスのオブジェクトを
  ! 構造型 ARGS に, new (initialize) メソッドを DCArgsOpen サブルーチンに,
  ! on メソッドを DCArgsOption サブルーチンに, parse メソッドを DCArgsGet
  ! サブルーチンに見立てている. 言語仕様の違いにより実装や仕様は
  ! それなりに異なるが, 構造型 ARGS の変数をオブジェクトに見立て,
  ! この変数に対してサブルーチンを作用させることによって
  ! オブジェクトへの操作やオブジェクトからの引数情報の取得を行うという点では
  ! OptionParser と同様である.
  !
  ! おまけ的機能であるが, dc_trace モジュールとの連携も図られており,
  ! Debug サブルーチンを使用することにより (使用法は上記参照), 再コン
  ! パイルすることなく, プログラムの実行の際に "-D" オプションをつける
  ! ことでデバッグメッセージを表示するモードに変更することもできる.
  !

  use dc_types, only : STRING
  use dc_hash, only: HASH
  implicit none
  private

  public:: ARGS
  public:: DCArgsOpen, DCArgsClose, DCArgsOption
  public:: DCArgsPutLine, DCArgsDebug, DCArgsHelp
  public:: DCArgsHelpMsg, DCArgsStrict, DCArgsGet
  public:: DCArgsNumber

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  public:: Open, Close, Option, PutLine, Debug, Help, HelpMsg, Strict, Get
  public:: Number

  type ARGS
    !
    ! コマンドライン引数解析用の構造体です.
    ! 初期化には DCArgsOpen を, 終了処理には DCArgsClose を用います.
    ! コマンドライン引数に与えられる引数や, プログラム内で 
    ! DCArgsOption, DCArgsHelpMsg サブルーチンによって与えられた情報を
    ! 格納します.
    !
    ! 詳しい使い方は dc_args の Usage を参照ください.
    !
    private
    type(OPT_ENTRY), pointer :: opt_table(:) => null()
                              ! DCArgsOption サブルーチンで指定される
                              ! オプションのリスト
    logical :: initialized = .false.
    type(CMD_OPTS_INTERNAL), pointer :: cmd_opts_list(:) => null()
                              ! コマンドライン引数のうち, オプションと
                              ! して識別されるものののリスト.
    type(HASH) :: helpmsg
  end type ARGS

  type OPT_ENTRY
    character(STRING), pointer:: options(:) => null()
                              ! オプション名リスト
    character(STRING) :: help_message
                              ! ヘルプメッセージ
    logical :: optvalue_flag
                              ! オプションの値の有無
  end type OPT_ENTRY

  type CMD_OPTS_INTERNAL
    character(STRING) :: name  ! オプション名
    character(STRING) :: value ! 値
    logical:: flag_called = .false.
                              ! このオプション名が DCArgsOption で呼ばれたもの
                              ! かどうかを判別するフラグ
  end type CMD_OPTS_INTERNAL

  interface DCArgsOpen
    module procedure DCArgsOpen0
  end interface

  interface DCArgsClose
    module procedure DCArgsClose0
  end interface

  interface DCArgsOption
    module procedure DCArgsOption0
  end interface

  interface DCArgsPutLine
    module procedure DCArgsPutLine0
  end interface

  interface DCArgsDebug
    module procedure DCArgsDebug0
  end interface

  interface DCArgsHelp
    module procedure DCArgsHelp0
  end interface

  interface DCArgsHelpMsg
    module procedure DCArgsHelpMsg0
  end interface

  interface DCArgsStrict
    module procedure DCArgsStrict0
  end interface

  interface DCArgsGet
    module procedure DCArgsGet0
  end interface

  interface DCArgsNumber
    module procedure DCArgsNumber0
  end interface

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  interface Open
    module procedure DCArgsOpen0
  end interface

  interface Close
    module procedure DCArgsClose0
  end interface

  interface Option
    module procedure DCArgsOption0
  end interface

  interface PutLine
    module procedure DCArgsPutLine0
  end interface

  interface Debug
    module procedure DCArgsDebug0
  end interface

  interface Help
    module procedure DCArgsHelp0
  end interface

  interface HelpMsg
    module procedure DCArgsHelpMsg0
  end interface

  interface Strict
    module procedure DCArgsStrict0
  end interface

  interface Get
    module procedure DCArgsGet0
  end interface

  interface Number
    module procedure DCArgsNumber0
  end interface


  !-------------------------------------
  ! BuildArgTable で設定される変数
  character(STRING), allocatable, save:: argstr_table(:)
                              ! 全引数の内容. (オプションかどうかなど
                              ! の判別は行っていない). BuildArgTable
                              ! で設定される.

  integer, save:: argind_count = -1
                              ! 全引数の数. BuildArgTable で
                              ! 設定される.

  !-------------------------------------
  ! SortArgTable で設定される変数
  type(CMD_OPTS_INTERNAL), allocatable, save :: cmd_opts_list(:)
                              ! コマンドライン引数のうち, オプションと
                              ! して識別されるものののリス
                              ! ト. SortArgTable で設定される.

  character(STRING), allocatable, save:: cmd_argv_list(:)
                              ! コマンドライン引数のうち, オプションで
                              ! はない引数のリスト. SortArgTable で設
                              ! 定される.

contains

  subroutine DCArgsOpen0(arg)
    !
    ! ARGS 型の変数を初期設定します. 
    !
    ! ARGS 型の変数を利用する際にはまずこのサブルーチンによって
    ! 初期設定を行ってください.
    !
    ! このサブルーチンは, より下層のサブルーチン内で IARGC や GETARG
    ! を用いて得られたコマンドライン引数の情報を引数 *arg*
    ! へと格納します.
    !
    use dc_message, only: MessageNotify
    use dc_types, only: STRING
    implicit none
    type(ARGS), intent(out) :: arg
    integer:: cmd_opts_max
    character(len = *), parameter :: subname = 'DCArgsOpen'
  continue
    if (arg % initialized) then
      call MessageNotify('W', subname, 'This argument (type ARGS) is already opend.')
      return
    end if
    call BuildArgTable
    call SortArgTable
    cmd_opts_max = size(cmd_opts_list)
    allocate(arg % cmd_opts_list(cmd_opts_max))
    arg % cmd_opts_list = cmd_opts_list
    nullify( arg % opt_table )
    arg % initialized = .true.
  end subroutine DCArgsOpen0

  subroutine DCArgsClose0(arg)
    !
    ! ARGS 型の変数の終了処理を行います. 
    !
    use dc_hash, only: DCHashDelete
    implicit none
    type(ARGS), intent(inout) :: arg
    integer :: i
  continue
    if (arg % initialized) then
      if ( associated( arg % opt_table ) ) then
        do i = 1, size(arg % opt_table)
          deallocate(arg % opt_table(i) % options)
        end do

        deallocate(arg % opt_table)
      end if

      deallocate(arg % cmd_opts_list)
      deallocate(argstr_table)
      deallocate(cmd_argv_list)
      deallocate(cmd_opts_list)

      call DCHashDelete(arg % helpmsg)
    end if
  end subroutine DCArgsClose0

  subroutine DCArgsOption0(arg, options, flag, value, help)
    !
    ! オプション情報の登録と取得を行います. 
    !
    ! コマンドライン引数のうち, *options* に与えるオプションに関する情
    ! 報を *flag* と *value* に取得します. *options* がコマンドライン
    ! 引数に与えられていれば *flag* に .true. が, そうでない場合は 
    ! .false. が返ります. オプションに値が指定される場合は *value* に
    ! その値が返ります. オプション自体が与えられていない場合には
    ! *value* には空文字が返ります.
    !
    ! *help* には *options* に関するヘルプメッセージを *arg* に
    ! 登録します. サブルーチン DCArgsHelp を
    ! 用いた際に, このメッセージが出力されます.
    ! *value* を与えているかどうかでこのメッセージは変化します.
    !
    !=== オプションの書式
    !
    ! コマンドライン引数のうち, オプションと判定されるのは以下の場合です.
    !
    ! * 1 文字目が '-' の場合. この場合は短いオプションとなり, '-'
    !   の次の一文字のみがオプションとして有効になります.
    !
    ! * 1-2文字目が '--' (ハイフン 2 文字) の場合.
    !   この場合は長いオプションとなり,
    !   '--' 以降の文字列がオプションとして有効になります.
    !
    ! オプションの値は, "=" よりも後ろの文字列になります.
    !
    ! 例
    !
    ! <b>コマンドライン引数</b>  :: <b>オプション名, 値      </b>
    ! -h                         ::    -h,           無し
    ! --help                     ::    --help,       無し
    ! -D=6                       ::    -D,            6
    ! -debug=                    ::    -d,           無し
    ! --include=/usr             ::    --include,    /usr
    !

    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    character(len = *), intent(in) :: options(:)
    logical, intent(out) :: flag
    character(len = *), intent(out), optional :: value
    character(len = *), intent(in), optional :: help
    integer :: i, j, options_size, table_size
    type(OPT_ENTRY), allocatable :: local_tables(:)
    character(len = STRING) :: opt_name, opt_value, opt_full
    character(len = *), parameter  :: subname = 'DCArgsOption'
  continue
    flag = .false.
    if (present(value)) value = ''
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Option in dc_args.')
      call DCArgsOpen(arg)
    end if
    options_size = size(options)
    if (options_size < 1) then
      return
    end if

    !-----------------------------------
    ! 構造体 ARGS へのヘルプメッセージ用の情報登録
    ! * まずはテーブル arg % opt_table を一つ広げる.
    !-----------------------------------
    if ( .not. associated( arg % opt_table ) ) then
      ! 1 つめのオプション指定
      !
      table_size = 0
      allocate(arg % opt_table(table_size + 1))
    else
      ! 2 つめ以降のオプション指定
      !
      table_size = size(arg % opt_table)
      allocate(local_tables(table_size))
      local_tables(1:table_size) = arg % opt_table(1:table_size)
      deallocate(arg % opt_table)
      allocate(arg % opt_table(table_size + 1))
      arg % opt_table(1:table_size) = local_tables(1:table_size)
      deallocate(local_tables)
    end if

    !----- 値の代入 -----
    allocate(arg % opt_table(table_size + 1) % options(options_size))
    arg % opt_table(table_size + 1) % options = options
    arg % opt_table(table_size + 1) % help_message = ''
    if (present(help)) then
      arg % opt_table(table_size + 1) % help_message = help
    end if
    arg % opt_table(table_size + 1) % optvalue_flag = present(value)


    !----- options の正規化 -----
    do i = 1, options_size
      opt_full = arg % opt_table(table_size + 1) % options(i)
      if (DCOptionFormC(opt_full, opt_name, opt_value)) then
        arg % opt_table(table_size + 1) % options(i) = opt_name
      else
        if (len(trim(adjustl(opt_full))) < 2) then
          arg % opt_table(table_size + 1) % options(i) = &
            & '-' // trim(adjustl(opt_full))
        else
          arg % opt_table(table_size + 1) % options(i) = &
            & '--' // trim(adjustl(opt_full))
        end if
      end if
    end do

    ! arg % cmd_opts_list 内の探査と flag, value への代入
    ! 呼ばれたものに関しては arg % cmd_opts_list % flag_called を
    ! .true. に
    do i = 1, options_size
      do j = 1, size(arg % cmd_opts_list)
        if (trim(arg % opt_table(table_size + 1) % options(i)) &
          &             == trim(arg % cmd_opts_list(j) % name)) then
          flag = .true.
          if (present(value)) then
            value = arg % cmd_opts_list(j) % value
          end if
          arg % cmd_opts_list(j) % flag_called = .true.
        end if
      end do
    end do
  end subroutine DCArgsOption0

  subroutine DCArgsDebug0(arg)
    !
    ! デバッグオプションの自動設定を行います. 
    !
    ! -D もしくは --debug が指定された際, 自動的に
    ! dc_trace#SetDebug を呼び出すよう *arg* を設定します.
    !
    use dc_types, only: STRING
    use dc_string, only: StoA, StoI
    use dc_trace, only: SetDebug
    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    logical :: OPT_debug
    character(STRING) :: VAL_debug
    character(len = *), parameter  :: subname = 'DCArgsDebug'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Debug in dc_args.')
      call DCArgsOpen(arg)
    end if
    call Option(arg, StoA('-D', '--debug'), OPT_debug, VAL_debug, &
      & help="call dc_trace#SetDebug (display a lot of messages for debug). " // &
      & "VAL is unit number (default is standard output)")
    if (OPT_debug) then
      if (trim(VAL_debug) == '') then
        call SetDebug
      else
        call SetDebug(StoI(VAL_debug))
      end if
    end if
    return
  end subroutine DCArgsDebug0


  subroutine DCArgsHelp0(arg, force)
    !
    ! ヘルプオプションの自動設定を行います. 
    !
    ! -h, -H, --help のいづれかが指定された際, 自動的に *arg* 内に設定された
    ! 情報をヘルプメッセージとして表示した後, プログラムを終了させます.
    ! 原則的に, このサブルーチンよりも前に DCArgsOption, DCArgsDebug 
    ! のサブルーチンを呼んで下さい.
    !
    ! *force* に .true. が指定される場合, -H, --help オプションが与え
    ! られない場合でもヘルプメッセージを表示した後, プログラムを終了さ
    ! せます.
    !
    ! ヘルプメッセージに表示される情報は, DCArgsOption, DCArgsHelpMsg
    ! サブルーチンによって付加することが可能です.
    !
    use dc_types, only: STRING, STDOUT
    use dc_string, only: StoA, StoI, Printf, Concat, JoinChar, UChar, LChar
    use dc_present, only: present_and_true
    use dc_message, only: MessageNotify
    use dc_hash, only: DCHashGet, DCHashDelete, DCHashRewind, DCHashNext
    implicit none
    type(ARGS), intent(inout) :: arg
    logical, intent(in), optional :: force
    logical :: OPT_help, found, end
    character(STRING) :: VAL_help, options_msg, help_msg, category
    character(STRING), pointer :: localopts(:) => null()
    integer :: unit, i
    character(len = *), parameter  :: subname = 'DCArgsHelp'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Help in dc_args.')
      call DCArgsOpen(arg)
    end if
    call DCArgsOption(arg, StoA('-h', '-H', '--help'), OPT_help, VAL_help, &
      & help="display this help and exit. " // &
      & "VAL is unit number (default is standard output)")
    if (.not. OPT_help .and. .not. present_and_true(force)) then
      return
    end if
    if (trim(VAL_help) == '') then
      unit = STDOUT
    else
      unit = StoI(VAL_help)
    end if

    call Printf(unit, '')

    call DCHashGet(arg % helpmsg, 'TITLE', help_msg, found)
    if (found) then
      call Printf(unit, '%c', c1=trim(help_msg))
      call Printf(unit, '')
      call DCHashDelete(arg % helpmsg, 'TITLE')
    end if

    call DCHashGet(arg % helpmsg, 'OVERVIEW', help_msg, found)
    if (found) then
      call Printf(unit, 'Overview::')
      call PrintAutoLinefeed(unit, help_msg, indent='     ')
      call Printf(unit, '')
      call DCHashDelete(arg % helpmsg, 'OVERVIEW')
    end if

    call DCHashGet(arg % helpmsg, 'USAGE', help_msg, found)
    if (found) then
      call Printf(unit, 'Usage::')
      call PrintAutoLinefeed(unit, help_msg, indent='     ')
      call Printf(unit, '')
      call DCHashDelete(arg % helpmsg, 'USAGE')
    end if

    call Printf(unit, 'Options::')
    if ( associated(arg % opt_table) ) then
      do i = 1, size(arg % opt_table)
        options_msg = ' '
        if (arg % opt_table(i) % optvalue_flag) then
          call Concat(arg % opt_table(i) % options, '=VAL', localopts)
        else
          allocate(localopts(size(arg % opt_table(i) % options)))
          localopts = arg % opt_table(i) % options
        end if
        options_msg = trim(options_msg) // trim(JoinChar(localopts))
        deallocate(localopts)
        call Printf(unit, ' %c', c1=trim(options_msg))
        call PrintAutoLinefeed(unit, &
          & arg % opt_table(i) % help_message, indent='     ')
        call Printf(unit, '')
      end do
    end if

    call DCHashRewind(arg % helpmsg)
    do
      call DCHashNext(arg % helpmsg, category, help_msg, end)
      if (end) exit

      call Printf(unit, '%c%c::', &
        & c1=trim(UChar(category(1:1))), c2=trim(LChar(category(2:))))
      call PrintAutoLinefeed(unit, help_msg, indent='     ')
      call Printf(unit, '')

    enddo

    call DCArgsClose(arg)

    stop
  end subroutine DCArgsHelp0

  subroutine DCArgsHelpMsg0(arg, category, msg)
    !
    ! ヘルプメッセージを追加します. 
    !
    ! サブルーチン DCArgsHelp を使用した際に出力されるメッセージを
    ! 付加します. *category* に +Title+, +Overview+, +Usage+ が
    ! 指定されたものは +Options+ よりも上部に,
    ! それ以外のものは下部に表示されます.
    ! *msg* にはメッセージを与えてください.
    !
    !=== 例
    !
    !     program dc_args_sample3
    !       use dc_types
    !       use dc_string, only: StoA
    !       use dc_args
    !       implicit none
    !       type(ARGS) :: arg
    !       logical :: OPT_namelist
    !       character(STRING) :: VAL_namelist
    !       character(STRING), pointer :: argv(:) => null()
    !       integer :: i
    !
    !       call DCArgsOpen( arg = arg )   ! (out)
    !       call DCArgsHelpMsg( arg = arg, &              ! (inout)
    !         & category = 'Title', &                     ! (in)
    !         & msg = 'dcargs $Revision: 1.2 $ ' // &
    !         &       ':: Test program of dc_args' )      ! (in)
    !       call DCArgsHelpMsg( arg = arg, &              ! (inout)
    !         & category = 'Usage', &                     ! (in)
    !         & msg = 'dcargs [Options] arg1, arg2, ...') ! (in)
    !       call DCArgsOption( arg = arg, &           ! (inout)
    !         & options = StoA('-N', '--namelist'), & ! (in)
    !         & flag = OPT_namelist, &                ! (out)
    !         & value = VAL_namelist, &               ! (out)
    !         & help = "Namelist filename")           ! (in)
    !       call DCArgsHelpMsg( arg = arg, &          ! (inout)
    !         & category = 'DESCRIPTION', &           ! (in)
    !         & msg = '(1) Define type "HASH". ' // &
    !         &       '(2) Open the variable. ' // &
    !         &       '(3) set HelpMsg. ' // &
    !         &       '(4) set Options. ' // &
    !         &       '(5) call Debug. ' // &
    !         &       '(6) call Help. ' // &
    !         &       '(7) call Strict.')             ! (in)
    !       call DCArgsHelpMsg( arg = arg, &                         ! (inout)
    !         & category = 'Copyright', &                            ! (in)
    !         & msg = 'Copyright (C) ' // &
    !         &       'GFD Dennou Club, 2008. All rights reserved.') ! (in)
    !       call DCArgsDebug( arg = arg )  ! (inout)
    !       call DCArgsHelp( arg = arg )   ! (inout)
    !       call DCArgsStrict( arg = arg ) ! (inout)
    !       call DCArgsGet( arg = arg, &   ! (inout)
    !         & argv = argv )              ! (out)
    !       write(*,*) '--namelist=', trim( VAL_namelist )
    !       do i = 1, size(argv)
    !         write(*,*) argv(i)
    !       end do
    !       deallocate( argv )
    !       call DCArgsClose( arg = arg )  ! (inout)
    !     program dc_args_sample3
    !
    ! コマンドライン引数に '-h', '-H', '--help' のいづれかのオプション
    ! を指定することで, HelpMsg で与えたメッセージと, オプションの一覧
    ! が標準出力に表示されます.
    !
    use dc_hash, only: DCHashPut
    use dc_string, only: UChar
    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    character(*), intent(in) :: category
    character(*), intent(in) :: msg
    character(len = *), parameter  :: subname = 'DCArgsHelpMsg'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Help in dc_args.')
      call DCArgsOpen(arg)
    end if
    call DCHashPut(arg % helpmsg, key=UChar(category), value=msg)
  end subroutine DCArgsHelpMsg0
  

  subroutine DCArgsStrict0(arg, severe)
    !
    ! オプションチェックを行います. 
    !
    ! コマンドライン引数のオプションとして指定されたものの内,
    ! DCArgsOption サブルーチンで設定されていないものが存在する
    ! 場合には警告を返します. *severe* に .true. を指定すると
    ! エラーを返して終了します.
    ! このサブルーチンを呼ぶ前に, DCArgsOption, DCArgsDebug, 
    ! DCArgsHelp サブルーチンを呼んでください.
    !
    ! 構造体 ARGS の変数に対してこのサブルーチンを適用しておく
    ! ことで, コマンドライン引数として与えたオプションが正しく
    ! プログラムが認識しているかどうかをチェックすることができます.
    !
    !
    use dc_types, only: STRING
    use dc_present, only: present_and_true
    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    logical, intent(in), optional :: severe
    character(STRING) :: err_mess
    integer :: i
    character(len = *), parameter  :: subname = 'DCArgsStrict'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Help in dc_args.')
      call DCArgsOpen(arg)
    end if
    do i = 1, size(arg % cmd_opts_list)
      err_mess = trim(arg % cmd_opts_list(i) % name) // ' is invalid option.'
      if (.not. arg % cmd_opts_list(i) % flag_called) then
        if (present_and_true(severe)) then
          call MessageNotify('E', subname, err_mess)
        else
          call MessageNotify('W', subname, err_mess)
        end if
      end if
    end do
  end subroutine DCArgsStrict0


  subroutine DCArgsGet0(arg, argv)
    !
    ! コマンドライン引数のうち, オプションではないものを
    ! *argv* に返します.
    !
    ! *argv* は文字型配列のポインタです.
    ! 引数として与える場合には必ず空状態して与えてください.
    !
    use dc_types, only: STRING
    use dc_string, only: StoA, StoI, Printf, Concat, JoinChar
    use dc_present, only: present_and_true
    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    character(*), pointer :: argv(:) !(out)
    integer :: i, cmd_argv_max
    character(len = *), parameter  :: subname = 'DCArgsGet'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Help in dc_args.')
      call DCArgsOpen(arg)
    end if
    cmd_argv_max = size(cmd_argv_list)
    allocate(argv(cmd_argv_max))
    do i = 1, cmd_argv_max
      argv(i) = cmd_argv_list(i)
    end do
  end subroutine DCArgsGet0

  function DCArgsNumber0(arg) result(result)
    !
    ! コマンドライン引数として与えられた引数の数を返します.
    !
    use dc_message, only: MessageNotify
    implicit none
    type(ARGS), intent(inout) :: arg
    integer :: result
    character(len = *), parameter  :: subname = 'DCArgsNumber'
  continue
    if (.not. arg % initialized) then
      call MessageNotify('W', subname, 'Call Open before Help in dc_args.')
      call DCArgsOpen(arg)
    end if
    result = size(cmd_argv_list)
  end function DCArgsNumber0

  subroutine DCArgsPutLine0(arg)
    !
    ! *arg* に関する情報を標準出力に表示します.
    !
    use dc_types, only: STDOUT
    use dc_string, only: Printf, JoinChar
    implicit none
    type(ARGS), intent(in) :: arg
    integer :: i
  continue
    if (.not. arg % initialized) then
      call Printf(STDOUT, '#<ARGS:: @initialized=%y>', l=(/arg % initialized/))
      return
    end if
    call Printf(STDOUT, '#<ARGS:: @initialized=%y,', l=(/arg % initialized/))
    call Printf(STDOUT, '  @opt_table(:)=')
    if ( associated(arg % opt_table) ) then
      do i = 1, size(arg % opt_table)
        call Printf(STDOUT, '    #<OPT_ENTRY:: ')
        call Printf(STDOUT, '      @options=%c, @help_message=%c, @optvalue_flag=%y', &
          & c1=trim(JoinChar(arg % opt_table(i) % options)), &
          & c2=trim(arg % opt_table(i) % help_message), &
          & l=(/arg % opt_table(i) % optvalue_flag/))
        call Printf(STDOUT, '    >')
      end do
    end if
    call Printf(STDOUT, '  ,')
    call Printf(STDOUT, '  @cmd_opts_list(:)=')
    do i = 1, size(arg % cmd_opts_list)
      call Printf(STDOUT, '    #<CMD_OPTS_INTERNAL:: ')
      call Printf(STDOUT, '      @name=%c, @value=%c, @flag_called=%y', &
        & c1=trim(arg % cmd_opts_list(i) % name), &
        & c2=trim(arg % cmd_opts_list(i) % value), &
        & l=(/arg % cmd_opts_list(i) % flag_called/))
      call Printf(STDOUT, '    >')
    end do
    call Printf(STDOUT, '  ,')
    call Printf(STDOUT, '  @cmd_argv_list(:)=%c', &
      & c1=trim(JoinChar(cmd_argv_list)))
    call Printf(STDOUT, '>')

  end subroutine DCArgsPutLine0

  subroutine PrintAutoLinefeed(unit, fmt, length, indent)
    !
    ! 文字列を自動改行して出力します. 
    ! このモジュール内部で用いるためのサブルーチンです.
    !
    ! *fmt* に与えられた文章を文字数 *length* (指定されない場合 70)
    ! 以内に改行し, 出力します. 出力の際, *indent* が指定されていると
    ! その文字列を行頭に挿入して出力を行います.
    ! 出力先はデフォルトは標準出力となります. *unit* に出力装置番号
    ! を設定することで出力先を変更できます.
    !
    use dc_types, only: STRING, STDOUT
    use dc_string, only: Split
    implicit none
    character(*), intent(in) :: fmt
    integer,      intent(in), optional :: length ! 一行の長さ
    character(*), intent(in), optional :: indent ! 字下げ文字列
    integer,      intent(in), optional :: unit   ! 出力装置
    character(STRING), pointer :: carray_tmp(:) => null()
    character(STRING) :: store_str
    integer, parameter :: default_len = 70
    integer :: i, split_len, indent_len, unit_num
    logical :: new_line_flag
  continue
    if (present(unit)) then
      unit_num = unit
    else
      unit_num = STDOUT
    end if

    if (present(indent)) then
      indent_len = len(indent)
    else
      indent_len = 0
    end if

    if (present(length)) then
      split_len = length - indent_len
    else
      split_len = default_len - indent_len
    end if


    nullify(carray_tmp)
    call Split(fmt, carray_tmp, '')
    store_str = ''
    new_line_flag = .true.
    i = 1
    do
      if (i > size(carray_tmp)) then
        write(unit_num, '(A)') trim(store_str)
        exit
      end if

      if (len(trim(store_str)) + len(trim(carray_tmp(i))) > split_len) then
        if (new_line_flag) then
          write(unit_num, '(A)') trim(carray_tmp(i))
          i = i + 1
        else
          write(unit_num, '(A)') trim(store_str)
          store_str = ''
          new_line_flag = .true.
        end if
        cycle
      end if

      if (new_line_flag .and. present(indent)) then
        store_str = indent // trim(carray_tmp(i))
      else
        store_str = trim(store_str) // ' ' // trim(carray_tmp(i))
      end if
      new_line_flag = .false.
      i = i + 1
    end do

  end subroutine PrintAutoLinefeed

  subroutine SortArgTable
    !
    ! 内部向けの引数振り分けのためのサブルーチンです. 
    !
    ! BuildArgTable で設定された argind_count, argstr_table を
    ! 用い, cmd_argv_list, cmd_opts_list を設定します.
    !
    ! 既に一度でも呼ばれている場合, 何もせずに終了します.
    !
    use dc_types, only: STRING
    implicit none
    character(STRING):: raw_arg, name, value
    integer:: i, cmd_argv_count, cmd_opts_count, cmd_argv_max, cmd_opts_max
  continue
    if (allocated(cmd_opts_list)) return
    cmd_argv_count = 0
    cmd_opts_count = 0
    check_count: do, i = 1, argind_count
      raw_arg = argstr_table(i)
      if (DCOptionFormC(raw_arg, name, value)) then
        cmd_opts_count = cmd_opts_count + 1
      else
        cmd_argv_count = cmd_argv_count + 1
      end if
    end do check_count

    cmd_argv_max = cmd_argv_count
    cmd_opts_max = cmd_opts_count

    allocate(cmd_argv_list(cmd_argv_max))
    allocate(cmd_opts_list(cmd_opts_max))

    cmd_argv_count = 0
    cmd_opts_count = 0
    arg_get : do, i = 1, argind_count
      raw_arg = argstr_table(i)
      if (DCOptionFormC(raw_arg, name, value)) then
        cmd_opts_count = cmd_opts_count + 1
        cmd_opts_list(cmd_opts_count) % name = name
        cmd_opts_list(cmd_opts_count) % value = value
        cmd_opts_list(cmd_opts_count) % flag_called = .false.
      else
        cmd_argv_count = cmd_argv_count + 1
        cmd_argv_list(cmd_argv_count) = raw_arg
      end if
    end do arg_get
  end subroutine SortArgTable

  subroutine BuildArgTable
    !
    ! 内部向けコマンドライン引数処理のサブルーチンです. 
    !
    ! モジュール sysdep の sysdep#SysdepArgCount, sysdep#ArgGet
    ! を呼び出し, その内容を argind_count と argstr_table に格納します.
    !
    ! 既に一度でも呼ばれている場合, 何もせずに終了します.
    !
    use sysdep, only: SysdepArgCount, SysdepArgGet
    use dc_types, only: STRING
    implicit none
    integer:: i, narg, nargmax
    character(len = STRING):: value
    character(len = STRING), allocatable:: localtab(:)
  continue
    if (argind_count >= 0) return
    nargmax = SysdepArgCount()
    allocate(localtab(nargmax))
    narg = 0
    do, i = 1, nargmax
      call SysdepArgGet(i, value)
      narg = narg + 1
      localtab(narg) = value
    enddo
    argind_count = narg
    allocate(argstr_table(narg))
    argstr_table(1: narg) = localtab(1: narg)
    deallocate(localtab)
  end subroutine BuildArgTable

  function DCOptionFormC(argument, name, value) result(result)
    !
    ! 引数としてで得られた文字列を *argument* に渡すことで,
    ! それがオプションなのかそうでないのかを判別し, もしも
    ! オプションと判別した場合には戻り値に .true. を返し,
    ! name にオプション名, *value* にその値を返す.
    ! オプションに値が付加されない場合は *value* には空白を返す.
    !
    ! オプションではない場合は戻り値に .false. を返し,
    ! *name*, *value* には空白を返す.
    !
    ! オプションと判定されるのは以下の場合です.
    !
    ! * 一文字目が '-' の場合. この場合は短いオプションとなり, '-'
    !   の次の一文字のみがオプションとして有効になります.
    !
    ! * 1-2文字目が '--' の場合. この場合は長いオプションとなり,
    !   '--' 以降の文字列がオプションとして有効になります.
    !
    ! オプションの値は, "=" よりも後ろの文字列になります.
    !
    !=== 例
    !
    ! *argument*    :: <b>name,      value, 返り値</b>
    ! arg           ::    空白,      空白,  .false.
    ! -O            ::    -O,        空白,  .true.
    ! -debug        ::    -d,        空白,  .true.
    ! --debug       ::    --debug,   空白,  .true.
    ! -I=/usr       ::    -I,        /usr,  .true.
    ! --include=/usr::    --include, /usr,  .true.
    !
    implicit none
    character(len = *), intent(in):: argument
    character(len = *), intent(out):: name, value
    logical :: result
    integer:: equal
  continue
    equal = index(argument, '=')
    if (argument(1:1) == '-' .and. argument(2:2) /= '-') then
      ! Short Option
      if (equal == 0) then
        name = argument(1:2)
        value = ""
      else
        name = argument(1:2)
        value = argument(equal+1: )
      endif
      result = .true.
    elseif (argument(1:2) == '--') then
      ! Long Option
      if (equal == 0) then
        name = argument
        value = ""
      else
        name = argument(1:equal-1)
        value = argument(equal+1: )
      endif
      result = .true.
!    elseif (equal == 0 .and. &
!      &     verify(argument(1:equal-1), WORDCHARS) == 0) then
!      ! ???
!      name = argument(1:equal-1)
!      value = argument(equal+1: )
!      result = .true.
    else
      ! No Option (normal arguments)
      name = ""
      value = ""
      result = .false.
    endif
  end function DCOptionFormC



end module dc_args
