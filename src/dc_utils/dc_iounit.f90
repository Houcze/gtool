!= ファイルオープン時の装置番号処理
!
!= Unit number handling at file open
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_iounit.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_iounit
  !
  != ファイルオープン時の装置番号処理
  !
  != Unit number handling at file open
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! ファイルをオープンする際に使用する装置番号の処理を行います. 
  ! サブルーチン FileOpen にファイル名とオープン時のモードを与えることで, 
  ! 利用可能な装置番号を内部で探査し, 
  ! 内部でファイルをモードに合わせてオープンしてから, 装置番号を返します. 
  ! このモジュールは, 装置番号の管理やファイルの読み取り/書き込み可能
  ! などの確認作業のためのソースコードを簡素化します.
  !
  ! Unit number is handled when a file is opened.
  ! Subroutine "FileOpen" receives filename and open mode firstly, and
  ! searches an available unit number internally, and
  ! opens the file according to the open mode internally, and
  ! returns the unit number finally.
  ! This module simplifies source codes for unit number management or
  ! readable/writable check.
  !
  !== Procedures List
  !
  ! FileOpen           :: ファイル名とモードを与えることでファイルを
  !                       オープンし, 装置番号を返します.
  ! ------------       :: ------------
  ! FileOpen           :: A file is opened and unit number is returned with
  !                       a filename and mode
  !
  !== Usage
  !
  ! このモジュールで提供されるサブルーチン FileOpen に
  ! ファイル名とオープン時のモードを与えてください.
  ! するとファイルがオープンされ, 引数 *unit* に装置番号が返ります.
  ! その装置番号を用い, ファイルの内容の読み込みや
  ! ファイルへの書き込みを行ってください. 読み込みや書き込みやクローズには 
  ! Fortran 組み込みの READ 文や WRITE 文, CLOSE 文を用いてください.
  !
  ! Give filename and open mode to subroutine "FileOpen" provided by
  ! this module.  Then the file is opened and unit number is returned
  ! to an argument *unit*.  Using the unit number, read the contents
  ! of the file or write in the file.  Use Fortran built-in READ,
  ! WRITE, and CLOSE statements for read/write and close.
  !
  !=== Example
  !
  !    program dc_iounit_sample
  !      use dc_types, only: TOKEN
  !      use dc_iounit, only: FileOpen
  !      implicit none
  !      integer:: unit00, unit01
  !      character(TOKEN):: char
  !      character(TOKEN):: filename = 'dc_iounit_sample.nml'
  !      integer:: int
  !      namelist /dc_iounit_sample_nml/ char, int
  !    continue
  !    
  !      call FileOpen(unit00, file = filename, mode = 'w')
  !      write(unit00, *) '&dc_iounit_sample_nml'
  !      write(unit00, *) '  char = "hogehoge",'
  !      write(unit00, *) '  int = 123'
  !      write(unit00, *) '/'
  !      close(unit00)
  !    
  !      call FileOpen(unit01, file = filename, mode = 'r')
  !      read(unit01, nml = dc_iounit_sample_nml)
  !      close(unit01)
  !      write(0, nml = dc_iounit_sample_nml)
  !    
  !    end program dc_iounit_sample
  !
  implicit none
  private

  public:: FileOpen

  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dc_iounit.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $'

  interface FileOpen
    module procedure FileOpen
  end interface

contains

  subroutine FileOpen( &
    & unit, file, mode, &
    & err )
    !
    ! ファイル名を *file* へ, オープンする際のモードを *mode* へと
    ! 与えることで, ファイルをオープンし, 装置番号を *unit* に返します. 
    ! *mode* には以下の文字列を指定します. 省略時は "r" が指定されたもの
    ! とみなします. 
    !
    ! "r"  :: ファイルを読み込みモードでオープンします.
    ! "w"  :: ファイルを書き込みモードでオープンします. 
    !         オープン時にファイルがすでに存在していればその内容を空にします.
    ! "a"  :: ファイルを書き込みモードでオープンします. 
    !         出力はファイルの末尾に追加されます.
    ! "rw" :: ファイルを読み書き両用モードでオープンします. 
    !         オープン時にファイルがすでに
    !         存在していればその内容を空にします.
    ! "ra" :: ファイルを読み書き両用モードでオープンします. 
    !         オープン時にファイルがすでに
    !         存在していれば読み書き位置がファイルの末尾にセットされます.
    !
    ! ファイルが *mode* で指定されるモードで開けない場合, プログラムは
    ! 強制終了します. 引数 *err* が与えられる場合, プログラムは強制終了せず, 
    ! 代わりに *err* に .true. が, *unit* に -1 が代入されます. 
    !
    ! Filename is given to *file*, and open mode is given to *mode*, 
    ! then the file is opened and unit number is returned.
    !
    ! "r"  :: A file is opened with read-only mode
    ! "w"  :: A file is opened with writable mode. 
    !         If a file is exist already, the contest of the file is emptied.
    ! "a"  :: A file is opened with writable mode.
    !         Output is appended at the end of the file.
    ! "rw" :: A file is opened with read/write mode. 
    !         If a file is exist already, the contest of the file is emptied. 
    ! "ra" :: A file is opened with read/write mode. 
    !         If a file is exist already, 
    !         a position of read/write is set at the end of the file.
    !
    ! If the file can not be opened with the mode, the program aborts. 
    ! If this *err* argument is given, .true. is substituted to *err* and
    ! -1 is substituted to *unit* and the program does not abort. 
    !
    use dc_types, only: STRING, TOKEN
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR, &
      & DC_EFILENAMEEMPTY, DC_EBADFILEOPMODE, DC_ENOUNITNUM, &
      & DC_ENOFILEEXIST, DC_ENOFILEREAD, DC_ENOFILEWRITE
    use dc_present, only: present_and_not_empty
    use dc_string, only: toChar, toLower
    implicit none
    integer, intent(out):: unit
    character(*), intent(in):: file
    character(*), intent(in), optional:: mode
    logical, intent(out), optional:: err

    !-----------------------------------
    !  作業変数
    !  Work variables
    integer, parameter:: max_unit = 99
                              ! NAMELIST ファイルをオープンするための
                              ! 装置番号の最大値. Fortran で使用可能な
                              ! 範囲 (0〜99) のうち, 
                              ! 最大値が設定されている.
                              ! 
                              ! Maximum unit number for open of
                              ! NAMELIST file. An maximum
                              ! value within the bounds of available number
                              ! in Fortran (0 - 99) is specified.
    integer, parameter:: min_unit = 0
                              ! NAMELIST ファイルをオープンするための
                              ! 装置番号の最小値. Fortran で使用可能な
                              ! 範囲 (0〜99) のうち, 
                              ! 最小値が設定されている.
                              ! 
                              ! Minimum unit number for open of
                              ! NAMELIST file. An minimum
                              ! value within the bounds of available number
                              ! in Fortran (0 - 99) is specified.
    character(TOKEN):: open_mode
    integer:: unit_work
    logical:: unit_exist_flag, unit_opend_flag
    logical:: file_exist_flag
    integer:: iostat
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'FileOpen'
  continue
    call BeginSub(subname, version)
    stat = DC_NOERR
    cause_c = ''
    unit = -1

    !-----------------------------------------------------------------
    !  オプショナル引数のチェック
    !  Check optional arguments
    !-----------------------------------------------------------------
    if (present_and_not_empty(mode)) then
      open_mode = mode
    else
      open_mode = 'r'
    end if
    call toLower(open_mode)

    !-----------------------------------------------------------------
    !  引数の正当性のチェック
    !  Validation of arguments
    !-----------------------------------------------------------------
    if ( trim(file) == '' ) then
      stat = DC_EFILENAMEEMPTY
      goto 999
    end if

    !----------------------------------------------------------------
    !  使用可能な装置番号の探査
    !  Search available unit number
    !----------------------------------------------------------------
    unit_work = max_unit
    do
      inquire(unit=unit_work, exist=unit_exist_flag, opened=unit_opend_flag)
      if (unit_exist_flag .and. .not. unit_opend_flag) then
        exit
      endif
      unit_work = unit_work - 1
      if (unit_work < min_unit) then
        cause_c = toChar(min_unit) // ' - ' // toChar(max_unit)
        stat = DC_ENOUNITNUM
        goto 999
      end if
    enddo

    !----------------------------------------------------------------
    !  モードの書式のチェック
    !  Check form of mode
    !----------------------------------------------------------------
    select case( trim(open_mode) )
    case ('r', 'w', 'rw', 'a', 'ra')
    case default
      cause_c = open_mode
      stat = DC_EBADFILEOPMODE
      goto 999
    end select

    !----------------------------------------------------------------
    !  ファイルの存在のチェック
    !  Check existance of a file
    !----------------------------------------------------------------
    select case( trim(open_mode) )
    case ('r')
      inquire(file=file, exist=file_exist_flag)
      if (.not. file_exist_flag) then
        cause_c = file
        stat = DC_ENOFILEEXIST
        goto 999
      end if
    end select

    !----------------------------------------------------------------
    !  ファイルの読み込み可能のチェック
    !  Check readable of a file
    !----------------------------------------------------------------
    select case( trim(open_mode) )
    case ('r')
      open(unit=unit_work, iostat=iostat, &
        & file=file, status='OLD', action='READ')
      if (.not. iostat == 0) then
        cause_c = file
        stat = DC_ENOFILEREAD
        goto 999
      end if
      close(unit=unit_work)
    end select

    !----------------------------------------------------------------
    !  ファイルの書き込み可能のチェック
    !  Check writable of a file
    !----------------------------------------------------------------
    select case( trim(open_mode) )
    case ('w', 'a', 'rw', 'ra')
      open(unit=unit_work, iostat=iostat, &
        & file=file, status='UNKNOWN', action='WRITE')
      if (.not. iostat == 0) then
        cause_c = file
        stat = DC_ENOFILEWRITE
        goto 999
      end if
      close(unit=unit_work)
    end select

    !----------------------------------------------------------------
    !  ファイルオープン
    !  Open a file
    !----------------------------------------------------------------
    select case( trim(open_mode) )
    case ('r')
      open(unit=unit_work, file=file, &
        & status='OLD', action='READ')

    case ('w')
      open(unit=unit_work, file=file, &
        & status='REPLACE', action='WRITE')

    case ('rw')
      open(unit=unit_work, file=file, &
        & status='REPLACE', action='READWRITE')

    case ('a')
      open(unit=unit_work, file=file, &
        & status='UNKNOWN', position='APPEND', action='WRITE')

    case ('ra')
      open(unit=unit_work, file=file, &
        & status='UNKNOWN', position='APPEND', action='READWRITE')

    end select

    unit = unit_work

999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine FileOpen

end module dc_iounit
