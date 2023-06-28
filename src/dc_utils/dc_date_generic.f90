!= dc_date より提供される手続の引用仕様宣言
!= Interface of procedures provided from dc_date
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_date_generic.f90,v 1.3 2009-06-01 15:17:23 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_date_generic
  != dc_date より提供される手続の引用仕様宣言
  != Interface of procedures provided from dc_date
  !
  ! モジュールの概要については, dc_date 
  ! を参照ください. 
  !
  ! See "dc_date" for brief of this module. 
  !
  !== Procedures List
  !
  ! 以下の手続きは構造型 dc_date_types#DC_DATETIME または
  ! dc_date_types#DC_DIFFTIME 変数 (日時, 時刻に関する情報を格納)
  ! を対象とします.
  !
  ! DCDateTimeCreate     :: dc_date_types#DC_DATETIME 型変数の初期設定
  ! DCDiffTimeCreate     :: dc_date_types#DC_DIFFTIME 型変数の初期設定
  !
  ! #assignment(=)       :: dc_date_types#DC_DATETIME 型変数および
  !                         dc_date_types#DC_DIFFTIME 型変数の初期設定
  !
  ! Eval                 :: 日時, 時刻情報を個別に取得
  !
  ! toChar               :: 日時, 時刻情報を文字型変数へ変換
  !
  ! EvalDay     :: 日数 (実数型) に換算して取得
  ! EvalHour    :: 時間 (実数型) に換算して取得
  ! EvalMin     :: 分 (実数型) に換算して取得
  ! EvalSec     :: 秒 (実数型) に換算して取得
  ! EvalNondim  :: 無次元時間 (実数型) に換算して取得
  ! EvalByUnit  :: 単位を指定し, 日, 時, 分, 秒のいづれか (実数型) 
  !                に換算して取得
  !
  ! #operator(+)  :: 加算 (dc_date_types#DC_DATETIME 型 および
  !                  dc_date_types#DC_DIFFTIME 型 同士)
  ! #operator(-)  :: 減算 (dc_date_types#DC_DATETIME 型 および
  !                  dc_date_types#DC_DIFFTIME 型 同士)
  ! #operator(*)  :: 乗算 (dc_date_types#DC_DIFFTIME 型と数値型)
  ! #operator(/)  :: 除算 (dc_date_types#DC_DIFFTIME 型と数値型)
  ! mod           :: 余り (dc_date_types#DC_DIFFTIME 型同士)
  ! #operator(==) :: 比較 (dc_date_types#DC_DATETIME 型同士)
  ! #operator(>)  :: 比較 (dc_date_types#DC_DATETIME 型同士)
  ! #operator(>=) :: 比較 (dc_date_types#DC_DATETIME 型同士)
  ! #operator(<)  :: 比較 (dc_date_types#DC_DATETIME 型同士)
  ! #operator(<=) :: 比較 (dc_date_types#DC_DATETIME 型同士)
  ! max           :: 大きい値を返す
  ! min           :: 小さい値を返す
  !
  ! SetZone       :: タイムゾーンを変更
  !
  ! DCDateTimePutLine    :: dc_date_types#DC_DATETIME 型変数に格納されている
  !                         日時, 時刻情報の印字
  ! DCDiffTimePutLine    :: dc_date_types#DC_DIFFTIME 型変数に格納されている
  !                         日時, 時刻情報の印字
  !
  !
  ! 以下の手続きは dc_date_types 内部の変数を変更します.
  !
  ! SetCaltype  :: 暦法のデフォルトを変更
  ! SetSecOfDay :: 1 日の秒数のデフォルトを変更
  !
  ! その他の手続き
  !
  ! ValidCaltype   :: 暦法が有効なものかをチェック
  ! ValidZone      :: タイムゾーンとして有効化をチェック
  ! ZoneToDiff     :: タイムゾーンを dc_date_types#DC_DIFFTIME 変数へと変換
  ! ParseTimeUnits :: 時間の単位を解析し, 単位のシンボルを返します. 
  !
  !== Usage
  !
  !=== 現在時刻の表示
  !
  ! dc_date_types#DC_DATETIME 型の変数にサブルーチン DCDateTimeCreate 
  ! を用いると, 時刻が設定されます.
  ! 下記のように特に年月日を指定しないと現在時刻が設定されます.
  ! 設定された時刻は toChar によって文字型変数へと変換できます.
  ! サブルーチン Printf に関しては dc_string#Printf を参照ください.
  !
  !     program dc_date_sapmle1
  !       use dc_string, only: Printf
  !       use dc_date, only: DC_DATETIME, DCDateTimeCreate, toChar
  !       implicit none
  !       type(DC_DATETIME) :: time
  !
  !       call DCDateTimeCreate( time = time ) ! (out)
  !       call Printf( fmt = 'current date and time is %c', c1 = trim( toChar(time) ) )
  !     end program dc_date_sapmle1
  !
  !=== 日時, 時刻情報の加算
  !
  ! dc_date_types#DC_DIFFTIME 型の変数は日時差を表現します. 下記の例では,
  ! 日時差を表現するための変数として *diff*
  ! を用意し, サブルーチン Create によって 25 日 + 12 時間 + 50 分の日時差
  ! を設定しています. dc_date_types#DC_DATETIME 型の変数 *time_before* と *diff* とを
  ! #operator(+) によって加算することで *time_before* から
  ! 25 日 + 12 時間 + 50 分を進めた日時 *time_after* を取得しています.
  !
  !     program dc_date_sapmle2
  !       use dc_types, only: DP
  !       use dc_string, only: Printf
  !       use dc_date, only: DC_DATETIME, DC_DIFFTIME, &
  !         & DCDateTimeCreate, DCDiffTimeCreate, toChar, operator(+)
  !       implicit none
  !       type(DC_DATETIME) :: time_before, time_after
  !       type(DC_DIFFTIME) :: diff
  !
  !       call DCDateTimeCreate( time = time_before, & ! (out)
  !         & year = 2006, mon = 6,  day = 10, &       ! (in)
  !         & hour = 14,   min = 15, sec = 0.0_DP )    ! (in)
  !       call DCDiffTimeCreate( diff = diff, &        ! (out)
  !         & day = 25, hour = 12, min = 50)           ! (in)
  !
  !       time_after = time_before + diff
  !
  !       call Printf( fmt = '%c + %c = %c', &
  !         & c1 = trim( toChar(time_before) ), c2 = trim( toChar(diff) ), &
  !         & c3 = trim( toChar(time_after) ) )
  !     end program dc_date_sapmle2
  !
  !
  !=== 時間積分のループへの応用
  !
  ! 以下は dA/dt = - αA (初期値 1, α=0.0001) を t = 12 (時間)
  ! まで解くプログラムの例です. 時間積分には前進差分を用いています.
  ! Δt, データの出力間隔, 計算時間に dc_date_types#DC_DIFFTIME を用いることで,
  ! ループの終了処理や
  ! データ出力の際の時刻の比較が容易となります.
  !
  !     program dc_date_sapmle3
  !       use dc_types, only: DP
  !       use dc_date, only: DC_DIFFTIME, &
  !         & DCDiffTimeCreate, EvalSec, EvalByUnit, mod, &
  !         & operator(*), operator(==), operator(>)
  !       implicit none
  !       real(DP)                :: func_a = 1.0d0    ! 関数 A の初期値
  !       real(DP), parameter     :: alph   = 0.0001d0 ! 係数 α
  !       character(*), parameter :: out_unit = 'hour' ! 出力される時刻の単位
  !       type(DC_DIFFTIME):: DelTimef, intervalf, calctimef
  !       integer :: i
  !     continue
  !       call DCDiffTimeCreate( &            !       Δt = 5.0 (秒)
  !         & diff = DelTimef, &              ! (out)
  !         & value = 5.0_DP, unit = 'sec')   ! (in)
  !       call DCDiffTimeCreate( &            !       データ出力間隔 = 1.0 (分)
  !         & diff = intervalf, &             ! (out)
  !         & value = 1.0_DP, unit = 'min')   ! (in)
  !       call DCDiffTimeCreate( &            !       計算時間 = 12.0 (時間)
  !         & diff = calctimef, &             ! (out)
  !         & value = 12.0_DP, unit = 'hour') ! (in)
  !     
  !       open( 10, file='dc_date_sample.dat' )
  !       write(10,'(A,A,A)') '#  ', out_unit, '                value'
  !     
  !       i = 1
  !       do
  !         if (DelTimef * i > calctimef) exit    ! 計算時間を過ぎたら終了
  !     
  !         !---------------------------------------------
  !         ! A_(n+1) = (1 - αΔt) * A_(n)
  !         !---------------------------------------------
  !         func_a = (1.0 - alph * EvalSec(DelTimef)) * func_a
  !     
  !         !---------------------------------------------
  !         ! intervalf (1 分) 毎にデータを出力
  !         !---------------------------------------------
  !         if (mod(DelTimef * i, intervalf) == 0) then
  !           write(10,*) ' ', EvalByUnit( DelTimef * i, out_unit ), func_a
  !         end if
  !         i = i + 1
  !       end do
  !     end program dc_date_sapmle3
  !
  !

  use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
  use dc_types, only: DP, STRING, TOKEN
  use dc_present, only: present_and_not_empty

  implicit none

  private
  public:: DCDateTimeCreate, DCDiffTimeCreate
  public:: DCDateTimePutLine, DCDiffTimePutLine
  public:: Eval
  public:: SetCaltype, SetZone, SetSecOfDay
  public:: ValidCaltype, ValidZone, ZoneToDiff, ParseTimeUnits

  public:: assignment(=)
  public:: mod, operator(/), operator(-), operator(+), operator(*)
  public:: operator(<), operator(>), operator(>=), operator(<=)
  public:: operator(==), max, min
  public:: toChar, toCharCal
  public:: EvalDay, EvalHour, EvalMin, EvalSec, EvalNondim, EvalByUnit
  public:: EvalSclSec

  public:: Create, PutLine
                    ! 後方互換用
                    ! For backward compatibility

  interface DCDateTimeCreate
    subroutine DCDateTimeCreate1(time, &
      & year, mon, day, hour, min, sec, &
      & zone, zone_hour, zone_min, caltype, caltype_str, day_seconds, &
      & sclyear, sclmon, sclday, sclsec, err) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DATETIME), intent(out):: time
      integer, intent(in), optional:: year, mon, day, hour, min
      real(DP),intent(in), optional:: sec, day_seconds
      character(*), intent(in), optional :: zone
      integer, intent(in), optional :: zone_hour
      integer, intent(in), optional :: zone_min
      integer, intent(in), optional:: caltype
      character(*), intent(in), optional:: caltype_str
      type(DC_SCALED_SEC), intent(in), optional:: sclyear, sclmon, sclday, sclsec
      logical, intent(out), optional:: err
    end subroutine DCDateTimeCreate1
  end interface

  interface DCDiffTimeCreate
    subroutine DCDiffTimeCreate1(diff, &
      & year, mon, day, hour, min, sec, day_seconds, nondim, &
      & sclyear, sclmon, sclday, sclsec ) !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DIFFTIME), intent(out) :: diff
      integer, intent(in), optional:: year, mon, day, hour, min
      real(DP),intent(in), optional:: sec, day_seconds, nondim
      type(DC_SCALED_SEC), intent(in), optional:: sclyear, sclmon, sclday, sclsec
    end subroutine DCDiffTimeCreate1

    subroutine DCDiffTimeCreate2D(diff, value, unit, unit_symbol, err) !:doc-priority 70:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out) :: diff
      real(DP), intent(in) :: value
      character(*), intent(in) :: unit
      integer, intent(in), optional :: unit_symbol
      logical, intent(out), optional :: err
    end subroutine DCDiffTimeCreate2D

    subroutine DCDiffTimeCreate2R(diff, value, unit, unit_symbol, err) !:doc-priority 80:
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out) :: diff
      real, intent(in) :: value
      character(*), intent(in) :: unit
      integer, intent(in), optional :: unit_symbol
      logical, intent(out), optional :: err
    end subroutine DCDiffTimeCreate2R

    subroutine DCDiffTimeCreate2I(diff, value, unit, unit_symbol, err) !:doc-priority 90:
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out) :: diff
      integer, intent(in) :: value
      character(*), intent(in) :: unit
      integer, intent(in), optional :: unit_symbol
      logical, intent(out), optional :: err
    end subroutine DCDiffTimeCreate2I

  end interface

  interface DCDateTimePutLine
    subroutine DCDateTimePutLine(time, unit, indent)
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(in) :: time
      integer, intent(in), optional :: unit
      character(*), intent(in), optional:: indent
    end subroutine DCDateTimePutLine
  end interface

  interface DCDiffTimePutLine
    subroutine DCDiffTimePutLine(diff, unit, indent)
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(in) :: diff
      integer, intent(in), optional :: unit
      character(*), intent(in), optional:: indent
    end subroutine DCDiffTimePutLine
  end interface

  interface assignment(=)

    subroutine DCDateTimeCreateI(time, sec) !:doc-priority 20:
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(out):: time
      integer, intent(in):: sec
    end subroutine DCDateTimeCreateI

    subroutine DCDateTimeCreateR(time, sec) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(out):: time
      real, intent(in):: sec
    end subroutine DCDateTimeCreateR

    subroutine DCDateTimeCreateD(time, sec) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(out):: time
      real(DP), intent(in):: sec
    end subroutine DCDateTimeCreateD

    subroutine DCDiffTimeCreateI(diff, sec) !:doc-priority 60:
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out):: diff
      integer, intent(in):: sec
    end subroutine DCDiffTimeCreateI

    subroutine DCDiffTimeCreateR(diff, sec) !:doc-priority 70:
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out):: diff
      real, intent(in):: sec
    end subroutine DCDiffTimeCreateR

    subroutine DCDiffTimeCreateD(diff, sec) !:doc-priority 80:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out):: diff
      real(DP), intent(in):: sec
    end subroutine DCDiffTimeCreateD

!!$    subroutine DCDateLetFC(diff, string)
!!$      use dc_date_types, only: DC_DIFFTIME
!!$      type(DC_DIFFTIME), intent(out):: diff
!!$      character(len = *), intent(in):: string
!!$    end subroutine DCDateLetFC
!!$
!!$    subroutine DCDateLetTC(time, string)
!!$      use dc_date_types, only: DC_DATETIME
!!$      type(DC_DATETIME), intent(out):: time
!!$      character(len = *), intent(in):: string
!!$    end subroutine DCDateLetTC

  end interface

  interface SetCaltype
    subroutine DCDateTimeSetCaltype(caltype)
      integer, intent(in):: caltype
    end subroutine DCDateTimeSetCaltype
  end interface

  interface SetSecOfDay
    subroutine DCDateTimeSetSecOfDay(sec)
      use dc_types, only: DP
      real(DP), intent(in):: sec
    end subroutine DCDateTimeSetSecOfDay
  end interface

  interface ValidCaltype
    function DCDateTimeValidCaltype(caltype) result(result)
      integer, intent(in):: caltype
      logical:: result
    end function DCDateTimeValidCaltype
  end interface

  interface ValidZone
    function DCDateTimeValidZone(zone) result(result)
      character(*), intent(in):: zone
      logical:: result
    end function DCDateTimeValidZone
  end interface

  interface ZoneToDiff
    function DCDateTimeZoneToDiff(zone) result(diff)
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME):: diff
      character(*), intent(in):: zone
    end function DCDateTimeZoneToDiff
  end interface

  interface ParseTimeUnits
    function DCDateTimeParseUnits(str) result(symbol)
      character(*), intent(in):: str
      integer:: symbol
    end function DCDateTimeParseUnits
  end interface

  interface SetZone
    subroutine DCDateTimeSetZone(time, zone, err)
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(inout):: time
      character(*), intent(in):: zone
      logical, intent(out), optional:: err
    end subroutine DCDateTimeSetZone
  end interface


  interface Eval

    subroutine DCDateTimeEval1(time, year, mon, day, hour, min, &
      & sec, caltype, zone, sclyear, sclmon, sclday, sclsec)  !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DATETIME), intent(in):: time
      integer, intent(out), optional:: year, mon, day, hour, min, caltype
      real(DP), intent(out), optional:: sec
      character(*), intent(out), optional:: zone
      type(DC_SCALED_SEC), intent(out), optional:: sclyear, sclmon, sclday, sclsec
    end subroutine DCDateTimeEval1

!!$    subroutine DCDateTimeEval0(time, mon, day, sec)
!!$      use dc_date_types, only: DC_DATETIME
!!$      use dc_types,      only: DP
!!$      type(DC_DATETIME), intent(in):: time
!!$      integer, intent(out):: mon, day
!!$      real(DP), intent(out):: sec
!!$    end subroutine DCDateTimeEval0

    subroutine DCDiffTimeEval1(diff, &
      & year, mon, day, hour, min, sec, nondim, &
      & sclyear, sclmon, sclday, sclsec, sclnondim, err)  !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(out), optional:: year, mon, day, hour, min
      real(DP), intent(out), optional:: sec, nondim
      type(DC_SCALED_SEC), intent(out), optional:: sclyear, sclmon, sclday, sclsec, sclnondim
      logical, intent(out), optional :: err
    end subroutine DCDiffTimeEval1

  end interface

  interface EvalDay
    function DCDateTimeEvalDay(time) result(result)  !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      real(DP):: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeEvalDay

    function DCDiffTimeEvalDay(diff) result(result)  !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDiffTimeEvalDay
  end interface

  interface EvalHour
    function DCDateTimeEvalHour(time) result(result) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      real(DP):: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeEvalHour

    function DCDifftimeEvalHour(diff) result(result) !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDifftimeEvalHour
  end interface

  interface EvalMin
    function DCDateTimeEvalMin(time) result(result) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      real(DP):: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeEvalMin

    function DCDifftimeEvalMin(diff) result(result) !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDifftimeEvalMin
  end interface

  interface EvalSec
    function DCDateTimeEvalSec(time) result(result) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      real(DP):: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeEvalSec

    function DCDifftimeEvalSec(diff) result(result) !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDifftimeEvalSec
  end interface

  interface EvalNondim
    function DCDiffTimeEvalNondim(diff) result(result)
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDiffTimeEvalNondim
  end interface

  interface EvalSclSec
    function DCDateTimeEvalSclSec(time) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_SCALED_SEC):: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeEvalSclSec

    function DCDifftimeEvalSclSec(diff) result(result) !:doc-priority 60:
      use dc_date_types, only: DC_DIFFTIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_SCALED_SEC):: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDifftimeEvalSclSec
  end interface

  interface EvalByUnit

    function DCDateTimeEvalByUnit(time, unit, unit_symbol) result(result)
      use dc_types, only: DP, TOKEN
      use dc_date_types, only: DC_DATETIME
      real(DP):: result
      type(DC_DATETIME), intent(in):: time
      character(*), intent(in), optional:: unit
      integer, intent(in), optional:: unit_symbol
    end function DCDateTimeEvalByUnit

    function DCDiffTimeEvalByUnit(diff, unit, unit_symbol) result(result)
      use dc_types, only: DP, TOKEN
      use dc_date_types, only: DC_DIFFTIME
      real(DP):: result
      type(DC_DIFFTIME), intent(in):: diff
      character(*), intent(in), optional:: unit
      integer, intent(in), optional:: unit_symbol
    end function DCDiffTimeEvalByUnit
  end interface



  interface toChar
    function DCDateTimeToChar(time) result(result) !:doc-priority 40:
      use dc_types, only: STRING
      use dc_date_types, only: DC_DATETIME
      character(STRING) :: result
      type(DC_DATETIME), intent(in):: time
    end function DCDateTimeToChar

    function DCDiffTimeToChar(diff) result(result) !:doc-priority 60:
      use dc_types, only: STRING
      use dc_date_types, only: DC_DIFFTIME
      character(STRING) :: result
      type(DC_DIFFTIME), intent(in):: diff
    end function DCDiffTimeToChar
  end interface

  interface toCharCal
    function DCDateTimeToCharCal(time, upcase) result(result)
      use dc_types, only: TOKEN
      use dc_date_types, only: DC_DATETIME
      character(TOKEN) :: result
      type(DC_DATETIME), intent(in):: time
      logical, intent(in), optional:: upcase
    end function DCDateTimeToCharCal
  end interface

  interface operator(+)
    type(DC_DATETIME) function dcdatetime_add_ft(diff, time) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      type(DC_DATETIME), intent(in):: time
    end function dcdatetime_add_ft

    type(DC_DATETIME) function dcdatetime_add_tf(time, diff) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_add_tf

    type(DC_DIFFTIME) function dcdatetime_add_ff(diff1, diff2) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_add_ff

    type(DC_DIFFTIME) function dcdatetime_add_fd(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      use dc_types, only: DP
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: sec
    end function dcdatetime_add_fd

    type(DC_DIFFTIME) function dcdatetime_add_fr(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: sec
    end function dcdatetime_add_fr

    type(DC_DIFFTIME) function dcdatetime_add_fi(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: sec
    end function dcdatetime_add_fi
  end interface

  interface operator(-)
    type(DC_DATETIME) function dcdatetime_sub_tf(time, diff) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_sub_tf

    type(DC_DIFFTIME) function dcdatetime_sub_tt(time1, time2) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_sub_tt

    type(DC_DIFFTIME) function dcdatetime_sub_ff(diff1, diff2) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_sub_ff

    type(DC_DIFFTIME) function dcdatetime_sub_fd(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      use dc_types, only: DP
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: sec
    end function dcdatetime_sub_fd

    type(DC_DIFFTIME) function dcdatetime_sub_fr(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: sec
    end function dcdatetime_sub_fr

    type(DC_DIFFTIME) function dcdatetime_sub_fi(diff, sec) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: sec
    end function dcdatetime_sub_fi
  end interface

  interface operator(*)
    type(DC_DIFFTIME) function dcdatetime_mul_if(factor, diff) result(result) !:doc-priority 51:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      integer, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_mul_if

    type(DC_DIFFTIME) function dcdatetime_mul_fi(diff, factor) result(result) !:doc-priority 52:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: factor
    end function dcdatetime_mul_fi

    type(DC_DIFFTIME) function dcdatetime_mul_rf(factor, diff) result(result) !:doc-priority 61:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      real, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_mul_rf

    type(DC_DIFFTIME) function dcdatetime_mul_fr(diff, factor) result(result) !:doc-priority 62:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: factor
    end function dcdatetime_mul_fr

    type(DC_DIFFTIME) function dcdatetime_mul_df(factor, diff) result(result) !:doc-priority 71:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      real(DP), intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_mul_df

    type(DC_DIFFTIME) function dcdatetime_mul_fd(diff, factor) result(result) !:doc-priority 72:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: factor
    end function dcdatetime_mul_fd

  end interface

  interface operator(/)
    type(DC_DIFFTIME) function dcdatetime_div_fi(diff, denominator) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: denominator
    end function dcdatetime_div_fi

    type(DC_DIFFTIME) function dcdatetime_div_fr(diff, denominator) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: denominator
    end function dcdatetime_div_fr

    type(DC_DIFFTIME) function dcdatetime_div_fd(diff, denominator) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      use dc_types, only: DP
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: denominator
    end function dcdatetime_div_fd

    real(DP) function dcdatetime_div_ff(diff1, diff2) result(result)
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      use dc_types, only: DP
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_div_ff
  end interface

  interface mod
    type(DC_DIFFTIME) function dcdatetime_mod_ff(diff1, diff2) result(result)
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_mod_ff
  end interface

  interface operator(==)
    logical function dcdatetime_eq_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_eq_tt

    logical function dcdatetime_eq_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_eq_ff

    logical function dcdatetime_eq_if(i, diff) result(result) !:doc-priority 51:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: i
    end function dcdatetime_eq_if

    logical function dcdatetime_eq_fi(diff, i) result(result) !:doc-priority 52:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: i
    end function dcdatetime_eq_fi

    logical function dcdatetime_eq_rf(r, diff) result(result) !:doc-priority 61:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: r
    end function dcdatetime_eq_rf

    logical function dcdatetime_eq_fr(diff, r) result(result) !:doc-priority 62:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real, intent(in):: r
    end function dcdatetime_eq_fr

    logical function dcdatetime_eq_df(d, diff) result(result) !:doc-priority 71:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: d
    end function dcdatetime_eq_df

    logical function dcdatetime_eq_fd(diff, d) result(result) !:doc-priority 72:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      real(DP), intent(in):: d
    end function dcdatetime_eq_fd

  end interface


  interface operator(>)
    logical function dcdatetime_gt_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_gt_tt

    logical function dcdatetime_gt_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_gt_ff

    logical function dcdatetime_gt_fi(diff, factor) result(result) !:doc-priority 42:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: factor
    end function dcdatetime_gt_fi

    logical function dcdatetime_gt_if(factor, diff) result(result) !:doc-priority 44:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      integer, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_gt_if
  end interface

  interface operator(<)
    logical function dcdatetime_lt_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_lt_tt

    logical function dcdatetime_lt_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_lt_ff

    logical function dcdatetime_lt_fi(diff, factor) result(result) !:doc-priority 42:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: factor
    end function dcdatetime_lt_fi

    logical function dcdatetime_lt_if(factor, diff) result(result) !:doc-priority 44:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      integer, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_lt_if
  end interface

  interface operator(>=)
    logical function dcdatetime_ge_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_ge_tt

    logical function dcdatetime_ge_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_ge_ff

    logical function dcdatetime_ge_fi(diff, factor) result(result) !:doc-priority 42:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: factor
    end function dcdatetime_ge_fi

    logical function dcdatetime_ge_if(factor, diff) result(result) !:doc-priority 44:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      integer, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_ge_if
  end interface

  interface operator(<=)
    logical function dcdatetime_le_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_le_tt

    logical function dcdatetime_le_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_le_ff

    logical function dcdatetime_le_fi(diff, factor) result(result) !:doc-priority 42:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff
      integer, intent(in):: factor
    end function dcdatetime_le_fi

    logical function dcdatetime_le_if(factor, diff) result(result) !:doc-priority 44:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      integer, intent(in):: factor
      type(DC_DIFFTIME), intent(in):: diff
    end function dcdatetime_le_if
  end interface

  interface max
    type(DC_DATETIME) function dcdatetime_max_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_max_tt

    type(DC_DIFFTIME) function dcdatetime_max_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_max_ff
  end interface

  interface min
    type(DC_DATETIME) function dcdatetime_min_tt(time1, time2) result(result) !:doc-priority 30:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DATETIME), intent(in):: time1, time2
    end function dcdatetime_min_tt

    type(DC_DIFFTIME) function dcdatetime_min_ff(diff1, diff2) result(result) !:doc-priority 40:
      use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
      type(DC_DIFFTIME), intent(in):: diff1, diff2
    end function dcdatetime_min_ff
  end interface

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  interface Create
    subroutine DCDateTimeCreate1_bc(time, &
      & year, mon, day, hour, min, sec, &
      & zone, caltype, day_seconds, err) !:doc-priority 40:
      use dc_types, only: DP
      use dc_date_types, only: DC_DATETIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DATETIME), intent(out):: time
      integer, intent(in), optional:: year, mon, day, hour, min
      real(DP),intent(in), optional:: sec, day_seconds
      character(*), intent(in), optional :: zone
      integer, intent(in), optional:: caltype
      logical, intent(out), optional:: err
    end subroutine DCDateTimeCreate1_bc

    subroutine DCDiffTimeCreate1_bc(diff, &
      & year, mon, day, hour, min, sec, day_seconds ) !:doc-priority 60:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      use dc_scaledsec, only: DC_SCALED_SEC
      type(DC_DIFFTIME), intent(out) :: diff
      integer, intent(in), optional:: year, mon, day, hour, min
      real(DP),intent(in), optional:: sec, day_seconds
    end subroutine DCDiffTimeCreate1_bc

    subroutine DCDiffTimeCreate2_bc(diff, value, unit, unit_symbol, err) !:doc-priority 70:
      use dc_types, only: DP
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(out) :: diff
      real(DP), intent(in) :: value
      character(*), intent(in) :: unit
      integer, intent(in), optional :: unit_symbol
      logical, intent(out), optional :: err
    end subroutine DCDiffTimeCreate2_bc
  end interface

  interface PutLine
    subroutine DCDateTimePutLine_bc(time, unit)
      use dc_date_types, only: DC_DATETIME
      type(DC_DATETIME), intent(in) :: time
      integer, intent(in), optional :: unit
    end subroutine DCDateTimePutLine_bc

    subroutine DCDiffTimePutLine_bc(diff, unit)
      use dc_date_types, only: DC_DIFFTIME
      type(DC_DIFFTIME), intent(in) :: diff
      integer, intent(in), optional :: unit
    end subroutine DCDiffTimePutLine_bc
  end interface

end module dc_date_generic
