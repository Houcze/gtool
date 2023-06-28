!== タイムゾーンに関する手続き
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimezone.f90,v 1.2 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimeSetZone(time, zone, err)
  !
  ! 引数 *time* のタイムゾーンを *zone* へと変更します.
  ! 実質的な日時は変更しません.
  !
  ! 引数 *zone* に不適切な値が与えられた場合,
  ! エラーを発生させます.
  ! 引数 *err* を与えている場合には *err* に .true. が返り,
  ! プログラムは続行します.
  !
  use dc_types,      only: STRING
  use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
  use dc_date_generic, only: ValidZone, ZoneToDiff, toChar, Eval, &
    & operator(-), operator(+)
  use dc_error, only: StoreError, DC_EBADTIMEZONE, DC_NOERR
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_string, only: StoI
  implicit none
  type(DC_DATETIME), intent(inout):: time
  character(*), intent(in):: zone
  logical, intent(out), optional:: err
  type(DC_DIFFTIME):: diff, diff_in
  integer :: stat
  character(STRING) :: zone_in, cause_c
  character(*), parameter :: subname = 'DCDateTimeSetZone'
continue
  call BeginSub(subname, 'time=%c, zone=%c', &
    & c1=trim(toChar(time)), c2=trim(zone))
  stat = DC_NOERR
  cause_c = ''

  if (.not. ValidZone(zone)) then
    stat = DC_EBADTIMEZONE
    cause_c = zone
    if (present(err)) then
      call MessageNotify('W', subname, &
        & 'zone=<%c> is invalid.', &
        & c1=trim(zone))
    else
      goto 999
    end if
  end if

  call Eval(time, zone = zone_in)
  diff_in = ZoneToDiff(zone_in)
  diff = ZoneToDiff(zone)

  time = time + (diff_in - diff)
  time % zone = zone

999 continue
  call StoreError(stat, subname, err, cause_c)
  call EndSub(subname, 'time=%c', &
    & c1=trim(toChar(time)))
end subroutine DCDateTimeSetZone

function DCDateTimeZoneToDiff(zone) result(diff)
  !
  ! 与えられるタイムゾーンを dc_date_types#DC_DIFFTIME 変数へと
  ! 変換して返します. タイムゾーンの表記が無効な場合は '+00:00'
  ! が与えられたと解釈します.
  !
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: ValidZone, DCDiffTimeCreate
  use dc_string, only: StoI
  implicit none
  type(DC_DIFFTIME):: diff
  character(*), intent(in):: zone
  integer:: hour, min, sgn
continue
  if (.not. ValidZone(zone)) then
    call DCDiffTimeCreate(diff)
  else
    if (zone(1:1) == '-') then
      sgn = 1
    else
      sgn = -1
    end if
    hour = StoI(zone(2:3))
    min = StoI(zone(5:6))
    call DCDiffTimeCreate(diff, hour = hour * sgn, min = min * sgn)
  end if
end function DCDateTimeZoneToDiff

function DCDateTimeValidZone(zone) result(result)
  !
  ! 与えられるタイムゾーンの表記が有効であれば
  ! .true. を, それ以外の場合は .false. を返します.
  !
  ! タイムゾーンの表記は '+09:00' のように, 1 文字目が '+' または '-',
  ! 2〜3, 5〜6 文字目が数値で, 4 文字目が ':' となります.
  !
  implicit none
  character(*), intent(in):: zone
  logical:: result
continue
  result = .false.
  if (len(zone) < 6) return
  if (verify(zone(1:1), '+-') /= 0) return
  if (verify(zone(2:3), '1234567890') /= 0) return
  if (verify(zone(5:6), '1234567890') /= 0) return
  if (zone(4:4) /= ':') return
  result = .true.
end function DCDateTimeValidZone
