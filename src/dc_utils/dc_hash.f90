!== Hash module
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_hash.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_hash
  !
  !== Overview
  !
  ! スクリプト言語ではおなじみとなっているハッシュ
  ! (連想配列) を提供します. 
  !
  ! ただし, 現在「値」として与えられるのは文字型のみです.
  !
  !== List
  !
  ! DCHashPut      :: ハッシュにキーと値を付加
  ! DCHashGet      :: キーを与え, ハッシュ内の関連する値を取得
  ! DCHashRewind   :: ハッシュ内全体を探査するための初期化
  ! DCHashNext     :: Rewind 参照
  ! DCHashDelete   :: キーを与え, ハッシュ内の関連する値を削除
  ! DCHashNumber   :: ハッシュのサイズを返す
  ! DCHashPutLine  :: ハッシュの内容を標準出力に出力 (デバック用)
  !
  !
  !== Usage
  !
  !      use dc_types
  !      use dc_hash
  !      type(HASH):: hashv
  !      character(len = STRING):: key, value
  !      logical:: end
  !
  !      call DCHashPut( hashv = hashv, &  ! (out)
  !        & key = 'key1', value = 'val1') ! (in)
  !      call DCHashPut( hashv = hashv, &  ! (inout)
  !        & key = 'key2', value = 'val2') ! (in)
  !      call DCHashPut( hashv = hashv, &  ! (inout)
  !        & key = 'key3', value = 'val3') ! (in)
  !
  !      call DCHashGet( hashv = hashv, & ! (inout)
  !        & key = 'key1', &              ! (in)
  !        & value = value )              ! (out)
  !      write(*,*) 'key=' // 'key1' // ', value=' // trim(value)
  !
  !      write(*,*) 'number(hashv)=', DCHashNumber( hashv )
  !
  !      call DCHashDelete( hashv = hashv, & ! (inout)
  !        & key = 'key1')                   ! (in)
  !
  !      call DCHashRewind( hashv ) ! (inout)
  !      do
  !        call DCHashNext( hashv = hashv, &        ! (inout)
  !          & key = key, value = value, end = end) ! (out)
  !        if (end) exit
  !        write(*,*) 'key=' // trim(key) // ', value=' // trim(value)
  !      enddo
  !
  !      call DCHashDelete( hashv ) ! (inout)
  !
  ! 以下のように出力されます.
  !
  !      key=key1, value=val1
  !      number(hashv)= 3
  !      key=key2, value=val2
  !      key=key3, value=val3
  !
  !== Note
  !
  !=== 「ハッシュ」という命名について
  !
  ! スクリプト言語 Ruby では, 連想配列の内部にデータ検索
  ! アルゴリズム「ハッシュ」が利用されることから, 
  ! そのクラス名に「Hash」という名前がついている. 
  ! 従ってアルゴリズムとしてハッシュを用いていないこのモジュールの名称
  ! が「dc_hash」であることは本来ふさわしくないのだが, 
  ! 適切な英名が無い事から, このような名称となっている. 
  !
  !=== 後方互換
  !
  ! バージョン 20071009 以前に利用可能だった以下の手続きは, 
  ! 後方互換のため, しばらくは利用可能です. 
  ! 
  ! * Put, PutLine, Get, Rewind, Next, Delete, Number
  !
  !
  use dc_types, only : STRING
  implicit none
  private

  public:: HASH
  public:: DCHashPut, DCHashPutLine, DCHashGet
  public:: DCHashRewind, DCHashNext, DCHashDelete, DCHashNumber

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  public:: Put, PutLine, Get, Rewind, Next, Delete, Number

  type HASH
    !
    ! 利用法に関しては dc_hash を参照してください.
    !
    private
    type(HASH_INTERNAL), pointer :: hash_table(:) => null()
    integer :: search_index = 0
  end type HASH

  type HASH_INTERNAL
    private
    character(STRING) :: key
    character(STRING) :: value
  end type HASH_INTERNAL

  interface DCHashPut
    module procedure DCHashPut0
  end interface

  interface DCHashNumber
    module procedure DCHashNumber0
  end interface

  interface DCHashPutLine
    module procedure DCHashPutLine0
  end interface

  interface DCHashRewind
    module procedure DCHashRewind0
  end interface

  interface DCHashNext
    module procedure DCHashNext0
  end interface

  interface DCHashGet
    module procedure DCHashGet0
  end interface

  interface DCHashDelete
    module procedure DCHashDelete0
  end interface

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  interface Put
    module procedure DCHashPut0
  end interface

  interface Number
    module procedure DCHashNumber0
  end interface

  interface PutLine
    module procedure DCHashPutLine0
  end interface

  interface Rewind
    module procedure DCHashRewind0
  end interface

  interface Next
    module procedure DCHashNext0
  end interface

  interface Get
    module procedure DCHashGet0
  end interface

  interface Delete
    module procedure DCHashDelete0
  end interface

contains

  subroutine DCHashPut0(hashv, key, value)
    !
    ! *hashv* のキー *key* に値 *value* を関連付けます.
    !
    implicit none
    type(HASH), intent(inout) :: hashv
    character(*), intent(in) :: key, value
    type(HASH_INTERNAL), pointer :: hash_table_tmp(:) => null()
    integer :: table_size, new_index, i
    logical :: found
    character(STRING) :: search_value
  continue
    call DCHashGet(hashv, key, search_value, found)
    if (.not. found) then
      table_size = DCHashNumber(hashv)
      if (table_size > 0) then
        allocate(hash_table_tmp(table_size))
        hash_table_tmp = hashv % hash_table
        deallocate(hashv % hash_table)
        allocate(hashv % hash_table(table_size + 1))
        hashv % hash_table(1:table_size) = hash_table_tmp(1:table_size)
        deallocate(hash_table_tmp)
        new_index = table_size + 1
      else
        allocate(hashv % hash_table(1))
        new_index = 1
      end if

      hashv % hash_table(new_index) % key = key
      hashv % hash_table(new_index) % value = value
    else
      do i = 1, size(hashv % hash_table)
        if (trim(hashv % hash_table(i) % key) == trim(key)) then
          hashv % hash_table(i) % value = value
        end if
      end do
    end if

  end subroutine DCHashPut0


  function DCHashNumber0(hashv) result(result)
    !
    ! *hashv* のサイズを返します.
    !
    implicit none
    type(HASH), intent(in) :: hashv
    integer :: result
  continue
    if (associated(hashv % hash_table)) then
      result = size(hashv % hash_table)
    else
      result = 0
    end if
  end function DCHashNumber0

  subroutine DCHashRewind0(hashv)
    !
    ! 主にハッシュの内容を取り出すことを目的として, 
    ! *hashv* の巻き戻しを行います. DCHashNext との組み合わせによって
    ! キーと値のリストを取得すること可能です.
    !
    ! 以下のサンプルソースコードを参照ください.
    !
    !    program hash_sample
    !      use dc_type
    !      use dc_hash
    !      type(HASH):: hashv
    !      character(len = STRING):: key, value
    !      logical:: end
    !
    !      call DCHashRewind( hashv ) ! (inout)
    !      do
    !        call DCHashNext( hashv = hashv, &        ! (inout)
    !          & key = key, value = value, end = end) ! (out)
    !        if (end) exit
    !        write(*,*) 'key=' // trim(key) // ', value=' // trim(value)
    !      enddo
    !    end program hash_sample
    !
    implicit none
    type(HASH), intent(inout) :: hashv
  continue
    hashv % search_index = 1
  end subroutine DCHashRewind0

  subroutine DCHashNext0(hashv, key, value, end)
    !

    ! *hashv* の内容を *key* と *value* に返します.
    ! 詳しくは DCHashRewind を参照してください.
    !
    implicit none
    type(HASH), intent(inout) :: hashv
    character(*), intent(out) :: key
    character(*), intent(out), optional :: value
    logical, intent(out) :: end
    integer :: table_size
    character(STRING) :: value_tmp
  continue
    table_size = DCHashNumber(hashv)
    if (table_size < hashv % search_index) then
      key = ''
      value_tmp = ''
      end = .true.
    else
      key = hashv % hash_table(hashv % search_index) % key
      value_tmp = hashv % hash_table(hashv % search_index) % value
      end = .false.
      hashv % search_index = hashv % search_index + 1
    end if
    if (present(value)) then
      value = value_tmp
    end if

  end subroutine DCHashNext0


  subroutine DCHashPutLine0(hashv)
    !
    ! *hashv* の内容を標準出力に表示します.
    !
    use dc_types, only: STRING
    use dc_string, only: Printf, JoinChar
    implicit none
    type(HASH), intent(in) :: hashv
    type(HASH) :: hashv_tmp
    character(len = STRING):: key, value
    logical:: end
  continue
    hashv_tmp = hashv

    call Printf(6, '#<HASH:: ')
    call DCHashRewind(hashv_tmp)
    do
      call DCHashNext(hashv_tmp, key, value, end)
      if (end) exit
      call Printf(6, '         "%c" -> "%c",', &
        & c1=trim(key), c2=trim(value))
    enddo
    call Printf(6, '> ')

  end subroutine DCHashPutLine0


  subroutine DCHashGet0(hashv, key, value, found)
    !
    ! *hashv* のキー *key* に関連する値を *value* に返します.
    ! *key* に関連する値が存在しない場合は *value* に
    ! 空文字を返します.
    !
    ! *found* を与えると, *key* に関連する値が見つからなかった
    ! 場合に .false. を返します.
    !
    use dc_types, only: STRING
    implicit none
    type(HASH), intent(inout) :: hashv
    character(*), intent(in)  :: key
    character(*), intent(out) :: value
    logical, intent(out), optional :: found
    character(STRING) :: search_key, search_value
    logical :: end
  continue
    call DCHashRewind(hashv)
    do
      call DCHashNext(hashv, search_key, search_value, end)
      if (end) then
        value = ''
        if (present(found)) found = .false.
        exit
      end if

      if (trim(search_key) == trim(key)) then
        value = search_value
        if (present(found)) found = .true.
        exit
      end if
    enddo

  end subroutine DCHashGet0

  subroutine DCHashDelete0(hashv, key)
    !
    ! *hashv* のキー *key* およびその関連する値を削除します.
    ! *hashv* 内に *key* が見つからない場合には何もしません.
    !
    ! *key* が省略される場合には *hashv* 内の全てのキーと値を
    ! 削除します.
    !
    implicit none
    type(HASH), intent(inout) :: hashv
    character(*), intent(in), optional :: key
    type(HASH_INTERNAL), pointer :: hash_table_tmp(:) => null()
    integer :: table_size, i, j
    logical :: found
    character(STRING) :: search_value
  continue
    if (present(key)) then
      call DCHashGet(hashv, key, search_value, found)
      table_size = DCHashNumber(hashv)
      if (found .and. table_size > 1) then
        allocate(hash_table_tmp(table_size))
        hash_table_tmp = hashv % hash_table
        deallocate(hashv % hash_table)
        allocate(hashv % hash_table(table_size - 1))
        j = 1
        do i = 1, table_size
          if (trim(hash_table_tmp(i) % key) /= trim(key)) then
            hashv % hash_table(j) % key = hash_table_tmp(i) % key
            hashv % hash_table(j) % value = hash_table_tmp(i) % value
            j = j + 1
          end if
        end do
        
        deallocate(hash_table_tmp)
      elseif (found .and. table_size == 1) then
        deallocate(hashv % hash_table)
      end if
    else
      if (associated(hashv % hash_table)) deallocate(hashv % hash_table)
    end if

  end subroutine DCHashDelete0

end module dc_hash
