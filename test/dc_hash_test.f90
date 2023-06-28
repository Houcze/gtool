program dc_hash_test
  use dc_types, only: STRING
  use dc_test, only: AssertEqual
  use dc_string, only: StoA
  use dc_hash, only: HASH, DCHashPut, DCHashPutLine, &
    & DCHashGet, DCHashNumber, DCHashDelete
  use dc_trace, only: SetDebug
  implicit none
  type(HASH) :: hashv
  character(STRING) :: value
  logical :: found
continue

  call SetDebug

  call DCHashPut(hashv, 'hoge', 'hero')
  call DCHashPut(hashv, 'key', 'value')
  call DCHashPut(hashv, 'hoge  ', 'foo')
  call DCHashPut(hashv, 'local', 'demo')
  call DCHashGet(hashv, 'hoge', value)
  call AssertEqual('Put, Get test 1', 'foo', value)

  call DCHashGet(hashv, 'not+found', value, found)
  call AssertEqual('Put, Get test 2', '', value)
  call AssertEqual('Put, Get test 3', .false., found)

  call DCHashGet(hashv, 'local', value, found)
  call AssertEqual('Put, Get test 4', 'demo', value)
  call AssertEqual('Put, Get test 5', .true., found)

  call AssertEqual('Number test', 3, DCHashNumber(hashv))

  call DCHashDelete(hashv, 'hogehoge')
  call AssertEqual('Delete test 1', 3, DCHashNumber(hashv))
  
  call DCHashDelete(hashv, 'hoge')
  call AssertEqual('Delete test 2', 2, DCHashNumber(hashv))
  
  call DCHashDelete(hashv, 'key')
  call AssertEqual('Delete test 3', 1, DCHashNumber(hashv))
  
  call DCHashDelete(hashv, 'local')
  call AssertEqual('Delete test 4', 0, DCHashNumber(hashv))
  
  call DCHashPut(hashv, 'new', 'val1')
!  call DCHashPutLine(hashv)

  call DCHashDelete(hashv)

end program dc_hash_test
