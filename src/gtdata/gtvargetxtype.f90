! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

! 外部型表現が存在する処理系に関しては型を示す文字列を、
! そうでない処理系に関しては空文字列を返す。

subroutine GTVarGetXtype(var, xtype)
    use gtdata_types, only: GT_VARIABLE
    use dc_string, only: VSTRING
    use gtdata_generic, only: get_attr
    implicit none
    type(GT_VARIABLE), intent(in):: var
    type(VSTRING), intent(out):: xtype
    call get_attr(var, "__xtype__", xtype, default="")
end subroutine
