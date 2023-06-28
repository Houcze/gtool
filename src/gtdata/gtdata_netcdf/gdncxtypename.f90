! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

subroutine GDNcXTypeName(ixtype, xtype)
  use netcdf, only: &
    & NF90_BYTE,  &
    & NF90_CHAR,  &
    & NF90_SHORT, &
    & NF90_INT,   &
    & NF90_FLOAT, &
    & NF90_DOUBLE
  implicit none
  integer, intent(in):: ixtype
  character(*), intent(out):: xtype
  select case(ixtype)
  case(0)
    xtype = "error"
  case(NF90_CHAR)
    xtype = "char"
  case(NF90_BYTE)
    xtype = "byte"
  case(NF90_SHORT)
    xtype = "short"
  case(NF90_INT)
    xtype = "int"
  case(NF90_FLOAT)
    xtype = "float"
  case(NF90_DOUBLE)
    xtype = "double"
  case default
    xtype = ""
  end select
end subroutine GDNcXTypeName
