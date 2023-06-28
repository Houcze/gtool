!= エラー処理
!
!= Error handling
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: dc_error.f90,v 1.3 2009-10-03 14:53:46 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module dc_error
  !
  != エラー処理用モジュール
  !
  != Error handling module
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! プログラムの部品は必ずエラーの取り扱いを明確に規定すべきものです。
  ! エラーとは当該部品への入力が不適切であるとか、
  ! 期待される動作をすることができないといった事態を指します。
  !
  ! gtool5 ライブラリがユーザに提供する手続
  ! (手続とはサブルーチンまたは関数の総称) はほとんどの場合、
  ! 以下の 2 つの方式のいずれかで呼び出し元にエラーを報告します。
  !
  ! * エラーが発生すると適切なメッセージを表示してプログラム終了
  ! * 論理型の省略できる引数 *err* が与えられた場合は、
  !   エラー時にはそれを <tt>.true.</tt> にします。
  !   err が省略された場合は上に同じ。
  !
  ! これらの処理はすべて *dc_error* モジュールの *StoreError*
  ! サブルーチンで行っています。引用仕様などに関しては *StoreError*
  ! を参照してください。
  !
  !
  ! Error handling about parts of programs should be regulated definitely.
  ! Error means that input to the part of program is invalid, or
  ! expected operation can not be done, etc.
  !
  ! Procedures (procedures is generic name of subroutines and functions)
  ! provided to users by gtool5 library almost report error to invoker
  ! in the following two manner.
  !
  ! * When error occurs, the program display appropriate messages and
  !   aborts.
  ! * If logical optional argument *err* is given, the argument *err* become
  !   <tt>.true.</tt> when error occurs. If *err* is abbreviated,
  !   the operation is same as above.
  !
  ! *StoreError* subroutine in *dc_error* module handle above all operations.
  ! See *StoreError* about the interfase of it.
  !
  !
  !== エラーコード一覧
  !== Error code list
  !
  ! gtool5 ライブラリにコードを追加するプログラマは適切な
  ! エラーコードで *StoreError* を呼び出すようにしなければなりません。
  ! そこで、 新しいエラーコードを定義する必要があるかどうかを
  ! 判定するために、 エラーコードの値と対応するメッセージを
  ! 以下に一覧します。
  ! エラーコードニーモニックを使用するためには、
  ! <b><tt>NF_E</tt></b> で始まる名前については netcdf_f77
  ! モジュールを引用するか include 'netcdf.inc' を行い（後者は推奨しません）、
  ! <b><tt>GT_E</tt></b>, <b><tt>DC_E</tt></b>, <b><tt>HST_E</tt></b>
  ! で始まる名前については dc_error モジュールを引用してください。
  ! また <b><tt>USR_ERRNO</tt></b> 番より小さい値は、
  ! 各々のユーザが適宜エラーコードを
  ! 定義して利用するために空けてあります。
  !
  ! エラーではない状態を表す非エラーコードは *DC_NOERR* です。
  !
  ! エラーコードの数値の欄を設けたのは、新たなエラーコードを
  ! 割り当てる際の指針を示すためです。
  ! ソースコードにはエラーコードをニーモニックで与えるべきであり、
  ! 数値をハードコードすることは厳に慎んで下さい。
  !
  !
  ! Programmers who add codes to gtool5 library must call
  ! *StoreError* with appropriate error code.
  ! And so values of error code and corresponding messages are listed
  ! as follows to figure out if a new error code is needed to declared.
  ! To use error codes mnemonic,
  ! require "netcdf_f77" module or include "netcdf.inc" (deprecated)
  ! about error codes with prefix <b><tt>NF_E</tt></b>, or
  ! require "dc_error" module
  ! about error codes with prefix <b><tt>GT_E</tt></b> and
  ! <b><tt>DC_E</tt></b> and <b><tt>HST_E</tt></b>.
  ! Error codes smaller than <b><tt>USR_ERRNO</tt></b> are saved
  ! as user-defined error codes.
  !
  ! Non error code that indicates normal (error-free) situation is
  ! *DC_NOERR*.
  !
  ! List of numerical values of error codes
  ! issues a guideline about declaration of new error codes.
  ! Give not numerical value but mnemonic of error code to source code.
  !
  !=== 利用しないコード
  !=== Unused codes
  !
  ! 正の整数値はエラーコードとして使用しません。
  !
  ! NetCDF ライブラリは libc のエラーコード errno を返す可能性があり、
  ! errno の数値には移植性がないため、全ての正の整数値は errno
  ! の仕様のために予約されているべきだからです。
  !
  ! Positive integer is not used as error codes.
  !
  ! NetCDF library might return "libc" error code "error".
  ! Numerical value of "errno" is no portable, so positive integer
  ! should be reserved for "errno".
  !
  !=== 非エラーコード
  !=== Non error code
  !
  ! 以下の非エラーコードに関しては dc_error モジュールを引用することで
  ! 利用してください。
  !
  ! Use following non error code by refering this "dc_error" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ]</b>
  !
  ! 0       :: [ <b>DC_NOERR       </b> ]
  !
  !
  !=== netCDF に関するエラーコード
  !=== Error codes for netCDF
  !
  ! 以下のエラーコードに関しては netcdf_f77 モジュールを引用することで
  ! 利用してください。
  !
  ! Use following error codes by refering this "netcdf_f77" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! 0       :: [ <b>NF_NOERR       </b> ]
  !            <b></b> :: No Error  (非エラーコードです)
  !
  ! -33     :: [ <b>NF_EBADID      </b> ]
  !            <b></b> :: Not a netCDF id:
  !
  ! -34     :: [ <b>NF_ENFILE      </b> ]
  !            <b></b> :: Too many netCDF files open:
  !
  ! -35     :: [ <b>NF_EEXIST      </b> ]
  !            <b></b> :: netCDF file exists && NC_NOCLOBBER:
  !
  ! -36     :: [ <b>NF_EINVAL      </b> ]
  !            <b></b> :: Invalid argument:
  !
  ! -37     :: [ <b>NF_EPERM       </b> ]
  !            <b></b> :: Write to read only:
  !
  ! -38     :: [ <b>NF_ENOTINDEFINE</b> ]
  !            <b></b> :: Operation not allowed in data mode
  !
  ! -39     :: [ <b>NF_EINDEFINE   </b> ]
  !            <b></b> :: Operation not allowed in define mode
  !
  ! -40     :: [ <b>NF_EINVALCOORDS</b> ]
  !            <b></b> :: Index exceeds dimension bound
  !
  ! -41     :: [ <b>NF_EMAXDIMS    </b> ]
  !            <b></b> :: NF_MAX_DIMS exceeded
  !
  ! -42     :: [ <b>NF_ENAMEINUSE  </b> ]
  !            <b></b> :: String match to name in use
  !
  ! -43     :: [ <b>NF_ENOTATT     </b> ]
  !            <b></b> :: Attribute not found
  !
  ! -44     :: [ <b>NF_EMAXATTS    </b> ]
  !            <b></b> :: NC_MAX_ATTRS exceeded
  !
  ! -45     :: [ <b>NF_EBADTYPE    </b> ]
  !            <b></b> :: Not a netCDF data type or _FillValue type mismatch
  !
  ! -46     :: [ <b>NF_EBADDIM     </b> ]
  !            <b></b> :: Invalid dimension id or name
  !
  ! -47     :: [ <b>NF_EUNLIMPOS   </b> ]
  !            <b></b> :: NC_UNLIMITED in the wrong index
  !
  ! -48     :: [ <b>NF_EMAXVARS    </b> ]
  !            <b></b> :: NF_MAX_VARS exceeded
  !
  ! -49     :: [ <b>NF_ENOTVAR     </b> ]
  !            <b></b> :: Variable not found
  !
  ! -50     :: [ <b>NF_EGLOBAL     </b> ]
  !            <b></b> :: Action prohibited on NC_GLOBAL varid
  !
  ! -51     :: [ <b>NF_ENOTNC      </b> ]
  !            <b></b> :: Not a netCDF file
  !
  ! -52     :: [ <b>NF_ESTS        </b> ]
  !            <b></b> :: In Fortran, string too short
  !
  ! -53     :: [ <b>NF_EMAXNAME    </b> ]
  !            <b></b> :: NC_MAX_NAME exceeded
  !
  ! -54     :: [ <b>NF_EUNLIMIT    </b> ]
  !            <b></b> :: NC_UNLIMITED size already in use
  !
  ! -55     :: [ <b>NF_ENORECVARS  </b> ]
  !            <b></b> :: NC_rec op when there are no record vars
  !
  ! -56     :: [ <b>NF_ECHAR       </b> ]
  !            <b></b> :: Attempt to convert between text & numbers
  !
  ! -57     :: [ <b>NF_EEDGE       </b> ]
  !            <b></b> :: Edge+start exceeds dimension bound
  !
  ! -58     :: [ <b>NF_ESTRIDE     </b> ]
  !            <b></b> :: Illegal stride
  !
  ! -59     :: [ <b>NF_EBADNAME    </b> ]
  !            <b></b> :: Attribute or variable name contains illegal characters
  !
  ! -60     :: [ <b>NF_ERANGE      </b> ]
  !            <b></b> :: Numeric conversion not representable
  !
  ! -61     :: [ <b>NF_ENOMEM      </b> ]
  !            <b></b> :: Memory allocation (malloc) failure
  !
  ! -62     :: [ <b>NF_EVARSIZE      </b> ]
  !            <b></b> :: One or more variable sizes violate format constraints
  !
  ! -63     :: [ <b>NF_EDIMSIZE      </b> ]
  !            <b></b> :: Invalid dimension size
  !
  ! -64     :: [ <b>NF_ETRUNC      </b> ]
  !            <b></b> :: File likely truncated or possibly corrupted
  !
  ! -62〜-99:: <b>               </b>
  !            <b></b> :: (将来の netCDF の拡張のための gtool5 の予約領域.
  !                       Reserved area for future extensions of netCDF)
  !
  !=== gtool5 のデータ構造 (gtdata) に関するエラーコード
  !=== Error codes for data structure of gtool5 (gtdata)
  !
  ! 以下のエラーコードに関しては dc_error モジュールを引用することで
  ! 利用してください。
  !
  ! Use following error codes by refering this "dc_error" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -100    :: [ <b>GT_EFAKE           </b> ]
  !            <b></b> :: function not implemented
  !
  ! -101    :: [ <b>GT_ENOMOREDIMS     </b> ]
  !            <b></b> :: dimension number %d is out of range
  !
  ! -102    :: [ <b>GT_EDIMNODIM       </b> ]
  !            <b></b> :: dimension variable has no dimension
  !
  ! -103    :: [ <b>GT_EDIMMULTIDIM    </b> ]
  !            <b></b> :: dimension variable has many dimensions
  !
  ! -104    :: [ <b>GT_EDIMOTHERDIM    </b> ]
  !            <b></b> :: dimension variable has another dimension
  !
  ! -105    :: [ <b>GT_EBADDIMNAME     </b> ]
  !            <b></b> :: <i>cause_c</i>: unknown dimension name
  !
  ! -106    :: [ <b>GT_ENOTVAR         </b> ]
  !            <b></b> :: variable not opened
  !
  ! -107    :: [ <b>GT_ENOMEM          </b> ]
  !            <b></b> :: allocate/deallocate error
  !
  ! -108    :: [ <b>GT_EOTHERFILE      </b> ]
  !            <b></b> :: specified dimensional variable not on the same file
  !
  ! -109    :: [ <b>GT_EARGSIZEMISMATCH</b> ]
  !            <b></b> :: arguments (<i>cause_c</i>) array size mismatch
  !
  ! -110    :: [ <b>GT_ENOMATCHDIM     </b> ]
  !            <b></b> :: dimension matching failed
  !
  ! -111    :: [ <b>GT_ELIMITED        </b> ]
  !            <b></b> :: variable already limited
  !
  ! -112    :: [ <b>GT_EBADVAR         </b> ]
  !            <b></b> :: variable type not supported
  !
  ! -113    :: [ <b>GT_ECHARSHORT      </b> ]
  !            <b></b> :: character length not enough
  !
  ! -114    :: [ <b>GT_ENOUNLIMITDIM   </b> ]
  !            <b></b> :: NC_UNLIMITED dimension is not found
  !
  ! -115    :: [ <b>GT_EBADATTRNAME    </b> ]
  !            <b></b> :: invalid attribute name
  !
  ! -116    :: [ <b>GT_EBADHISTORY     </b> ]
  !            <b></b> :: invalid GT_HISTORY variable
  !
  ! -117    :: [ <b>GT_EBADALLOCATESIZE</b> ]
  !            <b></b> :: invalid allocated size
  !
  ! -118    :: [ <b>GT_ERANKMISMATCH</b> ]
  !            <b></b> :: rank of data and argument is mismatch (<i>cause_c</i>)
  ! -119    :: [ <b>GT_ENOTURL     </b> ]
  !            <b></b> :: URL (<i>cause_c</i>) is not found
  !
  ! -120    :: [ <b>GT_EBADGT4COMMAGRAPHY     </b> ]
  !            <b></b> :: (<i>cause_c</i>) is not gtool4 comma-graphy (ex. "time=100.0,x=10:20,y=^1:^5")
  !
  !
  ! 〜-299  :: <b>                     </b>
  !            <b></b> :: (将来の gtdata 層の拡張のための予約.
  !                       Reserved area for future extensions of gtdata layer)
  !
  !=== GrADS データ入出力に関するエラーコード
  !=== Error codes for GrADS data I/O
  !
  ! 以下のエラーコードに関しては dc_error モジュールを引用することで
  ! 利用してください。
  !
  ! Use following error codes by refering this "dc_error" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -300    :: [ <b>GR_ENOTGR          </b> ]
  !            <b></b> :: invalid GrADS file
  !
  ! 〜-399  :: <b>                     </b>
  !            <b></b> :: (将来の GrADS data 入出力層の拡張のための予約.
  !                       Reserved area for future extensions of GrADS data I/O layer)
  !
  !=== DC ユーティリティ用エラーコード
  !=== Error codes for DC utilities
  !
  ! 以下のエラーコードに関しては dc_error モジュールを引用することで
  ! 利用してください。
  !
  ! Use following error codes by refering this "dc_error" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -400    :: [ <b>DC_ENOTINIT          </b> ]
  !            <b></b> :: object (<i>cause_c</i>) is not initialized
  !
  ! -401    :: [ <b>DC_EALREADYINIT      </b> ]
  !            <b></b> :: object (<i>cause_c</i>) is already initialized
  !
  ! -402    :: [ <b>DC_EBADUNIT</b> ]
  !            <b></b> :: unit (<i>cause_c</i>) is invalid
  !
  ! -403    :: [ <b>DC_EBADCALTYPE</b> ]
  !            <b></b> :: calendar type (<i>cause_i</i>) is invalid
  !
  ! -404    :: [ <b>DC_EBADTIMEZONE</b> ]
  !            <b></b> :: time zone (<i>cause_c</i>) is invalid
  !
  ! -405    :: [ <b>DC_EFILENAMEEMPTY</b> ]
  !            <b></b> :: filename is empty
  !
  ! -406    :: [ <b>DC_EBADFILEOPMODE</b> ]
  !            <b></b> :: file open mode (<i>cause_c</i>) is invalid
  !
  ! -407    :: [ <b>DC_ENOUNITNUM</b> ]
  !            <b></b> :: available unit number is not found within (<i>cause_c</i>)
  !
  ! -408    :: [ <b>DC_ENOFILEEXIST</b> ]
  !            <b></b> :: file (<i>cause_c</i>) is not found
  !
  ! -409    :: [ <b>DC_ENOFILEREAD</b> ]
  !            <b></b> :: file (<i>cause_c</i>) is not readable
  !
  ! -410    :: [ <b>DC_ENOFILEWRITE</b> ]
  !            <b></b> :: file (<i>cause_c</i>) is not writable
  !
  ! -411    :: [ <b>DC_ENEGATIVE          </b> ]
  !            <b></b> :: negative value is invalid for (<i>cause_c</i>)
  !
  ! -412    :: [ <b>DC_EARGLACK          </b> ]
  !            <b></b> :: lack of arguments (<i>cause_c</i>)
  !
  ! -413    :: [ <b>DC_ENOASSOC          </b> ]
  !            <b></b> :: argument (<i>cause_c</i>) is not associated
  !
  ! -414    :: [ <b>DC_ENOENTRY          </b> ]
  !            <b></b> :: entry of (<i>cause_c</i>) is not found
  !
  ! -415    :: [ <b>DC_ENODIMTIME</b> ]
  !            <b></b> :: dimensional time can not be converted into nondimensional time
  !
  ! -416    :: [ <b>DC_EDIMTIME</b> ]
  !            <b></b> :: nondimensional time can not be converted into dimensional time
  !
  ! -417    :: [ <b>DC_ETOOLARGETIME</b> ]
  !            <b></b> :: number is too large for time
  !
  ! -418    :: [ <b>DC_EBADDATE</b> ]
  !            <b></b> :: invalid expression of date
  !
  ! -419    :: [ <b>DC_EINCONSISTCALDATE</b> ]
  !            <b></b> :: calendar and date are inconsistent
  !
  !
  ! -420〜-499  :: <b>                     </b>
  !                <b></b> :: (将来の DC ユーティリティの拡張のための予約.
  !                           Reserved area for future extensions of DC utilities)
  !
  !=== データ入出力層 (gtool_history, gtool_history_nmlinfo, gtool_historyauto) エラーコード
  !=== Error codes for data I/O layer (gtool_history, gtool_history_nmlinfo, gtool_historyauto)
  !
  ! 以下のエラーコードに関しては dc_error モジュールを引用することで
  ! 利用してください。
  !
  ! Use following error codes by refering this "dc_error" module.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -500    :: [ <b>HST_ENOTINDEFINE</b> ]
  !            <b></b> :: operation (<i>cause_c</i>) not allowed in data mode
  !
  ! -501    :: [ <b>HST_EINDEFINE   </b> ]
  !            <b></b> :: operation (<i>cause_c</i>) not allowed in define mode
  !
  ! -502    :: [ <b>HST_EINTFILE   </b> ]
  !            <b></b> :: different intervals are applied to a file (<i>cause_c</i>)
  ! -503    :: [ <b>HST_EBADNAME   </b> ]
  !            <b></b> :: name (<i>cause_c</i>) is invalid
  !
  ! -504    :: [ <b>HST_ENOTTERMGTHIST   </b> ]
  !            <b></b> :: GT_HISTORY correspond to (<i>cause_c</i>) is not terminated
  !
  ! -505    :: [ <b>HST_ENODEPENDTIME   </b> ]
  !            <b></b> :: (<i>cause_c</i>) does not depend on time
  !
  ! -506    :: [ <b>HST_EBADVARNAME   </b> ]
  !            <b></b> :: variable name (<i>cause_c</i>) is invalid
  !
  ! -507    :: [ <b>HST_ENOTIMEDIM   </b> ]
  !            <b></b> :: time dimension is not found
  !
  ! -508    :: [ <b>HST_ENOAXISNAME   </b> ]
  !            <b></b> :: axis or weight (<i>cause_c</i>) is not found
  !
  ! -509    :: [ <b>HST_EVARINUSE   </b> ]
  !            <b></b> :: variable name (<i>cause_c</i>) is already used
  !
  ! -510    :: [ <b>HST_EALREADYREGVARFIX   </b> ]
  !            <b></b> :: already register of variables is fixed by (<i>cause_c</i>)
  !
  ! -511    :: [ <b>HST_EBADSLICE   </b> ]
  !            <b></b> :: slice options are invalid (<i>cause_c</i>)
  !
  ! -512    :: [ <b>HST_EBADNEWFILEINT   </b> ]
  !            <b></b> :: invalid newfile interval (<i>cause_c</i>)
  !
  ! -513    :: [ <b>HST_EMAXDIMSDEPENDED   </b> ]
  !            <b></b> :: variable (<i>cause_c</i>) depends on (<i>cause_i</i>) dimensions
  !
  ! -514    :: [ <b>HST_EINDIVISIBLE   </b> ]
  !            <b></b> :: (<i>cause_c</i>) can not be divided
  !
  ! -515    :: [ <b>HST_EBADTERMINUS   </b> ]
  !            <b></b> :: terminus options are invalid (<i>cause_c</i>)

  ! -516    :: [ <b>HST_EBADORIGIN   </b> ]
  !            <b></b> :: origin options are invalid (<i>cause_c</i>)
  !
  ! -517    :: [ <b>HST_EMPINOAXISDATA   </b> ]
  !            <b></b> :: data of axis (<i>cause_c</i>) for MPI is lack
  !
  !=== gtool5 の将来の拡張のために予約してあるエラーコード
  !=== Reserved error codes for future extensions of gtool5
  !
  ! 以下のエラーコードは今後の拡張も考えて予約してある部分です。
  !
  ! Following error codes are reserved for future extensions.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -600〜-999  :: <b>                     </b>
  !                <b></b> :: (将来の gtool5 の拡張のための予約.
  !                           Reserved area for future extensions of gtool5)
  !
  !=== ユーザ定義用エラーコード
  !=== User-defined error codes
  !
  ! -1000 よりも小さいエラーコードは、
  ! gtool5 の上位のプログラムが利用するエラーコードとして空けてあります。
  !
  ! Error codes smaller than -1000 are saved for as user-defined error codes
  ! used by upper programs.
  !
  ! <b>数値. Number</b> :: <b>  [ ニーモニック. Mnemonic ] エラーメッセージ. Error message</b>
  !
  ! -1000〜 :: [ <b>USR_ERRNO           </b> ]
  !            <b></b> :: <i>cause_c</i> (<i>cause_i</i>)
  !

  use netcdf, only: NF90_ENOTVAR, NF90_EINVAL
  use dc_types, only: STRING
  implicit none
  private
  public :: NF90_ENOTVAR, NF90_EINVAL

  ! エラー等を保持

  integer, public, parameter   :: DC_NOERR  = 0
  integer, private, save       :: errno     = DC_NOERR
  integer, private, save       :: cause_int = DC_NOERR
  logical, private, save       :: cause_int_valid = .false.
  character(STRING), private, save :: cause_string = ""
  character(STRING), private, save :: cause_location = ""

  ! 正のエラー番号は libc システムエラーメッセージのために
  ! あけてある。システム依存性が大きく、非常に大きな数値も
  ! 用いられるので空き領域を確保するのは困難である。
  !
  ! 負のエラー番号は netCDF が使っている。少々の拡張も見込んで、
  ! -99 までは使わないで置く。

  integer, parameter, public:: GT_EFAKE = -100

  !
  ! -101 以下: データ構造のエラー
  !
  integer, parameter, public:: GT_ENOMOREDIMS        = -101
  integer, parameter, public:: GT_EDIMNODIM          = -102
  integer, parameter, public:: GT_EDIMMULTIDIM       = -103
  integer, parameter, public:: GT_EDIMOTHERDIM       = -104
  integer, parameter, public:: GT_EBADDIMNAME        = -105
  integer, parameter, public:: GT_ENOTVAR            = -106
  integer, parameter, public:: GT_ENOMEM             = -107
  integer, parameter, public:: GT_EOTHERFILE         = -108
  integer, parameter, public:: GT_EARGSIZEMISMATCH   = -109
  integer, parameter, public:: GT_ENOMATCHDIM        = -110
  integer, parameter, public:: GT_ELIMITED           = -111
  integer, parameter, public:: GT_EBADVAR            = -112
  integer, parameter, public:: GT_ECHARSHORT         = -113
  integer, parameter, public:: GT_ENOUNLIMITDIM      = -114
  integer, parameter, public:: GT_EBADATTRNAME       = -115
  integer, parameter, public:: GT_EBADHISTORY        = -116
  integer, parameter, public:: GT_EBADALLOCATESIZE   = -117
  integer, parameter, public:: GT_ERANKMISMATCH      = -118
  integer, parameter, public:: GT_ENOTURL            = -119
  integer, parameter, public:: GT_EBADGT4COMMAGRAPHY = -120

  !
  ! -300 以下: GrADS 入出力のエラー
  !
  integer, parameter, public:: GR_ENOTGR = -300

  !
  ! -400 以下: dc ユーティリティのエラー
  !
  integer, parameter, public:: DC_ENOTINIT       = -400
  integer, parameter, public:: DC_EALREADYINIT   = -401
  integer, parameter, public:: DC_EBADUNIT       = -402
  integer, parameter, public:: DC_EBADCALTYPE    = -403
  integer, parameter, public:: DC_EBADTIMEZONE   = -404
  integer, parameter, public:: DC_EFILENAMEEMPTY = -405
  integer, parameter, public:: DC_EBADFILEOPMODE = -406
  integer, parameter, public:: DC_ENOUNITNUM     = -407
  integer, parameter, public:: DC_ENOFILEEXIST   = -408
  integer, parameter, public:: DC_ENOFILEREAD    = -409
  integer, parameter, public:: DC_ENOFILEWRITE   = -410
  integer, parameter, public:: DC_ENEGATIVE      = -411
  integer, parameter, public:: DC_EARGLACK       = -412
  integer, parameter, public:: DC_ENOASSOC       = -413
  integer, parameter, public:: DC_ENOENTRY       = -414
  integer, parameter, public:: DC_ENODIMTIME     = -415
  integer, parameter, public:: DC_EDIMTIME       = -416
  integer, parameter, public:: DC_ETOOLARGETIME  = -417
  integer, parameter, public:: DC_EBADDATE       = -418
  integer, parameter, public:: DC_EINCONSISTCALDATE = -419

  !
  ! -500 以下: データ入出力層のエラー
  !
  integer, parameter, public:: HST_ENOTINDEFINE      = -500
  integer, parameter, public:: HST_EINDEFINE         = -501
  integer, parameter, public:: HST_EINTFILE          = -502
  integer, parameter, public:: HST_EBADNAME          = -503
  integer, parameter, public:: HST_ENOTTERMGTHIST    = -504
  integer, parameter, public:: HST_ENODEPENDTIME     = -505
  integer, parameter, public:: HST_EBADVARNAME       = -506
  integer, parameter, public:: HST_ENOTIMEDIM        = -507
  integer, parameter, public:: HST_ENOAXISNAME       = -508
  integer, parameter, public:: HST_EVARINUSE         = -509
  integer, parameter, public:: HST_EALREADYREGVARFIX = -510
  integer, parameter, public:: HST_EBADSLICE         = -511
  integer, parameter, public:: HST_EBADNEWFILEINT    = -512
  integer, parameter, public:: HST_EMAXDIMSDEPENDED  = -513
  integer, parameter, public:: HST_EINDIVISIBLE      = -514
  integer, parameter, public:: HST_EBADTERMINUS      = -515
  integer, parameter, public:: HST_EBADORIGIN        = -516
  integer, parameter, public:: HST_EMPINOAXISDATA    = -517


  !
  ! -1000 以下: ユーザー定義
  !
  integer, parameter, public:: USR_ERRNO = -1000

  public:: StoreError, DumpError, GetErrorMessage, ErrorCode
  !
  ! === 手続引用仕様 ===
  !
  ! いずれ差し替えられるように外部関数にしておく。

  interface
    subroutine DumpError()
    end subroutine DumpError
  end interface

contains

  integer function ErrorCode() result(result)
    !
    ! 現在設定されているエラーコードを返します。
    !
    ! Return an error code specified currently.
    !
    result = errno
  end function ErrorCode

  subroutine GetErrorMessage(msg)
    !
    ! 現在設定されているエラーコードから対応するメッセージを返します。
    !
    ! Return messages corresponding to an error code specified currently.
    !
    use netcdf, only: nf90_strerror
    character(len = *), intent(out):: msg
    character(len = STRING):: message
    character(len = 20):: errno_c
    character(len = 20):: cause_int_c
  continue
    select case(errno)
    case(GT_EFAKE)
      msg = ' function not implemented'
      !
      ! -101 以下: データ構造のエラー
      ! -101 or less: Error of data structure
      !
    case(GT_ENOMOREDIMS)
      write(message, "(': dimension number', i4, ' is out of range')") cause_int
      msg = trim(message)
    case(GT_EBADDIMNAME)
      msg = '(' // trim(cause_string) // '): unknown dimension name'
    case(GT_ENOTVAR)
      msg = ' variable not opened'
    case(GT_ENOMEM)
      msg = ' allocate/deallocate error'
    case(GT_EDIMNODIM)
      msg = ' dimension variable has no dimension'
    case(GT_EDIMMULTIDIM)
      msg = ' dimension variable has many dimensions'
    case(GT_EDIMOTHERDIM)
      msg = ' dimension variable has another dimension'
    case(GT_EOTHERFILE)
      msg = ' specified dimensional variable not on the same file'
    case(GT_EARGSIZEMISMATCH)
      msg = ' arguments (' // trim(cause_string) //') array size mismatch'
    case(GT_ENOMATCHDIM)
      msg = ' dimension matching failed'
    case(GT_ELIMITED)
      msg = ' variable already limited'
    case(GT_EBADVAR)
      msg = ' variable type not supported'
    case(GT_ECHARSHORT)
      msg = ' character length not enough'
    case(GT_ENOUNLIMITDIM)
      msg = ' NC_UNLIMITED dimension is not found'
    case(GT_EBADATTRNAME)
      msg = ' invalid attribute name'
    case(GT_EBADALLOCATESIZE)
      msg = ' invalid allocated size'
    case(GT_ERANKMISMATCH)
      msg = ' rank of data and argument are mismatch (' // trim(cause_string) // ')'
    case(GT_ENOTURL)
      msg = ' URL (' // trim(cause_string) // ') is not found'
    case(GT_EBADGT4COMMAGRAPHY)
      msg = ' (' // trim(cause_string) // ') is not gtool4 comma-graphy (ex. "time=100.0,x=10:20,y=^1:^5")'
      !
      ! -300 以下: GrADS 入出力のエラー
      ! -300 or less: Error of GrADS I/O
      !
    case(GR_ENOTGR)
      msg = ' invalid GrADS file'
      !
      ! -400 以下: DC ユーティリティのエラー
      ! -400 or less: Error of DC utilities
      !
    case(DC_ENOTINIT)
      msg = ' object (' // trim(cause_string) // ') is not initialized'
    case(DC_EALREADYINIT)
      msg = ' object (' // trim(cause_string) // ') is already initialized'
    case(DC_EBADUNIT)
      msg = ' unit (' // trim(cause_string) // ') is invalid'
    case(DC_EBADCALTYPE)
      write(message, '(" calendar type (", i4, ") is invalid")') cause_int
      msg = trim(message)
    case(DC_EBADTIMEZONE)
      msg = ' time zone (' // trim(cause_string) // ') is invalid'
    case(DC_EFILENAMEEMPTY)
      msg = ' filename is empty'
    case(DC_EBADFILEOPMODE)
      msg = ' file open mode (' // trim(cause_string) // ') is invalid'
    case(DC_ENOUNITNUM)
      msg = ' available unit number is not found within (' // trim(cause_string) // ')'
    case(DC_ENOFILEEXIST)
      msg = ' file (' // trim(cause_string) // ') is not found'
    case(DC_ENOFILEREAD)
      msg = ' file (' // trim(cause_string) // ') is not readable'
    case(DC_ENOFILEWRITE)
      msg = ' file (' // trim(cause_string) // ') is not writable'
    case(DC_ENEGATIVE)
      msg = ' negative value is invalid for (' // trim(cause_string) // ')'
    case(DC_EARGLACK)
      msg = ' lack of arguments (' // trim(cause_string) // ')'
    case(DC_ENOASSOC)
      msg = ' argument (' // trim(cause_string) // ') is not associated'
    case(DC_ENOENTRY)
      msg = ' entry of (' // trim(cause_string) // ') is not found'
    case(DC_ENODIMTIME)
      msg = ' dimensional time can not be converted into nondimensional time'
    case(DC_EDIMTIME)
      msg = ' nondimensional time can not be converted into dimensional time'
    case(DC_ETOOLARGETIME)
      msg = ' number is too large for time'
    case(DC_EBADDATE)
      msg = ' invalid expression of date'
    case(DC_EINCONSISTCALDATE)
      msg = ' calendar and date are inconsistent'
      !
      ! -500 以下: データ入出力層のエラー
      ! -500 or less: Error of data I/O layer
      !
    case(HST_ENOTINDEFINE)
      msg = ' operation (' // trim(cause_string) // ') not allowed in data mode'
    case(HST_EINDEFINE)
      msg = ' operation (' // trim(cause_string) // ') not allowed in define mode'
    case(HST_EINTFILE)
      msg = ' different intervals are applied to a file (' // trim(cause_string) // ')'
    case(HST_EBADNAME)
      msg = ' name (' // trim(cause_string) // ') is invalid'
    case(HST_ENOTTERMGTHIST)
      msg = ' GT_HISTORY correspond to (' // trim(cause_string) // ') is not terminated'
    case(HST_ENODEPENDTIME)
      msg = ' (' // trim(cause_string) // ') does not depend on time'
    case(HST_EBADVARNAME)
      msg = ' variable name (' // trim(cause_string) // ') is invalid'
    case(HST_ENOTIMEDIM)
      msg = ' time dimension is not found'
    case(HST_ENOAXISNAME)
      msg = ' axis or weight (' // trim(cause_string) // ') is not found'
    case(HST_EVARINUSE)
      msg = ' variable name (' // trim(cause_string) // ') is already used'
    case(HST_EALREADYREGVARFIX)
      msg = ' already register of variables is fixed by (' // trim(cause_string) // ')'
    case(HST_EBADSLICE)
      msg = ' slice options are invalid (' // trim(cause_string) // ')'
    case(HST_EBADNEWFILEINT)
      msg = ' invalid newfile interval (' // trim(cause_string) // ')'
    case(HST_EMAXDIMSDEPENDED)
      write(message, '("(", i4, ")")') cause_int
      msg = trim(message)
      msg = ' variable (' // trim(cause_string) // ') depends on ' // trim(message) // ' dimensions'
    case(HST_EINDIVISIBLE)
      msg = ' (' // trim(cause_string) // ') can not be divided'
    case(HST_EBADTERMINUS)
      msg = ' terminus options are invalid (' // trim(cause_string) // ')'
    case(HST_EBADORIGIN)
      msg = ' origin options are invalid (' // trim(cause_string) // ')'
    case(HST_EMPINOAXISDATA)
      msg = ' data of axis (' // trim(cause_string) // ') for MPI is lack'

      !
      !
      ! -1000 以下: ユーザー定義
      ! -1000 or less: User-defined error
      !
    case(:USR_ERRNO)
      if (len(trim(adjustl(cause_string))) < 1) then
        cause_string = 'Unknown error'
      end if
      if (cause_int_valid) then
        write(cause_int_c, "(i8)") cause_int
        msg = trim(cause_string) // ' (' // trim(adjustl(cause_int_c)) // ')'
      else
        msg = trim(cause_string)
      end if
    case default
      goto 999
    end select
    write(errno_c, "(i8)") errno
    msg =  '*** ERROR (Code ' // trim(adjustl(errno_c))  // &
      & ') [' // trim(cause_location) // '] ***  ' // &
      & trim(msg)
    return

999 continue
    if (len(cause_string) > 0) then
      message = nf90_strerror(errno)
      write(errno_c, "(i8)") errno
      msg =  '*** ERROR (Code ' // trim(adjustl(errno_c)) // &
        & ') [' // trim(cause_location)             // &
        & '('   // trim(cause_string) // ')] ***  ' // &
        & trim(message)
    else if (cause_int_valid) then
      message = nf90_strerror(errno)
      write(errno_c, "(i8)") errno
      write(cause_int_c, "(i8)") cause_int
      msg =  '*** ERROR (Code ' // trim(adjustl(errno_c)) // &
        & ') [' // trim(cause_location)             // &
        & '('   // trim(adjustl(cause_int_c)) // ')] ***  ' // &
        & trim(message)
    else
      message = nf90_strerror(errno)
      write(errno_c, "(i8)") errno
      msg =  '*** ERROR (Code ' // trim(adjustl(errno_c))  // &
        & ') [' // trim(cause_location) // '] ***  ' // &
        & trim(message)
    endif
  end subroutine GetErrorMessage

  subroutine StoreError(number, where, err, cause_c, cause_i)
    !
    !== 典型的ライブラリ手続のために作られたエラー処理サブルーチン
    !== Error handling subroutine for typical procedures of library
    !
    ! 必要な引数は2つであり、第1引数 *number* には整数型のエラーコード、
    ! 第2引数 *where* には文字型でエラーの発生した手続名を与えます。
    ! デフォルトでは以下の形式の文字列が標準出力に表示されてプログラム
    ! は終了します。 エラーメッセージ error_message
    ! はエラーコードから自動的に決まります。
    ! 対応表がエラーコード一覧にあるので参照してください。
    !
    ! Number of necessary arguments is two. Give integer error code
    ! to first argument *number*, and procedure name where the error
    ! occurs to second argument *where*. By default, like a following
    ! string is displayed to standard output, and the program aborts
    ! Error message <error_message> is determined by error code automatically.
    ! See error code list.
    !
    !
    !     *** ERROR (Code number) [where] ***  error_message
    !
    !     *** ERROR (Code number) [where(cause_c)] ***  error_message
    !
    ! なお、gtool5 のライブラリ外からユーザがエラー処理用ツール
    ! として StoreError を用いることを想定し、<b><tt>USR_ERRNO</tt></b>
    ! 番より小さい
    ! エラーコードは空けてあります。<b><tt>USR_ERRNO</tt></b>
    ! より小さい値をエラーコードに与えると,
    ! StoreError は以下の形式の文字列を標準出力に出力してプログラムを
    ! 終了させます。より安易に使えるメッセージ出力およびエラー発生の
    ! ためのモジュールとして *dc_message* も用意してあるので
    ! そちらも参照してください。
    !
    ! In addition, for usage that users call StoreError as an error
    ! handling tool from the outside of gtool5 library,
    ! error codes smaller than <b><tt>USR_ERRNO</tt></b> is saved.
    ! When error codes smaller than <b><tt>USR_ERRNO</tt></b> is given,
    ! StoreError displays like a following string to standard output,
    ! and stops the program.
    ! *dc_message* module is prepared too. This module can be used
    ! more easily for message output and rise of error.
    !
    !
    !     *** ERROR (Code number) [where] ***  cause_c
    !
    !     *** ERROR (Code number) [where] ***  cause_c (cause_i)
    !
    !--
    !== 開発者向け解説
    !
    ! エラー番号 number を errno に格納する。同時に付随的情報
    ! where, cause_i を cause_location, cause_string,
    ! cause_int に格納する。
    ! err が与えられている場合、err は number が DC_NOERR の場合だけ偽になる。
    ! number が DC_NOERR ならば即復帰する。
    ! err が与えられていなければエラーメッセージを装置 * に出力して
    ! プログラムを終了する。
    !++

    integer,            intent(in)            :: number
                              ! エラーコード。
                              ! Error code
    character(len = *), intent(in)            :: where
                              ! エラー発生個所。
                              ! Place where error occurs
    logical,            intent(out), optional :: err
                              ! 例外処理用フラグ。
                              ! デフォルトでは、*number* に非エラーコード
                              ! 以外の値が与えられた場合、エラーメッセージを
                              ! 表示してプログラムは強制終了します。
                              ! 引数 *err* が与えられる場合、
                              ! プログラムは強制終了せず、代わりに
                              ! *err* に .true. が代入されます。
                              !
                              ! Exception handling flag.
                              ! By default, when error code (excluding
                              ! non error code) is given to *number*,
                              ! the program display error message and aborts.
                              ! If this *err* argument is given,
                              ! .true. is substituted to *err* and
                              ! the program does not abort.
    character(len = *), intent(in),  optional :: cause_c
                              ! 文字型メッセージ。
                              ! Character message
    integer,            intent(in),  optional :: cause_i
                              ! 整数型メッセージ。
                              ! Integer message
  continue
    if (present(err)) then
      err = (number /= DC_NOERR)
      return
    endif
    if (number == DC_NOERR) return
    errno = number
    cause_location = where
    if (present(cause_c)) then
      cause_string = trim(cause_c)
    else
      cause_string = ""
    endif
    if (present(cause_i)) then
      cause_int = cause_i
      cause_int_valid = .true.
    else
      cause_int_valid = .false.
    end if
    call DumpError
  end subroutine StoreError

end module dc_error

subroutine DumpError()
  !
  ! GetErrorMessage からエラーメッセージを取得後、
  ! それを sysdep#AbortProgram に渡してプログラムを終了させます。
  !
  ! Get error messages from "GetErrorMessage", and put the messages
  ! to sysdep#AbortProgram, and stop the program.
  !
  use dc_types, only: STRING
  use dc_error, only: GetErrorMessage
  use sysdep, only: AbortProgram
  character(len = STRING):: message
continue
  call GetErrorMessage(message)
  call AbortProgram(message)
end subroutine DumpError
