#!/bin/sh
#
#= Fortran 90/95 dependency lister
#
AUTHORS="Yasuhiro MORIKAWA"
VERSION='$Id: f90depend.sh,v 1.1 2009-03-25 08:17:36 morikawa Exp $'
COPYRIGHT='Copyright (C) GFD Dennou Club, 2008. All rights reserved.'
#
# 引数として受け取る Fortran 90/95 ファイル内の use 文および
# module 文を解析し, 依存関係を記したファイルを Makefile 形式
# で標準出力に書き出す.
#
# Ruby, Perl がインストールされていない環境での
# f90depend.rb, f90depend.pl の代替ツール
# (ソースの検索が Ruby に比べて甘い)
#

######################################################################
# Settings

# End Settings
######################################################################

f90dependhelp(){
    cat <<EOF

  Usage: 
    `basename $0` [OPTIONS] f90_file [f90_file [f90_file] .. ]

  Options
       -v:       verbose mode
       -q:       quiet mode
       -h:       show help messages

EOF

  f90dependprintversion
}

f90dependprintversion(){
    cat <<EOF
  `basename $0` Version ${VERSION}

  ${AUTHORS}
    ${COPYRIGHT}

EOF
}

#
# 引数取得
#
declare -a ArgvFiles
declare -a Files
declare -i Num=0
declare -i FNum=0
verbose=
while [ $# -gt 0 ]; do
    case $1 in
	"-v")
	    verbose=1
	    shift
	    ;;
	"-q")
	    verbose=
	    shift
	    ;;
	"-h")
	    f90dependhelp
	    exit 1
	    ;;
	"-"*)
	    echo "Error: \"$1\" is invalid option." >&2
	    echo "" >&2
	    exit 1
	    ;;
	*)
	    ArgvFiles[$Num]="$1"
	    Num="$Num + 1"
	    if [ ! -f "$1" ]; then
		echo "" >&2
		echo "  Error: \"$1\" is not found or not a regular file." >&2
		echo "" >&2
		exit 1
	    fi
	    shift
	    ;;
    esac
done

#
# 引数が 2 つ以上ない場合はヘルプ表示
#
if [[ ${#ArgvFiles[*]} < 1 ]]; then
    f90dependhelp
    exit 1
fi

echo "  WARNING: this script will take a long time" >&2
echo "" >&2

#
# 重複するファイルを削除
#
Num=0
FNum=0
while [[ -n "${ArgvFiles[$Num]}" ]]; do
    new=1
    for f in ${Files[*]}; do
	if [ "$f" = "${ArgvFiles[$Num]}" ]; then
	    new=
	fi
    done
    if [ -n "$new" ]; then
	Files[$FNum]=${ArgvFiles[$Num]}
	FNum="$FNum + 1"
    fi
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- File list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#  ${Files[$Num]}"
	Num="$Num + 1"
    done
    echo "#"
fi

#
# 引数が 2 つ以上ない場合はヘルプ表示 (改めて)
#
if [[ ${#Files[*]} < 1 ]]; then
    f90dependhelp
    exit 1
fi


#
# 各 f90 ファイルから USE 文検索
#
declare -a UseList
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    UseList[$Num]=$( grep -iE "^[ ]*USE[ ]+" "${Files[$Num]}" | sed "s/^[ ]*USE[ ]\+//i" | cut -d ',' -f 1 | sort | uniq )
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- Use list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#${Files[$Num]}: "
	echo "#   ${UseList[$Num]}" | tr -s '\n' ' '
	echo ""
	echo "#"
	Num="$Num + 1"
    done
    echo "#"
fi

#
# 各 f90 ファイルから MODULE 文検索
#
declare -a ModuleList
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    ModuleList[$Num]=$( grep -iE "^[ ]*MODULE[ ]+" "${Files[$Num]}" | grep -iv "^[ ]*MODULE[ ]\+PROCEDURE[ ]\+" | sed "s/^[ ]*MODULE[ ]\+//i" | cut -d ',' -f 1 | sort | uniq )
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- Module list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#${Files[$Num]}: "
	echo "#   ${ModuleList[$Num]}" | tr -s '\n' ' '
	echo ""
	echo "#"
	Num="$Num + 1"
    done
    echo ""
fi

#
# 各 f90 ファイルからメインファイル検索
#
declare -a MainList
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    MainList[$Num]=
    MainList[$Num]=$( grep -iE "^[ ]*PROGRAM[ ]+" "${Files[$Num]}" )
    if [ -n "${MainList[$Num]}" ]; then
	MainList[$Num]=$( echo "${Files[$Num]}" | sed 's/\.f9[05]$//i' )
    fi
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- Main list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#${Files[$Num]}: "
	echo "#   ${MainList[$Num]}" | tr -s '\n' ' '
	echo ""
	echo "#"
	Num="$Num + 1"
    done
    echo ""
fi

#
# オブジェクトファイル名の作成
#
declare -a ObjFiles
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    ObjFiles[$Num]=$( echo ${Files[$Num]} | sed 's/\.f9[05]$/.o/i' )
    Num="$Num + 1"
done


#
# USE エントリ情報をファイル名に変換 (存在しないものは削除)
#
declare -a DependList
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    DependList[$Num]=
    for ul in ${UseList[$Num]}; do
	FNum=0
	while [[ -n "${Files[$FNum]}" ]]; do
	    for ml in ${ModuleList[$FNum]}; do
	        if [ "$ul" = "$ml" ]; then
		    DependList[$Num]="${DependList[$Num]} ${ObjFiles[$FNum]}"
	        fi
	    done
	    FNum="$FNum + 1"
	done
    done
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- Depend list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#${Files[$Num]}: "
	echo "#   ${DependList[$Num]}" | tr -s '\n' ' '
	echo ""
	echo "#"
	Num="$Num + 1"
    done
    echo ""
fi

#
# 主プログラムに関する依存リスト作成
#
declare -a MainDepList
Num=0

while (( ${#MainList[*]} > $Num )) ; do
    if [ -z "${MainList[$Num]}" ]; then
	Num="$Num + 1"
	continue
    fi
    MainDepList[$Num]=
    for dl in ${DependList[$Num]}; do
	FNum=0
	while [[ -n "${ObjFiles[$FNum]}" ]]; do
	    if [ "$dl" = "${ObjFiles[$FNum]}" ]; then
		MainDepList[$Num]="${MainDepList[$Num]} ${ObjFiles[$FNum]} ${DependList[$FNum]}"
		FNum="$FNum + 1"
		continue
	    fi
	    FNum="$FNum + 1"
	done
    done
    MainDepList[$Num]=$( echo "${MainDepList[$Num]}" | tr -s ' ' '\n'| sort | uniq | tr -s '\n' ' ' )

    # 多階層チェック.
    PrevMainDepList=${MainDepList[$Num]}
    while true; do
	for dl in ${MainDepList[$Num]}; do
	    FNum=0
	    while [[ -n "${ObjFiles[$FNum]}" ]]; do
		if [ "$dl" = "${ObjFiles[$FNum]}" ]; then
		    MainDepList[$Num]="${MainDepList[$Num]} ${ObjFiles[$FNum]} ${DependList[$FNum]}"
		    FNum="$FNum + 1"
		    continue
		fi
		FNum="$FNum + 1"
	    done
	done
	MainDepList[$Num]=$( echo "${MainDepList[$Num]}" | tr -s ' ' '\n'| sort | uniq | tr -s '\n' ' ' )
	
	if [ "$PrevMainDepList" = "${MainDepList[$Num]}" ]; then
	    break
	else
	    PrevMainDepList=${MainDepList[$Num]}
	fi
    done
    
    Num="$Num + 1"
done

if [ -n "$verbose" ]; then
    echo "#  --- MainDep list --- "
    Num=0
    while [[ -n "${Files[$Num]}" ]]; do
	echo "#${Files[$Num]}: "
	echo "#   ${MainDepList[$Num]}" | tr -s '\n' ' '
	echo ""
	echo "#"
	Num="$Num + 1"
    done
    echo ""
fi

#
# 出力
#
Num=0
while [[ -n "${Files[$Num]}" ]]; do
    echo "${ObjFiles[$Num]}: ${DependList[$Num]}"
    Num="$Num + 1"
done
Num=0
while (( ${#MainList[*]} > $Num )) ; do
    if [ -z "${MainList[$Num]}" ]; then
	Num="$Num + 1"
	continue
    fi
    echo "${MainList[$Num]}: ${MainDepList[$Num]}"
    Num="$Num + 1"
done
