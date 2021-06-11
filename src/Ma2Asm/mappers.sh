#!/bin/bash

file_undef_h=mappers_undef.h

file_force_h=mapper_force.h

compile_dir=~/mappers-dir

file_log=$compile_dir/mappers.log

file_asm=chkma_ma.s



undef_macro()
{
    echo >>$file_undef_h
    echo "#ifdef $1" >>$file_undef_h
    echo "#undef $1" >>$file_undef_h
    echo "#endif" >>$file_undef_h
}

collect_all_arch()
{
    echo `grep ') *AC_DEFINE(M_' ../configure.in | awk  '{printf("%s/%s\n",$2,$3)}'|sed 's/AC_DEFINE(M_//g;s/)//g'|sort|uniq`
}

undef_one()
{
    proc_os=$1
    proc=`echo $proc_os|cut -d/ -f1`
    os=`echo $proc_os|cut -d/ -f2`
    proc_os=${proc}_${os}

    echo >>$file_undef_h
    echo "/* $proc / $os */"  >>$file_undef_h

    undef_macro M_$proc
    undef_macro M_$os
    undef_macro M_$proc_os
}


undef_all_arch()
{
    echo "/* created by mappers.sh from ../configure.in */" >$file_undef_h

    set `collect_all_arch`

    while [ $# -gt 0 ]; do
	proc_os=$1
	undef_one $1
	shift
    done
    echo undefs are in headef file: $file_undef_h
}

errors=0

test_one()
{
    oper=$1
    proc_os=$2
    proc=`echo $proc_os|cut -d/ -f1`
    os=`echo $proc_os|cut -d/ -f2`
    proc_os=${proc}_${os}

    echo >>$file_log
    echo "architecture: $proc_os" 
    echo "architecture: $proc_os" >>$file_log
    echo "========================================" >>$file_log

    echo "#define M_$proc" >$file_force_h
    echo "#define M_$os" >>$file_force_h
    echo "#define M_${proc}_$os" >>$file_force_h
    
    rm -f ma2asm_inst.o
     echo "make DEF_MAPPER='-DCOMPILE_MAPPER' $file_asm" >>$file_log
    if make DEF_MAPPER='-DCOMPILE_MAPPER' $file_asm >>$file_log 2>&1; then
	if [ $oper = cp ]; then
	    echo "   creating $compile_dir/$proc_os.s"
	    echo "cp $file_asm $compile_dir/$proc_os.s" >>$file_log
	    cp $file_asm $compile_dir/$proc_os.s
	else
	    echo "diff $file_asm $compile_dir/$proc_os.s" >>$file_log
	    if ! diff $file_asm $compile_dir/$proc_os.s >>$file_log 2>&1 ; then
		echo
		echo "***** DIFF ERROR $file_asm $compile_dir/$proc_os.s"
		echo
		errors=$((errors + 1))
	    fi

	fi
    else
	echo
	echo "***** COMPILATION ERROR"
	echo
	errors=$((errors + 1))
    fi
}

test_all_arch()
{
    oper=$1
    
    [ -d $compile_dir ] || mkdir $compile_dir || exit 1
    echo "compiling all" `date` >$file_log
    
    set `collect_all_arch`
    echo $l

    while [ $# -gt 0 ]; do
	test_one $oper $1
	shift
    done

    echo
    echo log file $file_log
    echo $errors errors
    test $errors = 0
}

if [ "$1" = undef ]; then
    undef_all_arch
elif [ "$1" = diff ]; then
    test_all_arch diff
elif [ "$1" = cp ]; then
    test_all_arch cp
else
    echo "   usage: $0 undef | cp | diff"
    exit
fi


