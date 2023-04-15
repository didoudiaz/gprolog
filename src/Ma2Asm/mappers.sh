#!/bin/bash

# this script checks the compilation of all mappers to check for regressions
#
# mappers.sh unset
#    creates a header file to unset all arch macros
#
# mappers.sh cp
#    compile all mappers and copy chkma_ma.s in a compile dir for future check
#
# mappers.sh diff
#    compile all mappers and diff them with files in the compile dir
#
# NB: the produced files are not "correct" because some depend on other
# definitions, e.g. defs in machine.h depending on macros defined by ./configure
#
# Anyway on the same platform it can serve to make a diff.
#
# This reduces the gap a bit:
#
# ./configure --with-c-flags=debug --disable-regs


file_undef_h=mappers_undef.h

file_force_h=mapper_force.h

compile_dir=~/mappers-dir

file_log=$compile_dir/mappers.log

file_ma=chkma_ma.ma

verbose=0

usage()
{
    echo "   usage: $0 (undef | cp | diff) [arch...]"
    echo
    echo "   if no arch is specified, use all archs found in ../configure"
    echo "   else each arch is of the form proc/os"
}

collect_archs()
{
    echo `grep ') *AC_DEFINE(M_' ../configure.in|awk '{printf("%s/%s\n",$2,$3)}'|sed 's/AC_DEFINE(M_//g;s/)//g'|sort|uniq`
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


undef_archs()
{
    echo "/* created by mappers.sh */" >$file_undef_h

    
    for a in $archs; do
	undef_one $a
    done
    echo undefs are in headef file: $file_undef_h
}

undef_macro()
{
    echo >>$file_undef_h
    echo "#ifdef $1" >>$file_undef_h
    echo "#undef $1" >>$file_undef_h
    echo "#endif" >>$file_undef_h
}


errors=0

test_one()
{
    proc_os=$1
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

    f1=/tmp/$proc_os.s
    f2=$compile_dir/$proc_os.s
    
    echo "rm -f ma2asm_inst.o"  >>$file_log
    make clean >/dev/null 2>&1
    echo "make FORCE_MAP='-DFORCE_MAPPER=2'" >>$file_log
    if make FORCE_MAP='-DFORCE_MAPPER=2' >>$file_log 2>&1; then
	echo "ma2asm --comment $file_ma -o $f1" >>$file_log
	if ma2asm --comment $file_ma -o $f1; then
	    if [ $oper = cp ]; then
		echo "   creating $f2"
		echo "cp $f1 $f2" >>$file_log
		cp $f1 $f2
	    else
		echo "diff -w $f1 $f2" >>$file_log
		if ! diff -w $f1 $f2 >>$file_log 2>&1 ; then
		    echo
		    echo "***** ERROR diff $f1 $f2"
		    echo
		    errors=$((errors + 1))
		fi
	    fi
	else
	    echo
	    echo "***** ERROR ma2asm --comment $file_ma -o $f1"
	    echo
	    errors=$((errors + 1))
	fi
    else
	echo
	echo "***** ERROR COMPILATION"
	echo
	errors=$((errors + 1))
    fi
}

test_archs()
{
    [ -d $compile_dir ] || mkdir $compile_dir || exit 1
    echo "compiling all" `date` >$file_log
    
    for a in $archs; do
	test_one $a
    done

    rm -f ma2asm_inst.o
    echo
    echo log file $file_log
    echo $errors errors
    warnings=`grep '.*:.*warning' $file_log|wc -l`
    echo $warnings warnings
    test $errors = 0
}

oper="$1"
shift
archs=${*:-`collect_archs`}

if [ $verbose = 1 ]; then
    echo $archs | tr ' ' '\n'
    echo
fi
       

case $oper in
    undef|unset)  undef_archs;;
    cp)           test_archs cp;;
    diff)         test_archs diff;;
    *)            usage; exit;;
esac



