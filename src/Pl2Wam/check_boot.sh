#!/bin/sh

mode=native

filter_wam() {
    egrep -v '(^%|^file_name\()' $1
}

do_diff() {
    tmp1=$(mktemp) || exit 1
    tmp2=$(mktemp) || exit 1

    filter_wam $1 > "$tmp1"
    filter_wam $2 > "$tmp2"

    diff -w "$tmp1" "$tmp2" \
	|| { echo "difference encountered: $1 <> $2"; rm -f "$tmp1" "$tmp2"; exit 1; }

    rm -f "$tmp1" "$tmp2"
}


copy_files() {
    for i in "$@"; do
	\cp $i ${i}1
    done
}


rm_make() {
    rm -f "$@"
    [ "$mode" = emul ] && mkflags='PL2WAM=pl2wam_emul'
#    [ "$emul" = emul ] && echo CHECK IN EMULATED Mode with make $mkflags
    [ "$mode" = interp ] && mkflags='PL2WAM=pl2wam_interp'
#    [ "$emul" = interp ] && echo CHECK IN INTERPRETED Mode with make $mkflags
    make $mkflags >/tmp/make.log 2>&1 || (echo /tmp/make.log ; exit 1)
    fix-wam.sh "$@"
    #make $mkflags || exit 1
}



verify_files() {
    for i in "$@"; do
#	echo checking file $i
	do_diff $i ${i}1 || exit 1
    done
}


do_all_bootstrap() {
    copy_files "$@" || exit 1
    rm_make "$@" || exit 1
    verify_files "$@" || exit 1
}


restore() {
    for i in "$@"; do
	i=${i%1} # remove 1 in case files are gives as .wam1
	\cp ${i}1 $i
    done
}

usage() {
    echo 'Usage check_boot.sh -c [FILES]  save FILES .wam to .wam1'
    echo '      check_boot.sh -m [FILES]  rm FILES .wam and make (rebuild)'
    echo '      check_boot.sh -v [FILES]  verify FILES .wam vs .wam1'
    echo '      check_boot.sh -a [FILES]  do all (copy, make, verify)'
    echo
    echo '      check_boot.sh -r [FILES]  restore .wam files from .wam1'
    echo
    echo 'Pass -e as first argument to run pl2wam in emulated    mode (for -m)'
    echo '     -i as first argument to run pl2wam in interpreted mode (for -m)'
}


if [ "$1" = '-e' ]; then
   mode=emul
   shift
elif [ "$1" = '-i' ]; then
   mode=interp
   shift
fi
 
case "$1" in
    -c) shift; files=${*:-*.wam}; copy_files $files || exit 1;;
    -m) shift; files=${*:-*.wam}; rm_make $files || exit 1;;
    -v) shift; files=${*:-*.wam}; verify_files $files || exit 1;;
    -a) shift; files=${*:-*.wam}; do_all_bootstrap $files || exit 1;;
    -r) shift; files=${*:-*.wam}; restore $files || exit 1;;
    *) usage; exit 1;;
esac


exit 0
