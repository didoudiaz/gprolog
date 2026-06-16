#!/bin/sh
# usage: CC=gcc CFLAGS='-m32 -O2 -DUNSURE' disass-trampoline-old.sh
#
CC=${CC:-gcc}
CFLAGS=${CFLAGS:-'-O2 -fno-pic -DUNSURE'}
echo $CC -S trampoline-old.c $CFLAGS -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -fno-asynchronous-unwind-tables -fno-unwind-tables -fno-stack-clash-protection -fno-stack-check -o -
$CC -S trampoline-old.c $CFLAGS -fno-stack-protector -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -fno-asynchronous-unwind-tables -fno-unwind-tables -fno-stack-clash-protection -fno-stack-check -o -

