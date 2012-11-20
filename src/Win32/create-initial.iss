#!/bin/sh 

# create-initial.iss [SRCFILE] [DSTFILE]

# this script recreates gp-setup.iss.in from a gp-setup.iss
# useful when modifying gp-setup.iss under Inno Setup interactive compiler

# replace version number by @VERSION@
#         temporary directory by @WIN_TMP_DIR@
#         compiler version by @WIN_CC_VER@


src=${1:-gp-setup.iss}
dst=${2:-gp-setup.iss.in}
set -x
tmp=`grep OutputDir $src | cut --delimiter== --fields=2 | \
    sed s!\\\\\\\\!\\\\\\\\\\\\\\\\!g`

echo "tmp = <$tmp>"

if [ "$tmp" = "" ]; then
    echo "CANNOT FIND WIN_TMP_DIR in $src"
    exit 1
fi

sed -e 's![0-9]\{1,2\}\.[0-9]\{1,2\}\.[0-9]\{1,2\}!@PROLOG_VERSION@!g' \
    -e "s!$tmp!@WIN_TMP_DIR@!g" \
    -e "s!mingw-[a-zA-z0-9_]\{1,6\}!@WIN_CC_VER@!g" \
    -e "s!msvc-[a-zA-z0-9_]\{1,6\}!@WIN_CC_VER@!g" \
    $src >$dst
