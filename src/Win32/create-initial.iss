#!/bin/sh 

# create-initial.iss [SRCFILE] [DSTFILE]

# this script recreates gp-setup.iss.in from a gp-setup.iss
# useful when modifying gp-setup.iss under Inno Setup interactive compiler

# simply replace version number by @VERSION@

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
    $src >$dst
