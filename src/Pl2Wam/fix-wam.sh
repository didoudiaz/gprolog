#!/bin/sh

# GNU Prolog utility: fix prolog path in WAM files

# Default replacement specification

REPL=1/src/=/home/diaz/GP/src/

# before and after path parts (both Matching and Replacing patterns)

BEFORE_PATH_M="^file_name('"
BEFORE_PATH_R="file_name('"

AFTER_PATH_M="'"').[ 	]*$'
AFTER_PATH_R="')."

# Temporary file name

TMP_FILE="/tmp/replace-pl-path.wam.$$"


if [ "$1" = "-h" -o "$1" = "--help" ]; then
    echo "Usage $0 [-r REPL] WAM_FILE..."
    echo
    echo "REPL is specified as <N><OLD>=<NEW> with (0 <= N <= 9) meaning:"
    echo "rewrite the pathname prefix from its beginning up to the Nth"
    echo "occurrence of OLD, replacing it with NEW."
    echo "For instance, if REPL equals to 2src/=/home/bar/ the pathname"
    echo "   /a/src/b/src/c/d/src/e.pl becomes"
    echo "   /home/bar/c/d/src/e.pl"
    echo
    echo "The default REPL (if -r is not provided) is $REPL"
    echo
    echo "if no WAM_FILE is specified, act as a filter (stdin->stdout)"
    exit 0
fi

if [ "$1" = "-r" ]; then
    REPL=$2
    shift 2
fi

three_args=`echo $REPL | sed -n 's/^\([0-9]\)\([^=]*\)=\(.*\)$/\1:\2\:\3/p'`

if [ -z "$three_args" ]; then
    echo "Warning: -r $REPL: invalid format (must be of the form <N><OLD>=<NEW>)"
    exit 1
fi

IFS=: read -r N OLD NEW <<EOF
$three_args
EOF

#echo "N: <$N> OLD: <$OLD>  NEW:<$NEW>"

# Escape regex meta-chars in OLD (for a litteral substition in sed s command)
# NB []...] is the way to include ] in a [...] regex

backslashify() {  # backslashify STRING META_CHARS (as a regex)
    echo "$1" | sed -e 's|'$2'|\\&|g'
}

OLD=$(backslashify "$OLD" '[].[^$*\\+()?|]')

#echo "N: <$N> OLD: <$OLD>  NEW:<$NEW>"

STR_NEW_PREFIX=$(backslashify "${BEFORE_PATH_R}${NEW}" '/')

# Common processing function: either on stdin (no args) or on files (args)
process_stream() {
    # tr -d '\r' <"$f" | sed...
    sed -e "/${BEFORE_PATH_M}.*${AFTER_PATH_M}/{
        h                               ; # save original line (h)
        s|${BEFORE_PATH_M}||            ; # remove before path part (this cannot fail)
        s|${AFTER_PATH_M}||             ; # remove after   path part (this cannot fail)
        s|${OLD}|@|${N}                 ; # replace the Nth occurrence by @ (this can fail)
        s|^[^@]*@|${STR_NEW_PREFIX}|    ; # rebuild the begin of the line (this can fail if no @ exists)
        s|$|${AFTER_PATH_R}|            ; # add the after path part
        /${STR_NEW_PREFIX}/!g           ; # if failure (! expected result) restore the original line (g)
    }" "$@"
}

# Filter mode (stdin -> stdout)
if [ $# -eq 0 ]; then
    if [ "$N" -eq 0 ]; then
	cat # N=0, simply copy stdin to stdout
    else
	process_stream
    fi
    exit 0
fi


# File mode (update the given files)

if [ "$N" -eq 0 ]; then
    exit 0
fi

files="$@"

for f in $files; do
    process_stream "$f" > "${TMP_FILE}"
    cmp -s "$f" "${TMP_FILE}" || { echo "replacing path in $f"; cp "${TMP_FILE}" "$f"; }
done

rm -f "${TMP_FILE}"
