#!/bin/sh

# this script recreates initial.iss from a gp-setup.iss
# useful when modifying gp-setup.iss under Inno Setup interactive compiler

# simply replace version number by @VERSION@

sed 's![0-9]\{1,2\}\.[0-9]\{1,2\}\.[0-9]\{1,2\}!@VERSION@!g' $*

