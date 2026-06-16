#!/bin/bash

gdb_file=~/.gdb-gprolog

cat >$gdb_file <<EOF
define wr
  call Pl_Write(\$arg0)
  printf("\n")
end

define a
  p pl_atom_tbl[$arg0]
end

define x
  p /x pl_reg_bank[$arg0]
end

EOF

sed -nE '
# MAP_REG_XXX  "REG"
s@^#define +MAP_REG_([A-Z0-9_]+)[[:space:]]+"([^"]*)".*@define \1\
  printf "\1 = $\2 = "\
  p /x $\2\
end\
@p

# MAP_OFFSET_XXX  ((NB_OF_X_REGS+K)*8)
s@^#define +MAP_OFFSET_([A-Z0-9_]+)[[:space:]]+.*NB_OF_X_REGS.*\+([0-9]+)\).*@define \1\
  printf "\1 = pl_reg_bank[256+\2] = "\
  p /x pl_reg_bank[256+\2]\
end\
@p
' wam_regs.h >>$gdb_file
