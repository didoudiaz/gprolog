#include <stdio.h>
#include <stdlib.h>

#include "unit_ctor.h"

static char name[32];

int count;
int mask;
int errors;

int no;
int inc;


PL_INITIALIZER(Init_Tables)
{
  if (no == 0)			/* first unit initialization */
    {
      if (UNIT_NO == 1) 
	{
	  printf("WARNING: it seems units are initialized from first to last\n");
	  printf("   better is from last to first, for this:\n   "
#ifdef UNIT_CHAIN_REVERSE_ORDER
		 "un"
#endif
		 "define constant UNIT_CHAIN_REVERSE_ORDER in unit_ctor.h\n");
	  //	  errors++;
	  inc = 1;
	}
      else
	inc = -1;
    }
  else
    {
      if (UNIT_NO != no + inc)
	{
	  printf("warning: unit %d found while expecting unit %d - it seems order of units is not predictible\n", 
		 UNIT_NO, no);
	  errors++;
	}
    }

  no = UNIT_NO;

  count++;
  mask |= (1 << no);

  sprintf(name, "unit #%d", UNIT_NO);
  printf("unit <%s> found  &name:%p\n", name, &name);
}
