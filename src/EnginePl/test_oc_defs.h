#include <stdio.h>
#include <stdlib.h>

#define OBJ_INIT Init_Tables

#include "obj_chain.h"

static char name[32];

int count;
int mask;
int errors;

int no;
int inc;


static void Init_Tables()
{
  if (no == 0)			/* first object initialization */
    {
      if (OBJ_NO == 1) 
	{
	  printf("WARNING: it seems objects are initialized from first to last\n");
	  printf("   better is from last to first, for this:\n   "
#ifdef OBJ_CHAIN_REVERSE_ORDER
		 "un"
#endif
		 "define constant OBJ_CHAIN_REVERSE_ORDER in obj_chain.h\n");
	  //	  errors++;
	  inc = 1;
	}
      else
	inc = -1;
    }
  else
    {
      if (OBJ_NO != no + inc)
	{
	  printf("warning: object %d found while expecting object %d - it seems order of objects is not predictible\n", 
		 OBJ_NO, no);
	  errors++;
	}
    }

  no = OBJ_NO;

  count++;
  mask |= (1 << no);

  sprintf(name, "object #%d", OBJ_NO);
  printf("object <%s> found  &name:%p\n", name, &name);
}
