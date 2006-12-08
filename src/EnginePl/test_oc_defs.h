#include <stdio.h>
#include <stdlib.h>

#define OBJ_INIT Init_Tables

#include "obj_chain.h"

static char name[32];
                 
int no;
int previous;

static void Init_Tables()  
{
  if (previous == 0)
    previous = 10000000;
  if (no == 0 && OBJ_NO == 1) {
    printf("error: objects are initialised from 1st to last (should be from last to 1st)\n"
#ifdef OBJ_CHAIN_REVERSE_ORDER
	   "un"
#endif
	   "define constant OBJ_CHAIN_REVERSE_ORDER in obj_chain.h\n");
    exit(1);
  }
  if (OBJ_NO >= previous)
    printf("warning: object %d found after object %d - it seems order of objects is not predictible\n", OBJ_NO, previous);

  previous = no;
  no = OBJ_NO;
  sprintf(name, "object #%d", OBJ_NO);
  printf("object <%s> found  &name:%p\n", name, &name); 
}
