#include <stdio.h>
#include <stdlib.h>

#include "obj_chain.h"

int count;
int mask;
int errors;

void
Pl_Fatal_Error(char *msg)
{
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int
main()
{
  int i;
#ifdef _MSC_VER
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif
  printf("Obj_chain tests started...\n");
  Pl_Find_Linked_Objects();

  printf("%d objects found\n", count);

  for(i = 1; i <= count; i++)
    if ((mask & (1 << i)) == 0)
      {
	printf("error: object %d is not initialized\n", i);
	errors++;
      }
  if (errors == 0)
    printf("Obj_chain tests succeded\n");
  else
    printf("Obj_chain tests failed: %d errors\n", errors);

  return (errors != 0);
}
