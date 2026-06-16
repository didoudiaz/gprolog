#include <stdio.h>
#include <stdlib.h>

#include "unit_ctor.h"

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
  printf("unit_ctor tests started...\n");
  Pl_Initialize_Units();

  printf("%d units found\n", count);

  for(i = 1; i <= count; i++)
    if ((mask & (1 << i)) == 0)
      {
	printf("error: unit %d is not initialized\n", i);
	errors++;
      }
  if (errors == 0)
    printf("unit_ctor tests succeded\n");
  else
    printf("unit_ctor tests failed: %d errors\n", errors);

  return (errors != 0);
}
