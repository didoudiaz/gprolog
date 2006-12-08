#include <stdio.h>
#include <stdlib.h>

#include "obj_chain.h"

int no;

void
Fatal_Error(char *msg)
{
  fprintf(stderr, msg);
  fprintf(stderr, "\n");
  exit(1);
}

int
main()
{
#ifdef _MSC_VER
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif
  puts("starting...");
  Find_Linked_Objects();
  if (no != 1) {
    printf("error: all objects are not found (last #: %d instead of 1)", no);
    exit(1);
  }
  puts("finished - OK !");
  return 0;
}
