#include <stdio.h>
#include <stdlib.h>

#include "obj_chain.h"


void Fatal_Error(char *msg)

{
 fprintf(stderr,msg);
 fprintf(stderr,"\n");
 exit(1);
}

int main()
{
#ifdef __MSC_VER
 setbuf(stdout,NULL);
 setbuf(stderr,NULL);
#endif
 Find_Linked_Objects();
 puts("\nfinished");
 return 0;
}



