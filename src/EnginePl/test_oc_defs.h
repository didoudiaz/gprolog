#include <stdio.h>

#define OBJ_INIT Init_Tables

#include "obj_chain.h"

static int ff[10];
static int gg[10] = { 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 };




#define Decl_Obj(m) \
static char *name=m; \
                \
static int a; \
static int b=12;  \
       int c;  \
static char d[]="abcdef" ; \
static char *e="ghijk" ; \
static int f[10]={0,1,2,3,4,5,6,7,8,9}; \
       int g[5]; \
                 \
 \
 \
static void Init_Tables()  \
{  \
 printf("object <%s> found\n",name); \
 printf("   start at:%#lx\n",(long) &obj_chain_start); \
 printf("   &name:%#lx\n",(long) &name); \
 printf(" n &a:%#lx\n",(long) &a); \
 printf("   &b:%#lx\n",(long) &b); \
 printf(" g &c:%#lx\n",(long) &c); \
 printf("   &d:%#lx\n",(long) &d); \
 printf("   &e:%#lx\n",(long) &e); \
 printf("   &f:%#lx\n",(long) &f); \
 printf(" g &g:%#lx\n",(long) &g); \
}
