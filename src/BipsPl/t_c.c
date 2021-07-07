/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : development only                                                *
 * File  : t_c.c                                                           *
 * Descr.: test - C part                                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/*
 * You can put your own test code in these files (see src/DEVELOPMENT)
 *    t.pl  (Prolog part)
 *    t_c.c (C part, eg. foreign code)
 */

#include <stdio.h>
#include <stdlib.h>

#if 0
#include "engine_pl.h"
#include "bips_pl.h"
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

/*-------------------------------------------------------------------------*
 *                                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/

#if 0

#include <gprolog.h>
#include <stdlib.h>

Bool segfault( char *in, PlTerm out )
{
        if( strcmp(in,"yes") == 0 ) {
                printf("Calling system error\n");
                Pl_Err_System(Pl_Create_Atom("segfault_system_error_test"));
                printf("return\n");
                return FALSE;
        } else {
                printf("unifying\n");
                Pl_Un_String_Check("Hello, World!", out);
                printf("return\n");
                return TRUE;
        }
        return FALSE;
}

#endif


#if 0
#include "gprolog.h"


int i=0;
Bool malloc_test(int size)
{
  void * tmp;
  i++;
  tmp = malloc(size);
  if (tmp == NULL)
    {
      printf("FAILED AT %d MB\n", i);
      exit(1);
    }
  printf("OK at: %lx\n", tmp);
  return TRUE;
}

#endif


#if 0
#include "gprolog.h"
int i=0;
Bool malloc_test(int size)
{
  void * tmp;

  i++;
  tmp = malloc(size);
  if (tmp == NULL)
    {
      Pl_Err_System(Pl_Create_Atom("malloc_test exception"));
    }
  printf("OK at: %p\n", tmp);
  return TRUE;
}

#endif

#if 0
int i=0;
Bool malloc_test(int size)
{
  Pl_Err_System(Pl_Create_Atom("malloc_test exception"));
  return TRUE;
}
#endif


#if 0
#include <string.h>
#include "gprolog.h" 
Bool
calling_c(PlTerm In,PlTerm* Out)
{
  PlTerm arg[2];
  int i;
  
  for(i=0;i<2;i++)
    arg[i]=X(1);

  Pl_Query_Begin(TRUE);
  Pl_Query_Call(Pl_Find_Atom("write"),1,&In);
  Pl_Query_End(PL_RECOVER);


  for(i=0;i<2;i++)
    X(1)=arg[i];

  *Out = Pl_Mk_Atom(atom_nil);

  return PL_SUCCESS; 
} 




Bool 
Is_Kbd_Empty(void)
{
  /* to put in stream_supp.c
  if (tty_ptr != NULL && *tty_ptr != '\0')
    return FALSE;
  */
  int result = !Pl_LE_Kbd_Is_Not_Empty();
  printf("result: %d\n", result);
  return result;
}
#endif




#if 0

#include "gprolog.h"

int sdl_init(PlTerm list)
{
  unsigned long flags=0;
  int atom;
  if(Pl_Builtin_List(list)) {
    PlTerm *terms=NULL;
    int len=Pl_List_Length(list);
    terms=malloc(sizeof(PlTerm)*len);
    if(terms) {
      unsigned i;
      Pl_Rd_Proper_List_Check(list, terms);

      for(i=0; i<len; i++) {
	char* s;
	switch(Pl_Type_Of_Term(terms[i])) {
	case PL_INT:
	  printf("integer.\n");
	  flags|=Pl_Rd_Integer(terms[i]);
	  break;
	case PL_ATM:
	  atom = Pl_Rd_Atom(terms[i]);
	  s=Pl_Atom_Name(atom);
	  printf("atom %s is '%s'\n",s,Pl_Is_Valid_Atom(Pl_Rd_Atom(terms[i])) ? "valid":"invalid");
	  break;
	}
      }
    }
  }
  return PL_TRUE;
}
#endif



#if 0
#include "gprolog.h"


Bool
My_Disp(PlTerm term, PlTerm t1)
{
  char *s = Pl_Write_To_String(term);
  
  return Pl_Unif(Pl_Read_From_String(s), t1);
  /*
  printf("my <%s>\n", s);
  free(s);
  */
}

#endif
