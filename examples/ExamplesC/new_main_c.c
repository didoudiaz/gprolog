/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : foreign facility test                                           *
 * File  : new_main_c.c                                                    *
 * Descr.: test file - C part                                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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

/* $Id$ */
 
#include <stdio.h>
#include <string.h>

#define __GPROLOG_FOREIGN_STRICT__
#include "gprolog.h"


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
 * MAIN                                                                    *
 *                                                                         *
 * See comments in EnginePl/main.c about the use of the wrapper function.  *
 *-------------------------------------------------------------------------*/
static int
Main_Wrapper(int argc, char *argv[])
{
  int func;
  PlTerm arg[10];
  char str[100];
  char *sol[100];
  int i, nb_sol = 0;
  PlBool res;

  Pl_Start_Prolog(argc, argv);

  func = Pl_Find_Atom("anc");
  for (;;)
    {
      printf("\nEnter a name (or 'end' to finish): ");
      fflush(stdout);
      if (scanf("%s", str))	/* avoid gcc warning warn_unused_result */
	;

      if (strcmp(str, "end") == 0)
	break;

      Pl_Query_Begin(PL_TRUE);

      arg[0] = Pl_Mk_Variable();
      arg[1] = Pl_Mk_String(str);
      nb_sol = 0;
      res = Pl_Query_Call(func, 2, arg);
      while (res)
	{
	  sol[nb_sol++] = Pl_Rd_String(arg[0]);
	  res = Pl_Query_Next_Solution();
	}
      Pl_Query_End(PL_RECOVER);

      for (i = 0; i < nb_sol; i++)
	printf("  solution: %s\n", sol[i]);
      printf("%d solution(s)\n", nb_sol);
    }

  Pl_Stop_Prolog();
  return 0;
}


int
main(int argc, char *argv[])
{
  return Main_Wrapper(argc, argv);
}

