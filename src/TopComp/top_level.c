/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Top-level                                                *
 * File  : top_level.c                                                     *
 * Descr.: top-level command-line option checking                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <string.h>

#include "gprolog.h"
#include "../TopComp/copying.c"




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

static void Display_Help(void);

#define Check_Arg(i, str)  (strncmp(argv[i], str, strlen(argv[i])) == 0)


#define EXEC_CMD_LINE_GOAL  X24657865635F636D645F6C696E655F676F616C
#define PREDICATE_TOP_LEVEL X746F705F6C6576656C

Prolog_Prototype(PREDICATE_TOP_LEVEL, 0);
Prolog_Prototype(EXEC_CMD_LINE_GOAL, 1);


/*-------------------------------------------------------------------------*
 * To define a top_level simply compile an empty source file (Prolog or C) *
 * (linking the Prolog top-level is done by default).                      *
 * This file is because we want to take into account some options/arguments*
 *-------------------------------------------------------------------------*/


/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/

/* A problem appeared in GCC 3.0.x: the main() function always use a frame *
 * (and thus ebp). To be able to use ebp main() simply calls an            *
 * intermediate function GCC_3_0_FIX_main() (the previous main() fuction). *
 * Since after Stop_Prolog() all registers are restored, ebp is correct    *
 * when returning in main() (which uses it to restore esp).                *
 * This is not a problem for EnginePl/main.c since ebp is not used between *
 * Start_Prolog() and Stop_Prolog() (while here it is used via argc/argv)  */

static int
GCC_3_0_FIX_main(int argc, char *argv[])
{
  int i;
  int new_argc = 0;
  char **new_argv;
  WamWord *entry_goal;
  int nb_entry_goal = 0;
  WamWord *query_goal;
  int nb_query_goal = 0;
  WamWord word;


  Start_Prolog(argc, argv);	/* argc and argv will be changed */

  new_argv = (char **) Malloc(sizeof(char *) * (argc + 1));
  new_argv[new_argc++] = argv[0];

  entry_goal = (WamWord *) Malloc(sizeof(WamWord) * argc);
  query_goal = (WamWord *) Malloc(sizeof(WamWord) * argc);

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (strcmp(argv[i], "--") == 0)
	    {
	      i++;
	      break;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      Display_Copying("Prolog top-Level");
	      exit(0);
	    }

	  if (Check_Arg(i, "--init-goal"))
	    {
	      if (++i >= argc)
		Fatal_Error("Goal missing after --init-goal option");

	      A(0) = Tag_ATM(Create_Atom(argv[i]));
	      Call_Prolog(Prolog_Predicate(EXEC_CMD_LINE_GOAL, 1));
	      Reset_Prolog();
	      continue;
	    }

	  if (Check_Arg(i, "--entry-goal"))
	    {
	      if (++i >= argc)
		Fatal_Error("Goal missing after --entry-goal option");

	      entry_goal[nb_entry_goal++] = Tag_ATM(Create_Atom(argv[i]));
	      continue;
	    }

	  if (Check_Arg(i, "--query-goal"))
	    {
	      if (++i >= argc)
		Fatal_Error("Goal missing after --query-goal option");

	      query_goal[nb_query_goal++] = Tag_ATM(Create_Atom(argv[i]));
	      continue;
	    }

	  if (Check_Arg(i, "-h") || Check_Arg(i, "--help"))
	    {
	      Display_Help();
	      exit(0);
	    }
	}
      /* unknown option is simply ignored (passed to Prolog) */
      new_argv[new_argc++] = argv[i];
    }

  while(i < argc)
    new_argv[new_argc++] = argv[i++];

  new_argv[new_argc] = NULL;

  os_argc = new_argc;
  os_argv = new_argv;

  if (nb_entry_goal)
    {
      word = Mk_Proper_List(nb_entry_goal, entry_goal);
      Blt_G_Assign(Tag_ATM(Create_Atom("$cmd_line_entry_goal")), word);
    }
  Free(entry_goal);

  if (nb_query_goal)
    {
      word = Mk_Proper_List(nb_query_goal, query_goal);
      Blt_G_Assign(Tag_ATM(Create_Atom("$cmd_line_query_goal")), word);
    }
  Free(query_goal);

  Reset_Prolog();
  Call_Prolog(Prolog_Predicate(PREDICATE_TOP_LEVEL, 0));

  Stop_Prolog();
  return 0;
}

int
main(int argc, char *argv[])
{
  return GCC_3_0_FIX_main(argc, argv);
}


/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_Help(void)
#define L(msg)  fprintf(stderr, "%s\n", msg)
{
  fprintf(stderr, "Usage: %s [OPTION]... \n", TOP_LEVEL);
  L("");
  L("  --init-goal GOAL            execute GOAL before top_level/0");
  L("  --entry-goal GOAL           execute GOAL inside top_level/0");
  L("  --query-goal GOAL           execute GOAL as a query for top_level/0");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("  --                          do not parse the rest of the command-line");
  L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
