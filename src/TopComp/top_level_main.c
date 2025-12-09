/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Top-level                                                *
 * File  : top_level.c                                                     *
 * Descr.: top-level command-line option checking                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


#include <stdio.h>
#include <string.h>

#include "../EnginePl/engine_pl.h"
#include "../Linedit/linedit.h"
#include "../BipsPl/c_supp.h"
#include "../BipsPl/inl_protos.h"
#include "../BipsPl/flag_supp.h"
#include "copying.c"





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


#define EXEC_CMD_LINE_GOAL  X1_24657865635F636D645F6C696E655F676F616C
#define PREDICATE_TOP_LEVEL X0_top_level

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
 * See comments in EnginePl/main.c about the use of the wrapper function.  *
 *-------------------------------------------------------------------------*/

static int
Main_Wrapper(int argc, char *argv[])
{
  int i;
  int new_argc = 0;
  char **new_argv;
  char **init_goal;
  int nb_init_goal = 0;
  char **consult_file;
  int nb_consult_file = 0;
  char **entry_goal;
  int nb_entry_goal = 0;
  char **query_goal;
  int nb_query_goal = 0;
  Bool quiet = 0;
  WamWord word;

  
  new_argv = (char **) Malloc(sizeof(char *) * (argc + 1));
  new_argv[new_argc++] = argv[0];

  consult_file = (char **) Malloc(sizeof(char *) * argc);
  init_goal = (char **) Malloc(sizeof(char *) * argc);
  entry_goal = (char **) Malloc(sizeof(char *) * argc);
  query_goal = (char **) Malloc(sizeof(char *) * argc);

  for (i = 1; i < argc; i++)
    {
      if (*argv[i] == '-' && argv[i][1] != '\0')
	{
	  if (strcmp(argv[i], "--") == 0)
	    {
	      i++;
	      break;
	    }

	  if (Check_Arg(i, "--init-goal"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("Goal missing after --init-goal option");

	      init_goal[nb_init_goal++] = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--consult-file"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("File missing after --consult-file option");

	      consult_file[nb_consult_file++] = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--entry-goal"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("Goal missing after --entry-goal option");

	      entry_goal[nb_entry_goal++] = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--query-goal"))
	    {
	      if (++i >= argc)
		Pl_Fatal_Error("Goal missing after --query-goal option");

	      query_goal[nb_query_goal++] = argv[i];
	      continue;
	    }

	  if (Check_Arg(i, "--no-gui-console"))
	    {
	      pl_le_no_gui = 1;	/* overwrite var of linedit.c */
	      continue;
	    }

	  if (Check_Arg(i, "--quiet"))
	    {
	      quiet = TRUE;
	      continue;
	    }

	  if (Check_Arg(i, "--version"))
	    {
	      Display_Copying("Prolog top-Level");
	      exit(0);
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

  Pl_Start_Prolog(new_argc, new_argv);

  if (quiet)
    Flag_Value(show_banner) = 0;

  for (i = 0; i < nb_init_goal; i++)
    {
      A(0) = Tag_ATM(Pl_Create_Atom(init_goal[i]));
      Pl_Call_Prolog(Prolog_Predicate(EXEC_CMD_LINE_GOAL, 1));
      Pl_Reset_Prolog();
    }

  if (nb_consult_file)
    {
      word = Pl_Mk_Proper_Atom_List(nb_consult_file, consult_file);
      Pl_Blt_G_Assign(Tag_ATM(Pl_Create_Atom("$cmd_line_consult_file")), word);
    }
  Free(consult_file);

  if (nb_entry_goal)
    {
      word = Pl_Mk_Proper_Atom_List(nb_entry_goal, entry_goal);
      Pl_Blt_G_Assign(Tag_ATM(Pl_Create_Atom("$cmd_line_entry_goal")), word);
    }
  Free(entry_goal);

  if (nb_query_goal)
    {
      word = Pl_Mk_Proper_Atom_List(nb_query_goal, query_goal);
      Pl_Blt_G_Assign(Tag_ATM(Pl_Create_Atom("$cmd_line_query_goal")), word);
    }
  Free(query_goal);

  Pl_Reset_Prolog();
  Pl_Call_Prolog(Prolog_Predicate(PREDICATE_TOP_LEVEL, 0));

  Pl_Stop_Prolog();
  return 0;
}

int
main(int argc, char *argv[])
{
  return Main_Wrapper(argc, argv);
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
  L("  --consult-file FILE         consult FILE inside the the top-level");
  L("  --init-goal    GOAL         execute GOAL before entering the top-level");
  L("  --entry-goal   GOAL         execute GOAL inside the top-level");
  L("  --query-goal   GOAL         execute GOAL as a query for the top-level");
  L("  --no-gui-console            disable Windows GUI console (run in text mode)");
  L("  --quiet                     suppress informational messages (banner, ...)");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L("  --                          do not parse the rest of the command-line");
  L("");
  L("Visit www.gprolog.org for more information.");
}

#undef L
