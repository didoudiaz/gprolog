/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Top-level                                                *
 * File  : top_level.c                                                     *
 * Descr.: top-level command-line option checking                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

#define OBJ_INIT Top_Level_Initializer

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




/*-------------------------------------------------------------------------*
 * To define a top_level simply compile an empty source file (Prolog or C) *
 * (linking the Prolog top-level). Since we want to recognize the --version*
 * option, we define an initializer to check the options (os_argc/os_argv) *
 *-------------------------------------------------------------------------*/

#define Check_Arg(i,str)           (strncmp(os_argv[i],str,strlen(os_argv[i]))==0)




/*-------------------------------------------------------------------------*
 * TOP_LEVEL_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Top_Level_Initializer(void)
{
  int i;

  for (i = 1; i < os_argc; i++)
    {
      if (*os_argv[i] == '-' && os_argv[i][1] != '\0')
	{
	  if (strcmp(os_argv[i], "--") == 0)
	    {
	      os_argv[i] = os_argv[0];
	      os_argv += i;
	      os_argc -= i;
	      break;
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
#if 0				/* unknown option is simply ignored (passed to Prolog) */
	  Fatal_Error("unknown option %s - try %s --help", os_argv[i],
		      TOP_LEVEL);
#endif
	}
    }
}




/*-------------------------------------------------------------------------*
 * DISPLAY_HELP                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_Help(void)
#define L(msg)  fprintf(stderr,"%s\n",msg);
{
  fprintf(stderr, "Usage: %s [OPTION]... \n", TOP_LEVEL);
  L(" ");
  L("  -h, --help                  print this help and exit");
  L("  --version                   print version number and exit");
  L
    ("  --                          do not check following arguments (passed to Prolog")
    L("");
  L("Report bugs to bug-prolog@gnu.org.");
}

#undef L
