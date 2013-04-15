/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_noecho.c                                                   *
 * Descr.: test file                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
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
#include <ctype.h>
#include <stdarg.h>

#define printf Pl_LE_Printf

#include "../EnginePl/gp_config.h"

#include "../EnginePl/set_locale.h"

#include "../W32GUICons/w32gc_interf.h" /* only to test GUI Console memory size dialog box */
int pl_max_atom;			/* to test the same dialog box */

#ifdef GUI_CONSOLE_WITH_STACK_SIZES
#define ENGINE_FILE                      /* to define stacks data */
typedef PlLong WamWord;
#include "../EnginePl/wam_stacks.h"
#endif

#include "linedit.h"

#include <windows.h>

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
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  int c;

  Set_Locale();
  /*  SetConsoleCP(1252);
  SetConsoleOutputCP(1252);
  */

  while ((c = Pl_LE_Get_Key(0, 1)) != EOF)
    {
      printf("Read Char (%d)", c);
      if (isprint(c))
	printf(" = %c", c);
      if (isalpha(c))
	printf(" isalpha");
      if (isascii(c))
	printf(" isascii");
      if (isupper(c))
	printf(" isupper");
      if (islower(c))
	printf(" islower");
      if (isspace(c))
	printf(" isspace");
      if (isprint(c))
	printf(" isprint");
      if (isgraph(c))
	printf(" isgraph");
      printf("\n");
    }

  return 0;
}
