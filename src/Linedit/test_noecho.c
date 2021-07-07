/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_noecho.c                                                   *
 * Descr.: test file                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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

#include "terminal.h"
#include "linedit.h"

#ifdef _WIN32
#include <windows.h>
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
      printf("Read Char: %d (%#x)", c, c);
      if (c < 256)
	{
	  if (c < 26)
	    printf(" CTRL+%c", c - 1 + 'A');
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
	}
      else if (IS_ESC_COMB(c) && isalpha(GET_ESC_COMB(c)))
	printf(" ESC+%c", GET_ESC_COMB(c));
      else 
	{
	  int modif = GET_MODIF(c);

	  if (modif)
	    {
	      printf(" ");

	      if (modif & KEY_MODIF_SHIFT)
		printf("SHIFT+");
	      if (modif & KEY_MODIF_ALT)
		printf("ALT+");
	      if (modif & KEY_MODIF_CTRL)
		printf("CTRL+");
	    }
	  c = GET_CODE(c);
	  if (!modif)
	    printf(" ");

	  switch(c)
	    {
	    case KEY_EXT_FCT_1:     printf("F1      "); break;
	    case KEY_EXT_FCT_2:     printf("F2      "); break;
	    case KEY_EXT_FCT_3:     printf("F3      "); break;
	    case KEY_EXT_FCT_4:     printf("F4      "); break;
	    case KEY_EXT_FCT_5:     printf("F5      "); break;
				               
	    case KEY_EXT_FCT_6:     printf("F6      "); break;
	    case KEY_EXT_FCT_7:     printf("F7      "); break;
	    case KEY_EXT_FCT_8:     printf("F8      "); break;
	    case KEY_EXT_FCT_9:     printf("F9      "); break;
	    case KEY_EXT_FCT_10:    printf("F10     "); break;
				               
	    case KEY_EXT_FCT_11:    printf("F11     "); break;
	    case KEY_EXT_FCT_12:    printf("F12     "); break;
				               
	    case KEY_EXT_UP:        printf("UP       "); break;
	    case KEY_EXT_DOWN:      printf("DOWN     "); break;
	    case KEY_EXT_RIGHT:     printf("RIGHT    "); break;
	    case KEY_EXT_LEFT:      printf("LEFT     "); break;
				               
	    case KEY_EXT_HOME:      printf("HOME     "); break;
	    case KEY_EXT_END:       printf("END      "); break;
	    case KEY_EXT_PAGE_UP:   printf("PAGE_UP  "); break;
	    case KEY_EXT_PAGE_DOWN: printf("PAGE_DOWN"); break;
	    case KEY_EXT_INSERT:    printf("INSERT   "); break;
	    case KEY_EXT_DELETE:    printf("DELETE   "); break;
	    }
	  if (modif)
	    printf(" modif=%d  k=code=%d(%#x)", modif, c, c);
	}
      printf("\n");
    }

  return 0;
}


