/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_noecho.c                                                   *
 * Descr.: test file                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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
#include <locale.h>
#include <ctype.h>
#include <stdarg.h>

#include "linedit.h"

#define printf LE_Printf




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

  setlocale(LC_ALL, "");

  while ((c = LE_FGetc_No_Echo(0, 1)) != EOF)
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
