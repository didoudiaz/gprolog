/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : copying.c                                                       *
 * Descr.: copying notice for --version option                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

/* This file included by top_comp.c, wam2ma.c, ma2asm.c,... */




/*-------------------------------------------------------------------------*
 * DISPLAY_COPYING                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_Copying(char *sub_part)
{
  if (sub_part)
    fprintf(stderr, "%s (%s) ", sub_part, PROLOG_NAME);
  else
    fprintf(stderr, "%s ", PROLOG_NAME);

  fprintf(stderr, "%s\n", PROLOG_VERSION);
  fprintf(stderr, "By Daniel Diaz\n");
  fprintf(stderr, "%s\n", PROLOG_COPYRIGHT);
  fprintf(stderr, "%s comes with ABSOLUTELY NO WARRANTY.\n", PROLOG_NAME);
  fprintf(stderr, "You may redistribute copies of %s\n", PROLOG_NAME);
  fprintf(stderr, "under the terms of the GNU General Public License.\n");
  fprintf(stderr, "For more information about these matters, "
	  "see the files named COPYING.\n");
}
