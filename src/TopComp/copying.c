/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog Compiler                                                 *
 * File  : copying.c                                                       *
 * Descr.: copying notice for --version option                             *
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


/* This file included by top_comp.c, wam2ma.c, ma2asm.c,... */


/*-------------------------------------------------------------------------*
 * MK_COPYING_MESSAGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static char *
Mk_Copying_Message(char *sub_part)
{
  static char buff[256];
  if (sub_part)
    sprintf(buff, "%s (%s) %s\n", sub_part, PROLOG_NAME, PROLOG_VERSION);
  else
    sprintf(buff, "%s %s\n", PROLOG_NAME, PROLOG_VERSION);

  strcat(buff, "By Daniel Diaz\n"
#ifdef ADDITIONAL_INFORMATION
         ADDITIONAL_INFORMATION
#endif
         PROLOG_COPYRIGHT "\n\n"
         PROLOG_NAME " comes with ABSOLUTELY NO WARRANTY.\n"
         "This is free software; see the source or the file\n"
         "named COPYING for copying conditions.\n");
  return buff;
}



/*-------------------------------------------------------------------------*
 * DISPLAY_COPYING                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Display_Copying(char *sub_part)
{
  fprintf(stderr, "%s", Mk_Copying_Message(sub_part));
}
