/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 installation                                              *
 * File  : create_bat.c                                                    *
 * Descr.: gprologvars.bat creation (for command-line mode)                *
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
#include <stdlib.h>
#include <string.h>

#define L(msg)  fprintf(f, "%s\n", msg)

/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * argv[1]: the system drive                                               *
 * argv[2]: the GNU Prolog root path                                       *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  FILE *f;
  char buff[1024];
  char call[1024];
  int defined = 0;

  if (argc != 3)
    {
      fprintf(stderr, "Usage: %s SYSTEM_DRIVE GPROLOG_PATH\n", argv[0]);
      return 1;
    }

  sprintf(buff, "%s\\gprologvars.bat", argv[1]);
  sprintf(call, "call %s\n", buff);
  if ((f = fopen(buff, "wt")) == NULL)
    {
      perror(buff);
      return 1;
    }
  L("@echo off");
  L("echo Setting environment for using GNU Prolog");
  sprintf(buff, "PATH=%%PATH%%;\"%s\\bin\"", argv[2]);
  L(buff);

  fclose(f);

#if 1
  sprintf(buff, "%s\\autoexec.bat", argv[1]);
  if ((f = fopen(buff, "r+t")) == NULL)
    return 0;
  
  for (;;)
    {
      fgets(buff, sizeof(buff), f);
      if (feof(f))
	break;

      if (strnicmp(buff, call, strlen(call)) == 0)
	defined = 1;
    }

  if (!defined)
    if (fputs(call, f) < 0)
      {
	perror("writing autoexec.bat");
	return 1;
      }

  fclose(f);
      
#endif

  return 0;
}

