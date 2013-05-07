/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 installation                                              *
 * File  : create_bat.c                                                    *
 * Descr.: gprologvars.bat creation (for command-line mode)                *
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
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>


/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * argv[1]: the GNU Prolog root path                                       *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  char buff[1024];
  int install;
  FILE *f;

  if (argc != 2)
    {
      fprintf(stderr, "Usage: %s GPROLOG_PATH\n", argv[0]);
      return 1;
    }

  sprintf(buff, "%s\\gprologvars.bat", argv[1]);
  if ((f = fopen(buff, "wt")) == NULL)
    {
      perror(buff);
      fprintf(stderr, "If needed, add %s\\bin to your PATH - press RETURN\n", argv[1]);
      fflush(stderr);
      gets(buff);
      return 1;
    }
  fprintf(f, "@echo off\n");
  fprintf(f, "echo Setting environment for using GNU Prolog\n");
  fprintf(f, "PATH=%%PATH%%;\"%s\\bin\"\n", argv[1]);
  fclose(f);

  return 0;
}
