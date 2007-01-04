/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 installation                                              *
 * File  : create_bat.c                                                    *
 * Descr.: gprologvars.bat creation (for command-line mode)                *
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
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#endif

#define L(msg)  fprintf(f, "%s\n", msg)

/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 * argv[1]: the system drive                                               *
 * argv[2]: the GNU Prolog root path                                       *
 * argv[3]: i(nstall) or u(ninstall)                                       *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  char buff[1024];
  char call[1024];
  int install;
  FILE *f;
  int fd, l;
  int file_size, size;
  char *data, *p, *q;
  char *newline;

  if (argc != 4)
    {
      fprintf(stderr, "Usage: %s SYSTEM_DRIVE GPROLOG_PATH INSTALL/UNINSTALL\n", argv[0]);
      return 1;
    }

  install = (argv[3][0] == 'i' || argv[3][0] == 'I');

  sprintf(buff, "%s\\gprologvars.bat", argv[1]);
  sprintf(call, "call %s", buff);
  l = strlen(call);
  if (install)
    {
      if ((f = fopen(buff, "wt")) == NULL)
	{
	  perror(buff);
	  fprintf(stderr, "If needed, add %s\\bin to your PATH - press RETURN\n",
		  argv[2]);
	  fflush(stderr);
	  gets(buff);
	  return 1;
	}
      L("@echo off");
      L("echo Setting environment for using GNU Prolog");
      sprintf(buff, "PATH=%%PATH%%;\"%s\\bin\"", argv[2]);
      L(buff);
      fclose(f);
    }
  else
    if (access(buff, 0) == 0 && unlink(buff) != 0)
      return 1;

  sprintf(buff, "%s\\autoexec.bat", argv[1]);

  if ((fd = open(buff, O_RDONLY | O_BINARY)) < 0)
    return 0;
  
  file_size = (int) lseek(fd, 0, SEEK_END);
  lseek(fd, 0, SEEK_SET);

  data = (char *) malloc(file_size + l + 32);
  if (data == NULL)
    return 1;
 
  if (read(fd, data, file_size) != file_size)
    return 1;
      
  close(fd);
  data[file_size] = '\0';

  q = strchr(data, '\r');
  if (q != NULL && q[1] == '\n')
    newline = "\r\n";
  else
    newline = "\n";

  if (data[file_size - 1] != '\n')
    strcpy(data + file_size, newline);

  p = data;
  for(;;)
    {
      if (strnicmp(p, call, l) != 0)
	{
	next_line:
	  p = strchr(p, '\n');
	  if (p == NULL)
	    break;
	  p++;
	  continue;
	}

      q = p + l;
      while(*q == ' ' || *q == '\t' || *q == '\r')
	q++;
      if (*q != '\n')
	goto next_line;
				/* here the call line is found */
      if (install)
	return 0;

      q++;
      memmove(p, q, strlen(q) + 1);  /* remove the line */
				/* reconsider this line */
    }

  size = strlen(data);
  if (install)
    {
      sprintf(data + size, "%s%s", call, newline);
      size += l + strlen(newline);
    }

  if (file_size == size)
    return 0;

  if ((fd = open(buff, O_WRONLY | O_BINARY | O_TRUNC)) < 0)
    return 1;

  if (write(fd, data, size) < 0)
    return 1;

  return close(fd) != 0;
}
