/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : installation                                                    *
 * File  : cpp_headers.c                                                   *
 * Descr.: General GNU Prolog header file maker                            *
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
#include <ctype.h>

#include "gp_config.h"

#if 1
#define DO_NOT_ADD_COMMENTS
#endif
#if 0
#define REMOVE_COMMENTS
#endif
#if 0
#define REMOVE_BLANK_LINES
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  char *name;
  char *parent;
  int line;
}
UsedFile;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

char **dir;
FILE *fout;
char buff[4096];
UsedFile used[1024];
int nb_used = 0;


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Cpp_File(char *name, int skip_comment);




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  if (argc < 4)
    {
      fprintf(stderr, "Usage cpp_headers in_file.h out_file.h search_dir...\n");
      return 1;
    }
  dir = argv + 3;
  if ((fout = fopen(argv[2], "w")) == NULL)
    {
      perror(argv[2]);
      return 1;
    }

#ifndef DO_NOT_ADD_COMMENTS
  fprintf(fout, "/* %s generated from %s using cpp_headers */\n",
	  argv[2], argv[1]);
#endif

  Cpp_File(argv[1], 0);
  fclose(fout);
  return 0;
}




#define SKIP_SPACE(p)   while(isspace(*p)) p++;

/*-------------------------------------------------------------------------*
 * CPP_FILE                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Cpp_File(char *name, int skip_comment)
{
  char **d;
  int i;
  FILE *fin;
  char *p, *q;
  int line = 0;
  char name1[1024];
#ifdef REMOVE_COMMENTS
  int inside_comment = 0;
#endif

#ifdef REMOVE_BLANK_LINES
  int can_be_removed = 1;
#endif


  for (d = dir; *d; d++)
    {
      sprintf(buff, "%s/%s", *d, name);
      if ((fin = fopen(buff, "r")) != NULL)
	break;
    }

  if (*d == NULL)
    {
      fprintf(stderr, "Cannot find the location of %s\n", name);
      exit(1);
    }

  while (fgets(buff, sizeof(buff), fin))
    {
      line++;
#ifdef REMOVE_COMMENTS
      if (skip_comment)
	{
	  if (!inside_comment)
	    {
	      for(p = buff; isspace(*p); p++)
		;

	      if (*p == '/' && p[1] == '*')
		{
		  if (strstr(p, "*/") == NULL)
		    inside_comment = 1;
		  continue;
		}
	    }
	  else
	    {
	      if (strstr(p, "*/") != NULL)
		inside_comment = 0;
	      continue;
	    }
	}
#endif

      if (*buff != '#')
	goto reflect_line;

      p = buff + 1;
      SKIP_SPACE(p);
      if (strncmp(p, "include", 7))
	goto reflect_line;
      p += 7;
      SKIP_SPACE(p);
      if (*p != '"')
	goto reflect_line;
      p++;
      q = p + strlen(p);
      while (*q != '"')
	q--;
      *q = '\0';
      strcpy(name1, p);
#ifndef DO_NOT_ADD_COMMENTS
      fprintf(fout, "/* %s:%d includes %s */\n", name, line, name1);
#endif
      for (i = 0; i < nb_used; i++)
	if (strcmp(used[i].name, name1) == 0)
	  break;

      if (i >= nb_used)
	{
	  used[nb_used].name = strdup(name1);
	  used[nb_used].parent = strdup(name);
	  used[nb_used].line = line;
	  nb_used++;
	  Cpp_File(name1, 1);
	}
#ifndef DO_NOT_ADD_COMMENTS
      else
	fprintf(fout, "/* already included by %s:%d */\n",
		used[i].parent, used[i].line);
#endif

      continue;

    reflect_line:
#ifdef REMOVE_BLANK_LINES
      if (can_be_removed)
	{
	  p = buff;
	  SKIP_SPACE(p);
	  if (*p == '\0')
	    continue;
	}

      p = buff + strlen(buff) - 1;
      while (isspace(*p))
	p--;
      can_be_removed = (*p != '\\');
#endif
      fputs(buff, fout);
    }

  fclose(fin);
}
