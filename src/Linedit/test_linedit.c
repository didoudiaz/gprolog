/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_linedit.c                                                  *
 * Descr.: test file                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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
#include <signal.h>
#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#else
#include <io.h>
#include <process.h>
#endif


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
 * INTERRUPT_HANDLER                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Interrupt_Handler(int sig)
{
  int c;
  char prefix[100];
  char *str;
  int nb, max_lg, is_last;

#if defined(__unix__) || defined(__CYGWIN__)
  sigset_t set;
#endif

  printf("\nCatching signal:%d\n", sig);
  printf
    ("e to exit - c to continue - C for completions - w for current word:");
  fflush(stdout);

#if defined(__unix__) || defined(__CYGWIN__)
  sigemptyset(&set);
  sigaddset(&set, sig);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
#else
  signal(sig, Interrupt_Handler);
#endif

  c = LE_FGetc_No_Echo(0, 1);
  printf("\n");

  if (c == 'e')
    exit(0);

  if (c == 'w')
    {
      LE_Get_Current_Word(prefix);
      printf("current word=<%s>\n", prefix);
      return;
    }

  if (c == 'C')
    {
      printf("Enter a prefix:");
      fgets(prefix, sizeof(prefix), stdin);
      prefix[strlen(prefix) - 1] = '\0';
      if ((str = LE_Compl_Init_Match(prefix, &nb, &max_lg)) == NULL)
	printf("no matching\n");
      else
	{
	  printf("common=<%s> nb=%d max_lg=%d\n", str, nb, max_lg);
	  while ((str = LE_Compl_Find_Match(&is_last)) != NULL)
	    printf("matching: <%s>\n", str);
	}
    }
}




/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  char line[256];
  char *p;
  char sep[100];

#if defined(_MSC_VER)
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#if 1
  signal(SIGINT, Interrupt_Handler);
#endif

  sep[0] = '\n';
  strcpy(sep + 1, LE_Get_Separators());

  printf("enter lines (EOF to finish)\n");

  for (;;)
    {
#if 0
      printf("tempo to allow you to buffer some chars before the read\n");
      sleep(2);
#endif

#if 0
      printf("enter a line:");
      if (LE_Gets(line) == NULL)
#else
      if (LE_FGets(line, 200, 0, 1, "enter a line:", 1) == NULL)
#endif
	break;
      printf("Line:(%s) len:%d\n", line, strlen(line));
      for (p = line; (p = strtok(p, sep)) != NULL; p = NULL)
	{
	  printf("adding word (%s) for completion\n", p);
	  LE_Compl_Add_Word(strdup(p), strlen(p));
	}
    }

  printf("End of testing\n");

  return 12;
}


/* To Compile with W32GUICons */
char **
M_Cmd_Line_To_Argv(char *cmd,int *argc)
{
  *argc = 0;
  return NULL;
}



