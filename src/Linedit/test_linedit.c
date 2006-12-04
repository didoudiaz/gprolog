/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_linedit.c                                                  *
 * Descr.: test file                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2006 Daniel Diaz                                     *
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
#include <locale.h>
#include <time.h>

#include "../EnginePl/gp_config.h"

#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#else
#include <io.h>
#include <process.h>
#endif

#include "ctrl_c.h"
#include "linedit.h"


#define printf LE_Printf


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MAX_SIZE 500000

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
 * CTRL_C_MANAGER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
long
Ctrl_C_Manager(int from_callback)
{
  int c;
  char prefix[100];
  char *str;
  int nb, max_lg, is_last;

  printf("\nCATCHING CTRL+C prompt length: %d   current pos: %d\n",
	 LE_Get_Prompt_Length(), LE_Get_Current_Position());

  printf("e: exit, c: continue, C: completions, w: current word: ");
  fflush(stdout);


  c = LE_Get_Key(1, 1);
  printf("\n");

  switch (c)
    {
    case 'e':
      exit(0);

    case 'w':
      LE_Get_Current_Word(prefix);
      printf("current word=<%s>\n", prefix);
      break;

    case 'C':
      printf("Enter a prefix:");
      LE_Gets(prefix);
      if ((str = LE_Compl_Init_Match(prefix, &nb, &max_lg)) == NULL)
	printf("no matching\n");
      else
	{
	  printf("common=<%s> nb=%d max_lg=%d\n", str, nb, max_lg);
	  while ((str = LE_Compl_Find_Match(&is_last)) != NULL)
	    printf("matching: <%s>\n", str);
	}
      break;
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * SET_TEST_LOCALE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Test_Locale(void)
{
  time_t ltime;
  struct tm *thetime;
  unsigned char str[100];

  setlocale(LC_ALL, "");
  time(&ltime);
  thetime = gmtime(&ltime);

  strftime(str, 100, "%d (%A) %m (%B) %Y", thetime);
  printf("Date in current locale with strftime: %s\n", str);
}



/*-------------------------------------------------------------------------*
 * MAIN                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
main(int argc, char *argv[])
{
  static char line[MAX_SIZE];
  char *p;
  char sep[100];
  int ret_val;
  int tempo = 0;

#if defined(_WIN32) && !defined(__CYGWIN__)
  setbuf(stdout, NULL);
  setbuf(stderr, NULL);
#endif

#if 1
  Install_Ctrl_C_Handler(Ctrl_C_Manager);
#endif

  Set_Test_Locale();

  if (argc > 1)
    tempo = atoi(argv[1]);

  sep[0] = '\n';
  strcpy(sep + 1, LE_Get_Separators());

  printf("enter lines (EOF to finish)\n");

#if 0
  {				/* test space overflow in WIN32 GUI console */
    int i;
    char buf[256];

    for (i = 0; i < 280; i++)
      {
	sprintf(buf,
		"line %3d tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt\n",
		i);
	printf(buf);
      }

  }
#endif

  for (;;)
    {
#ifdef __unix__
      if (tempo)
	{
	  printf("tempo %d secs to allow you to buffer some chars:\n");
	  sleep(tempo);
	}
#endif
#if 0
      printf("enter a line:");
      if (LE_Gets(line) == NULL)
#else
      if (LE_FGets(line, MAX_SIZE, "enter a line:", 1) == NULL)
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

  ret_val = 12;
  if (le_hook_exit_process)
    (*le_hook_exit_process) (ret_val);
  return ret_val;
}
