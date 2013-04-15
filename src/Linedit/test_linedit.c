/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : line-edit library                                               *
 * File  : test_linedit.c                                                  *
 * Descr.: test file                                                       *
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
#include <ctype.h>
#include <string.h>
#include <time.h>

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


#if defined(__unix__) || defined(__CYGWIN__)
#include <unistd.h>
#else
#include <io.h>
#include <process.h>
#endif

#include "ctrl_c.h"
#include "linedit.h"




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
PlLong
Ctrl_C_Manager(int from_callback)
{
  int c;
  char prefix[100];
  char *str;
  int nb, max_lg, is_last;

  printf("\nCATCHING CTRL+C prompt length: %d   current pos: %d\n",
         Pl_LE_Get_Prompt_Length(), Pl_LE_Get_Current_Position());

  printf("e: exit, c: continue, C: completions, w: current word: ");
  fflush(stdout);


  c = Pl_LE_Get_Key(1, 1);
  printf("\n");

  switch (c)
    {
    case 'e':
      exit(0);

    case 'w':
      Pl_LE_Get_Current_Word(prefix);
      printf("current word=<%s>\n", prefix);
      break;

    case 'C':
      printf("Enter a prefix:");
      Pl_LE_Gets(prefix);
      if ((str = Pl_LE_Compl_Init_Match(prefix, &nb, &max_lg)) == NULL)
        printf("no matching\n");
      else
        {
          printf("common=<%s> nb=%d max_lg=%d\n", str, nb, max_lg);
          while ((str = Pl_LE_Compl_Find_Match(&is_last)) != NULL)
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
  char str[100];

  /*  char c = 'é';*/

  Set_Locale();

  printf("Locale: %s\n", setlocale(LC_ALL, NULL));
  printf("Is char 233 (= %c) an alpha ? %s\n", 233, (isalpha(233) ? "YES": "NO"));

  time(&ltime);
  thetime = gmtime(&ltime);

  strftime(str, 100, "%d (%A) %m (%B) %Y", thetime);
  printf("Date in current locale with strftime: %s\n", str);
  printf("Float should be independent from locale pi: %f\n", 3.1415);
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
  Pl_Install_Ctrl_C_Handler(Ctrl_C_Manager);
#endif

  Set_Test_Locale();

  if (argc > 1)
    tempo = atoi(argv[1]);

  sep[0] = '\n';
  strcpy(sep + 1, Pl_LE_Get_Separators());

  printf("enter lines (EOF to finish)\n");

#if 0
  {                             /* test space overflow in WIN32 GUI console */
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
      if (Pl_LE_Gets(line) == NULL)
#else
        if (Pl_LE_FGets(line, MAX_SIZE, "enter a line:", 1) == NULL)
#endif
          break;
      printf("Line:(%s) len:%d\n", line, strlen(line));
      for (p = line; (p = strtok(p, sep)) != NULL; p = NULL)
        {
          printf("adding word (%s) for completion\n", p);
          Pl_LE_Compl_Add_Word(strdup(p), strlen(p));
        }
#if 0 // test an exception (under Win32 relaunch automatically the main !!! why ?
      if (*line == 'k' && line[1] == '\0')
        {
          *(int *) 12 = 45;
        }
#endif
    }

  printf("End of testing\n");

  ret_val = 12;
  if (pl_le_hook_exit_process)
    (*pl_le_hook_exit_process) (ret_val);
  return ret_val;
}
