/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : top_level_c.c                                                   *
 * Descr.: top Level - C part                                              *
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

#include <string.h>
#include <stdlib.h>
#include <signal.h>

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/



	  /* Error Messages */

#define ERR_DEBUGGER_NOT_FOUND     "top_level_c: debug/trace not found"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define ABORT                      X61626F7274

#define BREAK                      X627265616B

Prolog_Prototype(ABORT, 0);
Prolog_Prototype(BREAK, 0);

static void Ctrl_C_Handler(int sig);




/*-------------------------------------------------------------------------*
 * SET_CTRL_C_HANDLER_0                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Ctrl_C_Handler_0(void)
{
  signal(SIGINT, Ctrl_C_Handler);
  signal(SIGQUIT, Ctrl_C_Handler);
}




/*-------------------------------------------------------------------------*
 * CTRL_C_HANDLER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Ctrl_C_Handler(int sig)
{
  StmInf *pstm = stm_tbl + stm_top_level_output;
  PredInf *pred;
  int c;

#ifndef M_ix86_win32
  sigset_t set;
#endif

  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);

  Reset_Prolog_In_Signal();

start:
  Stream_Printf(pstm, "\nProlog interruption (h for help) ? ");
  Stream_Flush(pstm);

  c = Stream_Getc_No_Echo(stm_tbl + stm_top_level_input);
  Stream_Putc('\n', pstm);

  signal(SIGINT, Ctrl_C_Handler);
  signal(SIGQUIT, Ctrl_C_Handler);

#ifndef M_ix86_win32
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  sigaddset(&set, SIGQUIT);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
#endif

  switch (c)
    {
    case 'a':			/* abort */
      Execute_A_Continuation(Prolog_Predicate(ABORT, 0));
      break;

    case 'b':			/* break */
      Call_Prolog(Prolog_Predicate(BREAK, 0));
      goto start;
      break;

    case 'c':			/* continue */
      break;

    case 'e':			/* exit */
      exit(0);

    case 't':			/* trace */
    case 'd':			/* debug */
      if (SYS_VAR_DEBUGGER)
	{
	  pred =
	    Lookup_Pred(Create_Atom((c == 't') ? "trace" : "debug"), 0);
	  if (pred == NULL)
	    Fatal_Error(ERR_DEBUGGER_NOT_FOUND);	/* should not occur */

	  Call_Prolog((CodePtr) pred->codep);
	  break;
	}

    default:			/* help */
      Stream_Printf(pstm, "   a  abort        b  break\n");
      Stream_Printf(pstm, "   c  continue     e  exit\n");
      if (SYS_VAR_DEBUGGER)
	Stream_Printf(pstm, "   d  debug        t  trace\n");
      Stream_Printf(pstm, "  h/? help\n");
      goto start;
    }
}
