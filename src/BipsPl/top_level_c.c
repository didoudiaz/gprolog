/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : top_level_c.c                                                   *
 * Descr.: top Level - C part                                              *
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


#include "gp_config.h"

#ifndef NO_USE_LINEDIT
#include "ctrl_c.h"
#else
#include "../Linedit/ctrl_c.c"	/* must be included before other .h... */
#endif

#include "engine_pl.h"
#include "bips_pl.h"
#include <string.h>
#include <stdlib.h>
#include <signal.h>



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

static long Ctrl_C_Manager(int from_callback);




/*-------------------------------------------------------------------------*
 * SET_CTRL_C_HANDLER_0                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Ctrl_C_Handler_0(void)
{
  Install_Ctrl_C_Handler(Ctrl_C_Manager);
}




/*-------------------------------------------------------------------------*
 * CTRL_C_MANAGER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static long
Ctrl_C_Manager(int from_callback)
{
  StmInf *pstm = stm_tbl[stm_top_level_output];
  PredInf *pred;
  int c;
  CodePtr to_execute;

  Reset_Prolog_In_Signal();

start:
  Stream_Printf(pstm, "\nProlog interruption (h for help) ? ");
  Stream_Flush(pstm);

  c = Stream_Get_Key(stm_tbl[stm_top_level_input], TRUE, FALSE);
  Stream_Putc('\n', pstm);

  switch (c)
    {
    case 'a':			/* abort */
      to_execute = Prolog_Predicate(ABORT, 0);
      if (from_callback)
	return (long) to_execute;
      Execute_A_Continuation(to_execute);
      break;

    case 'b':			/* break */
      Call_Prolog(Prolog_Predicate(BREAK, 0));
      goto start;
      break;

    case 'c':			/* continue */
      break;

    case 'e':			/* exit */
      Exit_With_Value(0);

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
  return 0;
}
