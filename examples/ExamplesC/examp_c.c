/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : foreign facility test                                           *
 * File  : for_ex_c.c                                                      *
 * Descr.: test file - C part                                              *
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

#include "gprolog.h"


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
 * FIRST_OCCURRENCE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
first_occurrence(char *str, long c, long *pos)
{
  char *p;

  p = strchr(str, c);
  if (p == NULL)		/* C does not appear in A */
    return FALSE;		/* fail */

  *pos = p - str;		/* set the output argument */
  return TRUE;			/* succeed */
}




/*-------------------------------------------------------------------------*
 * OCCURRENCE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
occurrence(char *str, long c, long *pos)
{
  char **info_pos;
  char *p;

  info_pos = Get_Choice_Buffer(char **); /* recover the buffer */

  if (Get_Choice_Counter() == 0)	/* first invocation ? */
    *info_pos = str;

  p = strchr(*info_pos, c);
  if (p == NULL)		/* C does not appear */
    {
      No_More_Choice();		/* remove choice-point */
      return FALSE;		/* fail */
    }

  *pos = p - str;		/* set the output argument */
  *info_pos = p + 1;		/* update next starting pos */
  return TRUE;			/* succeed */
}




/*-------------------------------------------------------------------------*
 * OCCURRENCE2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
occurrence2(char *str, long c, long *pos)
{
  char **info_pos;
  char *p;

  info_pos = Get_Choice_Buffer(char **); /* recover the buffer */

  if (Get_Choice_Counter() == 0) /* first invocation ? */
    {
      p = strchr(str, c);
      if (p == NULL)		/* C does not appear at all */
	{
	  No_More_Choice();	/* remove choice-point */
	  return FALSE;		/* fail */
	}

      *info_pos = p;
    }
				/* info_pos = an occurrence */
  *pos = *info_pos - str;	/* set the output argument */

  p = strchr(*info_pos + 1, c);
  if (p == NULL)		/* no more occurrence */
    No_More_Choice();		/* remove choice-point */
  else
    *info_pos = p;		/* else update next solution */

  return TRUE;			/* succeed */
}




/*-------------------------------------------------------------------------*
 * CHAR_ASCII                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
char_ascii(FIOArg *c, FIOArg *ascii)
{
  if (!c->is_var)		/* Char is not a variable */
    {
      ascii->unify = TRUE;	/* enforce unif. of Code */
      ascii->value.l = c->value.l; /* set Code */
      return TRUE;		/* succeed */
    }

  if (ascii->is_var)		/* Code is also a variable */
    Pl_Err_Instantiation();	/* emit instantiation_error */

  c->value.l = ascii->value.l;	/* set Char */
  return TRUE;			/* succeed */
}




/*-------------------------------------------------------------------------*
 * CHAR_ASCII2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
char_ascii2(FIOArg *c, FIOArg *ascii)
{
  if (!c->is_var)
    {
      if (!ascii->is_var)
	return ascii->value.l == c->value.l;

      ascii->value.l = c->value.l;
      return TRUE;
    }

  if (ascii->is_var)
    Pl_Err_Instantiation();

  c->value.l = ascii->value.l;
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * MY_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
my_call(PlTerm goal)
{
  PlTerm *args;
  int functor, arity;
  int result;

  args = Rd_Callable_Check(goal, &functor, &arity);
  Pl_Query_Begin(FALSE);
  result = Pl_Query_Call(functor, arity, args);
  Pl_Query_End(PL_KEEP_FOR_PROLOG);
  if (result == PL_EXCEPTION)
    {
      PlTerm except = Pl_Get_Exception();
      Pl_Exec_Continuation(Find_Atom("throw"), 1, &except);
    }

  return result;
}




/*-------------------------------------------------------------------------*
 * MY_CALL2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
my_call2(PlTerm goal)
{
  PlTerm *args;
  int functor, arity;

  args = Rd_Callable_Check(goal, &functor, &arity);
  Pl_Exec_Continuation(functor, arity, args);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * ALL_OP                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
all_op(PlTerm list)
{
  PlTerm op[1024];
  PlTerm args[3];
  int n = 0;
  int result;

  Pl_Query_Begin(TRUE);
  args[0] = Mk_Variable();
  args[1] = Mk_Variable();
  args[2] = Mk_Variable();
  result = Pl_Query_Call(Find_Atom("current_op"), 3, args);
  while (result)
    {
      op[n++] = Mk_Atom(Rd_Atom(args[2])); /* arg #2 is the name of the op */
      result = Pl_Query_Next_Solution();
    }
  Pl_Query_End(PL_RECOVER);

  return Un_Proper_List_Check(n, op, list);
}
