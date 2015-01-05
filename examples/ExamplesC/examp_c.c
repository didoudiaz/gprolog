/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : foreign facility test                                           *
 * File  : examp_c.c                                                       *
 * Descr.: test file - C part                                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2015 Daniel Diaz                                     *
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
PlBool
first_occurrence(char *str, PlLong c, PlLong *pos)
{
  char *p;

  p = strchr(str, c);
  if (p == NULL)		/* C does not appear in A */
    return PL_FALSE;		/* fail */

  *pos = p - str;		/* set the output argument */
  return PL_TRUE;		/* succeed */
}




/*-------------------------------------------------------------------------*
 * OCCURRENCE                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
occurrence(char *str, PlLong c, PlLong *pos)
{
  char **info_pos;
  char *p;

  info_pos = Pl_Get_Choice_Buffer(char **); /* recover the buffer */

  if (Pl_Get_Choice_Counter() == 0) /* first invocation ? */
    *info_pos = str;

  p = strchr(*info_pos, c);
  if (p == NULL)		/* C does not appear */
    {
      Pl_No_More_Choice();	/* remove choice-point */
      return PL_FALSE;		/* fail */
    }

  *pos = p - str;		/* set the output argument */
  *info_pos = p + 1;		/* update next starting pos */
  return PL_TRUE;		/* succeed */
}




/*-------------------------------------------------------------------------*
 * OCCURRENCE2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
occurrence2(char *str, PlLong c, PlLong *pos)
{
  char **info_pos;
  char *p;

  info_pos = Pl_Get_Choice_Buffer(char **); /* recover the buffer */

  if (Pl_Get_Choice_Counter() == 0) /* first invocation ? */
    {
      p = strchr(str, c);
      if (p == NULL)		/* C does not appear at all */
	{
	  Pl_No_More_Choice();	/* remove choice-point */
	  return PL_FALSE;	/* fail */
	}

      *info_pos = p;
    }
				/* info_pos = an occurrence */
  *pos = *info_pos - str;	/* set the output argument */

  p = strchr(*info_pos + 1, c);
  if (p == NULL)		/* no more occurrence */
    Pl_No_More_Choice();	/* remove choice-point */
  else
    *info_pos = p;		/* else update next solution */

  return PL_TRUE;		/* succeed */
}




/*-------------------------------------------------------------------------*
 * CHAR_ASCII                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
char_ascii(PlFIOArg *c, PlFIOArg *ascii)
{
  if (!c->is_var)		/* Char is not a variable */
    {
      ascii->unify = PL_TRUE;	/* enforce unif. of Code */
      ascii->value.l = c->value.l; /* set Code */
      return PL_TRUE;		/* succeed */
    }

  if (ascii->is_var)		/* Code is also a variable */
    Pl_Err_Instantiation();	/* emit instantiation_error */

  c->value.l = ascii->value.l;	/* set Char */
  return PL_TRUE;		/* succeed */
}




/*-------------------------------------------------------------------------*
 * CHAR_ASCII2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
char_ascii2(PlFIOArg *c, PlFIOArg *ascii)
{
  if (!c->is_var)
    {
      if (!ascii->is_var)
	return ascii->value.l == c->value.l;

      ascii->value.l = c->value.l;
      return PL_TRUE;
    }

  if (ascii->is_var)
    Pl_Err_Instantiation();

  c->value.l = ascii->value.l;
  return PL_TRUE;
}




/*-------------------------------------------------------------------------*
 * MY_CALL                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
my_call(PlTerm goal)
{
  PlTerm *args;
  int functor, arity;
  int result;

  args = Pl_Rd_Callable_Check(goal, &functor, &arity);
  Pl_Query_Begin(PL_FALSE);
  result = Pl_Query_Call(functor, arity, args);
  Pl_Query_End(PL_KEEP_FOR_PROLOG);
  if (result == PL_EXCEPTION)
    {
      PlTerm except = Pl_Get_Exception();
#if 0
      Pl_Exec_Continuation(Pl_Find_Atom("throw"), 1, &except);
#else
      Pl_Throw(except);
#endif
    }

  return result;
}




/*-------------------------------------------------------------------------*
 * MY_CALL2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
my_call2(PlTerm goal)
{
  PlTerm *args;
  int functor, arity;

  args = Pl_Rd_Callable_Check(goal, &functor, &arity);
  Pl_Exec_Continuation(functor, arity, args);
  return PL_TRUE;
}




/*-------------------------------------------------------------------------*
 * ALL_OP                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlBool
all_op(PlTerm list)
{
  PlTerm op[1024];
  PlTerm args[3];
  int n = 0;
  int result;

  Pl_Query_Begin(PL_TRUE);
  args[0] = Pl_Mk_Variable();
  args[1] = Pl_Mk_Variable();
  args[2] = Pl_Mk_Variable();
  result = Pl_Query_Call(Pl_Find_Atom("current_op"), 3, args);
  while (result)
    {
      op[n++] = Pl_Mk_Atom(Pl_Rd_Atom(args[2])); /* arg #2 is the name of the op */
      result = Pl_Query_Next_Solution();
    }
  Pl_Query_End(PL_RECOVER);

  return Pl_Un_Proper_List_Check(n, op, list);
}
