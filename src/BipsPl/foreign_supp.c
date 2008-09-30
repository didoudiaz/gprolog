/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : foreign_supp.c                                                  *
 * Descr.: foreign interface support                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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

#include <string.h>

#define OBJ_INIT Foreign_Initializer

#define FOREIGN_SUPP_FILE

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define QUERY_STACK_SIZE           128




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

long foreign_long[NB_OF_X_REGS];
double foreign_double[NB_OF_X_REGS];

long *base_fl = foreign_long;	/* overwrite var of engine.c */
double *base_fd = foreign_double;	/* overwrite var of engine.c */

static FIOArg fio_arg_array[NB_OF_X_REGS];



static WamWord *query_stack[QUERY_STACK_SIZE];
static WamWord **query_stack_top = query_stack;
static WamWord *goal_H;

WamWord *query_top_b;		/* overwrite var of throw_c.c */
WamWord query_exception;	/* overwrite var of throw_c.c */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static CodePtr Prepare_Call(int func, int arity, WamWord *arg_adr);



#define CALL_INTERNAL              X2463616C6C5F696E7465726E616C

#define PL_QUERY_RECOVER_ALT       X24706C5F71756572795F7265636F7665725F616C74

Prolog_Prototype(PL_QUERY_RECOVER_ALT, 0);
Prolog_Prototype(CALL_INTERNAL, 2);




/*-------------------------------------------------------------------------*
 * FOREIGN_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Foreign_Initializer(void)
{
  goal_H = H;
  H = H + MAX_ARITY + 1;

  Set_Heap_Actual_Start(H);	/* reserve space for meta-call goal */
}




/*-------------------------------------------------------------------------*
 * FOREIGN_CREATE_CHOICE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Foreign_Create_Choice(CodePtr codep_alt, int arity, int choice_size)
{
  A(arity) = -1;		/* bkt_counter */
  Create_Choice_Point(codep_alt, arity + 1 + choice_size);
}




/*-------------------------------------------------------------------------*
 * FOREIGN_UPDATE_CHOICE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Foreign_Update_Choice(CodePtr codep_alt, int arity, int choice_size)
{
  foreign_bkt_counter = AB(B, arity) + 1;
  AB(B, arity) = foreign_bkt_counter;

  foreign_bkt_buffer = (char *) (&(AB(B, arity + choice_size)));

  if (foreign_bkt_counter > 0)
    {
      Update_Choice_Point(codep_alt, arity);
    }
}




/*-------------------------------------------------------------------------*
 * FOREIGN_JUMP_RET                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CodePtr
Foreign_Jump_Ret(CodePtr codep)
{
  return codep;
}




/*-------------------------------------------------------------------------*
 * FOREIGN_RD_IO_ARG                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
FIOArg *
Foreign_Rd_IO_Arg(int arg_long, WamWord start_word, long (*rd_fct) (),
		  int fio_arg_index)
{
  WamWord word, tag_mask;
  FIOArg *fa = fio_arg_array + fio_arg_index;

  DEREF(start_word, word, tag_mask);

  fa->is_var = fa->unify = (tag_mask == TAG_REF_MASK);

  if (rd_fct == NULL)
    fa->value.l = (long) word;
  else if (!fa->is_var)
    {
      if (arg_long)
	{
	  fa->value.l = (*rd_fct) (word);
	  if (arg_long == 2)	/* strdup needed */
	    fa->value.s = Strdup(fa->value.s);
	}
      else
	fa->value.d = (*(double (*)()) rd_fct) (word);
    }

  return fa;
}




/*-------------------------------------------------------------------------*
 * FOREIGN_UN_IO_ARG                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Foreign_Un_IO_Arg(int arg_long, Bool (*un_fct) (), FIOArg *fa,
		  WamWord start_word)
{
  if (!fa->unify)
    return TRUE;

  if (arg_long)
    return (*un_fct) (fa->value.l, start_word);

  return (*un_fct) (fa->value.d, start_word);
}




/*-------------------------------------------------------------------------*
 * EMIT_SYNTAX_ERROR                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Emit_Syntax_Error(char *file_name, int err_line, int err_col, char *err_msg)
{
  Set_Last_Syntax_Error(file_name, err_line, err_col, err_msg);
  Syntax_Error(Flag_Value(FLAG_SYNTAX_ERROR));
}




/*-------------------------------------------------------------------------*
 * TYPE_OF_TERM                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Type_Of_Term(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return Tag_From_Tag_Mask(tag_mask);
}




/*-------------------------------------------------------------------------*
 * PREPARE_CALL                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static CodePtr
Prepare_Call(int func, int arity, WamWord *arg_adr)
{
  PredInf *pred;
  WamWord *w;
  int i;
  int bip_func, bip_arity;

  pred = Lookup_Pred(func, arity);
  if (pred == NULL || !(pred->prop & MASK_PRED_NATIVE_CODE))
    {
      if (arity == 0)
	A(0) = Tag_ATM(func);
      else
	{
	  w = goal_H;
	  A(0) = Tag_STC(w);
	  *w++ = Functor_Arity(func, arity);
	  for (i = 0; i < arity; i++)
	    *w++ = *arg_adr++;
	}

      bip_func = Get_Current_Bip(&bip_arity);
      A(1) = Tag_INT(Call_Info(bip_func, bip_arity, 0));
      return (CodePtr) Prolog_Predicate(CALL_INTERNAL, 2);
    }

  for (i = 0; i < arity; i++)
    A(i) = *arg_adr++;

  return (CodePtr) (pred->codep);
}




/*-------------------------------------------------------------------------*
 * PL_EXEC_CONTINUATION                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Exec_Continuation(int func, int arity, WamWord *arg_adr)
{
  Execute_A_Continuation(Prepare_Call(func, arity, arg_adr));
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_BEGIN                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Query_Begin(Bool recoverable)

{
  if (query_stack_top - query_stack >= QUERY_STACK_SIZE)
    Fatal_Error("too many nested Pl_Query_Start() (max: %d)",
		QUERY_STACK_SIZE);

  if (recoverable)
    Create_Choice_Point(Prolog_Predicate(PL_QUERY_RECOVER_ALT, 0), 0);
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_CALL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Query_Call(int func, int arity, WamWord *arg_adr)
{
  *query_stack_top++ = query_top_b = B;
  query_exception = atom_void;

  return Call_Prolog(Prepare_Call(func, arity, arg_adr));
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_RECOVER_ALT_0                                                  *
 *                                                                         *
 * NB: This choice-point is invoked when PL_KEEP_FOR_PROLOG is used        *
 *-------------------------------------------------------------------------*/
void
Pl_Query_Recover_Alt_0(void)
{
  Delete_Choice_Point(0);	/* remove recover choice-point */
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_NEXT_SOLUTION                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Query_Next_Solution(void)
{
  if (query_stack_top == query_stack)
    Fatal_Error("Pl_Query_Next_Solution() but no query remaining");

  query_exception = atom_void;
  return Call_Prolog_Next_Sol(query_top_b);
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_END                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Query_End(int op)
{
  WamWord *query_b, *prev_b, *b;
  Bool recoverable;


  if (query_stack_top == query_stack)
    Fatal_Error("Pl_Query_End() but no query remaining");

  query_b = *--query_stack_top;
  query_top_b = query_stack_top[-1];

  recoverable =
    (ALTB(query_b) == Prolog_Predicate(PL_QUERY_RECOVER_ALT, 0));
  prev_b = BB(query_b);

  switch (op)
    {
    case PL_RECOVER:
      Assign_B(query_b);
      if (!recoverable)
	Fatal_Error("Pl_Query_End(PL_RECOVER) but unrecoverable query");

      Delete_Choice_Point(0);	/* remove recover chc-point */
      break;

    case PL_CUT:
      Assign_B((recoverable) ? prev_b : query_b);
      break;

    default:			/* case PL_KEEP_FOR_PROLOG */
      if (recoverable)
	{
	  if (B == query_b)
	    Assign_B(prev_b);
	  else
	    for (b = B; b > query_b; b = BB(b))	/* unlink recover chc-point */
	      if (BB(b) == query_b)
		BB(b) = prev_b;
	}
      Keep_Rest_For_Prolog(query_b);
    }
}




/*-------------------------------------------------------------------------*
 * PL_GET_EXCEPTION                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Get_Exception(void)
{
  return query_exception;
}
