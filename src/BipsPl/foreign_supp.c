/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : foreign_supp.c                                                  *
 * Descr.: foreign interface support                                       *
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

PlLong pl_foreign_long[NB_OF_X_REGS];
double pl_foreign_double[NB_OF_X_REGS];

PlLong *pl_base_fl = pl_foreign_long;	/* overwrite var of engine.c */
double *pl_base_fd = pl_foreign_double;	/* overwrite var of engine.c */

static PlFIOArg fio_arg_array[NB_OF_X_REGS];



static WamWord *query_stack[QUERY_STACK_SIZE];
static WamWord **query_stack_top = query_stack;
static WamWord *goal_H;

WamWord *pl_query_top_b;		/* overwrite var of throw_c.c */
WamWord pl_query_exception;		/* overwrite var of throw_c.c */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static CodePtr Prepare_Call(int func, int arity, WamWord *arg_adr);



#define CALL_INTERNAL              X1_2463616C6C5F696E7465726E616C
#define THROW_INTERNAL             X1_247468726F775F696E7465726E616C

#define PL_QUERY_RECOVER_ALT       X1_24706C5F71756572795F7265636F7665725F616C74

Prolog_Prototype(CALL_INTERNAL, 2);
Prolog_Prototype(THROW_INTERNAL, 2);

Prolog_Prototype(PL_QUERY_RECOVER_ALT, 0);




/*-------------------------------------------------------------------------*
 * FOREIGN_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Foreign_Initializer(void)
{
  goal_H = H;
  H = H + MAX_ARITY + 1;

  Pl_Set_Heap_Actual_Start(H);	/* reserve space for meta-call goal */
}




/*-------------------------------------------------------------------------*
 * PL_FOREIGN_CREATE_CHOICE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Foreign_Create_Choice(CodePtr codep_alt, int arity, int choice_size)
{
  A(arity) = -1;		/* bkt_counter */
  Pl_Create_Choice_Point(codep_alt, arity + 1 + choice_size);
}




/*-------------------------------------------------------------------------*
 * PL_FOREIGN_UPDATE_CHOICE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Foreign_Update_Choice(CodePtr codep_alt, int arity, int choice_size)
{
  pl_foreign_bkt_counter = AB(B, arity) + 1;
  AB(B, arity) = pl_foreign_bkt_counter;

  pl_foreign_bkt_buffer = (char *) (&(AB(B, arity + choice_size)));

  if (pl_foreign_bkt_counter > 0)
    {
      Pl_Update_Choice_Point(codep_alt, arity);
    }
}




/*-------------------------------------------------------------------------*
 * PL_FOREIGN_JUMP_RET                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
CodePtr
Pl_Foreign_Jump_Ret(CodePtr codep)
{
  return codep;
}




/*-------------------------------------------------------------------------*
 * PL_FOREIGN_RD_IO_ARG                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlFIOArg *
Pl_Foreign_Rd_IO_Arg(int arg_long, WamWord start_word, PlLong (*rd_fct) (),
		     int fio_arg_index)
{
  WamWord word, tag_mask;
  PlFIOArg *fa = fio_arg_array + fio_arg_index;

  DEREF(start_word, word, tag_mask);

  fa->is_var = fa->unify = (tag_mask == TAG_REF_MASK);

  if (rd_fct == NULL)
    fa->value.l = (PlLong) word;
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
 * PL_FOREIGN_UN_IO_ARG                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Foreign_Un_IO_Arg(int arg_long, Bool (*un_fct) (), PlFIOArg *fa,
		     WamWord start_word)
{
  if (!fa->unify)
    return TRUE;

  if (arg_long)
    return (*un_fct) (fa->value.l, start_word);

  return (*un_fct) (fa->value.d, start_word);
}




/*-------------------------------------------------------------------------*
 * PL_EMIT_SYNTAX_ERROR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Emit_Syntax_Error(char *file_name, int err_line, int err_col, char *err_msg)
{
  Pl_Set_Last_Syntax_Error(file_name, err_line, err_col, err_msg);
  Pl_Syntax_Error(Flag_Value(syntax_error));
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

  pred = Pl_Lookup_Pred(func, arity);
  if (pred == NULL || !(pred->prop & MASK_PRED_NATIVE_CODE) || 
      (pred->prop & MASK_PRED_CONTROL_CONSTRUCT))
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

      bip_func = Pl_Get_Current_Bip(&bip_arity);
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
  Pl_Execute_A_Continuation(Prepare_Call(func, arity, arg_adr));
}




/*-------------------------------------------------------------------------*
 * PL_THROW                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Throw(WamWord ball_word)
{
  int bip_func, bip_arity;

  bip_func = Pl_Get_Current_Bip(&bip_arity);

  A(0) = ball_word;
  A(1) = Tag_INT(Call_Info(bip_func, bip_arity, 0));
  Pl_Execute_A_Continuation(Prolog_Predicate(THROW_INTERNAL, 2));
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_BEGIN                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Query_Begin(Bool recoverable)

{
  if (query_stack_top - query_stack >= QUERY_STACK_SIZE)
    Pl_Fatal_Error("too many nested Pl_Query_Start() (max: %d)",
		QUERY_STACK_SIZE);

  if (recoverable)
    Pl_Create_Choice_Point(Prolog_Predicate(PL_QUERY_RECOVER_ALT, 0), 0);
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_CALL                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Query_Call(int func, int arity, WamWord *arg_adr)
{
  *query_stack_top++ = pl_query_top_b = B;
  pl_query_exception = pl_atom_void;

  return Pl_Call_Prolog(Prepare_Call(func, arity, arg_adr));
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_START                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Query_Start(int func, int arity, WamWord *arg_adr, Bool recoverable)
{
  Pl_Query_Begin(recoverable);
  return  Pl_Query_Call(func, arity, arg_adr);
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_RECOVER_ALT_0                                                  *
 *                                                                         *
 * NB: This choice-point is invoked when PL_KEEP_FOR_PROLOG is used        *
 *-------------------------------------------------------------------------*/
void
Pl_Query_Recover_Alt_0(void)
{
  Pl_Delete_Choice_Point(0);	/* remove recover choice-point */
}




/*-------------------------------------------------------------------------*
 * PL_QUERY_NEXT_SOLUTION                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Query_Next_Solution(void)
{
  if (query_stack_top == query_stack)
    Pl_Fatal_Error("Pl_Query_Next_Solution() but no query remaining");

  pl_query_exception = pl_atom_void;
  return Pl_Call_Prolog_Next_Sol(pl_query_top_b);
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
    Pl_Fatal_Error("Pl_Query_End() but no query remaining");

  query_b = *--query_stack_top;
  pl_query_top_b = query_stack_top[-1];

  recoverable =
    (ALTB(query_b) == Prolog_Predicate(PL_QUERY_RECOVER_ALT, 0));
  prev_b = BB(query_b);

  switch (op)
    {
    case PL_RECOVER:
      Assign_B(query_b);
      if (!recoverable)
	Pl_Fatal_Error("Pl_Query_End(PL_RECOVER) but unrecoverable query");

      Pl_Delete_Choice_Point(0);	/* remove recover chc-point */
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
      Pl_Keep_Rest_For_Prolog(query_b);
    }
}




/*-------------------------------------------------------------------------*
 * PL_GET_EXCEPTION                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Get_Exception(void)
{
  return pl_query_exception;
}


/*
 * The following functions are defined here to have a minimal gprolog.h
 */


/*-------------------------------------------------------------------------*
 * PL_NO_MORE_CHOICE                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_No_More_Choice(void)
{
  Delete_Last_Choice_Point();
}




/*-------------------------------------------------------------------------*
 * PL_TYPE_OF_TERM                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Type_Of_Term(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);

  return Tag_From_Tag_Mask(tag_mask);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_NAME                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Atom_Name(int atom)
{
  return pl_atom_tbl[atom].name;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_LENGTH                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Atom_Length(int atom)
{
  return pl_atom_tbl[atom].prop.length;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_NEEDS_QUOTE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Needs_Quote(int atom)
{
  return pl_atom_tbl[atom].prop.needs_quote;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_NEEDS_SCAN                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Atom_Needs_Scan(int atom)
{
  return pl_atom_tbl[atom].prop.needs_scan;
}



/*-------------------------------------------------------------------------*
 * PL_IS_VALID_ATOM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Is_Valid_Atom(int atom)
{
  return Is_Valid_Atom(atom);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_CHAR                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int Pl_Atom_Char(char c)
{
  return ATOM_CHAR(c);
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_NIL                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int Pl_Atom_Nil(void)
{
  return ATOM_NIL;
}



/*-------------------------------------------------------------------------*
 * PL_ATOM_FALSE                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int Pl_Atom_False(void)
{
  return pl_atom_false;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_TRUE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int Pl_Atom_True(void)
{
  return pl_atom_true;
}




/*-------------------------------------------------------------------------*
 * PL_ATOM_END_OF_FILE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int Pl_Atom_End_Of_File(void)
{
  return pl_atom_end_of_file;
}



/*-------------------------------------------------------------------------*
 * PL_UNIF                                                                 *
 *                                                                         *
 * do not use directly Pl_Unify because of FC (fast call)                  *
 *-------------------------------------------------------------------------*/
PlBool
Pl_Unif(PlTerm term1, PlTerm term2)
{
  return Pl_Unify(term1, term2);
}




/*-------------------------------------------------------------------------*
 * PL_UNIF_WITH_OCCURS_CHECK                                               *
 *                                                                         *
 * do not use directly Pl_Unify_Occurs_Check because of FC (fast call)     *
 *-------------------------------------------------------------------------*/
PlBool
Pl_Unif_With_Occurs_Check(PlTerm term1, PlTerm term2)
{
  return Pl_Unify_Occurs_Check(term1, term2);
}








/*-------------------------------------------------------------------------*
 * PL_BUILTIN_VAR                                                          *
 *                                                                         *
 * do not use directly Pl_Blt_Var because of FC (fast call)                *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Var(WamWord term)
{
  return Pl_Blt_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_NON_VAR                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Non_Var because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Non_Var(WamWord term)
{
  return Pl_Blt_Non_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_ATOM                                                         *
 *                                                                         *
 * do not use directly Pl_Blt_Atom because of FC (fast call)               *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Atom(WamWord term)
{
  return Pl_Blt_Atom(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_INTEGER                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Integer because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Integer(WamWord term)
{
  return Pl_Blt_Integer(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_FLOAT                                                        *
 *                                                                         *
 * do not use directly Pl_Blt_Float because of FC (fast call)              *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Float(WamWord term)
{
  return Pl_Blt_Float(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_NUMBER                                                       *
 *                                                                         *
 * do not use directly Pl_Blt_Number because of FC (fast call)             *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Number(WamWord term)
{
  return Pl_Blt_Number(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_ATOMIC                                                       *
 *                                                                         *
 * do not use directly Pl_Blt_Atomic because of FC (fast call)             *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Atomic(WamWord term)
{
  return Pl_Blt_Atomic(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_COMPOUND                                                     *
 *                                                                         *
 * do not use directly Pl_Blt_Compound because of FC (fast call)           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Compound(WamWord term)
{
  return Pl_Blt_Compound(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_Callable                                                     *
 *                                                                         *
 * do not use directly Pl_Blt_Callable because of FC (fast call)           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Callable(WamWord term)
{
  return Pl_Blt_Callable(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_FD_VAR                                                       *
 *                                                                         *
 * do not use directly Pl_Blt_Fd_Var because of FC (fast call)             *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Fd_Var(WamWord term)
{
  return Pl_Blt_Fd_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_NON_FD_VAR                                                   *
 *                                                                         *
 * do not use directly Pl_Blt_Non_Fd_Var because of FC (fast call)         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Non_Fd_Var(WamWord term)
{
  return Pl_Blt_Non_Fd_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_GENERIC_VAR                                                  *
 *                                                                         *
 * do not use directly Pl_Blt_Generic_Var because of FC (fast call)        *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Generic_Var(WamWord term)
{
  return Pl_Blt_Generic_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_NON_GENERIC_VAR                                              *
 *                                                                         *
 * do not use directly Pl_Blt_Non_Generic_Var because of FC (fast call)    *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Non_Generic_Var(WamWord term)
{
  return Pl_Blt_Non_Generic_Var(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_LIST                                                         *
 *                                                                         *
 * do not use directly Pl_Blt_List because of FC (fast call)               *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_List(WamWord term)
{
  return Pl_Blt_List(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_PARTIAL_LIST                                                 *
 *                                                                         *
 * do not use directly Pl_Blt_Partial_List because of FC (fast call)       *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Partial_List(WamWord term)
{
  return Pl_Blt_Partial_List(term);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_LIST_OR_PARTIAL_LIST                                         *
 *                                                                         *
 * do not use directly Pl_Blt_List_Or_Partial_List because of FC(fast call)*
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_List_Or_Partial_List(WamWord term)
{
  return Pl_Blt_List_Or_Partial_List(term);
}



/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_EQ                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Eq because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Eq(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Eq(term1, term2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_NEQ                                                     *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Neq because of FC (fast call)           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Neq(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Neq(term1, term2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_LT                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Lt because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Lt(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Lt(term1, term2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_LTE                                                     *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Lte because of FC (fast call)           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Lte(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Lte(term1, term2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_GT                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Gt because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Gt(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Gt(term1, term2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_TERM_GTE                                                     *
 *                                                                         *
 * do not use directly Pl_Blt_Term_Gte because of FC (fast call)           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Term_Gte(WamWord term1, WamWord term2)
{
  return Pl_Blt_Term_Gte(term1, term2);
}



/*-------------------------------------------------------------------------*
 * PL_BUILTIN_COMPARE                                                      *
 *                                                                         *
 * do not use directly Pl_Blt_Compare because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Compare(WamWord cmp_word, WamWord x, WamWord y)
{
  return Pl_Blt_Compare(cmp_word, x, y);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_ARG                                                          *
 *                                                                         *
 * do not use directly Pl_BLT_Arg because of FC (fast call)                *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word)
{
  return Pl_Blt_Arg(arg_no_word, term_word, sub_term_word);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_FUNCTOR                                                      *
 *                                                                         *
 * do not use directly Pl_BLT_Functor because of FC (fast call)            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Functor(WamWord term_word, WamWord functor_word, WamWord arity_word)
{
  return Pl_Blt_Functor(term_word, functor_word, arity_word);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_UNIV                                                         *
 *                                                                         *
 * do not use directly Pl_BLT_Univ because of FC (fast call)               *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Univ(WamWord term_word, WamWord list_word)
{
  return Pl_Blt_Univ(term_word, list_word);
}



/*-------------------------------------------------------------------------*
 * PL_BUILTIN_EQ                                                           *
 *                                                                         *
 * do not use directly Pl_Blt_Eq because of FC (fast call)                 *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Eq(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Eq(expr1, expr2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_NEQ                                                          *
 *                                                                         *
 * do not use directly Pl_Blt_Neq because of FC (fast call)                *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Neq(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Neq(expr1, expr2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_LT                                                           *
 *                                                                         *
 * do not use directly Pl_Blt_Lt because of FC (fast call)                 *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Lt(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Lt(expr1, expr2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_LTE                                                          *
 *                                                                         *
 * do not use directly Pl_Blt_Lte because of FC (fast call)                *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Lte(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Lte(expr1, expr2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_GT                                                           *
 *                                                                         *
 * do not use directly Pl_Blt_Gt because of FC (fast call)                 *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Gt(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Gt(expr1, expr2);
}


/*-------------------------------------------------------------------------*
 * PL_BUILTIN_GTE                                                          *
 *                                                                         *
 * do not use directly Pl_Blt_Gte because of FC (fast call)                *
 *-------------------------------------------------------------------------*/
Bool
Pl_Builtin_Gte(WamWord expr1, WamWord expr2)
{
  return Pl_Blt_Gte(expr1, expr2);
}



/*-------------------------------------------------------------------------*
 * PL_MATH_EVALUATE                                                        *
 *                                                                         *
 * do not use directly Pl_Math_Load_Value because of FC (fast call)        *
 *-------------------------------------------------------------------------*/
void
Pl_Math_Evaluate(WamWord expr, WamWord *result)
{
  Pl_Math_Load_Value(expr, result);
}
