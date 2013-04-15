/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : assert_c.c                                                      *
 * Descr.: dynamic predicate management - C part                           *
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


#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static DynCInf *last_clause;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Clause_Alt(DynCInf *clause, WamWord *w);

static Bool Retract_Alt(DynCInf *clause, WamWord *w);




/*-------------------------------------------------------------------------*
 * PL_ASSERT_5                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Assert_5(WamWord head_word, WamWord body_word,
	    WamWord asserta_word, WamWord check_perm_word, WamWord pl_file_word)
{
  Bool asserta = Pl_Rd_Integer(asserta_word);
  Bool check_perm = Pl_Rd_Integer(check_perm_word);
  int pl_file = Pl_Rd_Atom(pl_file_word);

  last_clause = Pl_Add_Dynamic_Clause(head_word, body_word, asserta,
				      check_perm, pl_file);
}




/*-------------------------------------------------------------------------*
 * PL_CLAUSE_3                                                             *
 *                                                                         *
 * for_what=0 for clause/2  (ie. error if not public)                      *
 *          2 for listing/1 (no check if public)                           *
 *                listing/1 tests before that it is not a native_code pred.*
 *-------------------------------------------------------------------------*/
Bool
Pl_Clause_3(WamWord head_word, WamWord body_word, WamWord for_what_word)
{
  WamWord word, tag_mask;
  WamWord *first_arg_adr;
  WamWord head_word1, body_word1;
  int func, arity;
  int for_what;
  PredInf *pred;
  DynPInf *dyn;
  DynCInf *clause;
  WamWord w[2];

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  DEREF(body_word, word, tag_mask);
  body_word = word;
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_callable, body_word);

  for_what = Pl_Rd_Integer_Check(for_what_word);

#ifdef DEBUG
  DBGPRINTF("clause/2: arity: %d", arity);
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
    }
  DBGPRINTF("\n");
#endif

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  if ((for_what == 0 && !(pred->prop & MASK_PRED_PUBLIC)) ||
      (for_what == 2 && (pred->prop & MASK_PRED_NATIVE_CODE)))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_access,
			pl_permission_type_private_procedure, word);
    }

  dyn = (DynPInf *) (pred->dyn);
  if (dyn == NULL)		/* no dynamic info */
    return FALSE;

  if (arity > 0)
    word = *first_arg_adr;

  w[0] = head_word;
  w[1] = body_word;

  clause = Pl_Scan_Dynamic_Pred(-1, 0, (DynPInf *) (pred->dyn), word,
				(ScanFct) Clause_Alt, DYN_ALT_FCT_FOR_TEST, 2,
				w);
  if (clause == NULL)
    return FALSE;

  Pl_Copy_Clause_To_Heap(clause, &head_word1, &body_word1);
  last_clause = clause;
  return Pl_Unify(head_word, head_word1) && Pl_Unify(body_word, body_word1);
}




/*-------------------------------------------------------------------------*
 * CLAUSE_ALT                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Clause_Alt(DynCInf *clause, WamWord *w)
{
  WamWord head_word1, body_word1;

  Pl_Copy_Clause_To_Heap(clause, &head_word1, &body_word1);
  last_clause = clause;
  return Pl_Unify(head_word1, w[0]) && Pl_Unify(body_word1, w[1]);
}




/*-------------------------------------------------------------------------*
 * PL_RETRACT_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Retract_2(WamWord head_word, WamWord body_word)
{
  WamWord word, tag_mask;
  WamWord *first_arg_adr;
  WamWord head_word1, body_word1;
  int func, arity;
  PredInf *pred;
  DynPInf *dyn;
  DynCInf *clause;
  WamWord w[2];

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  DEREF(body_word, word, tag_mask);
  body_word = word;
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_callable, body_word);

#ifdef DEBUG
  DBGPRINTF("retract/2: arity: %d", arity);
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
    }
  DBGPRINTF("\n");
#endif

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
    return FALSE;

  if (!(pred->prop & MASK_PRED_DYNAMIC))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  dyn = (DynPInf *) (pred->dyn);
  if (dyn == NULL)		/* no dynamic info */
    return FALSE;

  if (arity > 0)
    word = *first_arg_adr;

  w[0] = head_word;
  w[1] = body_word;

  clause = Pl_Scan_Dynamic_Pred(-1, 0, (DynPInf *) (pred->dyn), word,
				(ScanFct) Retract_Alt, DYN_ALT_FCT_FOR_TEST, 2,
				w);
  if (clause == NULL)
    return FALSE;

  Pl_Copy_Clause_To_Heap(clause, &head_word1, &body_word1);

  if (!Pl_Unify(head_word, head_word1) || !Pl_Unify(body_word, body_word1))
    return FALSE;

  Pl_Delete_Dynamic_Clause(clause);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * RETRACT_ALT                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Retract_Alt(DynCInf *clause, WamWord *w)
{
  WamWord head_word1, body_word1;

  Pl_Copy_Clause_To_Heap(clause, &head_word1, &body_word1);
  if (!Pl_Unify(head_word1, w[0]) || !Pl_Unify(body_word1, w[1]))
    return FALSE;

  Pl_Delete_Dynamic_Clause(clause);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_RETRACT_LAST_FOUND_0                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Retract_Last_Found_0(void)
{
  Pl_Delete_Dynamic_Clause(last_clause);
}




/*-------------------------------------------------------------------------*
 * SETARG_Of_LAST_FOUND_2                                                  *
 *                                                                         *
 * update in place the ArgNo th argument of last_clause. NewValue must be  *
 * a 1-tagged word data (atom, integer).                                   *
 *-------------------------------------------------------------------------*/
void
Pl_Setarg_Of_Last_Found_2(WamWord arg_no_word, WamWord new_value_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int arg_no;

  arg_no = Pl_Rd_Integer(arg_no_word) - 1;

  DEREF(last_clause->head_word, word, tag_mask);
  adr = UnTag_Address(word);

  DEREF(new_value_word, word, tag_mask);

  Arg(adr, arg_no) = word;
}




/*-------------------------------------------------------------------------*
 * PL_RETRACTALL_IF_EMPTY_HEAD_1                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Retractall_If_Empty_Head_1(WamWord head_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *arg_adr;
  int func, arity;
  PredInf *pred;
  WamWord *ref_adr[MAX_ARITY];
  int i, j;
  Bool ret;

  arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL)
    return TRUE;

  if (!(pred->prop & MASK_PRED_DYNAMIC))
    {
      word = Pl_Put_Structure(ATOM_CHAR('/'), 2);
      Pl_Unify_Atom(func);
      Pl_Unify_Integer(arity);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  ret = TRUE;			/* check if all args are singletons variables */
  for (i = 0; i < arity; i++)
    {
      DEREF(*arg_adr, word, tag_mask);
      if (tag_mask != TAG_REF_MASK)
	{
	  ret = FALSE;		/* not a var */
	  break;
	}
      adr = UnTag_REF(word);
      ref_adr[i] = adr;
      *adr = Tag_INT(0);	/* patch the argument to an INT */
      arg_adr++;
    }

  j = i;
  for (i = 0; i < j; i++)	/* restore the args */
    {
      adr = ref_adr[i];
      *adr = Make_Self_Ref(adr);
    }

  if (ret)
    Pl_Update_Dynamic_Pred(func, arity, 1, -1);

  return ret;
}




/*-------------------------------------------------------------------------*
 * PL_ABOLISH_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Abolish_1(WamWord pred_indic_word)
{
  int func, arity;

  func = Pl_Get_Pred_Indicator(pred_indic_word, TRUE, &arity);

  Pl_Update_Dynamic_Pred(func, arity, 3, -1);
}




/*-------------------------------------------------------------------------*
 * PL_REMOVE_PREDICATE_2                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Remove_Predicate_2(WamWord name_word, WamWord arity_word)
{
  int func, arity;

  func = Pl_Rd_Atom_Check(name_word);
  arity = Pl_Rd_Integer_Check(arity_word);

  Pl_Update_Dynamic_Pred(func, arity, 2, -1);
}
