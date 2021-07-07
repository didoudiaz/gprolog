/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : assert_c.c                                                      *
 * Descr.: dynamic predicate management - C part                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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
 * PL_ASSERT_4                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Assert_4(WamWord clause_word, WamWord asserta_word, 
	    WamWord check_perm_word, WamWord pl_file_word)
{
  WamWord head_word, body_word;
  int module;
  Bool asserta = Pl_Rd_Integer(asserta_word);
  Bool check_perm = Pl_Rd_Integer(check_perm_word);
  int pl_file = Pl_Rd_Atom(pl_file_word);

  module = Pl_Get_Head_And_Body_Top(clause_word, &head_word, &body_word);

  body_word = Pl_Term_To_Goal(body_word, module, NOT_A_WAM_WORD);

  last_clause = Pl_Add_Dynamic_Clause(module, head_word, body_word, asserta, check_perm, pl_file);
}




/*-------------------------------------------------------------------------*
 * RETRACT                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Retract(int module, WamWord head_word, WamWord body_word, Bool retractall)
{
  WamWord word, tag_mask;
  WamWord *first_arg_adr;
  WamWord head_word1, body_word1;
  int func, arity;
  PredInf *pred;
  DynCInf *clause;
  WamWord w[2];

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  DEREF(body_word, word, tag_mask);
  body_word = word;
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_callable, body_word);

#ifdef DEBUG
  DBGPRINTF("retract(all)/1: arity: %d", arity);
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
    }
  DBGPRINTF("\n");
#endif

  if ((pred = Pl_Lookup_Pred(module, func, arity)) == NULL)
    return FALSE;

  if (!(pred->prop & MASK_PRED_DYNAMIC))
    { /* dont use module here: can be != pred->mod->module (ex: foo:p/n can refer to user:p/n) */
      word = Pl_Build_Pred_Indic_Error(pred);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  if (pred->dyn == NULL)	/* no dynamic info */
    return FALSE;

  if (arity > 0)
    word = *first_arg_adr;

  w[0] = head_word;
  w[1] = body_word;

  clause = Pl_Scan_Dynamic_Pred(pred, word, -1, -1, 0, (ScanFct) Retract_Alt, 
				(retractall) ? DYN_ALT_FCT_FOR_FAIL : DYN_ALT_FCT_FOR_TEST, 2, w);
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
 * PL_RETRACT_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Retract_1(WamWord clause_word)
{
  WamWord head_word, body_word;
  int module;
  
  module = Pl_Get_Head_And_Body_Top(clause_word, &head_word, &body_word);

  return Retract(module, head_word, body_word, FALSE);
}





/*-------------------------------------------------------------------------*
 * PL_RETRACTALL_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Retractall_1(WamWord head_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *arg_adr;
  int module, func, arity;
  PredInf *pred;
  WamWord *ref_adr[MAX_ARITY];
  int i, j;
  Bool all_singleton;

  module = Pl_Strip_Module_Top(head_word, FALSE, TRUE, &head_word);

  arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  /* if pred does not exist, retractall implicitly creates it as a dynamic pred. */
  if ((pred = Pl_Lookup_Pred(module, func, arity)) == NULL)
    pred = Pl_Create_Dynamic_Pred(module, func, arity);
  
  if (!(pred->prop & MASK_PRED_DYNAMIC))
    { /* NB: module can be != pred->mod->module (ex: foo:p/n can refer to user:p/n) */
      word = Pl_Build_Pred_Indic_Error(pred);
      Pl_Err_Permission(pl_permission_operation_modify,
			pl_permission_type_static_procedure, word);
    }

  all_singleton = TRUE;			/* check if all args are singleton variables */
  for (i = 0; i < arity; i++)
    {
      DEREF(*arg_adr, word, tag_mask);
      if (tag_mask != TAG_REF_MASK)
	{
	  all_singleton = FALSE;		/* not a var */
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

  if (all_singleton)
    {
      Pl_Update_Dynamic_Pred(module, func, arity, 1, -1); /* remove all clauses for this pred */
      return;
    }

  Retract(module, head_word, Pl_Put_X_Variable(), TRUE); /* act like retract(p(...)), fail (failure in assert.pl) */
}




/*-------------------------------------------------------------------------*
 * PL_CLAUSE_3                                                             *
 *                                                                         *
 * check_public = TRUE  for for clause/2 (ie. error if not public)         *
 *              = FALSE for listing/1 (no check if public)                 *
 *                      NB: listing/1 tests before it is not native_code   *
 *-------------------------------------------------------------------------*/
Bool
Pl_Clause_3(WamWord head_word, WamWord body_word, WamWord check_public_word)
{
  WamWord word, tag_mask;
  WamWord *first_arg_adr;
  WamWord head_word1, body_word1;
  int module, func, arity;
  PredInf *pred;
  DynCInf *clause;
  WamWord w[2];
  Bool check_public = Pl_Rd_Boolean(check_public_word);

  module = Pl_Strip_Module_Top(head_word, FALSE, TRUE, &head_word);

  first_arg_adr = Pl_Rd_Callable_Check(head_word, &func, &arity);

  DEREF(body_word, word, tag_mask);
  body_word = word;
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK &&
      tag_mask != TAG_LST_MASK && tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_callable, body_word);

#ifdef DEBUG
  DBGPRINTF("clause/2: arity: %d", arity);
  if (arity > 0)
    {
      DBGPRINTF("\tfirst arg: ");
      Pl_Write(*first_arg_adr);
    }
  DBGPRINTF("\n");
#endif

  if ((pred = Pl_Lookup_Pred_Visible(module, func, arity)) == NULL)
    return FALSE;
  
  if (check_public && !(pred->prop & MASK_PRED_PUBLIC))
    {
      word = Pl_Build_Pred_Indic_Error(pred);
      Pl_Err_Permission(pl_permission_operation_access,
			pl_permission_type_private_procedure, word);
    }

  if (pred->dyn == NULL)	/* no dynamic info */
    return FALSE;

  if (arity > 0)
    word = *first_arg_adr;

  w[0] = head_word;
  w[1] = body_word;

  clause = Pl_Scan_Dynamic_Pred(pred, word, -1, -1, 0,
				(ScanFct) Clause_Alt, DYN_ALT_FCT_FOR_TEST, 2, w);
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




#if 0				/* OLD CODE */
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
 * SETARG_OF_LAST_FOUND_2                                                  *
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

#endif



/*-------------------------------------------------------------------------*
 * PL_ABOLISH_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Abolish_1(WamWord pred_indic_word)
{
  int module, func, arity;

  module = Pl_Get_Pred_Indicator_Top(pred_indic_word, FALSE, &func, &arity);

  Pl_Update_Dynamic_Pred(module, func, arity, 3, -1);
}
