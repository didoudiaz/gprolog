/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : expand_c.c                                                      *
 * Descr.: expand term management - C part                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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

#include <sys/types.h>

#define OBJ_INIT Expand_Initializer

#include "engine_pl.h"
#include "bips_pl.h"

#ifndef _WIN32
#include <unistd.h>
#include <sys/wait.h>
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord *top;
static Bool opt_term_unif;

static int atom_clause;
static int atom_phrase;
static int atom_if;

static WamWord dcg_2;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static WamWord Dcg_Head(WamWord dcg_head_word, WamWord *in_word,
			WamWord *out_word, WamWord **end_lst_adr);

static WamWord Dcg_Body(WamWord dcg_body_word, Bool for_alt,
			WamWord in_word, WamWord out_word, 
			WamWord *end_lst_adr);

static void Dcg_Body_On_Stack(WamWord dcg_body_word, WamWord in_word,
			      WamWord out_word);

static void Dcg_Term_List_On_Stack(WamWord *lst_adr, WamWord in_word,
				   WamWord out_word);

static WamWord Dcg_Compound2(int func, WamWord w1, WamWord w2);




/*-------------------------------------------------------------------------*
 * EXPAND_INITIALIZER                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Expand_Initializer(void)
{
  int atom_dcg;

  atom_dcg = Create_Atom("-->");
  atom_clause = Create_Atom(":-");
  atom_phrase = Create_Atom("phrase");
  atom_if = Create_Atom("->");

  dcg_2 = Functor_Arity(atom_dcg, 2);
}




/*-------------------------------------------------------------------------*
 * DCG_TRANS_RULE_2                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Dcg_Trans_Rule_2(WamWord rule_word, WamWord clause_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord in_word, out_word;
  WamWord head_word, body_word;
  WamWord *end_lst_adr;

  DEREF(rule_word, word, tag_mask);
  adr = UnTag_STC(word);
  if (tag_mask != TAG_STC_MASK || Functor_And_Arity(adr) != dcg_2)
    return FALSE;

  top = Local_Top;		/* use local stack for the stack */
  opt_term_unif = TRUE;

  head_word = Dcg_Head(Arg(adr, 0), &in_word, &out_word, &end_lst_adr);
  body_word = Dcg_Body(Arg(adr, 1), FALSE, in_word, out_word, end_lst_adr);

  Get_Structure(atom_clause, 2, clause_word);
  Unify_Value(head_word);
  Unify_Value(body_word);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * DCG_TRANS_BODY_4                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Dcg_Trans_Body_4(WamWord dcg_body_word, WamWord in_word, WamWord out_word,
		 WamWord body_word)
{
  top = Local_Top;		/* use local stack for the stack */
  opt_term_unif = TRUE;

  in_word = Globalize_If_In_Local(in_word); 
  out_word = Globalize_If_In_Local(out_word);

  return Unify(body_word,
	       Dcg_Body(dcg_body_word, FALSE, in_word, out_word, NULL));
}




/*-------------------------------------------------------------------------*
 * DCG_HEAD                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Dcg_Head(WamWord dcg_head_word, WamWord *in_word,
	 WamWord *out_word, WamWord **end_lst_adr)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *save_H, *p;
  int func, arity;
  Bool first;

  first = TRUE;
  *end_lst_adr = NULL;

start:
  adr = Rd_Callable_Check(dcg_head_word, &func, &arity);

  if (first && arity == 2 && func == ATOM_CHAR(','))
    {
      first = FALSE;
      dcg_head_word = *adr++;
      DEREF(*adr, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word != NIL_WORD)
	{
	  if (tag_mask != TAG_LST_MASK)
	    Pl_Err_Type(type_list, word);
	  
	  *end_lst_adr = UnTag_LST(word);
	}
      goto start;
    }

  p = save_H = H;
  *p++ = Functor_Arity(func, arity + 2);
  while (arity--)
    *p++ = *adr++;
  adr = p;
  *p++ = *in_word = Make_Self_Ref(adr);
  adr = p;
  *p++ = *out_word = Make_Self_Ref(adr);
  H = p;

  return Tag_STC(save_H);
}




/*-------------------------------------------------------------------------*
 * DCG_BODY                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Dcg_Body(WamWord dcg_body_word, Bool in_alt, WamWord in_word,
	 WamWord out_word, WamWord *end_lst_adr)
{
  WamWord new_out_word, word;
  WamWord *save_H, *p;
  WamWord *save_top = top;
  Bool save_opt_term_unif = opt_term_unif;
  WamWord *base;

  if (end_lst_adr)
    goto new_out_var;

  if (in_alt)
    {
      top++;
    new_out_var:
      new_out_word = Mk_Variable();
    }
  else
    new_out_word = out_word;

  base = top;
  Dcg_Body_On_Stack(dcg_body_word, in_word, new_out_word);
  if (end_lst_adr)
    Dcg_Term_List_On_Stack(end_lst_adr, out_word, new_out_word);
  else if (in_alt)
    {
      if (Blt_Term_Eq(in_word, new_out_word))
	*--base = Dcg_Compound2(ATOM_CHAR('='), new_out_word, out_word);
      else
	Unify(new_out_word, out_word);
    }

  if (top == base)
    {
      word = Tag_ATM(atom_true);
      goto finish;
    }

  word = *--top;
  while (top > base)
    {
      p = save_H = H;
      *p++ = Functor_Arity(ATOM_CHAR(','), 2);
      *p++ = *--top;
      *p++ = word;
      H = p;
      word = Tag_STC(save_H);
    }

finish:
  top = save_top;
  opt_term_unif = save_opt_term_unif;

  return word;
}




/*-------------------------------------------------------------------------*
 * DCG_BODY_ON_STACK                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Dcg_Body_On_Stack(WamWord dcg_body_word, WamWord in_word, WamWord out_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord w1, w2;
  WamWord *save_H, *p;
  int func, arity;

  DEREF(dcg_body_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      func = atom_phrase;
      arity = 1;
      goto non_term;
    }

  if (word == NIL_WORD)
    {
    in_is_out:
      Unify(in_word, out_word);
      return;
    }

  if (tag_mask == TAG_LST_MASK)
    {
      Dcg_Term_List_On_Stack(UnTag_LST(word), in_word, out_word);
      return;
    }

  adr = Rd_Callable_Check(word, &func, &arity);

  if (arity != 2 || func != ATOM_CHAR(','))
    opt_term_unif = FALSE;

  if (arity == 2)
    {
      if (func == ATOM_CHAR(','))
	{
	  word = Mk_Variable();
	  Dcg_Body_On_Stack(*adr++, in_word, word);
	  Dcg_Body_On_Stack(*adr, word, out_word);
	  return;
	}

      if (func == atom_if)
	{
	  word = Mk_Variable();
	  w1 = Dcg_Body(*adr++, FALSE, in_word, word, NULL);
	  w2 = Dcg_Body(*adr, FALSE, word, out_word, NULL);

	  *top++ = Dcg_Compound2(func, w1, w2);
	  return;
	}

      if (func == ATOM_CHAR(';'))
	{
	  w1 = Dcg_Body(*adr++, TRUE, in_word, out_word, NULL);
	  w2 = Dcg_Body(*adr, TRUE, in_word, out_word, NULL);

	  *top++ = Dcg_Compound2(func, w1, w2);
	  return;
	}
    }

  if (arity == 0 && func == ATOM_CHAR('!'))
    {
      *top++ = dcg_body_word;
      goto in_is_out;
    }

  if (arity == 1 && func == atom_curly_brackets)
    {
      *top++ = *adr;
      goto in_is_out;
    }


  /* other callable term = non terminal */
non_term:
  p = save_H = H;
  *p++ = Functor_Arity(func, arity + 2);
  while (arity--)
    *p++ = *adr++;
  *p++ = in_word;
  *p++ = out_word;
  H = p;
  *top++ = Tag_STC(save_H);
}




/*-------------------------------------------------------------------------*
 * DCG_TERM_LIST_ON_STACK                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Dcg_Term_List_On_Stack(WamWord *lst_adr, WamWord in_word, WamWord out_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *save_lst_adr = lst_adr;
  WamWord *save_H, *p;

  p = save_H = H;
  for (;;)
    {
      *p++ = Car(lst_adr);

      DEREF(Cdr(lst_adr), word, tag_mask);
      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, Tag_LST(save_lst_adr));

      lst_adr = UnTag_LST(word);
      adr = p + 1;
      *p++ = Tag_LST(adr);
    }
  *p++ = out_word;
  H = p;
  word = Tag_LST(save_H);

  if (opt_term_unif)
    Unify(in_word, word);
  else
    {
      opt_term_unif = TRUE;
      *top++ = Dcg_Compound2(ATOM_CHAR('='), in_word, word);
    }
}




/*-------------------------------------------------------------------------*
 * DCG_COMPOUND2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Dcg_Compound2(int func, WamWord w1, WamWord w2)
{
  WamWord *save_H, *p;

  p = save_H = H;
  *p++ = Functor_Arity(func, 2);
  *p++ = w1;
  *p++ = w2;
  H = p;
  return Tag_STC(save_H);
}
