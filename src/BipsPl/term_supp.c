/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_supp.c                                                     *
 * Descr.: term support                                                    *
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

#define TERM_SUPP_FILE

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

	  /* copy term variables */

static WamWord *base_copy;

static WamWord vars[MAX_VAR_IN_TERM * 2];	/* needs 2 words for a variable */
static WamWord *end_vars = vars + MAX_VAR_IN_TERM * 2;
static WamWord *top_vars;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Copy_Term_Rec(WamWord *dst_adr, WamWord *src_adr, WamWord **p);




/*-------------------------------------------------------------------------*
 * TERM_COMPARE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Term_Compare(WamWord start_u_word, WamWord start_v_word)
{
  WamWord u_word, u_tag, *u_adr;
  WamWord v_word, v_tag, *v_adr;
  int u_func, u_arity;
  WamWord *u_arg_adr;
  int v_func, v_arity;
  WamWord *v_arg_adr;
  int i, x;
  double d1, d2;


  Deref(start_u_word, u_word, u_tag, u_adr);
  Deref(start_v_word, v_word, v_tag, v_adr);

  switch (u_tag)
    {
    case REF:
      return (v_tag != REF) ? -1 : u_adr - v_adr;

    case FDV:
      if (v_tag == REF)
	return 1;

      return (v_tag != FDV) ? -1 : UnTag_FDV(u_word) - UnTag_FDV(v_word);

    case FLT:
      if (v_tag == REF || v_tag == FDV)
	return 1;

      if (v_tag != FLT)
	return -1;

      d1 = Obtain_Float(UnTag_FLT(u_word));
      d2 = Obtain_Float(UnTag_FLT(v_word));
      return (d1 < d2) ? -1 : (d1 == d2) ? 0 : 1;


    case INT:
      if (v_tag == REF || v_tag == FDV || v_tag == FLT)
	return 1;

      return (v_tag != INT) ? -1 : u_word - v_word;

    case ATM:
      if (v_tag == REF || v_tag == FDV || v_tag == FLT || v_tag == INT)
	return 1;

      return (v_tag != ATM) ? -1 : strcmp(atom_tbl[UnTag_ATM(u_word)].name,
					  atom_tbl[UnTag_ATM(v_word)].name);
    }

  /* u_tag==LST/STC */

  v_arg_adr = Rd_Compound(v_word, &v_func, &v_arity);
  if (v_arg_adr == NULL)	/* v_tag!=LST/STC */
    return 1;

  u_arg_adr = Rd_Compound(u_word, &u_func, &u_arity);

  if (u_arity != v_arity)
    return u_arity - v_arity;

  if (u_func != v_func)
    return strcmp(atom_tbl[u_func].name, atom_tbl[v_func].name);

  for (i = 0; i < u_arity; i++)
    if ((x = Term_Compare(*u_arg_adr++, *v_arg_adr++)) != 0)
      return x;

  return 0;
}




/*-------------------------------------------------------------------------*
 * TREAT_VARS_OF_TERM                                                      *
 *                                                                         *
 * Call fct for each variable found in a term.                             *
 *-------------------------------------------------------------------------*/
void
Treat_Vars_Of_Term(WamWord start_word, Bool generic_var, void (*fct) ())
{
  WamWord word, tag, *adr;
  int i;

terminal_rec:

  Deref(start_word, word, tag, adr);

  switch (tag)
    {
    case FDV:
      if (!generic_var)
	break;
      adr = UnTag_FDV(word);
    case REF:
      (*fct) (adr, word);
      break;

    case LST:
      adr = UnTag_LST(word);
      adr = &Car(adr);
      Treat_Vars_Of_Term(*adr++, generic_var, fct);

      start_word = *adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      i = Arity(adr);
      adr = &Arg(adr, 0);
      while (--i)
	Treat_Vars_Of_Term(*adr++, generic_var, fct);

      start_word = *adr;
      goto terminal_rec;

    default:
      break;
    }
}




/*-------------------------------------------------------------------------*
 * LIST_LENGTH                                                             *
 *                                                                         *
 * returns the length of a list or < 0 if not a list:                      *
 * -1: instantation error                                                  *
 * -2: type error (type_list)                                              *
 *-------------------------------------------------------------------------*/
int
List_Length(WamWord start_word)
{
  WamWord word, tag, *adr;
  int n = 0;

  for (;;)
    {
      Deref(start_word, word, tag, adr);

      if (word == NIL_WORD)
	return n;

      if (tag == REF)
	return -1;

      if (tag != LST)
	return -2;

      n++;
      adr = UnTag_LST(word);
      start_word = Cdr(adr);
    }
}




/*-------------------------------------------------------------------------*
 * TERM_SIZE                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Term_Size(WamWord start_word)
{
  WamWord word, tag, *adr;
  int i;
  int n = 0;			/* init to zero for terminal_rec */

terminal_rec:

  Deref(start_word, word, tag, adr);

  switch (tag)
    {
    case FDV:			/* 1+ for <REF,->fdv_adr> since Dont_Separate_Tag(FDV) */
      return n + 1 + Fd_Variable_Size(UnTag_FDV(word));

    case FLT:
#if WORD_SIZE==32
      return n + 1 + 2;
#else
      return n + 1 + 1;
#endif

    case LST:
      adr = UnTag_LST(word);
      adr = &Car(adr);
      n += 1 + Term_Size(*adr++);
      start_word = *adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      n += 2;			/* tagged word + f_n */

      i = Arity(adr);
      adr = &Arg(adr, 0);
      while (--i)
	n += Term_Size(*adr++);

      start_word = *adr;
      goto terminal_rec;

    default:
      return n + 1;
    }
}




/*-------------------------------------------------------------------------*
 * COPY_TERM                                                               *
 *                                                                         *
 * Copy a non contiguous term, the result is a contiguous term.            *
 *-------------------------------------------------------------------------*/
void
Copy_Term(WamWord *dst_adr, WamWord *src_adr)
{
  WamWord *qtop, *base;
  WamWord *p;

  base_copy = dst_adr++;

  base = top_vars = vars;

  Copy_Term_Rec(base_copy, src_adr, &dst_adr);

  /* restore original self references */
  qtop = top_vars;
  while (qtop != base)
    {
      p = (WamWord *) (*--qtop);	/* address to restore */
      *p = *--qtop;		/* word    to restore */
    }
}




/*-------------------------------------------------------------------------*
 * COPY_TERM_REC                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Copy_Term_Rec(WamWord *dst_adr, WamWord *src_adr, WamWord **p)
{
  WamWord word, tag, *adr;
  WamWord *q;
  int i;

terminal_rec:

  Deref(*src_adr, word, tag, adr);

  switch (tag)
    {
    case REF:
      q = *p;
      if (adr < q && adr >= base_copy)	/* already a copy */
	{
	  *dst_adr = word;
	  return;
	}

      if (top_vars >= end_vars)
	Pl_Err_Representation(representation_too_many_variables);

      *top_vars++ = word;	/* word to restore    */
      *top_vars++ = (WamWord) adr;	/* address to restore */
      *adr = *dst_adr = Tag_Value(REF, dst_adr);	/* bind to a new copy */
      return;

    case FDV:
      adr = UnTag_FDV(word);
      q = *p;
      if (adr < q && adr >= base_copy)	/* already a copy */
	{
	  *dst_adr = Tag_Value(REF, adr);	/* since Dont_Separate_Tag(FDV) */
	  return;
	}

      if (top_vars >= end_vars)
	Pl_Err_Representation(representation_too_many_variables);

      *top_vars++ = word;	/* word to restore    */
      *top_vars++ = (WamWord) adr;	/* address to restore */
      q = *p;
      *p = q + Fd_Copy_Variable(q, adr);
      *adr = *dst_adr = Tag_Value(REF, q);	/* bind to a new copy */
      return;

    case FLT:
      adr = UnTag_FLT(word);
      q = *p;
      q[0] = adr[0];
#if WORD_SIZE==32
      q[1] = adr[1];
      *p = q + 2;
#else
      *p = q + 1;
#endif
      *dst_adr = Tag_Value(FLT, q);
      return;

    case LST:
      adr = UnTag_LST(word);
      q = *p;
      *dst_adr = Tag_Value(LST, q);

      *p = &Cdr(q) + 1;
      q = &Car(q);
      adr = &Car(adr);
      Copy_Term_Rec(q++, adr++, p);

      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      q = *p;
      *dst_adr = Tag_Value(STC, q);

      Functor_And_Arity(q) = Functor_And_Arity(adr);

      i = Arity(adr);
      *p = &Arg(q, i - 1) + 1;

      q = &Arg(q, 0);
      adr = &Arg(adr, 0);
      while (--i)
	Copy_Term_Rec(q++, adr++, p);

      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    default:
      *dst_adr = word;
      return;
    }
}




/*-------------------------------------------------------------------------*
 * COPY_CONTIGUOUS_TERM                                                    *
 *                                                                         *
 * Copy a contiguous term (dereferenced), the result is a contiguous term. *
 *-------------------------------------------------------------------------*/
void
Copy_Contiguous_Term(WamWord *dst_adr, WamWord *src_adr)
#define Old_Adr_To_New_Adr(adr)  ((dst_adr)+((adr)-(src_adr)))
{
  WamWord word, *adr;
  WamWord *q;
  int i;

terminal_rec:

  word = *src_adr;

  switch (Tag_Of(word))
    {
    case REF:
      adr = UnTag_REF(word);
      q = Old_Adr_To_New_Adr(adr);
      *dst_adr = Tag_Value(REF, q);
      if (adr > src_adr)	/* only useful for Dont_Separate_Tag() */
	Copy_Contiguous_Term(q, adr);
      return;

    case FDV:
      adr = UnTag_FDV(word);
      Fd_Copy_Variable(dst_adr, adr);
      return;

    case FLT:
      adr = UnTag_FLT(word);
      q = Old_Adr_To_New_Adr(adr);
      q[0] = adr[0];
#if WORD_SIZE==32
      q[1] = adr[1];
#endif
      *dst_adr = Tag_Value(FLT, q);
      return;

    case LST:
      adr = UnTag_LST(word);
      q = Old_Adr_To_New_Adr(adr);
      *dst_adr = Tag_Value(LST, q);
      q = &Car(q);
      adr = &Car(adr);
      Copy_Contiguous_Term(q++, adr++);
      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      q = Old_Adr_To_New_Adr(adr);
      *dst_adr = Tag_Value(STC, q);

      Functor_And_Arity(q) = Functor_And_Arity(adr);

      i = Arity(adr);

      q = &Arg(q, 0);
      adr = &Arg(adr, 0);
      while (--i)
	Copy_Contiguous_Term(q++, adr++);

      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    default:
      *dst_adr = word;
      return;
    }
}




/*-------------------------------------------------------------------------*
 * GET_PRED_INDICATOR                                                      *
 *                                                                         *
 * returns the functor and initializes the arity of the predicate indicator*
 * func= -1 if its a variable, arity= -1 if a variable                     *
 *-------------------------------------------------------------------------*/
int
Get_Pred_Indicator(WamWord pred_indic_word, Bool must_be_ground, int *arity)
{
  WamWord word, tag, *adr;
  int func;

  Deref(pred_indic_word, word, tag, adr);
  if (tag == REF && must_be_ground)
    Pl_Err_Instantiation();

  if (!Get_Structure(ATOM_CHAR('/'), 2, pred_indic_word))
    {
      if (!Flag_Value(FLAG_STRICT_ISO) &&
	  Rd_Callable(word, &func, arity) != NULL)
	return func;
    pi_error:
      Pl_Err_Type(type_predicate_indicator, pred_indic_word);
    }

  pi_name_word = Unify_Variable();
  pi_arity_word = Unify_Variable();

  if (must_be_ground)
    func = Rd_Atom_Check(pi_name_word);
  else
    {
      Deref(pi_name_word, word, tag, adr);
      if (tag == REF)
	func = -1;
      else
	{
	  if (tag != ATM)
	    goto pi_error;
	  func = UnTag_ATM(word);
	}
    }

  if (must_be_ground)
    {
      *arity = Rd_Positive_Check(pi_arity_word);

      if (*arity > MAX_ARITY)
	Pl_Err_Representation(representation_max_arity);
    }
  else
    {
      Deref(pi_arity_word, word, tag, adr);
      if (tag == REF)
	*arity = -1;
      else
	{
	  if (tag != INT)
	    goto pi_error;

	  *arity = UnTag_INT(word);
	  if (*arity < 0 || *arity > MAX_ARITY)
	    goto pi_error;
	}
    }

  return func;
}




/*-------------------------------------------------------------------------*
 * GET_PRED_INDIC_3                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Pred_Indic_3(WamWord pred_indic_word, WamWord func_word,
		 WamWord arity_word)
{
  int func, arity;

  func = Get_Pred_Indicator(pred_indic_word, TRUE, &arity);

  return Get_Atom(func, func_word) && Get_Integer(arity, arity_word);
}




/*-------------------------------------------------------------------------*
 * FLOAT_TO_STRING                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Float_To_String(double d)
{
  char *p, *q, *e;
  static char buff[32];

  sprintf(buff, "%#.17g", d);	/* a . with 16 significant digits */

  p = buff;			/* skip leading blanks */
  while (*p == ' ')
    p++;

  if (p != buff)		/* remove leading blanks */
    {
      q = buff;
      while ((*q++ = *p++))
	;
    }

  p = strchr(buff, '.');
  if (p == NULL)		/* if p==NULL then NaN or +/-inf (ignore) */
    return buff;

  if (p[1] == '\0')		/* a dot but no decimal numbers */
    {
      strcat(buff, "0");
      return buff;
    }

  e = strchr(buff, 'e');	/* search exposant part */
  if (e == NULL)
    e = buff + strlen(buff);
  p = e - 1;
  while (*p == '0')
    p--;

  q = (*p == '.') ? p + 2 : p + 1;	/* but keep at least one 0 */

  if (q != e)
    while ((*q++ = *e++))	/* move exposant part */
      ;

  return buff;
}
