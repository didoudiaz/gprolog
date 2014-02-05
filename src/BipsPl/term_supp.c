/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : term_supp.c                                                     *
 * Descr.: term support                                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2014 Daniel Diaz                                     *
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

#define OBJ_INIT Term_Supp_Initializer

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


	/* term to goal variables */

static WamWord t2g_term_word;
static WamWord t2g_module_word;
static WamWord t2g_call_info_word;


	/* atoms */

static int atom_call;
static int atom_call_internal;
static int atom_clause;
static int atom_if;
static int atom_soft_if;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Copy_Term_Rec(WamWord *dst_adr, WamWord *src_adr, WamWord **p);

static Bool Term_Hash(WamWord start_word, PlLong depth, unsigned *hash);

static Bool Term_Hash_Rec(WamWord start_word, PlLong depth, HashIncrInfo *hi);




/*-------------------------------------------------------------------------*
 * TERM_SUPP_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Term_Supp_Initializer(void)
{
  atom_call = Pl_Create_Atom("call");
  atom_call_internal = Pl_Create_Atom("$call_internal");
  atom_clause = Pl_Create_Atom(":-");
  atom_if = Pl_Create_Atom("->");
  atom_soft_if = Pl_Create_Atom("*->");
}




/*-------------------------------------------------------------------------*
 * PL_TERM_COMPARE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PlLong Pl_Term_Compare(WamWord start_u_word, WamWord start_v_word)
{
  WamWord u_word, u_tag_mask;
  WamWord v_word, v_tag_mask;
  WamWord u_tag, v_tag;
  int u_func, u_arity;
  WamWord *u_arg_adr;
  int v_func, v_arity;
  WamWord *v_arg_adr;
  int i, x;
  double d1, d2;

  DEREF(start_u_word, u_word, u_tag_mask);
  DEREF(start_v_word, v_word, v_tag_mask);

  u_tag = Tag_From_Tag_Mask(u_tag_mask);
  v_tag = Tag_From_Tag_Mask(v_tag_mask);

  switch (u_tag)
    {
    case REF:
      return (v_tag != REF) ? -1 :  UnTag_REF(u_word) - UnTag_REF(v_word);

#ifndef NO_USE_FD_SOLVER
    case FDV:
      if (v_tag == REF)
	return 1;

      return (v_tag != FDV) ? -1 : UnTag_FDV(u_word) - UnTag_FDV(v_word);
#endif

    case FLT:
      if (v_tag == REF
#ifndef NO_USE_FD_SOLVER
	  || v_tag == FDV
#endif
	  )
	return 1;

      if (v_tag != FLT)
	return -1;

      d1 = Pl_Obtain_Float(UnTag_FLT(u_word));
      d2 = Pl_Obtain_Float(UnTag_FLT(v_word));
      return (d1 < d2) ? -1 : (d1 == d2) ? 0 : 1;


    case INT:
      if (v_tag == REF ||
#ifndef NO_USE_FD_SOLVER
	  v_tag == FDV ||
#endif
	  v_tag == FLT)
	return 1;

      return (v_tag != INT) ? -1 : UnTag_INT(u_word) - UnTag_INT(v_word);

    case ATM:
      if (v_tag == REF ||
#ifndef NO_USE_FD_SOLVER
	  v_tag == FDV ||
#endif
	  v_tag == FLT || v_tag == INT)
	return 1;

      return (v_tag != ATM) ? -1 : strcmp(pl_atom_tbl[UnTag_ATM(u_word)].name,
					  pl_atom_tbl[UnTag_ATM(v_word)].name);
    }

				/* u_tag == LST / STC */

  v_arg_adr = Pl_Rd_Compound(v_word, &v_func, &v_arity);
  if (v_arg_adr == NULL)	/* v_tag != LST / STC */
    return 1;

  u_arg_adr = Pl_Rd_Compound(u_word, &u_func, &u_arity);

  if (u_arity != v_arity)
    return u_arity - v_arity;

  if (u_func != v_func)
    return strcmp(pl_atom_tbl[u_func].name, pl_atom_tbl[v_func].name);

  for (i = 0; i < u_arity; i++)
    if ((x = Pl_Term_Compare(*u_arg_adr++, *v_arg_adr++)) != 0)
      return x;

  return 0;
}




/*-------------------------------------------------------------------------*
 * PL_TREAT_VARS_OF_TERM                                                   *
 *                                                                         *
 * Call fct for each variable found in a term.                             *
 *-------------------------------------------------------------------------*/
Bool
Pl_Treat_Vars_Of_Term(WamWord start_word, Bool generic_var, Bool (*fct) ())
{
  WamWord word, tag_mask;
  WamWord *adr;
  int i;

terminal_rec:

  DEREF(start_word, word, tag_mask);

  switch (Tag_Of(word))
    {
    case REF:
      if (!(*fct) (UnTag_REF(word), word))
	return FALSE;
      break;

#ifndef NO_USE_FD_SOLVER
    case FDV:
      if (generic_var)
	if (!(*fct) (UnTag_FDV(word), word))
	  return FALSE;
      break;
#endif

    case LST:
      adr = UnTag_LST(word);
      adr = &Car(adr);
      if (!Pl_Treat_Vars_Of_Term(*adr++, generic_var, fct))
	return FALSE;

      start_word = *adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      i = Arity(adr);
      adr = &Arg(adr, 0);
      while (--i)
	if (!Pl_Treat_Vars_Of_Term(*adr++, generic_var, fct))
	  return FALSE;

      start_word = *adr;
      goto terminal_rec;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_LIST_LENGTH                                                          *
 *                                                                         *
 * returns the length of a list or < 0 if not a list:                      *
 * -1: instantation error                                                  *
 * -2: type error (type_list)                                              *
 *-------------------------------------------------------------------------*/
int
Pl_List_Length(WamWord start_word)
{
  WamWord word, tag_mask;
  int n = 0;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	return n;

      if (tag_mask == TAG_REF_MASK)
	return -1;

      if (tag_mask != TAG_LST_MASK)
	return -2;

      n++;
      start_word = Cdr(UnTag_LST(word));
    }
}




/*-------------------------------------------------------------------------*
 * PL_TERM_SIZE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Term_Size(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int i;
  int n = 0;			/* init to zero for terminal_rec */

terminal_rec:

  DEREF(start_word, word, tag_mask);

  switch (Tag_From_Tag_Mask(tag_mask))
    {
#ifndef NO_USE_FD_SOLVER
    case FDV:		/* 1+ for <REF,->fdv_adr> since Dont_Separate_Tag */
      return n + 1 + Fd_Variable_Size(UnTag_FDV(word));
#endif

    case FLT:
#if WORD_SIZE == 32
      return n + 1 + 2;
#else
      return n + 1 + 1;
#endif

    case LST:
      adr = UnTag_LST(word);
      adr = &Car(adr);
      n += 1 + Pl_Term_Size(*adr++);
      start_word = *adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      n += 2;			/* tagged word + f_n */

      i = Arity(adr);
      adr = &Arg(adr, 0);
      while (--i)
	n += Pl_Term_Size(*adr++);

      start_word = *adr;
      goto terminal_rec;

    default:
      return n + 1;
    }
}




/*-------------------------------------------------------------------------*
 * PL_COPY_TERM                                                            *
 *                                                                         *
 * Copy a non contiguous term, the result is a contiguous term.            *
 *-------------------------------------------------------------------------*/
void
Pl_Copy_Term(WamWord *dst_adr, WamWord *src_adr)
{
  WamWord *qtop, *base;
  WamWord *p;
/* fix_bug is because when gcc sees &xxx where xxx is a fct argument variable
 * it allocates a frame even with -fomit-frame-pointer.
 * This corrupts ebp on ix86 */
  static WamWord *fix_bug;

  base_copy = dst_adr++;

  base = top_vars = vars;

  fix_bug = dst_adr;
  Copy_Term_Rec(base_copy, src_adr, &fix_bug);

				/* restore original self references */
  qtop = top_vars;
  while (qtop != base)
    {
      p = (WamWord *) (*--qtop);	/* address to restore */
      *p = *--qtop;		        /* word    to restore */
    }
}




/*-------------------------------------------------------------------------*
 * COPY_TERM_REC                                                           *
 *                                                                         *
 * p is the next address to use to store the rest of a term.               *
 *-------------------------------------------------------------------------*/
static void
Copy_Term_Rec(WamWord *dst_adr, WamWord *src_adr, WamWord **p)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord *q;
  int i;

terminal_rec:

  DEREF(*src_adr, word, tag_mask);

  switch (Tag_From_Tag_Mask(tag_mask))
    {
    case REF:
      adr = UnTag_REF(word);
      q = *p;
      if (adr < q && adr >= base_copy)	/* already a copy */
	{
	  *dst_adr = word;
	  return;
	}

      if (top_vars >= end_vars)
	Pl_Err_Representation(pl_representation_too_many_variables);

      *top_vars++ = word;	                /* word to restore    */
      *top_vars++ = (WamWord) adr;	        /* address to restore */
      *adr = *dst_adr = Tag_REF(dst_adr);	/* bind to a new copy */
      return;

#ifndef NO_USE_FD_SOLVER
    case FDV:
      adr = UnTag_FDV(word);
      q = *p;
      if (adr < q && adr >= base_copy)	/* already a copy */
	{
	  *dst_adr = Tag_REF(adr);	/* since Dont_Separate_Tag */
	  return;
	}

      if (top_vars >= end_vars)
	Pl_Err_Representation(pl_representation_too_many_variables);

      *top_vars++ = word;	        /* word to restore    */
      *top_vars++ = (WamWord) adr;	/* address to restore */
      q = *p;
      *p = q + Fd_Copy_Variable(q, adr);
      *adr = *dst_adr = Tag_REF(q);	/* bind to a new copy */
      return;
#endif

    case FLT:
      adr = UnTag_FLT(word);
      q = *p;
      q[0] = adr[0];
#if WORD_SIZE == 32
      q[1] = adr[1];
      *p = q + 2;
#else
      *p = q + 1;
#endif
      *dst_adr = Tag_FLT(q);
      return;

    case LST:
      adr = UnTag_LST(word);
      q = *p;
      *dst_adr = Tag_LST(q);

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
      *dst_adr = Tag_STC(q);

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
 * PL_COPY_CONTIGUOUS_TERM                                                 *
 *                                                                         *
 * Copy a contiguous term (dereferenced), the result is a contiguous term. *
 *-------------------------------------------------------------------------*/
void
Pl_Copy_Contiguous_Term(WamWord *dst_adr, WamWord *src_adr)
#define Old_Adr_To_New_Adr(adr)  ((dst_adr) + ((adr) - (src_adr)))
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
      *dst_adr = Tag_REF(q);
      if (adr > src_adr)	/* only useful for Dont_Separate_Tag */
	Pl_Copy_Contiguous_Term(q, adr);
      return;

#ifndef NO_USE_FD_SOLVER
    case FDV:
      adr = UnTag_FDV(word);
      Fd_Copy_Variable(dst_adr, adr);
      return;
#endif

    case FLT:
      adr = UnTag_FLT(word);
      q = Old_Adr_To_New_Adr(adr);
      q[0] = adr[0];
#if WORD_SIZE == 32
      q[1] = adr[1];
#endif
      *dst_adr = Tag_FLT(q);
      return;

    case LST:
      adr = UnTag_LST(word);
      q = Old_Adr_To_New_Adr(adr);
      *dst_adr = Tag_LST(q);
      q = &Car(q);
      adr = &Car(adr);
      Pl_Copy_Contiguous_Term(q++, adr++);
      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      q = Old_Adr_To_New_Adr(adr);
      *dst_adr = Tag_STC(q);

      Functor_And_Arity(q) = Functor_And_Arity(adr);

      i = Arity(adr);

      q = &Arg(q, 0);
      adr = &Arg(adr, 0);
      while (--i)
	Pl_Copy_Contiguous_Term(q++, adr++);

      dst_adr = q;
      src_adr = adr;
      goto terminal_rec;

    default:
      *dst_adr = word;
      return;
    }
}




/*-------------------------------------------------------------------------*
 * PL_ACYCLIC_TERM_1                                                       *
 *                                                                         *
 * This implementation is not very satisfactory because:                   *
 * - it does not handle terminal recursion (useful for lists).             *
 * - it does not take into account sharing.                                *
 * However, it is simple and enough until a full support for cyclic term   *
 * is implemented (at least in the unification).                           *
 *-------------------------------------------------------------------------*/
Bool
Pl_Acyclic_Term_1(WamWord start_word)

#define MARK Tag_LST(0)

{
  WamWord word, tag_mask;
  WamWord word1;
  WamWord *adr;
  int arity;
  Bool ret;

  DEREF(start_word, word, tag_mask);

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      arity = 2;
      adr = &Car(adr);
    }
  else if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_STC(word);
      arity = Arity(adr);
      adr = &Arg(adr, 0);
    }
  else
    return TRUE;


  while (--arity >= 0)
    {
      word1 = *adr;
      DEREF(word1, word, tag_mask);

      if (word == MARK)	/* marked = cyclic */
	return FALSE;

      if (tag_mask == TAG_LST_MASK || tag_mask == TAG_STC_MASK)
	{
	  *adr = MARK;		/* mark it */

	  ret = Pl_Acyclic_Term_1(word1);
      
	  *adr = word1;		/* unmark it */
	  if (!ret)
	    return FALSE;
	}
      adr++;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * TERM_HASH                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static
Bool Term_Hash(WamWord start_word, PlLong depth, unsigned *hash)
{
  HashIncrInfo hi;

  Pl_Hash_Incr_Init(&hi);

  if (depth != 0 && !Term_Hash_Rec(start_word, depth, &hi))
    return FALSE;

  *hash = Pl_Hash_Incr_Term(&hi);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * TERM_HASH_REC                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool 
Term_Hash_Rec(WamWord start_word, PlLong depth, HashIncrInfo *hi)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int func, arity;

terminal_rec:

  /* here depth is != 0 this is checked before (recursive) call */

  /* NB: the depth-- should be done inside the terminal_rec label (not before !)
   * here it is only done for lists and structures (since atomic terms do not need it)
   */

  DEREF(start_word, word, tag_mask);

  switch (Tag_From_Tag_Mask(tag_mask))
    {
    case REF:
#ifndef NO_USE_FD_SOLVER
    case FDV:
#endif
      return FALSE;

    case ATM:
      Pl_Hash_Incr_Int32(hi, pl_atom_tbl[UnTag_ATM(word)].hash);
      break;

    case INT:
      Pl_Hash_Incr_Int64(hi, UnTag_INT(word));
      break;

    case FLT:
      Pl_Hash_Incr_Double(hi, Pl_Obtain_Float(UnTag_FLT(word)));
      break;

      /* For faster list hasing we simply hash Car and then Cdr
       * h([a,b]) != h([a,[b]]) since h(a), h(b), h([]) != h(a), h(b), h([]), h([])
       * NB: if depth == 0 (stop hashing) we hash '.' / 2
       */
    case LST: 
      if (--depth == 0)
	{
	  Pl_Hash_Incr_Int32(hi, pl_atom_tbl[ATOM_CHAR('.')].hash);
	  Pl_Hash_Incr_Int32(hi, 2);
	  break;
	}

      adr = UnTag_LST(word);
      if (!Term_Hash_Rec(Car(adr), depth, hi))
	return FALSE;

      start_word = Cdr(adr);
      goto terminal_rec;

    case STC:
      adr = UnTag_STC(word);
      func = Functor(adr);
      arity = Arity(adr);	/* do not hash the word <f/n> since it is runtime dependent */

      Pl_Hash_Incr_Int32(hi, pl_atom_tbl[func].hash);
      Pl_Hash_Incr_Int32(hi, arity);

      if (--depth == 0)
	break;

      adr = &Arg(adr, 0);
      while(--arity)
	{
	  if (!Term_Hash_Rec(*adr++, depth, hi))
	    return FALSE;
	}
      start_word = *adr;
      goto terminal_rec;      
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_TERM_HASH_4                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Term_Hash_4(WamWord start_word, WamWord depth_word, WamWord range_word, 
	       WamWord hash_word)
{
  PlLong depth = Pl_Rd_Integer_Check(depth_word);
  PlLong range = Pl_Rd_Positive_Check(range_word);
  unsigned hash;

  if (range <= 0 || range > HASH_MOD_VALUE)
    range = HASH_MOD_VALUE;

  Pl_Check_For_Un_Integer(hash_word);

  /* Term_Hash fails if the term is not ground, in that case leave hash_word unbound */

  if (!Term_Hash(start_word, depth, &hash))
    return TRUE;

  return Pl_Un_Integer(hash % range, hash_word);
}




/*-------------------------------------------------------------------------*
 * PL_TERM_HASH_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Term_Hash_2(WamWord start_word, WamWord hash_word)
{
  return Pl_Term_Hash_4(start_word, Tag_INT(-1), Tag_INT(0), hash_word);
}




/*-------------------------------------------------------------------------*
 * PL_STRIP_MODULE                                                         *
 *                                                                         *
 * Decompose a meta-term (qualified term): stop when no more (:)/2 or if   *
 * the module part is not an atom (consume var if accept_var = TRUE)       *
 *                                                                         *
 * The remaining term forms the goal_word (dereferenced).                  *
 *                                                                         *
 * Return: the module_word (dereferenced) (atom or var) or NOT_A_WAM_WORD  *
 *                                                                         *
 * The topmost module name is saved in pl_calling_module (if an atom)      *
 * if it is not yet defined. See Pl_Unset_Calling_Module                   *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Strip_Module(WamWord start_word, Bool accept_var, Bool raise_error,
		WamWord *goal_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  WamWord module_word = NOT_A_WAM_WORD;

  for(;;)
    {
      DEREF(start_word, word, tag_mask);
      *goal_word = word;

      if (tag_mask != TAG_STC_MASK)
	break;

      adr = UnTag_STC(word);
      if (Functor_And_Arity(adr) == Functor_Arity(ATOM_CHAR(':'), 2))
	{
	  DEREF(Arg(adr, 0), word, tag_mask);

	  if (tag_mask == TAG_ATM_MASK)
	    {
	    eat_it:
	      module_word = word;
	      start_word = Arg(adr, 1);
	      if (pl_calling_module < 0)	     /* see Pl_Unset_Calling_Module */
		pl_calling_module = UnTag_ATM(word); /* keep the top-most module */
	      continue;
	    }

	  if (tag_mask == TAG_REF_MASK)
	    {
	      if (accept_var)
		goto eat_it;

	      if (raise_error)
		Pl_Err_Instantiation();

	      break;
	    }

	  if (raise_error)
	    Pl_Err_Type(pl_type_atom, word);

	  break;
	}

#ifdef META_TERM_HIDDEN
      if (Functor_And_Arity(adr) == Functor_Arity(pl_atom_meta_term, 1))
	{
	  word = Arg(adr, 1); /* 0 is the offset */
	  DEREF(word, word, tag_mask);
	  if (tag_mask == TAG_ATM_MASK) 
	    {
	      module_word = word;
	      start_word = Arg(adr, 2);
	      continue;
	    }
	}
#endif


      break;
    }

  return module_word;
}




/*-------------------------------------------------------------------------*
 * PL_STRIP_MODULE_TOP                                                     *
 *                                                                         *
 * Same as Pl_Strip_Module but initializes the calling module.             *
 *                                                                         *
 * Return: the module (-1 if var), and the goal                            *
 *-------------------------------------------------------------------------*/
int
Pl_Strip_Module_Top(WamWord start_word, Bool accept_var, Bool raise_error,
		    WamWord *goal_word)
{
  WamWord module_word;

  Pl_Unset_Calling_Module();
  module_word = Pl_Strip_Module(start_word, FALSE, TRUE, goal_word);

  /* normally called from meta-argument so module_word != NOT_A_WAM_WORD (qualified with module) */
  if (module_word == NOT_A_WAM_WORD)
    return pl_atom_user;

  if (Tag_Mask_Of(module_word) == TAG_REF_MASK)
    return -1;
  
  return UnTag_ATM(module_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_PRED_INDICATOR                                                   *
 *                                                                         *
 * Initializes the functor and arity of the predicate indicator.           *
 * func and arity: -1 means a variable.                                    *
 * Return the module (see Pl_Strip_Module)                                 *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Get_Pred_Indicator(WamWord pred_indic_word, Bool accept_var, 
		      int *func, int *arity)
{
  WamWord pred_indic_word1;
  WamWord module_word;
  WamWord word, tag_mask;
  PlLong arity1;

  module_word = Pl_Strip_Module(pred_indic_word, accept_var, TRUE, 
				&pred_indic_word1);
  pl_pi_module_word = module_word;

  DEREF(pred_indic_word1, word, tag_mask);
  if (tag_mask == TAG_REF_MASK && !accept_var)
    Pl_Err_Instantiation();

  if (!Pl_Get_Structure(ATOM_CHAR('/'), 2, pred_indic_word1))
    {
#if 0 /* no longer accept a callable when a predicate indicator is expected */
      if (!Flag_Value(strict_iso) && Pl_Rd_Callable(word, func, arity) != NULL)
	return module_word;
#endif
      Pl_Err_Type(pl_type_predicate_indicator, pred_indic_word);
    }

  pl_pi_name_word = Pl_Unify_Variable();
  pl_pi_arity_word = Pl_Unify_Variable();

  DEREF(pl_pi_name_word, word, tag_mask);
  if (accept_var && tag_mask == TAG_REF_MASK)
    *func = -1;
  else
    *func = Pl_Rd_Atom_Check(word); /* emit instantiation error if needed */


  DEREF(pl_pi_arity_word, word, tag_mask);
  if (accept_var && tag_mask == TAG_REF_MASK)
    *arity = -1;
  else
    {				/* use a PlLong for arity1 to avoid truncations */
      arity1 = Pl_Rd_Positive_Check(pl_pi_arity_word);
      
      if (arity1 > MAX_ARITY)
	Pl_Err_Representation(pl_representation_max_arity);

      *arity = arity1;
    }

  return module_word;
}




/*-------------------------------------------------------------------------*
 * PL_GET_PRED_INDICATOR_TOP                                               *
 *                                                                         *
 * Same as Pl_Get_Pred_Indicator but initializes the calling module.       *
 * Return module, func and arity: -1 means a variable.                     *
 *-------------------------------------------------------------------------*/
int
Pl_Get_Pred_Indicator_Top(WamWord pred_indic_word, Bool accept_var, 
			  int *func, int *arity)
{
  WamWord module_word;

  Pl_Unset_Calling_Module();
  module_word = Pl_Get_Pred_Indicator(pred_indic_word, accept_var, func, arity);

  /* normally called from meta-argument so module_word != NOT_A_WAM_WORD (qualified with module) */
  if (module_word == NOT_A_WAM_WORD)
    return pl_atom_user;

  if (Tag_Mask_Of(module_word) == TAG_REF_MASK)
    return -1;
  
  return UnTag_ATM(module_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_PRED_INDIC_5                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Pred_Indic_5(WamWord pred_indic_word, WamWord default_module_word,
		    WamWord module_word, WamWord func_word, WamWord arity_word)
{
  int func, arity;

  Pl_Get_Pred_Indicator(pred_indic_word, FALSE, &func, &arity);

  if (pl_pi_module_word == NOT_A_WAM_WORD)
    pl_pi_module_word = default_module_word;
  
  return Pl_Unify(pl_pi_module_word, module_word) &&
    Pl_Unify(pl_pi_name_word, func_word) && Pl_Unify(pl_pi_arity_word, arity_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_PRED_INDIC_VAR_5                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Pred_Indic_Var_5(WamWord pred_indic_word, WamWord default_module_word,
			WamWord module_word, WamWord func_word, WamWord arity_word)
{
  int func, arity;

  Pl_Get_Pred_Indicator(pred_indic_word, TRUE, &func, &arity);

  if (pl_pi_module_word == NOT_A_WAM_WORD)
    pl_pi_module_word = default_module_word;
  
  return Pl_Unify(pl_pi_module_word, module_word) &&
    Pl_Unify(pl_pi_name_word, func_word) && Pl_Unify(pl_pi_arity_word, arity_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_HEAD_AND_BODY                                                    *
 *                                                                         *
 * Get the head and body of a clause.                                      *
 * Accepts terms of the form M:Clause and M:Head:-Body                     *
 * Return the Module (or NOT_A_WAM_WORD), Head and Body.                   *
 * Head and Body are not checked for errors (callable,...).                *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Get_Head_And_Body(WamWord clause_word, WamWord *head_word, WamWord *body_word)
{
  WamWord module_word = Pl_Strip_Module(clause_word, FALSE, TRUE, &clause_word);
  WamWord module_word1;
  int func, arity;
  WamWord *adr = Pl_Rd_Callable_Check(clause_word, &func, &arity);


  if (func == atom_clause && arity == 2)
    {
      module_word1 = Pl_Strip_Module(adr[0], FALSE, TRUE, head_word);
      if (module_word1 == NOT_A_WAM_WORD || module_word == NOT_A_WAM_WORD || 
	  module_word1 == module_word)
	{
	  *body_word = adr[1];
	}
      else
	{
	  *body_word = Pl_Put_Structure(ATOM_CHAR(':'), 2);
	  Pl_Unify_Value(module_word); /* body has the module of the context */
	  Pl_Unify_Value(adr[1]);
	}
      if (module_word1 != NOT_A_WAM_WORD)
	module_word = module_word1; /* module of the clause = module of the head */
    }
  else
    {
      *head_word = clause_word;
      *body_word = Tag_ATM(pl_atom_true);
    }

  return module_word;
}




/*-------------------------------------------------------------------------*
 * PL_GET_HEAD_AND_BODY_TOP                                                *
 *                                                                         *
 * Same as Pl_Get_Head_And_Body but initializes the calling module.        *
 * Return the module (never -1), head_word and body_word.                  *
 *-------------------------------------------------------------------------*/
int
Pl_Get_Head_And_Body_Top(WamWord clause_word, WamWord *head_word, WamWord *body_word)
{
  WamWord module_word;

  Pl_Unset_Calling_Module();
  module_word = Pl_Get_Head_And_Body(clause_word, head_word, body_word);

  /* normally called from meta-argument so module_word != NOT_A_WAM_WORD (qualified with module) */
  if (module_word == NOT_A_WAM_WORD)
    return pl_atom_user;
  
  return UnTag_ATM(module_word);
}




/*-------------------------------------------------------------------------*
 * PL_GET_HEAD_AND_BODY_4                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Head_And_Body_4(WamWord clause_word, WamWord module_word, 
		       WamWord head_word, WamWord body_word)
{
  WamWord module_word1, head_word1, body_word1;

  module_word1 = Pl_Get_Head_And_Body(clause_word, &head_word1, &body_word1);

  return (module_word1 == NOT_A_WAM_WORD || Pl_Unify(module_word, module_word1)) && 
    Pl_Unify(head_word, head_word1) && Pl_Unify(body_word, body_word1);
}




/*-------------------------------------------------------------------------*
 * TERM_TO_GOAL_REC                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Term_To_Goal_Rec(WamWord term_word, WamWord module_word, WamWord *goal_word)
{
  WamWord word, tag_mask;
  int func, arity;
  WamWord *adr;
  WamWord *cur_H;

#define FOR_ASSERT  (t2g_call_info_word == NOT_A_WAM_WORD)

 terminal_rec:

  DEREF(term_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    {
    surround_with_call:
      if (FOR_ASSERT)		/* Term = var X --> Goal = call(X) */
	{
	  if (module_word != t2g_module_word)	/* Module != entry Module --> call(Module:X) */
	    {
	      *goal_word = Pl_Put_Structure(atom_call, 1);
	      Pl_Unify_Structure(ATOM_CHAR(':'), 2);
	      Pl_Unify_Value(module_word);
	      Pl_Unify_Value(word);
	    }
	  else
	    {
	      *goal_word = Pl_Put_Structure(atom_call, 1);
	      Pl_Unify_Value(word);
	    }
	}
      else			/* Term = var X --> Goal '$call_internal'(X, Module, CallInfo) */
	{
	  *goal_word = Pl_Put_Structure(atom_call_internal, 3);
	  Pl_Unify_Value(word);
	  Pl_Unify_Value(module_word);
	  Pl_Unify_Value(t2g_call_info_word);
	}
      return;
    }


  if (tag_mask == TAG_ATM_MASK || tag_mask == TAG_LST_MASK)
    {				/* Goal = Term or Module:Term */
    not_a_control_construct:
      if (module_word == t2g_module_word)
	{
	  *goal_word = word;		/* Goal = Term (unchanged) */
	}
      else if (Tag_Mask_Of(module_word) == TAG_REF_MASK)
	{
	  goto surround_with_call;
	}
      else
	{
	  *goal_word = Pl_Put_Structure(ATOM_CHAR(':'), 2);
	  Pl_Unify_Value(module_word);
	  Pl_Unify_Value(word);
	}
      return;
    }

  if (tag_mask != TAG_STC_MASK)
    Pl_Err_Type(pl_type_callable, t2g_term_word);


  adr = UnTag_STC(word);
  func = Functor(adr);
  arity = Arity(adr);

  if (arity != 2)
    goto not_a_control_construct;


  if (func == ATOM_CHAR(':')) 	/* module qualification */
    {
      adr = &Arg(adr, 0);
      DEREF(*adr, word, tag_mask);
      if (tag_mask == TAG_REF_MASK || tag_mask == TAG_ATM_MASK)
	module_word = word;
      else
	Pl_Err_Type(pl_type_atom, word);
      term_word = adr[1];
      goto terminal_rec;
    }

  if (func != ATOM_CHAR(',') && func != ATOM_CHAR(';') && func != atom_if && func != atom_soft_if)
    goto not_a_control_construct;

  /* create a term on the heap, recursing on args for term to goal conversion */
  *goal_word = Tag_STC(H);
  *H++ = *adr++;		/* copy functor / arity (Functor_And_Arity(adr)) */


  /* here we have arity words to put on the heap */
  cur_H = H;
  H += arity;			/* reserve space on the heap */
  while(--arity)
    Term_To_Goal_Rec(*adr++, module_word, cur_H++);

  term_word = *adr;
  goal_word = cur_H;
  goto terminal_rec;
}




/*-------------------------------------------------------------------------*
 * PL_TERM_TO_GOAL                                                         *
 *                                                                         *
 * Converts a term to a goal. Used for meta-call and for assert(a/z).      *
 * call_info_word = NOT_A_WAM_WORD for assert else the dereferenced        *
 * call_info of call.                                                      *
 *-------------------------------------------------------------------------*/
WamWord
Pl_Term_To_Goal(WamWord term_word, WamWord module_word, WamWord call_info_word)
{
  WamWord goal_word;

  t2g_term_word = term_word;
  t2g_module_word = module_word;
  t2g_call_info_word = call_info_word;

  Term_To_Goal_Rec(term_word, module_word, &goal_word);
  return goal_word;
}




/*-------------------------------------------------------------------------*
 * PL_TERM_TO_GOAL_4                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Term_To_Goal_4(WamWord term_word, WamWord module_word, WamWord call_info_word, 
		  WamWord goal_word)
{
  WamWord goal_word1;
  WamWord word, tag_mask;

  DEREF(call_info_word, word, tag_mask);
  call_info_word = (tag_mask == TAG_INT_MASK) ? word : NOT_A_WAM_WORD;
  
  goal_word1 = Pl_Term_To_Goal(term_word, module_word, call_info_word);

  return Pl_Unify(goal_word, goal_word1);
}
