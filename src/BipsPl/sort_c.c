/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : sort_c.c                                                        *
 * Descr.: sort management - C part                                        *
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


#define OBJ_INIT Sort_Initializer

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

static WamWord minus_2;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static PlLong Keysort_Cmp(WamWord u_word, WamWord v_word);

static int Merge_Sort(WamWord *base, WamWord *aux, int n,
		      Bool keep_dup, PlLong (*cmp) ());




/*-------------------------------------------------------------------------*
 * SORT_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Sort_Initializer(void)
{
  minus_2 = Functor_Arity(ATOM_CHAR('-'), 2);
}




/*-------------------------------------------------------------------------*
 * CHK_PAIR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Chk_Pair(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_STC_MASK || 
      Functor_And_Arity(UnTag_STC(word)) != minus_2))
    Pl_Err_Type(pl_type_pair, word);
}




/*-------------------------------------------------------------------------*
 * GET_PAIR                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamWord
Get_Pair(WamWord start_word)
{
  WamWord word, tag_mask;

  DEREF(start_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_STC_MASK || Functor_And_Arity(UnTag_STC(word)) != minus_2)
    Pl_Err_Type(pl_type_pair, word);

  return word;			/* store dereferenced words in the array */
}





/*-------------------------------------------------------------------------*
 * PL_SORT_LIST_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Sort_List_2(WamWord list1_word, WamWord list2_word)
{
  WamWord *arg;
  int n;
  int sort_type;

  sort_type = SYS_VAR_OPTION_MASK;	/* 0=sort/2, 1=msort/2, 2=keysort/2 */

  arg = H;			/* array in the heap */

  if (sort_type != 2)
    {
      Pl_Check_For_Un_List(list2_word);
      n = Pl_Rd_Proper_List_Check(list1_word, arg);
    }
  else
    {
      Pl_Check_For_Un_List2(list2_word, Chk_Pair);
      n = Pl_Rd_Proper_List_Check2(list1_word, arg, Get_Pair);
    }

  if (n == 0)
    return Pl_Un_Atom(ATOM_NIL, list2_word);

  if (n == 1)
    return Pl_Unify(list1_word, list2_word);

  n = Merge_Sort(arg, arg + n, n, sort_type,
		 (sort_type != 2) ? Pl_Term_Compare : Keysort_Cmp);

  /* n can have changed here (if dup removed) */

  return Pl_Unify(Pl_Mk_Proper_List(n, arg), list2_word);
}




/*-------------------------------------------------------------------------*
 * PL_SORT_LIST_1                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Sort_List_1(WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord *adr, *arg, *prev;
  int n;
  int sort_type;

  sort_type = SYS_VAR_OPTION_MASK;	/* 0=sort/1, 1=msort/1, 2=keysort/1 */

  arg = H;

  if (sort_type != 2)
    {
      n = Pl_Rd_Proper_List_Check(list_word, arg);
    }
  else
    {
      n = Pl_Rd_Proper_List_Check2(list_word, arg, Get_Pair);
    }

  if (n <= 1)
    return;

  n = Merge_Sort(arg, arg + n, n, sort_type,
		 (sort_type != 2) ? Pl_Term_Compare : Keysort_Cmp);
  /* n can have changed here (if dup removed) */
  /* update in-place the list */
  do
    {
      DEREF(list_word, word, tag_mask);
      adr = UnTag_LST(word);
      Car(adr) = *arg++;
      prev = &Cdr(adr);
      list_word = Cdr(adr);
    }
  while (--n);
  *prev = NIL_WORD;
}




/*-------------------------------------------------------------------------*
 * KEYSORT_CMP                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static PlLong
Keysort_Cmp(WamWord u_word, WamWord v_word)
{
  /* here we know that u_word and v_word are dereferenced (and are pairs) */

  u_word = Arg(UnTag_STC(u_word), 0);
  v_word = Arg(UnTag_STC(v_word), 0);

  return Pl_Term_Compare(u_word, v_word);
}




/*-------------------------------------------------------------------------*
 * MERGE_SORT                                                              *
 *                                                                         *
 * Merge sort on an array on n WamWords starting at base using an auxiliary*
 * array for the merge at aux. The comparaison function cmp will receive 2 *
 * elements of the array (2 WamWords) and classically returns <0, 0, >0.   *
 *-------------------------------------------------------------------------*/
static int
Merge_Sort(WamWord *base, WamWord *aux, int n, Bool keep_dup, PlLong (*cmp) ())
{
  WamWord *l1, *l2;
  int n1, n2;
  WamWord *p;

  if (n <= 1)
    return n;

  n1 = n / 2;
  n2 = n - n1;
  l1 = base;
  l2 = base + n1;

  n1 = Merge_Sort(l1, aux, n1, keep_dup, cmp);
  n2 = Merge_Sort(l2, aux, n2, keep_dup, cmp);
  n = n1 + n2;

  p = aux;
  while (n1 > 0 && n2 > 0)
    {
      if ((*cmp) (*l1, *l2) <= 0)	/* copy smaller element to aux */
	{
	  *p++ = *l1++;
	  n1--;
	}
      else
	{
	  *p++ = *l2++;
	  n2--;
	}
    }

  while (n1-- > 0)		/* copy n1 elements to aux */
    *p++ = *l1++;

  if (keep_dup)
    {
      n1 = n - n2;		/* there are n2 elements already in place */
      p = aux;
      while (n1-- > 0)		/* copy n-n2 elemens to base */
	*base++ = *p++;

      return n;
    }

  while (n2-- > 0)		/* copy n2 elements to aux (needed for test dup) */
    *p++ = *l2++;

  p = aux;
  *base = *p++;			/* copy 1st element to base (init test dup) */
  n1 = n - 1;
  while (n1-- > 0)		/* copy n-1 elemens to base + test dup */
    {
      if (cmp(*base, *p) < 0)
	*++base = *p++;
      else
	{
	  n--;
	  p++;
	}
    }

  return n;
}
