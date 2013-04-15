/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_symbolic_c.c                                                 *
 * Descr.: symbolic constraints management - C part                        *
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

#include "engine_fd.h"
#include "bips_fd.h"




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

static Bool Fd_All_Different_Rec(WamWord list_word, PlLong x_tag, WamWord x_word,
				 WamWord save_list_word);




/*-------------------------------------------------------------------------*
 * PL_FD_ALL_DIFFERENT_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_All_Different_1(WamWord list_word, WamWord save_list_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;


  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (word == NIL_WORD)
    return TRUE;

  if (tag_mask != TAG_LST_MASK)
    Pl_Err_Type(pl_type_list, save_list_word);

  lst_adr = UnTag_LST(word);
  DEREF(Car(lst_adr), word, tag_mask);

  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK &&
      tag_mask != TAG_FDV_MASK)
    Pl_Err_Type(pl_type_fd_variable, word);

  return Fd_All_Different_Rec(Cdr(lst_adr), tag_mask, word, save_list_word) &&
    Pl_Fd_All_Different_1(Cdr(lst_adr), save_list_word);
}




/*-------------------------------------------------------------------------*
 * FD_ALL_DIFFERENT_REC                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Fd_All_Different_Rec(WamWord list_word, PlLong x_tag, WamWord x_word,
		     WamWord save_list_word)
{
  WamWord word, tag_mask;
  WamWord *lst_adr;
  int ret;


  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (word == NIL_WORD)
    return TRUE;

  if (tag_mask != TAG_LST_MASK)
    Pl_Err_Type(pl_type_list, save_list_word);

  lst_adr = UnTag_LST(word);
  DEREF(Car(lst_adr), word, tag_mask);

  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_INT_MASK &&
      tag_mask != TAG_FDV_MASK)
    Pl_Err_Type(pl_type_fd_variable, word);

  if (x_tag == TAG_INT_MASK)
    ret = (tag_mask == TAG_INT_MASK) ? x_word != word :
             pl_x_neq_c(word, x_word);
  else
    ret = (tag_mask == TAG_INT_MASK) ? pl_x_neq_c(x_word, word) :
             pl_x_neq_y(x_word, word);

  return ret &&
    Fd_All_Different_Rec(Cdr(lst_adr), x_tag, x_word, save_list_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_I                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_I(Range *i, WamWord *l)
{
  int n = *l;			/* I in 1..N in sparse mode */

  Range_Init_Interval(i, 1, n);
  Pl_Range_Becomes_Sparse(i);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_I_TO_V                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_I_To_V(Range *v, Range *i, WamWord *l)
{
  int val;
  int j;

  /* when I changes -> update V */

  Vector_Allocate(v->vec);
  Pl_Vector_Empty(v->vec);

  if (i->min == i->max || Is_Interval(i))
    {
      for (j = i->min; j <= i->max; j++)
	{
	  val = l[j];
	  Vector_Set_Value(v->vec, val);
	}
    }
  else
    {
      VECTOR_BEGIN_ENUM(i->vec, j);

      val = l[j];
      Vector_Set_Value(v->vec, val);

      VECTOR_END_ENUM;
    }

  Pl_Range_From_Vector(v);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_V_TO_I                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_V_To_I(Range *i, Range *v, WamWord *l)
{
  int val;
  int n;
  int j;

  /* when V changes -> update I */

  Vector_Allocate(i->vec);
  Pl_Vector_Empty(i->vec);

  n = *l;

  for (j = 1; j <= n; j++)
    {
      val = l[j];		/* val=Lj */
      if (Pl_Range_Test_Value(v, val))
	Vector_Set_Value(i->vec, j);
    }

  Pl_Range_From_Vector(i);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_VAR_I                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_Var_I(Range *i, WamWord *l)
{
  int n = *l;			/* I in 1..N in sparse mode */

  Range_Init_Interval(i, 1, n);
  Pl_Range_Becomes_Sparse(i);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_VAR_I_TO_V                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_Var_I_To_V(Range *v, Range *i, WamWord **l)
{
  WamWord *fdv_adr;
  int j;

  v->extra_cstr = FALSE;
  v->vec = 0;
  Set_To_Empty(v);

  /* when I or L changes -> update V */

  if (i->min == i->max || Is_Interval(i))
    {
      for (j = i->min; j <= i->max; j++)
	{
	  fdv_adr = l[j];
	  Pl_Range_Union(v, Range(fdv_adr));
	}
    }
  else
    {
      VECTOR_BEGIN_ENUM(i->vec, j);

      fdv_adr = l[j];
      Pl_Range_Union(v, Range(fdv_adr));

      VECTOR_END_ENUM;
    }
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_VAR_V_TO_I                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Element_Var_V_To_I(Range *i, Range *v, WamWord **l)
{
  WamWord *fdv_adr;
  PlLong n;
  int j;

  Vector_Allocate(i->vec);
  Pl_Vector_Empty(i->vec);
  /* when V or L changes -> update I */

  n = (PlLong) *l;

  for (j = 1; j <= n; j++)
    {
      fdv_adr = l[j];
      if (!Pl_Range_Test_Null_Inter(Range(fdv_adr), v))
	  Vector_Set_Value(i->vec, j);
    }

  Pl_Range_From_Vector(i);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ELEMENT_V_TO_XI                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Element_V_To_Xi(int i, WamWord **array, Range *v)
{
  WamWord *fdv_adr = array[i];

  if (Fd_Variable_Is_Ground(fdv_adr))
    return Pl_Fd_Tell_Int_Range(fdv_adr, v);

  return Pl_Fd_Tell_Range_Range(fdv_adr, v);
}




/*-------------------------------------------------------------------------*
 * PL_FD_ATMOST                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Atmost(int n, WamWord **array, int v)
{
  WamWord **p;
  WamWord word = Tag_INT(v);
  PlLong size = (PlLong) array[0];
  int nb = 0;
  int i;

  array++;
  p = array;
  for (i = 0; i < size; i++)
    {
      if (FD_Tag_Value(*p) == word)
	nb++;

      p++;
    }

  if (nb > n)
    return FALSE;

  if (nb == n)
    {
      p = array;
      for (i = 0; i < size; i++)
	{
	  if (!Fd_Variable_Is_Ground(*p))
	    if (!Pl_Fd_Tell_Not_Value(*p, v))
	      return FALSE;
	  p++;
	}
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_ATLEAST                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Atleast(int n, WamWord **array, int v)
{
  WamWord **p;
  PlLong size = (PlLong) array[0];
  int nb = size;
  int i;

  array++;
  p = array;
  for (i = 0; i < size; i++)
    {
      if (!Pl_Range_Test_Value(Range(*p), v))
	  nb--;

      p++;
    }

  if (nb < n)
    return FALSE;

  if (nb == n)
    {
      p = array;
      for (i = 0; i < size; i++)
	{
	  if (Pl_Range_Test_Value(Range(*p), v))
	    if (!Pl_Fd_Tell_Value(*p, v))
	        return FALSE;

	  p++;
	}
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_EXACTLY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Exactly(int n, WamWord **array, int v)
{
  WamWord **p;
  WamWord word = Tag_INT(v);
  PlLong size = (PlLong) array[0];
  int nb1 = 0, nb2 = size;
  int i;

  array++;
  p = array;
  for (i = 0; i < size; i++)
    {
      if (FD_Tag_Value(*p) == word)
	nb1++;
      else if (!Pl_Range_Test_Value(Range(*p), v))
	  nb2--;

      p++;
    }

  if (nb1 > n || nb2 < n)
    return FALSE;

  if (nb1 == n)
    {
      p = array;
      for (i = 0; i < size; i++)
	{
	  if (!Fd_Variable_Is_Ground(*p))
	    if (!Pl_Fd_Tell_Not_Value(*p, v))
	      return FALSE;
	  p++;
	}

      return TRUE;
    }

  if (nb2 == n)
    {
      p = array;
      for (i = 0; i < size; i++)
	{
	  if (Pl_Range_Test_Value(Range(*p), v))
	    if (!Pl_Fd_Tell_Value(*p, v))
	        return FALSE;

	  p++;
	}
    }
  return TRUE;
}
