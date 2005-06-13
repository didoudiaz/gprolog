/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_values_c.c                                                   *
 * Descr.: FD variable values management - C part                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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

#include <stdlib.h>

#include "engine_pl.h"
#include "bips_pl.h"

#include "engine_fd.h"
#include "bips_fd.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define METHOD_MIN                 0
#define METHOD_MAX                 1
#define METHOD_MIDDLE              2
#define METHOD_LIMITS              3
#define METHOD_RANDOM_V            4

#define METHOD_STANDARD            0
#define METHOD_FIRST_FAIL          1
#define METHOD_MOST_CONSTRAINED    2
#define METHOD_SMALLEST            3
#define METHOD_LARGEST             4
#define METHOD_MAX_REGRET          5
#define METHOD_RANDOM              6




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef Bool (*CmpFct) ();




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static Bool Indomain_Min(WamWord *fdv_adr);

static Bool Indomain_Max(WamWord *fdv_adr);

static Bool Indomain_Middle(WamWord *fdv_adr);

static Bool Indomain_Limits(WamWord *fdv_adr);

static Bool Indomain_Random(WamWord *fdv_adr);



static Bool Cmp_First_Fail(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Most_Constrained(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Smallest(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Largest(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Max_Regret(WamWord *last_fdv_adr, WamWord *new_fdv_adr);



#define INDOMAIN_MIN_ALT           X24696E646F6D61696E5F6D696E5F616C74

#define INDOMAIN_MAX_ALT           X24696E646F6D61696E5F6D61785F616C74

#define INDOMAIN_MIDDLE_ALT        X24696E646F6D61696E5F6D6964646C655F616C74

#define INDOMAIN_LIMITS_ALT        X24696E646F6D61696E5F6C696D6974735F616C74

#define INDOMAIN_RANDOM_ALT        X24696E646F6D61696E5F72616E646F6D5F616C74

#define EXTRA_CSTR_ALT             X2465787472615F637374725F616C74



Prolog_Prototype(INDOMAIN_MIN_ALT, 0);
Prolog_Prototype(INDOMAIN_MAX_ALT, 0);
Prolog_Prototype(INDOMAIN_MIDDLE_ALT, 0);
Prolog_Prototype(INDOMAIN_LIMITS_ALT, 0);
Prolog_Prototype(INDOMAIN_RANDOM_ALT, 0);
Prolog_Prototype(EXTRA_CSTR_ALT, 0);

  /* defined in fd_values_fd.fd */

Bool fd_domain(WamWord list_word, WamWord l_word, WamWord u_word);

Bool fd_domain_r(WamWord list_word, WamWord r_word);




/*-------------------------------------------------------------------------*
 * FD_DOMAIN_BOOL_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Domain_Bool_1(WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;


  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_INT_MASK ||
      tag_mask == TAG_FDV_MASK)
    return Fd_Check_For_Bool_Var(word);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      if (!Fd_Check_For_Bool_Var(Car(lst_adr)))
	return FALSE;

      list_word = Cdr(lst_adr);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * FD_DOMAIN_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Domain_2(WamWord list_word, WamWord r_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;


  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_INT_MASK || 
      tag_mask == TAG_FDV_MASK)
    return fd_domain_r(word, r_word);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      if (!fd_domain_r(Car(lst_adr), r_word))
	return FALSE;

      list_word = Cdr(lst_adr);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * FD_DOMAIN_3                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Domain_3(WamWord list_word, WamWord l_word, WamWord u_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;


  if (l_word == Tag_INT(0) && u_word == Tag_INT(1))
    return Fd_Domain_Bool_1(list_word);

  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_INT_MASK || tag_mask == TAG_FDV_MASK)
    return fd_domain(word, l_word, u_word);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      if (!fd_domain(Car(lst_adr), l_word, u_word))
	return FALSE;

      list_word = Cdr(lst_adr);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_2(WamWord fdv_word, WamWord method_word)
{
  WamWord word, tag_mask;
  WamWord *fdv_adr;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  if (tag_mask == TAG_INT_MASK)
    return TRUE;

  fdv_adr = UnTag_FDV(word);

  switch (Rd_Integer_Check(method_word))
    {
    case METHOD_MIN:
      return Indomain_Min(fdv_adr);

    case METHOD_MAX:
      return Indomain_Max(fdv_adr);

    case METHOD_MIDDLE:
      return Indomain_Middle(fdv_adr);

    case METHOD_LIMITS:
      return Indomain_Limits(fdv_adr);

    case METHOD_RANDOM_V:
      return Indomain_Random(fdv_adr);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MIN                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Indomain_Min(WamWord *fdv_adr)
{
  Range *range;
  int cur, end;

  range = Range(fdv_adr);

  end = Max(fdv_adr);
  cur = Min(fdv_adr);

  A(0) = (WamWord) fdv_adr;
  A(1) = (WamWord) range;
  A(2) = end;
  A(3) = cur;

  Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MIN_ALT, 0), 4);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MIN_ALT_0                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_Min_Alt_0(void)
{
  WamWord *fdv_adr;
  Range *range;
  int cur, end;

  Update_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MIN_ALT, 0), 0);

  fdv_adr = (WamWord *) AB(B, 0);
  range = (Range *) AB(B, 1);
  end = AB(B, 2);
  cur = AB(B, 3);

  cur = Range_Next_After(range, cur);

  SYS_VAR_FD_BCKTS++;

  if (cur != end)
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = (WamWord) fdv_adr;
      AB(B, 1) = (WamWord) range;
      AB(B, 2) = end;
#endif
      AB(B, 3) = cur;
    }
  else if (Extra_Cstr(fdv_adr))
    Update_Choice_Point((CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0), 0);
  else
    Delete_Choice_Point(0);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MAX                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Indomain_Max(WamWord *fdv_adr)
{
  Range *range;
  int cur, end;

  range = Range(fdv_adr);

  end = Min(fdv_adr);
  cur = Max(fdv_adr);

  A(0) = (WamWord) fdv_adr;
  A(1) = (WamWord) range;
  A(2) = end;
  A(3) = cur;

  Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MAX_ALT, 0), 4);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MAX_ALT_0                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_Max_Alt_0(void)
{
  WamWord *fdv_adr;
  Range *range;
  int cur, end;

  Update_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MAX_ALT, 0), 0);

  fdv_adr = (WamWord *) AB(B, 0);
  range = (Range *) AB(B, 1);
  end = AB(B, 2);
  cur = AB(B, 3);

  cur = Range_Next_Before(range, cur);

  SYS_VAR_FD_BCKTS++;

  if (cur != end)
    {
#if 0 /* the following data is unchanged */ 
      AB(B, 0) = (WamWord) fdv_adr;
      AB(B, 1) = (WamWord) range;
      AB(B, 2) = end;
#endif
      AB(B, 3) = cur;
    }
  else if (Extra_Cstr(fdv_adr))
    Update_Choice_Point((CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0), 0);
  else
    Delete_Choice_Point(0);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MIDDLE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Indomain_Middle(WamWord *fdv_adr)
{
  Range *range;
  int end, cur;
  int cur_left, cur_right;
  Bool is_right;
  int i;

  range = Range(fdv_adr);

  end = Max(fdv_adr);
  i = (Nb_Elem(fdv_adr) + 1) / 2;
  cur = Range_Ith_Elem(range, i);
  cur_left = cur_right = cur;
  is_right = Nb_Elem(fdv_adr) % 2;	/* is_rigth if nb of elements is odd */

  A(0) = (WamWord) fdv_adr;
  A(1) = (WamWord) range;
  A(2) = end;
  A(3) = cur_left;
  A(4) = cur_right;
  A(5) = (WamWord) is_right;

  Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MIDDLE_ALT, 0),
		      6);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_MIDDLE_ALT_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_Middle_Alt_0(void)
{
  WamWord *fdv_adr;
  Range *range;
  int end, cur;
  int cur_left, cur_right;
  Bool is_right;

  Update_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_MIDDLE_ALT, 0),
		      0);

  fdv_adr = (WamWord *) AB(B, 0);
  range = (Range *) AB(B, 1);
  end = AB(B, 2);
  cur_left = AB(B, 3);
  cur_right = AB(B, 4);
  is_right = (Bool) AB(B, 5);

  if (is_right)
    {
      cur_left = Range_Next_Before(range, cur_left);
      cur = cur_left;
      is_right = FALSE;
    }
  else
    {
      cur_right = Range_Next_After(range, cur_right);
      cur = cur_right;
      is_right = TRUE;
    }


  SYS_VAR_FD_BCKTS++;

  if (cur != end)
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = (WamWord) fdv_adr;
      AB(B, 1) = (WamWord) range;
      AB(B, 2) = end;
#endif
      AB(B, 3) = cur_left;
      AB(B, 4) = cur_right;
      AB(B, 5) = is_right;
    }
  else if (Extra_Cstr(fdv_adr))
    ALTB(B) = (CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0);
  else
    Delete_Last_Choice_Point();

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_LIMITS                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Indomain_Limits(WamWord *fdv_adr)
{
  Range *range;
  int end, cur;
  int cur_left, cur_right;
  Bool is_right;
  int i;


  range = Range(fdv_adr);

  i = Nb_Elem(fdv_adr) / 2 + 1;
  end = Range_Ith_Elem(range, i);
  cur = Min(fdv_adr);
  cur_left = cur;
  cur_right = -1;
  is_right = FALSE;

  A(0) = (WamWord) fdv_adr;
  A(1) = (WamWord) range;
  A(2) = end;
  A(3) = cur_left;
  A(4) = cur_right;
  A(5) = (WamWord) is_right;

  Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_LIMITS_ALT, 0),
		      6);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_LIMITS_ALT_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_Limits_Alt_0(void)
{
  WamWord *fdv_adr;
  Range *range;
  int end, cur;
  int cur_left, cur_right;
  Bool is_right;

  Update_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_LIMITS_ALT, 0),
		      0);

  fdv_adr = (WamWord *) AB(B, 0);
  range = (Range *) AB(B, 1);
  end = AB(B, 2);
  cur_left = AB(B, 3);
  cur_right = AB(B, 4);
  is_right = (Bool) AB(B, 5);

  if (is_right)
    {
      cur_left = Range_Next_After(range, cur_left);
      cur = cur_left;
      is_right = FALSE;
    }
  else
    {
      cur_right = (cur_right >= 0) ? Range_Next_Before(range, cur_right)
	: Max(fdv_adr);
      cur = cur_right;
      is_right = TRUE;
    }

  SYS_VAR_FD_BCKTS++;

  if (cur != end)
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = (WamWord) fdv_adr;
      AB(B, 1) = (WamWord) range;
      AB(B, 2) = end;
#endif
      AB(B, 3) = cur_left;
      AB(B, 4) = cur_right;
      AB(B, 5) = is_right;
    }
  else if (Extra_Cstr(fdv_adr))
    ALTB(B) = (CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0);
  else
    Delete_Last_Choice_Point();

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_RANDOM                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Indomain_Random(WamWord *fdv_adr)
{
  Vector vec;
  int n, cur;

  if (!Fd_Use_Vector(fdv_adr))
    return FALSE;

  n = Nb_Elem(fdv_adr);

  A(0) = (WamWord) fdv_adr;
  A(1) = n;

  Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_RANDOM_ALT, 0),
		      2 + vec_size);

  vec = (Vector) &AB(B, 2 - 1 + vec_size);

  Vector_Copy(vec, Vec(fdv_adr));

  cur = Vector_Ith_Elem(vec, M_Random_Integer(n) + 1);
  Vector_Reset_Value(vec, cur);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * INDOMAIN_RANDOM_ALT_0                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Indomain_Random_Alt_0(void)
{
  WamWord *fdv_adr;
  Vector vec;
  int n, cur;

  Update_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_RANDOM_ALT, 0),
		      0);

  fdv_adr = (WamWord *) AB(B, 0);
  n = AB(B, 1);
  vec = (Vector) (&AB(B, 2 - 1 + vec_size));

  SYS_VAR_FD_BCKTS++;
  n--;

  if (n > 1)
    {
#if 0 /* the following data is unchanged */
      AB(B, 0) = (WamWord) fdv_adr;
#endif
      AB(B, 1) = n;
#if 0 /* the following data is changed below */
      AB(B,2...2-1+vecsize) = ...
#endif
	}
  else if (Extra_Cstr(fdv_adr))
    ALTB(B) = (CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0);
  else
    Delete_Last_Choice_Point();

  cur = Vector_Ith_Elem(vec, M_Random_Integer(n) + 1);
  Vector_Reset_Value(vec, cur);

  return Fd_Assign_Value(fdv_adr, cur);
}




/*-------------------------------------------------------------------------*
 * EXTRA_CSTR_ALT_0                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Extra_Cstr_Alt_0(void)
{
  WamWord *fdv_adr;

  fdv_adr = (WamWord *) AB(B, 0);

  Delete_Choice_Point(0);
  Fd_Display_Extra_Cstr(fdv_adr);

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * FD_SEL_ARRAY_FROM_LIST_2                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Sel_Array_From_List_2(WamWord list_word, WamWord sel_array_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  int n = 0;
  WamWord *fdv_adr;
  WamWord *array;
  WamWord *save_array;


  array = CS;

  save_list_word = list_word;
  save_array = array;

  array++;			/* +1 for the nb of elems */


  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (tag_mask != TAG_FDV_MASK && tag_mask != TAG_INT_MASK)
	Pl_Err_Type(type_fd_variable, word);

      if (tag_mask == TAG_FDV_MASK)
	{
	  fdv_adr = UnTag_FDV(word);
	  *array++ = (WamWord) fdv_adr;
	  n++;
	}

      list_word = Cdr(lst_adr);
    }

  *save_array = n;

  CS = array;

  return Get_Integer(Cstr_Offset(save_array), sel_array_word);
}




/*-------------------------------------------------------------------------*
 * FD_SEL_ARRAY_PICK_VAR_4                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Sel_Array_Pick_Var_4(WamWord sel_array_word, WamWord method_word,
			WamWord reorder_word, WamWord fdv_word)
#if 1
#define PACK_ARRAY
#endif
{
  WamWord **array;
  WamWord **p, **end;
  CmpFct cmp_meth;
  long n;
  int i;
  WamWord *fdv_adr;
  WamWord **res_elem = NULL;
  Bool reorder;

#ifdef PACK_ARRAY
  WamWord **q;
  int nb_ground = 0;
#endif

  array = (WamWord **) (Cstr_Stack + Rd_Integer_Check(sel_array_word));

  n = (long) array[0];
  if (n == 0)
    return FALSE;

  array++;
  end = array + n;

  reorder = Rd_Integer_Check(reorder_word);

  switch (Rd_Integer_Check(method_word))
    {
    case METHOD_FIRST_FAIL:
      cmp_meth = Cmp_First_Fail;
      break;

    case METHOD_MOST_CONSTRAINED:
      cmp_meth = Cmp_Most_Constrained;
      break;

    case METHOD_SMALLEST:
      cmp_meth = Cmp_Smallest;
      break;

    case METHOD_LARGEST:
      cmp_meth = Cmp_Largest;
      break;

    case METHOD_MAX_REGRET:
      cmp_meth = Cmp_Max_Regret;
      break;

    case METHOD_RANDOM:
      for (;;)
	{
	  i = M_Random_Integer(n);
	  end--;
	  n--;
	  fdv_adr = array[i];
	  array[i] = *end;
	  *end = fdv_adr;

	  if (!Fd_Variable_Is_Ground(fdv_adr))
	    {
	      Trail_OV(array - 1);
	      array[-1] = (WamWord *) n;
	      goto finish;
	    }

	  if (n == 0)
	    return FALSE;
	}
    }

  for (p = array; p < end; p++)
    {
      fdv_adr = *p;

      if (!Fd_Variable_Is_Ground(fdv_adr))
	{
	  if (res_elem == NULL)
	    res_elem = p;
	  else if ((*cmp_meth) (*res_elem, fdv_adr))
	    {
	      if (reorder)
		{
		  *p = *res_elem;
		  *res_elem = fdv_adr;
		}
	      else
		res_elem = p;
	    }
	}

#ifdef PACK_ARRAY
      else
	nb_ground++;
#endif
    }

  if (res_elem == NULL)
    return FALSE;

#ifdef PACK_ARRAY
  if (n > 50 && nb_ground >= n / 2)
    {
      n = n - nb_ground;
      Trail_MV(array - 1, n + 1);
      array[-1] = (WamWord *) n;
      for (p = q = array; n; p++)
	{
	  fdv_adr = *p;
	  if (!Fd_Variable_Is_Ground(fdv_adr))
	    {
	      *q++ = *p;
	      n--;
	    }
	}
    }
#endif

  fdv_adr = *res_elem;

finish:
  return Unify(Tag_REF(fdv_adr), fdv_word);
}




/*-------------------------------------------------------------------------*
 * CMP_FIRST_FAIL                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Cmp_First_Fail(WamWord *last_fdv_adr, WamWord *new_fdv_adr)
{
  return Nb_Elem(new_fdv_adr) < Nb_Elem(last_fdv_adr);
}




/*-------------------------------------------------------------------------*
 * CMP_MOST_CONSTRAINED                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Cmp_Most_Constrained(WamWord *last_fdv_adr, WamWord *new_fdv_adr)
{
  int l_nb = Nb_Elem(last_fdv_adr);
  int n_nb = Nb_Elem(new_fdv_adr);

  return n_nb < l_nb ||
    (n_nb == l_nb && Nb_Cstr(new_fdv_adr) > Nb_Cstr(last_fdv_adr));
}




/*-------------------------------------------------------------------------*
 * CMP_SMALLEST                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Cmp_Smallest(WamWord *last_fdv_adr, WamWord *new_fdv_adr)
{
  int l_min = Min(last_fdv_adr);
  int n_min = Min(new_fdv_adr);

  return n_min < l_min ||
    (n_min == l_min && Nb_Cstr(new_fdv_adr) > Nb_Cstr(last_fdv_adr));
}




/*-------------------------------------------------------------------------*
 * CMP_LARGEST                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Cmp_Largest(WamWord *last_fdv_adr, WamWord *new_fdv_adr)
{
  int l_max = Max(last_fdv_adr);
  int n_max = Max(new_fdv_adr);

  return n_max > l_max ||
    (n_max == l_max && Nb_Cstr(new_fdv_adr) > Nb_Cstr(last_fdv_adr));
}




/*-------------------------------------------------------------------------*
 * CMP_MAX_REGRET                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static Bool
Cmp_Max_Regret(WamWord *last_fdv_adr, WamWord *new_fdv_adr)
{
  int l_diff;
  int n_diff;
  int min;

  min = Min(last_fdv_adr);
  l_diff = Range_Next_After(Range(last_fdv_adr), min) - min;

  min = Min(new_fdv_adr);
  n_diff = Range_Next_After(Range(new_fdv_adr), min) - min;

  return n_diff > l_diff ||
    (n_diff == l_diff && Nb_Cstr(new_fdv_adr) > Nb_Cstr(last_fdv_adr));
}
