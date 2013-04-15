/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_values_c.c                                                   *
 * Descr.: FD variable values management - C part                          *
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
#define METHOD_RANDOM_V            2
#define METHOD_MIDDLE              3
#define METHOD_BISECT              4
#define METHOD_LIMITS              5

#define METHOD_LIMITS_MIN          METHOD_LIMITS
#define METHOD_LIMITS_MAX          (METHOD_LIMITS + 1)



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

static Bool Cmp_First_Fail(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Most_Constrained(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Smallest(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Largest(WamWord *last_fdv_adr, WamWord *new_fdv_adr);

static Bool Cmp_Max_Regret(WamWord *last_fdv_adr, WamWord *new_fdv_adr);



#define INDOMAIN_ALT               X1_24696E646F6D61696E5F616C74

#define EXTRA_CSTR_ALT             X1_2465787472615F637374725F616C74

Prolog_Prototype(INDOMAIN_ALT, 0);

Prolog_Prototype(EXTRA_CSTR_ALT, 0);


  /* defined in fd_values_fd.fd */

Bool pl_fd_domain_r(WamWord x_word, WamWord r_word);




/*-------------------------------------------------------------------------*
 * PL_FD_DOMAIN_2                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Domain_2(WamWord list_word, WamWord r_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;


  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_INT_MASK || tag_mask == TAG_FDV_MASK)
    return pl_fd_domain_r(word, r_word);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      if (!pl_fd_domain_r(Car(lst_adr), r_word))
	return FALSE;

      list_word = Cdr(lst_adr);
    }

  return TRUE;
}



/* domain(X,L,U) is optimized here.
 * Previously it called pl_fd_domain (defined in fd_values_fd.fd as X in L..U)
 * This version avoids AFrame creation and FD var creation if interval is empty or an INT
 *
 * We do not do the same for domain(X,L) (which continues to call pl_fd_domain_r)
 * Because we have to be cautious when handling range directly (see Pl_Tell_Range_Range
 * which recovers CS/save_CS). Also to avoid to handle extra cstr warning messages
 * (for instance when a range is empty or a singleton and extr_cstr is TRUE).
 *
 * The 2 following functions could be moved to EngineFD/fd_inst.c (is often use): 
 *    Pl_Fd_Domain_Interval 
 *    Pl_Fd_Domain_Var_3  (renamed as Pl_Fd_Prolog_Domain)
 * 
 */


/*-------------------------------------------------------------------------*
 * Pl_FD_DOMAIN_INTERVAL                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Domain_Interval(WamWord x_word, int min, int max)
{
  WamWord word, tag_mask;
  WamWord *adr, *fdv_adr;
  PlLong v;

  DEREF(x_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    {
      if (min > max)
	return FALSE;

      if (min == max)
	return Pl_Get_Integer(min, x_word);

      adr = UnTag_REF(word);
      fdv_adr = Pl_Fd_New_Variable_Interval(min, max);
      Bind_UV(adr, Tag_REF(fdv_adr));
      return TRUE;
    }

  if (tag_mask == TAG_INT_MASK)
    {
      v = UnTag_INT(word);
      return (v >= min && v <= max); /* also detects if min > max */
    }

  if (tag_mask != TAG_FDV_MASK)
    Pl_Err_Type(pl_type_fd_variable, word);

  return Pl_Fd_In_Interval(UnTag_FDV(word), min, max);
}




/*-------------------------------------------------------------------------*
 * PL_FD_DOMAIN_VAR_3                                                      *
 *                                                                         *
 * Only accepts a var (not a list)                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Domain_Var_3(WamWord x_word, WamWord l_word, WamWord u_word)
{
  int min, max;

  min = Pl_Fd_Prolog_To_Value(l_word);
  if (min < 0)
    min = 0;
  max = Pl_Fd_Prolog_To_Value(u_word);

  return Pl_Fd_Domain_Interval(x_word, min, max);  
}




/*-------------------------------------------------------------------------*
 * PL_FD_DOMAIN_3                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Domain_3(WamWord list_word, WamWord l_word, WamWord u_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  int min, max;

  min = Pl_Fd_Prolog_To_Value(l_word);
  if (min < 0)
    min = 0;
  max = Pl_Fd_Prolog_To_Value(u_word);

  DEREF(list_word, word, tag_mask);
  if (tag_mask == TAG_REF_MASK || tag_mask == TAG_INT_MASK || tag_mask == TAG_FDV_MASK)
    return Pl_Fd_Domain_Interval(word, min, max);

  save_list_word = list_word;
  for (;;)
    {
      DEREF(list_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (word == NIL_WORD)
	break;

      if (tag_mask != TAG_LST_MASK)
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      if (!Pl_Fd_Domain_Interval(Car(lst_adr), min, max))
	return FALSE;

      list_word = Cdr(lst_adr);
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * SELECT_VALUE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Select_Value(WamWord *fdv_adr, int value_method)
{
  int n;
  switch(value_method)
    {
    case METHOD_MIN:
    case METHOD_LIMITS_MIN:
      return Min(fdv_adr);

    case METHOD_MAX:
    case METHOD_LIMITS_MAX:
      return Max(fdv_adr);

    case METHOD_BISECT:
    case METHOD_MIDDLE:
      n = Nb_Elem(fdv_adr) / 2;			   /* here nb_elem > 1 => n >= 1 */
      return Pl_Range_Ith_Elem(Range(fdv_adr), n); /* Ith is in 1..nb_elem */
      
    case METHOD_RANDOM_V:
      n = Nb_Elem(fdv_adr);
      n = Pl_M_Random_Integer(n);		       /* random returns in 0..nb_elem-1 */
      return Pl_Range_Ith_Elem(Range(fdv_adr), n + 1); /* Ith is in 1..nb_elem */
    }

  return 0;
}




/*-------------------------------------------------------------------------*
 * PL_INDOMAIN_2                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Indomain_2(WamWord x_word, WamWord method_word)
{
  WamWord word, tag_mask;
  WamWord *fdv_adr;
  int value_method;
  int value;

  value_method = Pl_Rd_Integer(method_word);

  Fd_Deref_Check_Fd_Var(x_word, word, tag_mask);

  fdv_adr = UnTag_FDV(word);

 bisect_terminal_rec:
  if (tag_mask == TAG_INT_MASK)
    return TRUE;

  value = Select_Value(fdv_adr, value_method);
  
  A(0) = (WamWord) fdv_adr | Extra_Cstr(fdv_adr);
  A(1) = value_method;
  A(2) = value;

  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_ALT, 0), 3);

  if (value_method == METHOD_BISECT)
    {
      if (!Pl_Fd_In_Interval(fdv_adr, 0, value))
	return FALSE;

      tag_mask = Tag_Mask_Of(*fdv_adr);

      goto bisect_terminal_rec;
    }

  return Pl_Fd_Assign_Value_Fast(fdv_adr, value);
}




/*-------------------------------------------------------------------------*
 * PL_INDOMAIN_ALT_0                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Indomain_Alt_0(void)
{
  WamWord *fdv_adr;
  int extra_cstr;
  int value_method;
  int value;


  Pl_Delete_Choice_Point(3);

  SYS_VAR_FD_BCKTS++;

  fdv_adr = (WamWord *) (A(0) & ~1);
  extra_cstr = A(0) & 1;
  value_method = A(1);
  value = A(2);

  if (value_method == METHOD_LIMITS_MIN)
    value_method = METHOD_LIMITS_MAX;
  else if (value_method == METHOD_LIMITS_MAX)
    value_method = METHOD_LIMITS_MIN;
  else if (value_method == METHOD_BISECT)
    {
      /* NB: not need to test extra_cstr because it is handled by Pl_Fd_In_Interval() 
       * (when the var becomes ground or if empty domain (failure) 
       */
      if (!Pl_Fd_In_Interval(fdv_adr, value + 1, INTERVAL_MAX_INTEGER))
	return FALSE;

      /* simple and enough (like in Prolog) */
      return Pl_Indomain_2(*fdv_adr, Tag_INT(value_method));
    }

 
  if (!Pl_Fd_Remove_Value(fdv_adr, value))
    {
      if (extra_cstr)
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      return FALSE;
    }

  if (Tag_Mask_Of(*fdv_adr) == TAG_INT_MASK)
    {
      if (extra_cstr)
	{
	  /* A(0) = fdv_adr; */
	  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(EXTRA_CSTR_ALT, 0), 1);
	}
      return TRUE;
    }

  value = Select_Value(fdv_adr, value_method);


  /*  A(0) = (WamWord) fdv_adr | Extra_Cstr(fdv_adr); */
  A(1) = value_method;		/* can change for METHOD_LIMITS */
  A(2) = value;

  Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(INDOMAIN_ALT, 0), 3);

  return Pl_Fd_Assign_Value_Fast(fdv_adr, value);
}




/*-------------------------------------------------------------------------*
 * PL_EXTRA_CSTR_ALT_0                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Extra_Cstr_Alt_0(void)
{
  WamWord *fdv_adr;

  fdv_adr = (WamWord *) AB(B, 0);

  Pl_Delete_Choice_Point(0);
  Pl_Fd_Display_Extra_Cstr(fdv_adr);

  return FALSE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_SEL_ARRAY_FROM_LIST_2                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Sel_Array_From_List_2(WamWord list_word, WamWord sel_array_word)
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
	Pl_Err_Type(pl_type_list, save_list_word);

      lst_adr = UnTag_LST(word);
      DEREF(Car(lst_adr), word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	Pl_Err_Instantiation();

      if (tag_mask != TAG_FDV_MASK && tag_mask != TAG_INT_MASK)
	Pl_Err_Type(pl_type_fd_variable, word);

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

  return Pl_Get_Integer(Cstr_Offset(save_array), sel_array_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_SEL_ARRAY_PICK_VAR_4                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Sel_Array_Pick_Var_4(WamWord sel_array_word, WamWord method_word,
			   WamWord reorder_word, WamWord fdv_word)
#if 1
#define PACK_ARRAY
#endif
{
  WamWord **array;
  WamWord **p, **end;
  CmpFct cmp_meth;
  PlLong n;
  int i;
  WamWord *fdv_adr;
  WamWord **res_elem = NULL;
  Bool reorder;

#ifdef PACK_ARRAY
  WamWord **q;
  int nb_ground = 0;
#endif

  array = (WamWord **) (Cstr_Stack + Pl_Rd_Integer_Check(sel_array_word));

  n = (PlLong) array[0];
  if (n == 0)
    return FALSE;

  array++;
  end = array + n;

  reorder = Pl_Rd_Integer_Check(reorder_word);

  switch (Pl_Rd_Integer_Check(method_word))
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
	  i = Pl_M_Random_Integer(n);
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
  return Pl_Unify(Tag_REF(fdv_adr), fdv_word);
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
  l_diff = Pl_Range_Next_After(Range(last_fdv_adr), min) - min;

  min = Min(new_fdv_adr);
  n_diff = Pl_Range_Next_After(Range(new_fdv_adr), min) - min;

  return n_diff > l_diff ||
    (n_diff == l_diff && Nb_Cstr(new_fdv_adr) > Nb_Cstr(last_fdv_adr));
}
