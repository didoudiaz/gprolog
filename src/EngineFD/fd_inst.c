/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_inst.c                                                       *
 * Descr.: FD instruction implementation                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

#include <stdio.h>
#include <stdlib.h>

#define FD_INST_FILE

#define OBJ_INIT Fd_Inst_Initializer

#include "engine_pl.h"
#include "engine_fd.h"

#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define MSG_VECTOR_TOO_SMALL       "Warning: Vector too small - maybe lost solutions (FD Var:_%d)\n"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static WamWord DATE;
static WamWord *TP;

static WamWord dummy_fd_var[FD_VARIABLE_FRAME_SIZE];

static PlULong always_date = -1;	/* must be always > DATE */
static PlULong never_date = 0;		/* must be always < DATE */

void (*pl_fd_init_solver) () = Pl_Fd_Init_Solver0;	/* overwrite var of if_no_fd.c */
void (*pl_fd_reset_solver) () = Pl_Fd_Reset_Solver0;	/* overwrite var of if_no_fd.c */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void All_Propagations(WamWord *fdv_adr, int propag);




/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/

#define Trail_Fd_Int_Variable_If_Necessary(fdv_adr)  	\
  do							\
    {							\
      if (Word_Needs_Trailing(&FD_Tag_Value(fdv_adr)))	\
	{						\
	  Trail_OV(&FD_Tag_Value(fdv_adr));		\
	  Trail_OV(&FD_INT_Date(fdv_adr));		\
	  Trail_Range_If_Necessary(fdv_adr);		\
	}						\
    }							\
  while (0)




#define Trail_Range_If_Necessary(fdv_adr)                 	\
  do								\
    {								\
      if (Range_Stamp(fdv_adr) != STAMP)			\
	{							\
	  Trail_MV(fdv_adr + OFFSET_RANGE, RANGE_SIZE);		\
	  if (Is_Sparse(Range (fdv_adr)))			\
	      Trail_MV((WamWord *) Vec(fdv_adr), pl_vec_size);	\
								\
	  Range_Stamp(fdv_adr) = STAMP;				\
	}							\
    }								\
  while (0)





#define Trail_Chains_If_Necessary(fdv_adr)           		\
  do								\
    {								\
      if (Chains_Stamp(fdv_adr) != STAMP)			\
	{							\
	  Trail_MV(fdv_adr + OFFSET_CHAINS, CHAINS_SIZE);	\
	  Chains_Stamp(fdv_adr) = STAMP;			\
	}							\
    }								\
  while (0)




#define Update_Range_From_Int(fdv_adr, n, propag)  	\
  do							\
    {							\
      propag = MASK_EMPTY;				\
							\
      Trail_Fd_Int_Variable_If_Necessary(fdv_adr);	\
      Nb_Elem(fdv_adr) = 1;				\
							\
      Set_Min_Max_Mask(propag);				\
      Set_Dom_Mask(propag);				\
      Set_Val_Mask(propag);				\
							\
      if (Min(fdv_adr) != n)				\
	{						\
	  Min(fdv_adr) = n;				\
	  Set_Min_Mask(propag);				\
	}						\
							\
      if (Max(fdv_adr) != n)				\
	{						\
	  Max(fdv_adr) = n;				\
	  Set_Max_Mask(propag);				\
	}						\
							\
      Vec(fdv_adr) = NULL;				\
      FD_Tag_Value(fdv_adr) = Tag_INT(n);		\
      FD_INT_Date(fdv_adr) = DATE;			\
    }							\
  while (0)




#define Update_Interval_From_Interval(fdv_adr, nb_elem, min, max, propag) \
  do									  \
    {									  \
      propag = MASK_EMPTY;						  \
									  \
      if (Nb_Elem(fdv_adr) != nb_elem)					  \
	{								  \
	  Trail_Range_If_Necessary(fdv_adr);				  \
	  Nb_Elem(fdv_adr) = nb_elem;					  \
									  \
	  Set_Min_Max_Mask(propag);					  \
	  Set_Dom_Mask(propag);						  \
									  \
	  if (Min(fdv_adr) != min)					  \
	    {								  \
	      Min(fdv_adr) = min;					  \
	      Set_Min_Mask(propag);					  \
	    }								  \
									  \
	  if (Max(fdv_adr) != max)					  \
	    {								  \
	      Max(fdv_adr) = max;					  \
	      Set_Max_Mask(propag);					  \
	    }								  \
	}								  \
    }									  \
  while (0)




#define FD_Word_Needs_Trailing(adr)  ((adr) <  CSB(B))




#define FD_Bind_OV(adr, word)       		\
  do						\
    {						\
      if (FD_Word_Needs_Trailing(adr))		\
	Trail_OV(adr);				\
      *(adr) = (word);				\
    }						\
  while (0)




#define Update_Range_From_Range(fdv_adr, nb_elem, range, propag)           \
  do									   \
    {									   \
      Range *r = Range (fdv_adr);					   \
									   \
      propag = MASK_EMPTY;						   \
									   \
      if (Min(fdv_adr) != (range)->min)					   \
	{								   \
	  Set_Min_Mask(propag);						   \
	  Set_Min_Max_Mask(propag);					   \
	}								   \
									   \
      if (Max(fdv_adr) != (range)->max)					   \
	{								   \
	  Set_Max_Mask(propag);						   \
	  Set_Min_Max_Mask(propag);					   \
	}								   \
									   \
      if (Nb_Elem(fdv_adr) != nb_elem)					   \
	Set_Dom_Mask(propag);						   \
									   \
      if (propag || (Is_Interval(r) && Is_Sparse(range)))		   \
	{								   \
	  Trail_Range_If_Necessary(fdv_adr);				   \
	  Nb_Elem(fdv_adr) = nb_elem;					   \
	  Pl_Range_Copy(r, range);				       	   \
	}								   \
      else if (r->extra_cstr != (range)->extra_cstr)			   \
	{								   \
	  FD_Bind_OV((WamWord *) &(r->extra_cstr), (range->extra_cstr));   \
	  Set_Dom_Mask(propag);						   \
	}								   \
    }									   \
  while (0)





/*-------------------------------------------------------------------------*
 * FD_INST_INITIALIZER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Fd_Inst_Initializer(void)
{
  pl_fd_init_solver = Pl_Fd_Init_Solver0;
  pl_fd_reset_solver = Pl_Fd_Reset_Solver0;
}





/*-------------------------------------------------------------------------*
 * PL_FD_INIT_SOLVER0                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Init_Solver0(void)
{
  char *p;
  int max_val;


  p = (char *) getenv(ENV_VAR_VECTOR_MAX);
  if (p && *p)
    sscanf(p, "%d", &max_val);
  else
    max_val = DEFAULT_VECTOR_MAX;

  Pl_Define_Vector_Size(max_val);

  Pl_Fd_Reset_Solver0();

  pl_fd_unify_with_integer = Pl_Fd_Unify_With_Integer0;
  pl_fd_unify_with_fd_var = Pl_Fd_Unify_With_Fd_Var0;
  pl_fd_variable_size = Pl_Fd_Variable_Size0;
  pl_fd_copy_variable = Pl_Fd_Copy_Variable0;
  pl_fd_variable_to_string = Pl_Fd_Variable_To_String0;
}



/*-------------------------------------------------------------------------*
 * PL_FD_RESET_SOLVER0                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Reset_Solver0(void)
{
  STAMP = DATE = 0;
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_FD_VAR                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_Prolog_To_Fd_Var(WamWord arg_word, Bool pl_var_ok)
{
  WamWord word, tag_mask;
  WamWord *adr, *fdv_adr;


  DEREF(arg_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    {
      if (!pl_var_ok)
	Pl_Err_Instantiation();

      adr = UnTag_REF(word);
      fdv_adr = Pl_Fd_New_Variable();
      Bind_UV(adr, Tag_REF(fdv_adr));
      return fdv_adr;
    }

  if (tag_mask == TAG_INT_MASK)
    return Pl_Fd_New_Int_Variable(UnTag_INT(word));

  if (tag_mask == TAG_FDV_MASK)
    return UnTag_FDV(word);

  Pl_Err_Type(pl_type_fd_variable, word);
  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_RANGE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Range *
Pl_Fd_Prolog_To_Range(WamWord list_word)
{
  Range *range;

  range = (Range *) CS;
  CS += sizeof(Range) / sizeof(WamWord);

  range->vec = NULL;

  Pl_Fd_List_Int_To_Range(range, list_word);

  return range;
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_VALUE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Fd_Prolog_To_Value(WamWord arg_word)
{
  WamWord word, tag_mask;

  DEREF(arg_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    Pl_Err_Instantiation();

  if (tag_mask != TAG_INT_MASK)
    Pl_Err_Type(pl_type_integer, word);

  return UnTag_INT(word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_LIST_INT_TO_RANGE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_List_Int_To_Range(Range *range, WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  WamWord val;
  int n = 0;


  save_list_word = list_word;

  range->extra_cstr = FALSE;
  Vector_Allocate_If_Necessary(range->vec);
  Pl_Vector_Empty(range->vec);

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

      if (tag_mask != TAG_INT_MASK)
	Pl_Err_Type(pl_type_integer, word);


      val = UnTag_INT(word);

      if ((unsigned) val > (unsigned) pl_vec_max_integer)
	range->extra_cstr = TRUE;
      else
	{
	  Vector_Set_Value(range->vec, val);
	  n++;
	}

      list_word = Cdr(lst_adr);
    }

  if (n == 0)
    Set_To_Empty(range);
  else
    Pl_Range_From_Vector(range);
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_ARRAY_INT                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_Prolog_To_Array_Int(WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  WamWord val;
  int n = 0;
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

      if (tag_mask != TAG_INT_MASK)
	Pl_Err_Type(pl_type_integer, word);


      val = UnTag_INT(word);

      *array++ = val;
      n++;

      list_word = Cdr(lst_adr);
    }


  *save_array = n;

  CS = array;

  return save_array;
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_ARRAY_ANY                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_Prolog_To_Array_Any(WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  int n = 0;
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

      *array++ = Car(lst_adr);
      n++;

      list_word = Cdr(lst_adr);
    }


  *save_array = n;

  CS = array;

  return save_array;
}




/*-------------------------------------------------------------------------*
 * PL_FD_PROLOG_TO_ARRAY_FDV                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_Prolog_To_Array_Fdv(WamWord list_word, Bool pl_var_ok)
{
  WamWord word, tag_mask;
  WamWord save_list_word;
  WamWord *lst_adr;
  int n = 0;
  WamWord *save_array;
  WamWord *array;


  /* compute the length of the list to */
  /* reserve space in the heap for the */
  /* array before pushing new FD vars. */

  save_list_word = list_word;

  for (;;)
    {
      DEREF(list_word, word, tag_mask);
      if (tag_mask != TAG_LST_MASK)
	break;
      lst_adr = UnTag_LST(word);
      n++;
      list_word = Cdr(lst_adr);
    }

  array = CS;
  CS = CS + n + 1;


  list_word = save_list_word;
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

      *array++ = (WamWord) Pl_Fd_Prolog_To_Fd_Var(Car(lst_adr), pl_var_ok);

      list_word = Cdr(lst_adr);
    }


  *save_array = n;

  return save_array;
}




/*-------------------------------------------------------------------------*
 * PL_FD_CREATE_C_FRAME                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_Create_C_Frame(PlLong (*cstr_fct) (), WamWord *AF, WamWord *fdv_adr,
		  Bool optim2)
{
  WamWord *CF = CS;

  AF_Pointer(CF) = AF;
  Optim_Pointer(CF) = (optim2 && fdv_adr) ? &FD_INT_Date(fdv_adr)
    : (WamWord *) &always_date;
  Cstr_Address(CF) = cstr_fct;

  /* if ground Nb_Cstr not allocated (Fd_Int_Frame) */
  if (fdv_adr && !Fd_Variable_Is_Ground(fdv_adr))
    Nb_Cstr(fdv_adr)++;

  CS += CONSTRAINT_FRAME_SIZE;

  return CF;
}




/*-------------------------------------------------------------------------*
 * PL_FD_ADD_DEPENDENCY                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Add_Dependency(WamWord *fdv_adr, int chain_nb, WamWord *CF)
{
  WamWord **chain_adr;

  if (Fd_Variable_Is_Ground(fdv_adr))
    return;

  Trail_Chains_If_Necessary(fdv_adr);

  Chains_Mask(fdv_adr) |= (1 << chain_nb);

  chain_adr = (WamWord **) (&Chain_Min(fdv_adr) + chain_nb);

  CF_Pointer(CS) = CF;
  Next_Chain(CS) = *chain_adr;

  *chain_adr = CS;

  CS += CHAIN_RECORD_FRAME_SIZE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_ADD_LIST_DEPENDENCY                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Add_List_Dependency(WamWord *array, int chain_nb, WamWord *CF)
{
  int n = *array++;

  while (n--)
    Pl_Fd_Add_Dependency((WamWord *) (*array++), chain_nb, CF);
}




/*-------------------------------------------------------------------------*
 * PL_FD_NEW_VARIABLE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_New_Variable(void)
{
  WamWord *fdv_adr = CS;

  FD_Tag_Value(fdv_adr) = Tag_FDV(fdv_adr);
  FD_INT_Date(fdv_adr) = always_date;	/* must be>DATE while tag==FDV */
  Queue_Date_At_Push(fdv_adr) = 0;
  Queue_Propag_Mask(fdv_adr) = 0;
  Queue_Next_Fdv_Adr(fdv_adr) = (WamWord) NULL;

  Range_Stamp(fdv_adr) = STAMP;
  Nb_Elem(fdv_adr) = INTERVAL_MAX_INTEGER + 1;
  Range_Init_Interval(Range(fdv_adr), 0, INTERVAL_MAX_INTEGER);

  Chains_Stamp(fdv_adr) = STAMP;
  Nb_Cstr(fdv_adr) = 0;
  Chains_Mask(fdv_adr) = MASK_EMPTY;
  Chain_Min(fdv_adr) = Chain_Max(fdv_adr) = Chain_Min_Max(fdv_adr) =
    (WamWord) NULL;
  Chain_Dom(fdv_adr) = Chain_Val(fdv_adr) = (WamWord) NULL;

  CS += FD_VARIABLE_FRAME_SIZE;
  return fdv_adr;
}




/*-------------------------------------------------------------------------*
 * PL_FD_NEW_BOOL_VARIABLE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_New_Bool_Variable(void)
{
  WamWord *fdv_adr = Pl_Fd_New_Variable();

  Nb_Elem(fdv_adr) = 2;
  Min(fdv_adr) = 0;
  Max(fdv_adr) = 1;

  return fdv_adr;
}




/*-------------------------------------------------------------------------*
 * PL_FD_NEW_INT_VARIABLE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamWord *
Pl_Fd_New_Int_Variable(int n)
{
  WamWord *fdv_adr = CS;

  FD_Tag_Value(fdv_adr) = Tag_INT(n);
  FD_INT_Date(fdv_adr) = DATE;	/* put a great value to have an exact optim #2 */
  Queue_Date_At_Push(fdv_adr) = 0;
  Queue_Propag_Mask(fdv_adr) = 0;
  Queue_Next_Fdv_Adr(fdv_adr) = (WamWord) NULL;

  Range_Stamp(fdv_adr) = STAMP;
  Nb_Elem(fdv_adr) = 1;
  Range_Init_Interval(Range(fdv_adr), n, n);

  CS += FD_INT_VARIABLE_FRAME_SIZE;

  return fdv_adr;
}




/*-------------------------------------------------------------------------*
 * PL_FD_BEFORE_ADD_CSTR                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Before_Add_Cstr(void)
{
  TP = dummy_fd_var;
  DATE++;
  if (DATE < 0)
    DATE = 1;
}




/*-------------------------------------------------------------------------*
 * PL_FD_TELL_VALUE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Tell_Value(WamWord *fdv_adr, int n)
{
  int propag;

  if (!Pl_Range_Test_Value(Range(fdv_adr), n))
    {
      if (Extra_Cstr(fdv_adr) && n > pl_vec_max_integer)
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      return FALSE;
    }

  if (Fd_Variable_Is_Ground(fdv_adr))
    return TRUE;

  Update_Range_From_Int(fdv_adr, n, propag);
  All_Propagations(fdv_adr, propag);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_TELL_NOT_VALUE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Tell_Not_Value(WamWord *fdv_adr, int n)
{
  Range *r;
  int min, max;
  int propag;

start:

  r = Range(fdv_adr);

  if (!Pl_Range_Test_Value(r, n))
    return TRUE;

  if (Fd_Variable_Is_Ground(fdv_adr))
    {
      if (Extra_Cstr(fdv_adr))
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      return FALSE;
    }

  min = r->min;
  max = r->max;


  if (Is_Interval(r) && n != min && n != max)
    {
      if (min > pl_vec_max_integer)
	{
	  Pl_Fd_Display_Extra_Cstr(fdv_adr);
	  return FALSE;
	}

      if (min == pl_vec_max_integer)
	{
	  Pl_Fd_Display_Extra_Cstr(fdv_adr);
	  Update_Range_From_Int(fdv_adr, min, propag);
	  All_Propagations(fdv_adr, propag);
	  return TRUE;
	}

      Trail_Range_If_Necessary(fdv_adr);
      Pl_Range_Becomes_Sparse(r);
      Nb_Elem(fdv_adr) = r->max - r->min + 1;
      if (r->extra_cstr)	/* the max has been changed */
	{
	  propag = MASK_EMPTY;
	  Set_Max_Mask(propag);
	  Set_Min_Max_Mask(propag);
	  Set_Dom_Mask(propag);
	  All_Propagations(fdv_adr, propag);
	}
      goto start;
    }


  if (Nb_Elem(fdv_adr) == 2)
    {
      if (n == min)
	min = max;

      Update_Range_From_Int(fdv_adr, min, propag);
      goto do_propag;
    }

  /* here if sparse OR n==min OR n==max */
  Trail_Range_If_Necessary(fdv_adr);

  propag = MASK_EMPTY;
  Set_Dom_Mask(propag);
  if (Is_Sparse(r))
    Vector_Reset_Value(r->vec, n);

  Nb_Elem(fdv_adr)--;

  if (n == min)
    {
      Set_Min_Mask(propag);
      Set_Min_Max_Mask(propag);
      r->min = (Is_Interval(r)) ? n + 1 : Pl_Vector_Next_After(r->vec, n);
      goto do_propag;
    }

  if (n == max)
    {
      Set_Max_Mask(propag);
      Set_Min_Max_Mask(propag);
      r->max = (Is_Interval(r)) ? n - 1 : Pl_Vector_Next_Before(r->vec, n);
      goto do_propag;
    }

do_propag:
  All_Propagations(fdv_adr, propag);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_TELL_INT_RANGE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Tell_Int_Range(WamWord *fdv_adr, Range *range)
{
  int n = Min(fdv_adr);

  if (!Pl_Range_Test_Value(range, n))
    {
      if (n > pl_vec_max_integer && range->extra_cstr)
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      return FALSE;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_TELL_INTERV_INTERV                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Tell_Interv_Interv(WamWord *fdv_adr, int min, int max)
{
  int nb_elem;
  int propag;
  int min1, max1;


  min1 = Min(fdv_adr);
  max1 = Max(fdv_adr);

  min = math_max(min, min1);
  max = math_min(max, max1);

  if (min > max)		/* detects also if the initial   */
    return FALSE;		/* interval (min, max) was empty */

  if (min == max)
    Update_Range_From_Int(fdv_adr, min, propag);
  else
    {
      nb_elem = max - min + 1;
      Update_Interval_From_Interval(fdv_adr, nb_elem, min, max, propag);
    }

  if (propag)
    All_Propagations(fdv_adr, propag);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_TELL_RANGE_RANGE                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Tell_Range_Range(WamWord *fdv_adr, Range *range)
{
  int nb_elem;
  int propag;
  WamWord *save_CS = CS;

  if (range->vec)
    CS = (WamWord *) range->vec;
  CS += pl_vec_size;

  Pl_Range_Inter(range, Range(fdv_adr));

  CS = save_CS;

  if (Is_Empty(range))
    {
      if (range->extra_cstr)
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      return FALSE;
    }

  if (range->min == range->max)
    {
      if (range->extra_cstr)
	Pl_Fd_Display_Extra_Cstr(fdv_adr);

      Update_Range_From_Int(fdv_adr, range->min, propag);
    }
  else
    {
      nb_elem = Pl_Range_Nb_Elem(range);
      Update_Range_From_Range(fdv_adr, nb_elem, range, propag);
    }

  if (propag)
    All_Propagations(fdv_adr, propag);

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * ALL_PROPAGATIONS                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
All_Propagations(WamWord *fdv_adr, int propag)
{
  if (propag &= Chains_Mask(fdv_adr))
    {
      if (Queue_Date_At_Push(fdv_adr) < DATE)	/* not yet in the queue */
	{
	  Queue_Date_At_Push(fdv_adr) = DATE;
	  Queue_Propag_Mask(fdv_adr) = propag;
	  Queue_Next_Fdv_Adr(TP) = (WamWord) fdv_adr;
	  TP = fdv_adr;
	}
      else			/* already in the queue */
	Queue_Propag_Mask(fdv_adr) |= propag;
    }
}




/*-------------------------------------------------------------------------*
 * PL_FD_AFTER_ADD_CSTR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_After_Add_Cstr(void)
{
  WamWord *fdv_adr;
  WamWord propag;
  WamWord *record_adr;
  WamWord *chain_adr;
  WamWord *CF;
  WamWord *BP;
  PlULong date = DATE;
  PlULong *pdate;
  WamWord *AF;
  PlLong (*fct) ();

  if (TP == dummy_fd_var)
    return TRUE;

  BP = dummy_fd_var;
  BP = (WamWord *) Queue_Next_Fdv_Adr(BP);

  for (;;)
    {
      fdv_adr = (WamWord *) BP;
      propag = Queue_Propag_Mask(fdv_adr);
      Queue_Propag_Mask(fdv_adr) = 0;

      chain_adr = &Chain_Min(fdv_adr);

      for (; propag; propag >>= 1, chain_adr++)
	if (propag & 1)
	  {
	    record_adr = (WamWord *) (*chain_adr);
	    do
	      {
		CF = CF_Pointer(record_adr);

#if 1
		/* optim #2 */
		pdate = (PlULong *) Optim_Pointer(CF);
		if (*pdate < date)
		  continue;
#endif

		fct = Cstr_Address(CF);
		AF = AF_Pointer(CF);

		fct = (PlLong (*)()) (*fct) (AF);

		if (fct == (PlLong (*)()) FALSE)
		  return FALSE;
#if 1				/* FD switch */
		if (fct != (PlLong (*)()) TRUE)	/* FD switch case triggered */
		  {
		    if ((*fct) (AF) == FALSE)
		      return FALSE;

		    Pl_Fd_Stop_Constraint(CF);
		  }
#endif
	      }
	    while ((record_adr = Next_Chain(record_adr)) != NULL);
	  }

      if (Queue_Propag_Mask(fdv_adr) == 0)
	{
	  if (BP == TP)
	    break;

	  BP = (WamWord *) Queue_Next_Fdv_Adr(BP);
	  Queue_Date_At_Push(fdv_adr) = 0;
	}
    }


  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FD_STOP_CONSTRAINT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Stop_Constraint(WamWord *CF)
{
  FD_Bind_OV((WamWord *) (CF + OFFSET_OF_OPTIM_POINTER),
	     (WamWord) (&never_date));
}




/*-------------------------------------------------------------------------*
 * PL_FD_ASSIGN_VALUE                                                      *
 *                                                                         *
 * fdv_adr is a FDV and n belongs to the range of the FD var               *
 * like Pl_Fd_Unify_With_Integer0() but specialized Fd_Tell_Value without  *
 * useless tests (ie. groundness and Pl_Range_Test_Value())                *
 * Used by labeling predicates.                                            *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Assign_Value(WamWord *fdv_adr, int n)
{
  int propag;

  /* Pl_Unify(X,n) == X in n..n */
  Pl_Fd_Before_Add_Cstr();

  Update_Range_From_Int(fdv_adr, n, propag);
  All_Propagations(fdv_adr, propag);
  return Pl_Fd_After_Add_Cstr();
}




/*-------------------------------------------------------------------------*
 * PL_FD_UNIFY_WITH_INTEGER0                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Unify_With_Integer0(WamWord *fdv_adr, int n)
{
  /* Pl_Unify(X,n) == X in n..n */
  Pl_Fd_Before_Add_Cstr();

  return Pl_Fd_Tell_Value(fdv_adr, n) && Pl_Fd_After_Add_Cstr();
}




/*-------------------------------------------------------------------------*
 * PL_FD_UNIFY_WITH_FD_VAR0                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Unify_With_Fd_Var0(WamWord *fdv_adr1, WamWord *fdv_adr2)
{
  Bool pl_unify_x_y(WamWord x, WamWord y);

  /* defined in fd_unify.fd as a constraint */
  return pl_unify_x_y(Tag_REF(fdv_adr1), Tag_REF(fdv_adr2));
}




/*-------------------------------------------------------------------------*
 * PL_FD_USE_VECTOR                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Use_Vector(WamWord *fdv_adr)
{
  Range range;

  if (Is_Sparse(Range(fdv_adr)))
    return TRUE;

  Pl_Fd_Before_Add_Cstr();
  {
    WamWord *save_CS=CS;	/* code of fd_allocate (from fd_to_c.h) */
    CS+=pl_vec_size;

    Range_Init_Interval(&range, 0, INTERVAL_MAX_INTEGER);

    Pl_Range_Becomes_Sparse(&range);

    CS=save_CS;			/* code of fd_deallocate (from fd_to_c.h) */
  }
  return Pl_Fd_Tell_Range_Range(fdv_adr, &range) && Pl_Fd_After_Add_Cstr();
}




/*-------------------------------------------------------------------------*
 * PL_FD_CHECK_FOR_BOOL_VAR                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Check_For_Bool_Var(WamWord x_word)
{
  WamWord word, tag_mask;
  WamWord *adr, *fdv_adr;
  Range range;


  DEREF(x_word, word, tag_mask);

  if (tag_mask == TAG_REF_MASK)
    {
      adr = UnTag_REF(word);
      fdv_adr = Pl_Fd_New_Bool_Variable();
      Bind_UV(adr, Tag_REF(fdv_adr));
      return TRUE;
    }

  if (tag_mask == TAG_INT_MASK)
    return (PlULong) (UnTag_INT(word)) <= 1;

  if (tag_mask != TAG_FDV_MASK)
    Pl_Err_Type(pl_type_fd_variable, word);

  fdv_adr = UnTag_FDV(word);

  if (Min(fdv_adr) > 1)
    return FALSE;

  if (Max(fdv_adr) <= 1)
    return TRUE;
				/* here max > 1 */
  if (Min(fdv_adr) == 1)
    return Pl_Fd_Unify_With_Integer0(fdv_adr, 1);

				/* here min == 0 */

  if (!Pl_Range_Test_Value(Range(fdv_adr), 1))
    return Pl_Fd_Unify_With_Integer0(fdv_adr, 0);


				/* Check Bool == X in 0..1 */
  Pl_Fd_Before_Add_Cstr();

  if (Is_Sparse(Range(fdv_adr)))
    {
      Range_Init_Interval(&range, 0, 1);

      if (!Pl_Fd_Tell_Range_Range(fdv_adr, &range))
	return FALSE;
    }
  else if (!Pl_Fd_Tell_Interv_Interv(fdv_adr, 0, 1))
    return FALSE;

  return Pl_Fd_After_Add_Cstr();
}




/*-------------------------------------------------------------------------*
 * PL_FD_VARIABLE_SIZE0                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Fd_Variable_Size0(WamWord *fdv_adr)
{
  int size = FD_VARIABLE_FRAME_SIZE;

  if (Is_Sparse(Range(fdv_adr)))
      size += pl_vec_size;

  return size;
}




/*-------------------------------------------------------------------------*
 * PL_FD_COPY_VARIABLE0                                                    *
 *                                                                         *
 * returns the size of the created fd var.                                 *
 *-------------------------------------------------------------------------*/
int
Pl_Fd_Copy_Variable0(WamWord *dst_adr, WamWord *fdv_adr)
{
  WamWord *save_CS;
  int size;

  save_CS = CS;
  CS = dst_adr;

  Pl_Fd_New_Variable();		/* we know that it is pushed at CS (=dst_adr) */
  Nb_Elem(dst_adr) = Nb_Elem(fdv_adr);

  Pl_Range_Copy(Range(dst_adr), Range(fdv_adr));

  size = CS - dst_adr;
  CS = save_CS;

  return size;
}




/*-------------------------------------------------------------------------*
 * PL_FD_VARIABLE_TO_STRING0                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Fd_Variable_To_String0(WamWord *fdv_adr)
{
  return Pl_Range_To_String(Range(fdv_adr));
}




/*-------------------------------------------------------------------------*
 * PL_FD_DISPLAY_EXTRA_CSTR                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Display_Extra_Cstr(WamWord *fdv_adr)
{
  Pl_Stream_Printf(pl_stm_tbl[pl_stm_stdout], MSG_VECTOR_TOO_SMALL,
		Cstr_Offset(fdv_adr));
}
