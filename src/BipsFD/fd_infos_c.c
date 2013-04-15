/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_infos_c.c                                                    *
 * Descr.: FD variable information management - C part                     *
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
#include "engine_fd.h"

#include "bips_pl.h"
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




/*-------------------------------------------------------------------------*
 * PL_FD_VECTOR_MAX_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Vector_Max_1(WamWord max_word)
{
  return Pl_Un_Integer_Check(pl_vec_max_integer, max_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_SET_VECTOR_MAX_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Fd_Set_Vector_Max_1(WamWord max_word)
{
  Pl_Define_Vector_Size(Pl_Rd_Positive_Check(max_word));
}




/*-------------------------------------------------------------------------*
 * PL_FD_MAX_INTEGER_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Max_Integer_1(WamWord inf_word)
{
  return Pl_Un_Integer_Check(INTERVAL_MAX_INTEGER, inf_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_MIN_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Min_2(WamWord fdv_word, WamWord min_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = UnTag_INT(word);
  else
    n = Min(UnTag_FDV(word));

  return Pl_Un_Integer_Check(n, min_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_MAX_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Max_2(WamWord fdv_word, WamWord max_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = UnTag_INT(word);
  else
    n = Max(UnTag_FDV(word));

  return Pl_Un_Integer_Check(n, max_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_DOM_2                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Dom_2(WamWord fdv_word, WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord *fdv_adr;
  int x, end;
  int vec_elem;

  Pl_Check_For_Un_List(list_word);

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      x = UnTag_INT(word);

      if (!Pl_Get_List(list_word) || !Pl_Unify_Integer(x))
	return FALSE;

      list_word = Pl_Unify_Variable();
    }
  else
    {
      fdv_adr = UnTag_FDV(word);
      if (Is_Interval(Range(fdv_adr)))
	{
	  end = Max(fdv_adr);
	  for (x = Min(fdv_adr); x <= end; x++)
	    {
	      if (!Pl_Get_List(list_word) || !Pl_Unify_Integer(x))
		return FALSE;

	      list_word = Pl_Unify_Variable();
	    }
	}
      else
	{
	  VECTOR_BEGIN_ENUM(Vec(fdv_adr), vec_elem);

	  if (!Pl_Get_List(list_word) || !Pl_Unify_Integer(vec_elem))
	    return FALSE;

	  list_word = Pl_Unify_Variable();

	  VECTOR_END_ENUM;
	}
    }

  return Pl_Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_SIZE_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Size_2(WamWord fdv_word, WamWord size_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = 1;
  else
    n = Nb_Elem(UnTag_FDV(word));

  return Pl_Un_Integer_Check(n, size_word);
}




/*-------------------------------------------------------------------------*
 * PL_FD_HAS_EXTRA_CSTR_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Has_Extra_Cstr_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_FDV_MASK && Extra_Cstr(UnTag_FDV(word));
}




/*-------------------------------------------------------------------------*
 * PL_FD_HAS_VECTOR_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Has_Vector_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_FDV_MASK && Is_Sparse(Range(UnTag_FDV(word)));
}




/*-------------------------------------------------------------------------*
 * PL_FD_USE_VECTOR_1                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Fd_Use_Vector_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_INT_MASK || Pl_Fd_Use_Vector(UnTag_FDV(word));
}
