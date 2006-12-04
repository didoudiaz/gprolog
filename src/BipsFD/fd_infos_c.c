/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_infos_c.c                                                    *
 * Descr.: FD variable information management - C part                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2006 Daniel Diaz                                     *
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
 * FD_VECTOR_MAX_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Vector_Max_1(WamWord max_word)
{
  return Un_Integer_Check(vec_max_integer, max_word);
}




/*-------------------------------------------------------------------------*
 * FD_SET_VECTOR_MAX_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Set_Vector_Max_1(WamWord max_word)
{
  Define_Vector_Size(Rd_Positive_Check(max_word));
}




/*-------------------------------------------------------------------------*
 * FD_MAX_INTEGER_1                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Max_Integer_1(WamWord inf_word)
{
  return Un_Integer_Check(INTERVAL_MAX_INTEGER, inf_word);
}




/*-------------------------------------------------------------------------*
 * FD_MIN_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Min_2(WamWord fdv_word, WamWord min_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = UnTag_INT(word);
  else
    n = Min(UnTag_FDV(word));

  return Un_Integer_Check(n, min_word);
}




/*-------------------------------------------------------------------------*
 * FD_MAX_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Max_2(WamWord fdv_word, WamWord max_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = UnTag_INT(word);
  else
    n = Max(UnTag_FDV(word));

  return Un_Integer_Check(n, max_word);
}




/*-------------------------------------------------------------------------*
 * FD_DOM_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Dom_2(WamWord fdv_word, WamWord list_word)
{
  WamWord word, tag_mask;
  WamWord *fdv_adr;
  int x, end;
  int vec_elem;

  Check_For_Un_List(list_word);

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    {
      x = UnTag_INT(word);

      if (!Get_List(list_word) || !Unify_Integer(x))
	return FALSE;

      list_word = Unify_Variable();
    }
  else
    {
      fdv_adr = UnTag_FDV(word);
      if (Is_Interval(Range(fdv_adr)))
	{
	  end = Max(fdv_adr);
	  for (x = Min(fdv_adr); x <= end; x++)
	    {
	      if (!Get_List(list_word) || !Unify_Integer(x))
		return FALSE;

	      list_word = Unify_Variable();
	    }
	}
      else
	{
	  VECTOR_BEGIN_ENUM(Vec(fdv_adr), vec_elem);

	  if (!Get_List(list_word) || !Unify_Integer(vec_elem))
	    return FALSE;

	  list_word = Unify_Variable();

	  VECTOR_END_ENUM;
	}
    }

  return Get_Nil(list_word);
}




/*-------------------------------------------------------------------------*
 * FD_SIZE_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Size_2(WamWord fdv_word, WamWord size_word)
{
  WamWord word, tag_mask;
  int n;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);
  if (tag_mask == TAG_INT_MASK)
    n = 1;
  else
    n = Nb_Elem(UnTag_FDV(word));

  return Un_Integer_Check(n, size_word);
}




/*-------------------------------------------------------------------------*
 * FD_HAS_EXTRA_CSTR_1                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Has_Extra_Cstr_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_FDV_MASK && Extra_Cstr(UnTag_FDV(word));
}




/*-------------------------------------------------------------------------*
 * FD_HAS_VECTOR_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Has_Vector_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_FDV_MASK && Is_Sparse(Range(UnTag_FDV(word)));
}




/*-------------------------------------------------------------------------*
 * FD_USE_VECTOR_1                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Use_Vector_1(WamWord fdv_word)
{
  WamWord word, tag_mask;

  Fd_Deref_Check_Fd_Var(fdv_word, word, tag_mask);

  return tag_mask == TAG_INT_MASK || Fd_Use_Vector(UnTag_FDV(word));
}
