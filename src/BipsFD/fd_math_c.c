/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_math_c.c                                                     *
 * Descr.: mathematical predicate management - C part                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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




/*-------------------------------------------------------------------------*
 * FD_SET_FULL_AC_FLAG_1                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fd_Set_Full_Ac_Flag_1(WamWord full_ac_word)
{
  full_ac = Rd_Integer(full_ac_word);
}




/*-------------------------------------------------------------------------*
 * FD_EQ_2                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Eq_2(WamWord le_word, WamWord re_word)
{
  int mask;
  WamWord l_word, r_word;
  long c;


#ifdef DEBUG
  cur_op = (full_ac) ? "#=#" : "#=";
#endif

  if (!Load_Left_Right(TRUE, le_word, re_word, &mask, &c, &l_word, &r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      if (c != 0)
	return FALSE;
      goto term_load;

    case MASK_LEFT:
      if (c > 0)
	return FALSE;

      PRIM_CSTR_2(x_eq_c, l_word, Tag_INT(-c));
      goto term_load;

    case MASK_RIGHT:
      if (c < 0)
	return FALSE;

      PRIM_CSTR_2(x_eq_c, r_word, Tag_INT(c));
      goto term_load;
    }

  if (c > 0)
    {
      MATH_CSTR_3(x_plus_c_eq_y, l_word, Tag_INT(c), r_word);
      goto term_load;
    }

  if (c < 0)
    {
      MATH_CSTR_3(x_plus_c_eq_y, r_word, Tag_INT(-c), l_word);
      goto term_load;
    }
  /* if c == 0 nothing to do since preference via pref_load_word */
term_load:
  return Term_Math_Loading(l_word, r_word);
}




/*-------------------------------------------------------------------------*
 * FD_NEQ_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Neq_2(WamWord le_word, WamWord re_word)
{
  int mask;
  WamWord l_word, r_word;
  long c;


#ifdef DEBUG
  cur_op = (full_ac) ? "#\\=#" : "#\\=";
#endif

  if (!Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word, &r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      if (c == 0)
	return FALSE;
      goto term_load;

    case MASK_LEFT:
      if (c > 0)
	{
	  Fd_Prolog_To_Fd_Var(l_word, TRUE);
	  goto term_load;
	}

      PRIM_CSTR_2(x_neq_c, l_word, Tag_INT(-c));
      goto term_load;

    case MASK_RIGHT:
      if (c < 0)
	{
	  Fd_Prolog_To_Fd_Var(r_word, TRUE);
	  goto term_load;
	}

      PRIM_CSTR_2(x_neq_c, r_word, Tag_INT(c));
      goto term_load;
    }

  if (c > 0)
    {
      PRIM_CSTR_3(x_plus_c_neq_y, l_word, Tag_INT(c), r_word);
      goto term_load;
    }

  if (c < 0)
    {
      PRIM_CSTR_3(x_plus_c_neq_y, r_word, Tag_INT(-c), l_word);
      goto term_load;
    }


  PRIM_CSTR_2(x_neq_y, l_word, r_word);
term_load:
  return Term_Math_Loading(l_word, r_word);
}




/*-------------------------------------------------------------------------*
 * FD_LT_2                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Lt_2(WamWord le_word, WamWord re_word)
{
  int mask;
  WamWord l_word, r_word;
  long c;


#ifdef DEBUG
  cur_op = (full_ac) ? "#<#" : "#<";
#endif

  if (!Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word,
		       &r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      if (c >= 0)
	return FALSE;
      goto term_load;

    case MASK_LEFT:
      if (c >= 0)
	return FALSE;

      PRIM_CSTR_2(x_lte_c, l_word, Tag_INT(-c - 1));
      goto term_load;

    case MASK_RIGHT:
      if (c < 0)
	{
	  Fd_Prolog_To_Fd_Var(r_word, TRUE);
	  goto term_load;
	}

      PRIM_CSTR_2(x_gte_c, r_word, Tag_INT(c + 1));
      goto term_load;
    }

  if (c > 0)
    {
      PRIM_CSTR_3(x_plus_c_lte_y, l_word, Tag_INT(c + 1), r_word);
      goto term_load;
    }

  if (c < 0)
    {
      PRIM_CSTR_3(x_plus_c_gte_y, r_word, Tag_INT(-c - 1), l_word);
      goto term_load;
    }


  PRIM_CSTR_2(x_lt_y, l_word, r_word);
term_load:
  return Term_Math_Loading(l_word, r_word);
}




/*-------------------------------------------------------------------------*
 * FD_LTE_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Fd_Lte_2(WamWord le_word, WamWord re_word)
{
  int mask;
  WamWord l_word, r_word;
  long c;


#ifdef DEBUG
  cur_op = (full_ac) ? "#=<#" : "#=<";
#endif

  if (!Load_Left_Right(FALSE, le_word, re_word, &mask, &c, &l_word,
		       &r_word))
    return FALSE;

  switch (mask)
    {
    case MASK_EMPTY:
      if (c > 0)
	return FALSE;
      goto term_load;

    case MASK_LEFT:
      if (c > 0)
	return FALSE;

      PRIM_CSTR_2(x_lte_c, l_word, Tag_INT(-c));
      goto term_load;

    case MASK_RIGHT:
      if (c <= 0)
	{
	  Fd_Prolog_To_Fd_Var(r_word, TRUE);
	  goto term_load;
	}

      PRIM_CSTR_2(x_gte_c, r_word, Tag_INT(c));
      goto term_load;
    }

  if (c > 0)
    {
      PRIM_CSTR_3(x_plus_c_lte_y, l_word, Tag_INT(c), r_word);
      goto term_load;
    }

  if (c < 0)
    {
      PRIM_CSTR_3(x_plus_c_gte_y, r_word, Tag_INT(-c), l_word);
      goto term_load;
    }


  PRIM_CSTR_2(x_lte_y, l_word, r_word);
term_load:
  return Term_Math_Loading(l_word, r_word);
}
