/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : random_c.c                                                      *
 * Descr.: random number generator management - C part                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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
 * SET_SEED_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Seed_1(WamWord seed_word)
{
  M_Set_Seed(Rd_Positive_Check(seed_word));
}




/*-------------------------------------------------------------------------*
 * GET_SEED_1                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Seed_1(WamWord seed_word)
{
  return Un_Positive_Check(M_Get_Seed(), seed_word);
}




/*-------------------------------------------------------------------------*
 * RANDOM_1                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Random_1(WamWord n_word)
{
  Check_For_Un_Variable(n_word);
  Get_Float(M_Random_Float(1.0), n_word);
}




/*-------------------------------------------------------------------------*
 * RANDOM_3                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Random_3(WamWord l_word, WamWord u_word, WamWord n_word)
{
  double l, u;
  long l1, u1;
  long i;
  double d;


  l = Rd_Number_Check(l_word);
  u = Rd_Number_Check(u_word);
  Check_For_Un_Variable(n_word);

  if (l >= u)
    return FALSE;

  l1 = (long) l;
  u1 = (long) u;

  if (l1 == l && u1 == u)
    {
      i = l1 + M_Random_Integer(u1 - l1);
      Get_Integer(i, n_word);
    }
  else
    {
      d = l + M_Random_Float(u - l);
      Get_Float(d, n_word);
    }

  return TRUE;
}
