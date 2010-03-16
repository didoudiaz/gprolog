/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : random_c.c                                                      *
 * Descr.: random number generator management - C part                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
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
 * PL_SET_SEED_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Seed_1(WamWord seed_word)
{
  Pl_M_Set_Seed(Pl_Rd_Positive_Check(seed_word));
}




/*-------------------------------------------------------------------------*
 * PL_GET_SEED_1                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Seed_1(WamWord seed_word)
{
  return Pl_Un_Positive_Check(Pl_M_Get_Seed(), seed_word);
}




/*-------------------------------------------------------------------------*
 * PL_RANDOM_1                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Random_1(WamWord n_word)
{
  Pl_Check_For_Un_Variable(n_word);
  Pl_Get_Float(Pl_M_Random_Float(1.0), n_word);
}




/*-------------------------------------------------------------------------*
 * PL_RANDOM_3                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Random_3(WamWord l_word, WamWord u_word, WamWord n_word)
{
  double l, u;
  long l1, u1;
  long i;
  double d;


  l = Pl_Rd_Number_Check(l_word);
  u = Pl_Rd_Number_Check(u_word);
  Pl_Check_For_Un_Variable(n_word);

  if (l >= u)
    return FALSE;

  l1 = (long) l;
  u1 = (long) u;

  if (l1 == l && u1 == u)
    {
      i = l1 + Pl_M_Random_Integer(u1 - l1);
      Pl_Get_Integer(i, n_word);
    }
  else
    {
      d = l + Pl_M_Random_Float(u - l);
      Pl_Get_Float(d, n_word);
    }

  return TRUE;
}
