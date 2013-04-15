/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_prime_c.c                                                    *
 * Descr.: prime constraint management - C part                            *
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

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int prime_vec_size;
static Range prime_range;
static Range not_prime_range;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Compute_Prime_Range(void);




/*-------------------------------------------------------------------------*
 * PL_PRIME_RANGE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Prime_Range(Range *r)
{
  if (prime_vec_size != pl_vec_size)
    Compute_Prime_Range();

  Pl_Range_Copy(r, &prime_range);
}




/*-------------------------------------------------------------------------*
 * PL_NOT_PRIME_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Not_Prime_Range(Range *r)
{
  if (prime_vec_size != pl_vec_size)
    Compute_Prime_Range();

  Pl_Range_Copy(r, &not_prime_range);
}




/*-------------------------------------------------------------------------*
 * COMPUTE_PRIME_RANGE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Compute_Prime_Range(void)
{
  int i, j;
  Vector vec, nvec, end;

  if (prime_range.vec)
    {
      Free(prime_range.vec);
      Free(not_prime_range.vec);
    }

  prime_range.vec = vec = (Vector) Malloc(pl_vec_size * sizeof(VecWord));
  not_prime_range.vec = nvec = (Vector) Malloc(pl_vec_size * sizeof(VecWord));

  Pl_Vector_Full(vec);
  Vector_Reset_Value(vec, 0);
  Vector_Reset_Value(vec, 1);

  i = 2;
  do
    {
      j = i;
      while ((j += i) <= pl_vec_max_integer)
	Vector_Reset_Value(vec, j);

      j = i;
      i = Pl_Vector_Next_After(vec, i);
    }
  while (i > 0);

  prime_range.extra_cstr = TRUE;
  prime_range.min = 2;
  prime_range.max = j;

  not_prime_range.extra_cstr = TRUE;
  not_prime_range.min = 0;
  not_prime_range.max = (j < pl_vec_max_integer) ? pl_vec_max_integer
    : pl_vec_max_integer - 1;


  end = vec + pl_vec_size;

  do
    {
      *nvec = ~(*vec);
      vec++;
      nvec++;
    }
  while (vec < end);

  prime_vec_size = pl_vec_size;
}
