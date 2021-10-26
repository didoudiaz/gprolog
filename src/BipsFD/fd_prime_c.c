/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver buit-in predicates                         *
 * File  : fd_prime_c.c                                                    *
 * Descr.: prime constraint management - C part                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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

static int computed_prime_range = 0;
static Range prime_range;
static Range not_prime_range;
/* Choose a maximum value for the prime range      *
 * max_prime_value = 10^2 ==>         25 primes    *
 * max_prime_value = 10^3 ==>        168 primes    *
 * max_prime_value = 10^4 ==>       1229 primes    *
 * max_prime_value = 10^5 ==>       9592 primes    *
 * max_prime_value = 10^6 ==>     78,498 primes    */

#if 0
static int max_prime_value = 1000;
#else
static int max_prime_value = 10000;
#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static void Compute_Prime_Range(void);

static void Add_Value_To_Prime_Range(int value);



/*-------------------------------------------------------------------------*
 * PL_PRIME_RANGE                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Prime_Range(Range *r)
{
  if (!computed_prime_range)
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
  if (!computed_prime_range)
    Compute_Prime_Range();

  Pl_Range_Copy(r, &not_prime_range);
}


static void
Add_Value_To_Prime_Range(int value) {
  Chunk *chunk = (Chunk*) Malloc(sizeof(Chunk));
  chunk->min = value;
  chunk->max = value;
  prime_range.last->next = chunk;
  chunk->prev = prime_range.last;
  chunk->next = NULL;
  prime_range.last = chunk;
}

/*-------------------------------------------------------------------------*
 * COMPUTE_PRIME_RANGE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Compute_Prime_Range(void)
{
  // add first few values (for now separate 2 and 3)
  Chunk *first_chunk = (Chunk*) Malloc(sizeof(Chunk));
  prime_range.first = first_chunk;
  first_chunk->min = 2;
  first_chunk->max = 2;
  prime_range.last = first_chunk;
  Add_Value_To_Prime_Range(3);
  Add_Value_To_Prime_Range(5);
  Add_Value_To_Prime_Range(7);

  int val = 10;
  while (val++ < max_prime_value) {
    // iterate over the chunks until sqrt(val)
    // search for divisors
    int sqrt_val = Pl_Sqrt_Dn(val);
    Chunk *chunk = first_chunk;
    while (chunk->min <= sqrt_val) {
      if (val % chunk->min == 0) 
        break;
      chunk = chunk->next;
    }
    // no divisors found ==> add it to the list
    if (chunk->min > sqrt_val) {
      Add_Value_To_Prime_Range(val);
    }
  }

  // combine 2 and 3
  prime_range.first = first_chunk->next;
  prime_range.first->prev = NULL;
  prime_range.first->min = 2;

  prime_range.min = prime_range.first->min;
  prime_range.max = prime_range.last->max;


  // not_prime_range

  // first chunk
  first_chunk->min = INTERVAL_MIN_INTEGER;
  first_chunk->max = 1;
  not_prime_range.first = first_chunk;
  not_prime_range.last = first_chunk;

  // [prev_prime.max + 1 .. prime.min - 1]
  Chunk *prime_chunk = prime_range.first->next;
  int prev_max = 3;
  while (prime_chunk != NULL) {
    Chunk *chunk = (Chunk*) Malloc(sizeof(Chunk));
    chunk->min = prev_max + 1;
    chunk->max = prime_chunk->min - 1;
    not_prime_range.last->next = chunk;
    chunk->prev = not_prime_range.last;
    chunk->next = NULL;
    not_prime_range.last = chunk;
    prev_max = prime_chunk->max;
    prime_chunk = prime_chunk->next;
  }
  
  // last chunk
  Chunk *chunk = (Chunk*) Malloc(sizeof(Chunk));
  chunk->min = prev_max + 1;
  chunk->max = INTERVAL_MAX_INTEGER;
  not_prime_range.last->next = chunk;
  chunk->prev = not_prime_range.last;
  chunk->next = NULL;

  not_prime_range.min = INTERVAL_MIN_INTEGER;
  not_prime_range.max = INTERVAL_MAX_INTEGER;

  computed_prime_range = 1;
  return;
}
