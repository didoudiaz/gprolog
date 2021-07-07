/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.c                                                      *
 * Descr.: FD Range Implementation                                         *
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


#include <stdio.h>
#include <string.h>

#include "bool.h"

#define FD_RANGE_FILE

#include "engine_pl.h"
#include "engine_fd.h"



/*-------------------------------------------------------------------------*
 * TODO: update this text                                                  *
 * The file fd_hook_range.h must contains the definition of:               *
 *                                                                         *
 * INTERVAL_MAX_INTEGER: an integer constant corresponding to the greatest *
 *                       value for intervals (i.e. 0..INTERVAL_MAX_INTEGER)*
 *                                                                         *
 * RANGE_TOP_STACK     : a long * variable corresponding to the top of the *
 *                       stack where are allocated the bit-vectors.        *
 *                       The user must handle the (re)initialization of    *
 *                       this pointer to a valid (read/write) memory area. *
 *                       Allocated vectors are never recovered, so the user*
 *                       should take care of reinitializations (GC) of the *
 *                       top of stack if needed.                           *
 *                                                                         *
 * The following macros can be redefined:                                  *
 *                                                                         *
 * WORD_SIZE           : a constant defining sizeof(void*) in bits (32/64).*
 *-------------------------------------------------------------------------*/




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/


#define WRITE_BEGIN_RANGE          ""
#define WRITE_END_RANGE            ""
#define WRITE_LIMITS_SEPARATOR     ".."
#define WRITE_INTERVALS_SEPARATOR  ":"
#define WRITE_EXTRA_CSTR_SYMBOL    "@"




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*---------------------------------*
 * Auxiliary macros                *
 *---------------------------------*/

#define math_min(x, y)             ((x) <= (y) ? (x) : (y))
#define math_max(x, y)             ((x) >= (y) ? (x) : (y))
#define Same_Sign(x, y)   				 (((x) ^ (y)) >= 0)
#define Mod(a,b)                   ((Same_Sign(a,b)) ? ((a)%(b)) : ((b) + ((a)%(b))) )

/*-------------------------------------------------------------------------*
 * PL_RANGE_TEST_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Range_Test_Value(Range *range, int n)
{
  int min = range->min;
  int max = range->max;

  if (n < min || n > max)
    return FALSE;

  if (Is_Interval(range) || n == min || n == max)
    return TRUE;

  return Pl_Sparse_Test_Value(range, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_TEST_NULL_INTER                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Range_Test_Null_Inter(Range *range, Range *range1)
{
  int swt;


  if (range->min > range1->max || range1->min > range->max)
    return TRUE;

  if (range->min == range1->min || range->min == range1->max ||
      range->max == range1->min || range->max == range1->max)
    return FALSE;

  swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);

  if (swt == 0)			/* Interval with Interval */
    return FALSE;

  if (swt == 1)			/* Interval with Sparse */
    return Pl_Sparse_Interval_Test_Null_Inter(range1,range);

  if (swt == 2)			/* Sparse with Interval */
    return Pl_Sparse_Interval_Test_Null_Inter(range,range1);

  //if (swt == 3)		/* Sparse with Sparse */
  return Pl_Sparse_Sparse_Test_Null_Inter(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_COPY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Copy(Range *range, Range *range1)
{
  range->min = range1->min;
  range->max = range1->max;


  // Perform a deep copy if range1 is sparse
  if (Is_Sparse(range1)) {
    Chunk *chunk = range1->first;
    Chunk *copy_chunk;

    // first chunk
    copy_chunk = Pl_Create_Interval_Chunk(chunk->min, chunk->max);
    range->first = copy_chunk;
    range->last = copy_chunk;

    // remaining chunks
    chunk = chunk->next;
    while(chunk != NULL) {
      copy_chunk = Pl_Create_Interval_Chunk(chunk->min, chunk->max);
      Pl_Set_Chunk_At_End_Of_Range(range, copy_chunk);

      chunk = chunk->next;
    }
  }
  else {
    range->first = NULL;
    range->last = NULL;
  }
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NB_ELEM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Nb_Elem(Range *range)
{
  if (Is_Empty(range)) return 0;
  if (Is_Interval(range))
    return range->max - range->min + 1;

  return Pl_Sparse_Nb_Elem(range);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_ITH_ELEM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Ith_Elem(Range *range, int i)
{
  int n;

  if (i<=0) return INTERVAL_MIN_INTEGER-1;

  if (Is_Empty(range))
    return INTERVAL_MIN_INTEGER-1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      n = range->min + i - 1;

      return n < range->min || n > range->max ? ((INTERVAL_MIN_INTEGER) - 1) : n;
    }


  return Pl_Sparse_Ith_Elem(range, i);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NEXT_AFTER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Next_After(Range *range, int n)
{
  if (Is_Empty(range))
    return INTERVAL_MIN_INTEGER-1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      if (n >= range->max)
	return INTERVAL_MIN_INTEGER-1;

      n++;
      if (n < range->min)
	n = range->min;

      return n;
    }


  return Pl_Sparse_Next_After(range, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NEXT_BEFORE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Next_Before(Range *range, int n)
{
  if (Is_Empty(range))
    return INTERVAL_MIN_INTEGER-1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      if (n <= range->min)
	return INTERVAL_MIN_INTEGER-1;

      n--;
      if (n > range->max)
	n = range->max;

      return n;
    }


  return Pl_Sparse_Next_Before(range,n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_BECOMES_SPARSE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Becomes_Sparse(Range *range)
{
  if (Is_Sparse(range)) return;
 
  /* Initialize the first Chunk (that uses Interval Representation) */
  range->first = Pl_Create_Interval_Chunk(range->min, range->max);
  range->last = range->first;
}


/*-------------------------------------------------------------------------*
 * PL_RANGE_SET_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Set_Value(Range *range, int n)
{

  if (Is_Empty(range))
    {
      Range_Init_Interval(range, n, n);
      return;
    }

  if (Is_Interval(range))
    {
      if (n >= range->min && n <= range->max)
	return;

      if (n == range->min - 1)
	{
	  range->min--;
	  return;
	}

      if (n == range->max + 1)
	{
	  range->max++;
	  return;
	}

      Pl_Range_Becomes_Sparse(range);
    }

  Pl_Sparse_Set_Value(range, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_RESET_VALUE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Reset_Value(Range *range, int n)
{
  if (Is_Empty(range) || n < range->min || n > range->max)
    return;

  if (range->min == range->max) {
    Set_To_Empty(range);
    return;
  }

  if (Is_Interval(range)) {
    if (n == range->min) {
	    range->min++;
	    return;
	  }

    if (n == range->max) {
	    range->max--;
	    return;
	  }

  	Pl_Range_Becomes_Sparse(range);
  }

  Pl_Sparse_Reset_Value(range,n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_UNION                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Union(Range *range, Range *range1)
{
  int swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);
  Range r;

  /* Empty Ranges */
  if (Is_Empty(range))
  {
    Pl_Range_Copy(range, range1);
    return;
  }
  if (Is_Empty(range1))
    return;

  if (swt == 0)			/* Interval with Interval */
  {
		if (range1->min <= range->max + 1 && range->min <= range1->max + 1)
		{
		  range->min = math_min(range->min, range1->min);
		  range->max = math_max(range->max, range1->max);
		  return;
		}

    Pl_Range_Becomes_Sparse(range);
    Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
    range1 = &r;
    Pl_Range_Becomes_Sparse(range1);
  }
  else if (swt == 1)		/* Interval with Sparse */
    Pl_Range_Becomes_Sparse(range);
  else if (swt == 2)		/* Sparse with Interval */
  {
    Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
    range1 = &r;
    Pl_Range_Becomes_Sparse(range1);
  }

  /* Sparse with Sparse */

  Pl_Sparse_Union(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_INTER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Inter(Range *range, Range *range1)
{
  int swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);
  Range r;

  /* Empty Ranges */
  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
  {
    Set_To_Empty(range);
    return;
  }

  if (swt == 0)			/* Interval with Interval */
  {
    range->min = math_max(range->min, range1->min);
    range->max = math_min(range->max, range1->max);
    return;
  }
  if (swt == 1)			/* Interval with Sparse */
    Pl_Range_Becomes_Sparse(range);
  else if (swt == 2)		/* Sparse with Interval */
  {
    Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */

    range1 = &r;
    Pl_Range_Becomes_Sparse(range1);
  }

  /* Sparse with Sparse */
  Pl_Sparse_Inter(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_COMPL                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Compl(Range *range)
{
  if (Is_Interval(range))	/* Interval */
    {
      if (Is_Empty(range))
	{
	  range->min = INTERVAL_MIN_INTEGER;
	  range->max = INTERVAL_MAX_INTEGER;
	  return;
	}

      if (range->min <= INTERVAL_MIN_INTEGER)
	{
	  if (range->max >= INTERVAL_MAX_INTEGER)
	    Set_To_Empty(range);
	  else
	    {
	      range->min = range->max + 1;
	      range->max = INTERVAL_MAX_INTEGER;
	    }

	  return;
	}

      if (range->max >= INTERVAL_MAX_INTEGER)
	{
	  range->max = range->min - 1;
	  range->min = INTERVAL_MIN_INTEGER;

	  return;
	}

      /* Create two chunks for the complement */
      Chunk *chunk = Pl_Create_Interval_Chunk(range->max+1, INTERVAL_MAX_INTEGER);
      range->max = range->min-1;
      range->min = INTERVAL_MIN_INTEGER;
      Pl_Range_Becomes_Sparse(range);
      Pl_Set_Chunk_At_End_Of_Range(range,chunk);

      return;
    }
  /* Sparse */
  Pl_Sparse_Compl(range);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_ADD_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Add_Range(Range *range, Range *range1)
{
  int swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);
  Range r;

  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  if (swt == 0)			/* Interval with Interval */
    {
      range->min += range1->min;
      range->max += range1->max;
      return;
    }
  else if (swt == 1)		/* Interval with Sparse */
    Pl_Range_Becomes_Sparse(range);
  else if (swt == 2)		/* Sparse with Interval */
    {
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }

  /* Sparse with Sparse */
  Pl_Sparse_Add_Range(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_SUB_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Sub_Range(Range *range, Range *range1)
{
  int swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);
  Range r;

  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  if (swt == 0)			/* Interval with Interval */
    {
      range->min -= range1->max;
      range->max -= range1->min;
      return;
    }
  else if (swt == 1)		/* Interval with Sparse */
    Pl_Range_Becomes_Sparse(range);
  else if (swt == 2)		/* Sparse with Interval */
    {
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }

  /* Sparse with Sparse */
  Pl_Sparse_Sub_Range(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_MUL_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Mul_Range(Range *range, Range *range1)
{
  Range r;

  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  if (Is_Interval(range))
    Pl_Range_Becomes_Sparse(range);

  if (Is_Interval(range1))
    {
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }
  /* Sparse with Sparse */
  Pl_Sparse_Mul_Range(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_DIV_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Div_Range(Range *range, Range *range1)
{
  Range r;

  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  if (Is_Interval(range))
    Pl_Range_Becomes_Sparse(range);

  if (Is_Interval(range1))
    {
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }
  /* Sparse with Sparse */
  Pl_Sparse_Div_Range(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_MOD_RANGE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Mod_Range(Range *range, Range *range1)
{
  Range r;

  if (Is_Empty(range))
    return;

  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  if (Is_Interval(range))
    Pl_Range_Becomes_Sparse(range);

  if (Is_Interval(range1))
    {
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }

  /* Sparse with Sparse */
  Pl_Sparse_Mod_Range(range,range1);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_ADD_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Add_Value(Range *range, int n)
{
  if (n == 0 || Is_Empty(range))
    return;

  if (Is_Interval(range))	/* Interval */
    {
      range->min = math_max(range->min + n, INTERVAL_MIN_INTEGER);
      range->max = math_min(range->max + n, INTERVAL_MAX_INTEGER);
      // if new range is out of bounds, it will be Empty because min > max
      return;
    }
  /* Sparse */
  Pl_Sparse_Add_Value(range, n);
}



/*-------------------------------------------------------------------------*
 * PL_RANGE_MUL_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Mul_Value(Range *range, int n)
{
  if (n == 1 || Is_Empty(range))
    return;

  if (Is_Interval(range))	/* Interval */
    Pl_Range_Becomes_Sparse(range);

  /* Sparse */
  Pl_Sparse_Mul_Value(range,n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_DIV_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Div_Value(Range *range, int n)
{
  if (n == 1 || Is_Empty(range))
    return;

  if (n==0) {
    Set_To_Empty(range);
    return;
  }

  if (Is_Interval(range))	/* Interval */ {
      // similar to DivDn and DivUp
      int min_r = (n>0) ? range->min : range->max;
      int max_r = (n>0) ? range->max : range->min;

      range->min = ( ((min_r ^ n) >= 0) ? ((min_r / n) + (min_r % n != 0)) : (min_r / n) );
      range->max = ( ((max_r ^ n) >= 0) ? (max_r / n) : ((max_r / n) - (max_r % n != 0)) );
      return;
  }
  /* Sparse */
  Pl_Sparse_Div_Value(range,n);
}

/*-------------------------------------------------------------------------*
 * PL_RANGE_MOD_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Mod_Value(Range *range, int n)
{
  if (Is_Empty(range))
    return;

  if (n==0) {
    Set_To_Empty(range);
    return;
  }

  // Modulo also works for negative numbers
  if (Is_Interval(range))	/* Interval */
  {
    // are all values (0..n-1) or (-n+1..0) going to be set?
    if (range->max - range->min + 1 >= abs(n)) {
    	if (n>0) {
    	  range->min = 0;
    	  range->max = n-1;
    	} else {
    	  range->min = n+1;
    	  range->max = 0;
    	}
    	return;
    }

    if (n > 0) {
      // [-3..2] -> [0..2][n-3..n-1]
      if (range->min < 0 && range->max >= 0) {
        int tmp_min = range->min;
        range->min = 0;
        Pl_Range_Becomes_Sparse(range);
        Pl_Set_Chunk_At_End_Of_Range(range, Pl_Create_Interval_Chunk(n+tmp_min,n-1));
        return;
      }

      int a = Mod(range->min, n);
      int b = Mod(range->max, n);
      // [-4..-2] -> [a..b] or [0..b][a..n-1]
      // [2..4]   -> [a..b] or [0..b][a..n-1]
      if (a>b) {
        range->min = 0;
        range->max = b;
        Pl_Range_Becomes_Sparse(range);
        Pl_Set_Chunk_At_End_Of_Range(range, Pl_Create_Interval_Chunk(a,n-1));
      } else {
        range->min = a;
        range->max = b;
      }

    } else {
      // [-3..2] -> [n+1..n+2][-3..0]
      if (range->min <= 0 && range->max > 0) {
        int tmp_min = range->min;
        range->min = n+1;
        range->max = n+range->max;
        Pl_Range_Becomes_Sparse(range);
        Pl_Set_Chunk_At_End_Of_Range(range, Pl_Create_Interval_Chunk(tmp_min,0));
        return;
      }

      int a = Mod(range->min, n);
      int b = Mod(range->max, n);
      // [-4..-2] -> [a..b] or [0..b][a..n-1]
      // [2..4]   -> [a..b] or [0..b][a..n-1]
      if (a>b) {
        range->min = n+1;
        range->max = b;
        Pl_Range_Becomes_Sparse(range);
        Pl_Set_Chunk_At_End_Of_Range(range, Pl_Create_Interval_Chunk(a,0));
      } else {
        range->min = a;
        range->max = b;
      }

    }
    return;
  }

  /* Sparse */
  Pl_Sparse_Mod_Value(range,n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_TO_STRING                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Range_To_String(Range *range)
{
  static char buff[4096];

  if (Is_Empty(range))
    {
      strcpy(buff, "<empty range>");
      return buff;
    }

  if (range->min == range->max)
    {
      sprintf(buff, "{%d}", range->min);
      return buff;
    }

  if (Is_Interval(range))
    {
      sprintf(buff, "%s%d%s%d%s",
	      WRITE_BEGIN_RANGE,
	      range->min, WRITE_LIMITS_SEPARATOR, range->max,
	      WRITE_END_RANGE);
      return buff;
    }

  // sparse
  strcpy(buff, WRITE_BEGIN_RANGE);

  Chunk *chunk = range->first;
  while (chunk != NULL) {

    if (chunk->min == chunk->max) {
      sprintf(buff + strlen(buff), "%d",
        chunk->min);
    } else {
      sprintf(buff + strlen(buff), "%d%s%d",
        chunk->min, WRITE_LIMITS_SEPARATOR, chunk->max);
    }

    chunk = chunk->next;

    // end of range
    if (chunk == NULL)
      sprintf(buff + strlen(buff), "%s", WRITE_END_RANGE);
    else 
      sprintf(buff + strlen(buff), "%s", WRITE_INTERVALS_SEPARATOR);
  }

  return buff;
}

/*-------------------------------------------------------------------------*
 * CHUNKS    		                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/

Chunk *
Pl_Create_Interval_Chunk(int min, int max) {
  Chunk *chunk;
  Chunk_Allocate(chunk);

  chunk->min = min;
  chunk->max = max;
  chunk->prev = NULL;
  chunk->next = NULL;

  //if (nb_chunks++ > MAX_CHUNKS) {
  //  Pl_Err_Resource(Pl_Create_Atom("too many holes in FD"));
  //}
  return chunk;
}

Chunk *
Pl_Get_Chunk_For_Value_Recur(Chunk* chunk, int n) {
  if (chunk->max >= n) 
    return chunk; // this chunk contains n (or n is in gap between this and previous)
  else if (chunk->next != NULL) 
    return Pl_Get_Chunk_For_Value_Recur(chunk->next, n); // next chunk
  else 
    return chunk; // no chunk contains n (because chunks are ordered) : returns last chunk
}

/* Returns the Chunk that contains n. (linear search) *
 * If no Chunk contains n, return the fist chunk that is > n (for n > range->max: return range->max)     */
Chunk *
Pl_Get_Chunk_For_Value(Range* range, int n) { 
  if (range->first->max >= n) 
    return range->first; // first chunk (also if n < min)
  else if (range->last->min <= n) /* Even though the last chunk is found recursively, it is checked now because it is more probable to hit/miss */
    return range->last; // last chunk (also if n > max)
  else /* in the interval*/
    return Pl_Get_Chunk_For_Value_Recur(range->first, n);
}

Bool 
Pl_Sparse_Test_Value(Range *range, int n) {

  Chunk *c = Pl_Get_Chunk_For_Value(range,n);
  return (c->min <= n && c->max >= n);
}

void 
Pl_Sparse_Set_Value(Range *range, int n) {
	if (Is_Interval(range)) {
		range->min = n;
		range->max = n;
		Pl_Range_Becomes_Sparse(range);
		return;
	}
  Chunk *chunk = Pl_Get_Chunk_For_Value(range,n);

  // Interval Chunk
  /* Done if already set */
  if (chunk->min <= n && chunk->max >= n) return; 
  else if (n == chunk->min-1) chunk->min --;
  else if (n == chunk->max+1) chunk->max ++;
  /* Pl_Get_Chunk_For_Value returns a chunk > n, so check previous Chunk too*/
  else if (chunk->prev != NULL && n == ((Chunk*)chunk->prev)->max+1) ((Chunk*)chunk->prev)->max ++;
  else if (n < chunk->min) {
    /* There is no chunk that contains n, so create one */
    Chunk *new_chunk = Pl_Create_Interval_Chunk(n,n);
    Pl_Set_Chunk_Before_Chunk(range,new_chunk,chunk);
  }
  else { //(n > chunk->max)
    Chunk *new_chunk = Pl_Create_Interval_Chunk(n,n);
    Pl_Set_Chunk_At_End_Of_Range(range,new_chunk);
  }
  // Remove Chunk if hole is filled in
  if (chunk->prev != NULL && ((Chunk*)chunk->prev)->max + 1 >= chunk->min) {
  	((Chunk*)chunk->prev)->max = chunk->max;
  	Pl_Remove_Interval_Chunk(range,chunk);
  }
  else if (chunk->next != NULL && chunk->max + 1 >= ((Chunk*)chunk->next)->min) {
  	((Chunk*)chunk->next)->min = chunk->min;
  	Pl_Remove_Interval_Chunk(range,chunk);
  }

  // update min and max
  if (Is_Not_Empty(range)) {
	  range->min = range->first->min;
	  range->max = range->last->max;
  }
}

void 
Pl_Sparse_Reset_Value(Range *range, int n) {

  Chunk *chunk = Pl_Get_Chunk_For_Value(range, n);
  if (chunk->min <= n && chunk->max >= n) {
    /* boundaries */
    if (chunk->min == n) {
      chunk->min ++;
      Pl_Remove_Interval_Chunk_If_Needed(range,chunk);
      if (Is_Interval(range))
        Set_To_Empty(range);
      else // possibly update range min 
        range->min = range->first->min;
    }
    else if (chunk->max == n) {
      chunk->max --;
      Pl_Remove_Interval_Chunk_If_Needed(range,chunk);
      if (Is_Interval(range))
        Set_To_Empty(range);
      else // possibly update range max 
        range->max = range->last->max;
    }
    /* possible empty chunk now, remove if needed */
    else {
      /* create new chunk */
      // [chunk->prev][chunk][new_chunk][chunk->next]
      Chunk *new_chunk = Pl_Create_Interval_Chunk(n+1,chunk->max);
      chunk->max = n-1;
      if (chunk->next != NULL) ((Chunk*)chunk->next)->prev = new_chunk;
      new_chunk->next = chunk->next;
      new_chunk->prev = chunk;
      chunk->next = new_chunk;
      if (range->last == chunk) range->last = new_chunk;
    }
  }
}

void
Pl_Sparse_Inter(Range *range, Range *range1) {

  /* Don't keep any chunk, try to make new ones because otherwise memory will overlap */
  Chunk *chunk, *other_chunk;
  chunk = range->first;
  other_chunk = range1->first;

  /* Stop if we reached the end of a chunk list */
  while (chunk != NULL && other_chunk != NULL) {

    if (chunk->max < other_chunk->min) {
      //("#Case 1\n");
      // remove chunk
      // chunk->next
      Chunk *tmp_chunk = chunk->next;
      Pl_Remove_Interval_Chunk(range,chunk);
      chunk = tmp_chunk;
    }
    else if (chunk->max <= other_chunk->max) {
      //("#Case 2,3\n");
      // remove chunk
      // create gap for overlap
      // chunk->next
      Pl_Set_Chunk_Before_Chunk(range,Pl_Create_Interval_Chunk(
        math_max(chunk->min, other_chunk->min), 
        math_min(chunk->max, other_chunk->max)), 
      chunk);

      Chunk *tmp_chunk = chunk->next;
      Pl_Remove_Interval_Chunk(range,chunk);
      chunk = tmp_chunk; 
    }
    else if (chunk->min > other_chunk->max) {
      //("#Case 6\n");
      // other_chunk->next
      other_chunk = other_chunk->next;
    }
    else if (chunk->max > other_chunk->max) {
      //("#Case 4,5\n");
      // create gap for overlap
      // chunk->next
      Pl_Set_Chunk_Before_Chunk(range,Pl_Create_Interval_Chunk(
        math_max(chunk->min, other_chunk->min), 
        math_min(chunk->max, other_chunk->max)), 
      chunk);

      other_chunk = other_chunk->next;
    }
  }
  // remove all next chunks
  while (chunk != NULL) {
		Chunk *tmp_chunk = chunk->next;
		Pl_Remove_Interval_Chunk(range,chunk);
		chunk = tmp_chunk;
 	}

 	if (range->first == NULL) {
 		Set_To_Empty(range);
 	}

  // Check new min and max
  else if (Is_Not_Empty(range)) {
	  range->min = range->first->min;
	  range->max = range->last->max;
	}
}

void
Pl_Sparse_Union(Range *range, Range *range1) {

  Chunk *chunk, *other_chunk;
  chunk = range->first;
  other_chunk = range1->first;

  /* Stop if we reached the end of a chunk list */
  while (chunk != NULL && other_chunk != NULL) 
  {
		if (chunk->min > other_chunk->max + 1) 
		{
			// other_chunk before chunk->min, so create new chunk
			Chunk *new_chunk = Pl_Create_Interval_Chunk(other_chunk->min, other_chunk->max);
			Pl_Set_Chunk_Before_Chunk(range,new_chunk,chunk);
			// advance other_chunk
			other_chunk = other_chunk->next;
		}
		else if (other_chunk->min <= chunk->max + 1) // and (chunk->min <= other_chunk->max + 1) 
		{
			// combine chunks
			chunk->min = math_min(chunk->min, other_chunk->min);
			chunk->max = math_max(chunk->max, other_chunk->max);
			// combine with upcoming chunks
			Chunk *tmp_chunk = chunk->next;
			while (tmp_chunk != NULL && tmp_chunk->min <= chunk->max+1) {
				chunk->max = math_max(chunk->max, tmp_chunk->max);
				Pl_Remove_Interval_Chunk(range,tmp_chunk);
				tmp_chunk = chunk->next;
			}

			// advance other_chunk
			other_chunk = other_chunk->next;
		} else //(other_chunk->min > chunk->max + 1) and (chunk->min <= other_chunk->max + 1) 
		{
			// advance to next chunk
			chunk = chunk->next;
		}
  }

  // add remaining chunks from range1
  while (other_chunk != NULL) {
  	chunk = Pl_Create_Interval_Chunk(other_chunk->min, other_chunk->max);
  	Pl_Set_Chunk_At_End_Of_Range(range,chunk);
  	other_chunk = other_chunk->next;
  }

  // Check new min and max
  if (Is_Not_Empty(range)) {
	  range->min = range->first->min;
	  range->max = range->last->max;
	}
}

void
Pl_Set_Chunk_At_End_Of_Range(Range *range, Chunk *chunk) {
  range->last->next = chunk;
  chunk->prev = range->last;
  chunk->next = NULL;
  range->last = chunk;
  range->max = chunk->max; // assuming correct ordering
}

void
Pl_Set_Chunk_Before_Chunk(Range *range, Chunk *new_chunk, Chunk *old_chunk) {
  new_chunk->next = old_chunk;
  new_chunk->prev = old_chunk->prev;
  old_chunk->prev = new_chunk;
  if (new_chunk->prev != NULL)
      ((Chunk*)new_chunk->prev)->next = new_chunk;
  else if (range->first == old_chunk) {
    range->first = new_chunk;
    range->min = new_chunk->min; // assuming correct ordering
  }
}

/* Removes the given chunk from the range, updates the prev and next pointers of the surrounding chunks and updates the range->[min,max] if needed */
void 
Pl_Remove_Interval_Chunk(Range *range, Chunk *chunk) {

  if (range->first == chunk) {
    if (range->last == chunk) {
      // 4
      Set_To_Empty(range);
    } else {
      // 1
      range->first = (Chunk*) chunk->next;
      range->min = range->first->min;
      range->first->prev = NULL;
    }
  } else if (range->last == chunk) {
    // 3
    range->last = (Chunk*) chunk->prev;
    range->max = range->last->max;
    range->last->next = NULL;
  } else {
    // 2
    ((Chunk*) chunk->prev)->next = chunk->next;
    ((Chunk*) chunk->next)->prev = chunk->prev;
  }
}


void
Pl_Remove_Interval_Chunk_If_Needed(Range *range, Chunk *chunk) {
	if (chunk->min <= chunk->max) return;
	/* Remove chunk */
	Pl_Remove_Interval_Chunk(range,chunk);
}

int
Pl_Chunk_Count(Range *range) {
	if (Is_Interval(range)) return 0;
	int chunk_count = 1;
	Chunk *c = range->first;
	while (c != range->last) {
		c = c->next;
		chunk_count ++;
	}
	return chunk_count;
}

void 
Pl_Sparse_Compl(Range *range) {
	Chunk *chunk = range->first;
	// If possible, make first chunk
	if (range->min > INTERVAL_MIN_INTEGER) {
		Chunk *new_chunk = Pl_Create_Interval_Chunk(INTERVAL_MIN_INTEGER, range->min - 1);
		Pl_Set_Chunk_Before_Chunk(range,new_chunk,chunk);
	}

	// adapt remaining chunks
	while (chunk != NULL) {
		Chunk *tmp_chunk = chunk->next;
		if (tmp_chunk != NULL) {
			chunk->min = chunk->max + 1;
			chunk->max = tmp_chunk->min-1;	
		}
		else {
			if (chunk->max >= INTERVAL_MAX_INTEGER) {
				// remove chunk
				Pl_Remove_Interval_Chunk(range,chunk);
			}
			else {
				chunk->min = chunk->max + 1;
				chunk->max = INTERVAL_MAX_INTEGER;
			}
 		}
 		chunk = chunk->next;
	}

	if (range->first == NULL) 
		Set_To_Empty(range);
  else {
	  range->min = range->first->min;
	  range->max = range->last->max;
	}
}

void 
Pl_Sparse_Mul_Value(Range *range, int n) {
  Chunk *chunk = range->first;
  Chunk *new_chunk;
  int min_value = - (abs(INTERVAL_MIN_INTEGER) / abs(n));
  // //assert(MIN<=0);
  // round up needed for positive MIN
  // if (MIN > 0 && (MIN % abs(n) != 0)) min_value ++;
  int max_value = INTERVAL_MAX_INTEGER / abs(n);
  // //assert(MAX>=0);
  // round up needed for negative MAX
  // if (MAX < 0 && (MAX % abs(n) != 0)) max_value --;

  while (chunk != NULL) {
    // make sure that no overflow problems occur 
    // (MIN_INT <= [min_c * n] , [max_c * n] <= MAX_INT) 
    int min_c = math_max(chunk->min, min_value);
    int max_c = math_min(chunk->max, max_value);

    // for each value in the range, create a chunk containing the multiple
    for (;min_c <= max_c; min_c++) {
      new_chunk = Pl_Create_Interval_Chunk(min_c * n, min_c * n);
      if (n > 0)
        Pl_Set_Chunk_Before_Chunk(range,new_chunk,chunk);
      else {
        Pl_Set_Chunk_Before_Chunk(range,new_chunk,range->first);
      }
    }

    // remove chunk and continue with the next chunk
    Chunk *tmp_chunk = chunk->next;
    Pl_Remove_Interval_Chunk(range,chunk);
    chunk = tmp_chunk;
  }

  if (Is_Sparse(range)) {
    range->min = range->first->min;
    range->max = range->last->max;
  }
  else Set_To_Empty(range);
}

void 
Pl_Sparse_Div_Value(Range *range, int n) {
  Chunk *chunk = range->first;

  while (chunk != NULL) {
    int min_c = (n>0) ? chunk->min : chunk->max;
    int max_c = (n>0) ? chunk->max : chunk->min;
    Chunk *tmp_chunk = chunk->next;

    // similar to DivDn and DivUp
    chunk->min = ( ((min_c ^ n) >= 0) ? ((min_c / n) + (min_c % n != 0)) : (min_c / n) );
    chunk->max = ( ((max_c ^ n) >= 0) ? (max_c / n) : ((max_c / n) - (max_c % n != 0)) );

    if (chunk->min > chunk->max) {
      // remove empty chunk
      Pl_Remove_Interval_Chunk(range,chunk);
    } 
    else {
      if (n > 0) {
        if (chunk->prev != NULL && chunk->min <= ((Chunk*)chunk->prev)->max+1) {
          // update previous chunk and remove this one
          ((Chunk*)chunk->prev)->max = chunk->max;
          Pl_Remove_Interval_Chunk(range,chunk);
        }
      } else {
        if (chunk != range->first) {
          // can we combine this chunk with the first one?
          Pl_Remove_Interval_Chunk(range,chunk);
          if (chunk->max + 1 >= range->first->min)
            range->first->min = chunk->min;
          else 
            Pl_Set_Chunk_Before_Chunk(range,chunk,range->first);
        }
      }
    }
    chunk = tmp_chunk;
  }

  if (Is_Sparse(range)) {
    range->min = range->first->min;
    range->max = range->last->max;
  }
  else Set_To_Empty(range);
}

void
Pl_Sparse_Add_Value(Range *range, int n) {
  Chunk *chunk = range->first;
  while (chunk != NULL) {
    chunk->min = math_max(chunk->min + n, INTERVAL_MIN_INTEGER);
    chunk->max = math_min(chunk->max + n, INTERVAL_MAX_INTEGER);

    Chunk *tmp_chunk = chunk->next;
    // chunk could get out of bounds
    Pl_Remove_Interval_Chunk_If_Needed(range,chunk);
    chunk = tmp_chunk;
  }
  
  if (Is_Sparse(range)) {
    range->min = range->first->min;
    range->max = range->last->max;
  }
  else Set_To_Empty(range);
}



void
Pl_Sparse_Mod_Value(Range *range, int n) {
  /* Takes the modulus of every chunk and combines these */
  /* together with the Pl_Range_Union */

  //assert(Is_Sparse(range));
  if (n==0){
    Set_To_Empty(range);
    return;
  }

  // auxillary range
  Range r;
  Chunk *chunk = range->first;
  // set range to empty for now
  Set_To_Empty(range);

  while (chunk != NULL) {
    Chunk *next_chunk = chunk->next;
    chunk->next = NULL;
    r.first = chunk;
    r.last = chunk;

    // are all values (0..n-1) or (-n+1..0) going to be set?
    if (chunk->max - chunk->min + 1 >= abs(n)) {
      if (n>0) {
        chunk->min = 0;
        chunk->max = n-1;
      } else {
        chunk->min = n+1;
        chunk->max = 0;
      }
    }
    else if (n > 0) {
      // [-3..2] -> [0..2][n-3..n-1]
      if (chunk->min < 0 && chunk->max >= 0) {
        int tmp_min = chunk->min;
        chunk->min = 0;
        Pl_Set_Chunk_At_End_Of_Range(&r, Pl_Create_Interval_Chunk(n+tmp_min,n-1));
      } else {
        int a = Mod(chunk->min, n);
        int b = Mod(chunk->max, n);
        // [-4..-2] -> [a..b] or [0..b][a..n-1]
        // [2..4]   -> [a..b] or [0..b][a..n-1]
        if (a>b) {
          chunk->min = 0;
          chunk->max = b;
          Pl_Set_Chunk_At_End_Of_Range(&r, Pl_Create_Interval_Chunk(a,n-1));
        } else {
          chunk->min = a;
          chunk->max = b;
        }
      }
    } else {
      // [-3..2] -> [n+1..n+2][-3..0]
      if (chunk->min <= 0 && chunk->max > 0) {
        int tmp_min = chunk->min;
        chunk->min = n+1;
        chunk->max = n+chunk->max;
        Pl_Set_Chunk_At_End_Of_Range(&r, Pl_Create_Interval_Chunk(tmp_min,0));
      } else {
        int a = Mod(chunk->min, n);
        int b = Mod(chunk->max, n);
        // [-4..-2] -> [a..b] or [0..b][a..n-1]
        // [2..4]   -> [a..b] or [0..b][a..n-1]
        if (a>b) {
          chunk->min = n+1;
          chunk->max = b;
          Pl_Set_Chunk_At_End_Of_Range(&r, Pl_Create_Interval_Chunk(a,0));
        } else {
          chunk->min = a;
          chunk->max = b;
        }
      }
    }

    r.min = r.first->min;
    r.max = r.last->max;
    Pl_Range_Union(range,&r);
    chunk = next_chunk;
  }
}


void 
Pl_Sparse_Add_Range(Range *range, Range *range1) {
  /* For every chunk of range1: add it to range and combine
     it with the previous additions */

  Range r, range_copy;
  Pl_Range_Copy(&range_copy, range);
  Set_To_Empty(range);
  Chunk *other_chunk = range1->first;

  while (other_chunk != NULL) {
    Set_To_Empty(&r);
    // first chunk (of original range)
    Chunk *chunk = range_copy.first;
    r.first = Pl_Create_Interval_Chunk(chunk->min+other_chunk->min,
                                       chunk->max+other_chunk->max);
    r.last = r.first;
    chunk = chunk->next;
    // remaining chunks
    while (chunk != NULL) {
      // range is strictly increasing (prevent overlap)
      int tmp_min = chunk->min+other_chunk->min;
      if (tmp_min <= r.last->max + 1) {
        r.last->max = chunk->max + other_chunk->max;
      } else {
        Pl_Set_Chunk_At_End_Of_Range(&r,
          Pl_Create_Interval_Chunk(chunk->min+other_chunk->min,
                                   chunk->max+other_chunk->max));
      }
      chunk = chunk->next;
    }
    r.min = r.first->min;
    r.max = r.last->max;

    // update new range
    Pl_Range_Union(range,&r);
    other_chunk = other_chunk->next;
  }
}

void 
Pl_Sparse_Sub_Range(Range *range, Range *range1) {
  /* For every chunk of range1: subtract it from range and combine
     it with the previous subtractions */

  Range r, range_copy;
  Pl_Range_Copy(&range_copy, range);
  Set_To_Empty(range);
  Chunk *other_chunk = range1->first;

  while (other_chunk != NULL) {
    Set_To_Empty(&r);
    // first chunk (of original range)
    Chunk *chunk = range_copy.first;
    r.first = Pl_Create_Interval_Chunk(chunk->min - other_chunk->max,
                                       chunk->max - other_chunk->min);
    r.last = r.first;
    chunk = chunk->next;
    // remaining chunks
    while (chunk != NULL) {
      // range is strictly increasing (prevent overlap)
      int tmp_min = chunk->min - other_chunk->max;
      if (tmp_min <= r.last->max + 1) {
        r.last->max = chunk->max - other_chunk->min;
      } else {
        Pl_Set_Chunk_At_End_Of_Range(&r,
          Pl_Create_Interval_Chunk(chunk->min - other_chunk->max,
                                   chunk->max - other_chunk->min));
      }
      chunk = chunk->next;
    }
    r.min = r.first->min;
    r.max = r.last->max;

    // update new range
    Pl_Range_Union(range,&r);
    other_chunk = other_chunk->next;
  }
}

void 
Pl_Sparse_Mul_Range(Range *range, Range *range1) {
  /* For every value in range1: multipy it with range and combine
     this with the previous multiplications */

  Range r, copy_range;
  Pl_Range_Copy(&copy_range,range);
  Set_To_Empty(range);
  int i = 0;
  int val;
  do {
    val = Pl_Range_Ith_Elem(range1,++i);
    Pl_Range_Copy(&r,&copy_range);
    Pl_Range_Mul_Value(&r,val);
    Pl_Range_Union(range,&r);
  } while (val != range1->max);
}

void 
Pl_Sparse_Div_Range(Range *range, Range *range1) {
  /* For every value in range1: divide it range by it and combine
     this with the previous divisions */

  Range r, copy_range;
  Pl_Range_Copy(&copy_range,range);
  Set_To_Empty(range);
  int i = 0;
  int val;
  do {
    val = Pl_Range_Ith_Elem(range1,++i);
    Pl_Range_Copy(&r,&copy_range);
    Pl_Range_Div_Value(&r,val);
    Pl_Range_Union(range,&r);
  } while (val != range1->max);
}

void 
Pl_Sparse_Mod_Range(Range *range, Range *range1) {
  /* For every value in range1: apply modulo N on range
   and combine this with the previous modulars */

  Range r, copy_range;
  Pl_Range_Copy(&copy_range,range);
  Set_To_Empty(range);
  int i = 0;
  int val;
  do {
    val = Pl_Range_Ith_Elem(range1,++i);
    Pl_Range_Copy(&r,&copy_range);
    Pl_Range_Mod_Value(&r,val);
    Pl_Range_Union(range,&r);
  } while (val != range1->max);
}

int 
Pl_Sparse_Nb_Elem(Range *range) {

  Chunk *chunk = range->first;
  int nb_count = 0;

  // iterate over all chunks and count the number of elements
  while (chunk != NULL) {
    nb_count += chunk->max - chunk->min + 1;
    chunk = chunk->next;
  }

  return nb_count;
}

int 
Pl_Sparse_Ith_Elem(Range *range, int i) {
  if (i<= 0) 
    return INTERVAL_MIN_INTEGER-1;

  Chunk *chunk = range->first;
  while (chunk != NULL) {
    if (i > chunk->max - chunk->min + 1) {
      i -= chunk->max - chunk->min + 1;
      chunk = chunk->next;
    }
    else {
      return chunk->min + i - 1;
    }
  }

  return INTERVAL_MIN_INTEGER-1;
}

int 
Pl_Sparse_Next_Before(Range *range, int n) {
  n--;
  Chunk *chunk = Pl_Get_Chunk_For_Value(range,n);

  if (chunk->min <= n && chunk->max >= n) return n;
  else if (chunk->min > n) {
    if (chunk->prev!=NULL) return ((Chunk*)chunk->prev)->max;
  } else {
    return chunk->max;
  }

  return INTERVAL_MIN_INTEGER-1;
}

int 
Pl_Sparse_Next_After(Range *range, int n) {
  n++;
  Chunk *chunk = Pl_Get_Chunk_For_Value(range,n);

  if (chunk->min <= n && chunk->max >= n) return n;
  else if (chunk->min > n) {
    return chunk->min;
  } else { // chunk->max < n
    if (chunk->next!=NULL) return ((Chunk*)chunk->next)->min;
  }

  return INTERVAL_MIN_INTEGER-1;}


Bool 
Pl_Sparse_Interval_Test_Null_Inter(Range *range, Range *range1) {
  /* If interval range overlaps with part of the sparse range, return FALSE*/
  if (range1->min <= range->min && range1->max >= range->max)
    return FALSE;

  Chunk *chunk = range->first;
  while(chunk!=NULL) {
    if (chunk->max < range1->min)
      chunk = chunk->next;
    else
      return (chunk->min > range1->max) ? TRUE : FALSE;
  }
  return TRUE;
}

Bool 
Pl_Sparse_Sparse_Test_Null_Inter(Range *range, Range *range1) {
  Chunk *chunk = range->first;
  Chunk *chunk1 = range1->first;

  while(chunk!=NULL && chunk1!=NULL) {
    if (chunk->max < chunk1->min) {
      chunk = chunk->next;
    }
    else if (chunk->min > chunk1->max) {
      chunk1 = chunk1->next;
    }
    else {
      return FALSE;
    }
  }
  return TRUE;
}
