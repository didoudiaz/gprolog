/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.c                                                      *
 * Descr.: FD Range Implementation                                         *
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


#include <stdio.h>
#include <string.h>

#include "bool.h"

#define FD_RANGE_FILE

#include "engine_pl.h"
#include "engine_fd.h"





/*-------------------------------------------------------------------------*
 * The file fd_hook_range.h must contains the definition of:               *
 *                                                                         *
 * INTERVAL_MAX_INTEGER: an integer constant corresponding to the greatest *
 *                       value for intervals (i.e. 0..INTERVAL_MAX_INTEGER)*
 *                                                                         *
 * pl_vec_max_integer  : an integer variable corresponding to the greatest *
 *                       value for vectors   (i.e. 0..pl_vec_max_integer). *
 * pl_vec_size         : an integer variable corresponding to the size of a*
 *                       vector in words(i.e. pl_vec_max_integer/WORD_SIZE)*
 *                       (see Pl_Define_Vector_Size() function).           *
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

#define ALL_1                      ((VecWord) -1)

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




/*-------------------------------------------------------------------------*
 * PL_DEFINE_VECTOR_SIZE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Define_Vector_Size(int max_val)
{
  pl_vec_size = max_val / WORD_SIZE + 1;
  pl_vec_max_integer = pl_vec_size * WORD_SIZE - 1;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_FROM_INTERVAL                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_From_Interval(Vector vec, int min, int max)
{
  Vector w_min = vec + Word_No(min);
  Vector w_max = vec + Word_No(max);
  Vector end = vec + pl_vec_size;

  for (;;)
    if (vec == w_min)
      break;
    else
      *vec++ = 0;

  for (;;)
    if (vec > w_max)
      break;
    else
      *vec++ = ALL_1;

  for (;;)
    if (vec == end)
      break;
    else
      *vec++ = 0;

  *w_min &= ALL_1 << Bit_No(min);
  *w_max &= ALL_1 >> (WORD_SIZE - 1 - Bit_No(max));
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_NB_ELEM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Vector_Nb_Elem(Vector vec)
{
  register Vector end = vec + pl_vec_size;
  register int nb_elem = 0;

  do
    {
      nb_elem += Pl_Count_Set_Bits(*vec);
      vec++;
    }
  while (vec < end);

  return nb_elem;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_ITH_ELEM                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Vector_Ith_Elem(Vector vec, int i)
{
  int vec_elem;

  if (i > 0)			/* 1 <= i  <= nb_elem */
    {
      VECTOR_BEGIN_ENUM(vec, vec_elem);

      if (--i == 0)
	return vec_elem;

      VECTOR_END_ENUM;
    }

  return -1;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_NEXT_AFTER                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Vector_Next_After(Vector vec, int n)
{
  int word_no;
  int bit_no;
  Vector start;
  Vector end;
  VecWord word;
  int bit;

  if (n >= 0)			/* n >= 0 find next */
    {
      if (n > pl_vec_max_integer)
	return -1;

      word_no = Word_No(n);
      bit_no = Bit_No(n) + 1;

      start = vec + word_no;

      word = (bit_no == WORD_SIZE) ? 0 : *start & ~(((PlLong)1 << bit_no) - 1);
    }
  else				/* n < 0 find first */
    {
      start = vec;
      word = *start;
    }


  end = vec + pl_vec_size;

  while (word == 0)
    {
      if (++start >= end)
	return -1;

      word = *start;
    }

  bit = Pl_Least_Significant_Bit(word);
  n = Word_No_And_Bit_No(start - vec, bit);

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_NEXT_BEFORE                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Vector_Next_Before(Vector vec, int n)
{
  int word_no;
  int bit_no;
  Vector start;
  Vector end;
  VecWord word;
  int bit;

  if (n <= pl_vec_max_integer)	/* n <= pl_vec_max_integer find previous */
    {
      if (n < 0)
	return -1;

      word_no = Word_No(n);
      bit_no = Bit_No(n);

      end = vec + word_no;

      word = *end & (((PlLong)1 << bit_no) - 1);
    }
  else				/* n > pl_vec_max_integer find last */
    {
      end = vec + pl_vec_size - 1;
      word = *end;
    }


  start = vec;

  while (word == 0)
    {
      if (--end < start)
	return -1;

      word = *end;
    }

  bit = Pl_Most_Significant_Bit(word);
  n = Word_No_And_Bit_No(end - vec, bit);

  return n;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_EMPTY                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Empty(Vector vec)
{
  Vector end = vec + pl_vec_size;

  do
    *vec++ = 0;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_FULL                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Full(Vector vec)
{
  Vector end = vec + pl_vec_size;

  do
    *vec++ = ALL_1;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_TEST_NULL_INTER                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Vector_Test_Null_Inter(Vector vec, Vector vec1)
{
  Vector end = vec + pl_vec_size;

  do
    if (*vec++ & *vec1++)
      return FALSE;

  while (vec < end);

  return TRUE;
}


/*-------------------------------------------------------------------------*
 * PL_VECTOR_COPY                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Copy(Vector vec, Vector vec1)
{
  Vector end = vec + pl_vec_size;

  do
    *vec++ = *vec1++;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_UNION                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Union(Vector vec, Vector vec1)
{
  Vector end = vec + pl_vec_size;

  do
    *vec++ |= *vec1++;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_INTER                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Inter(Vector vec, Vector vec1)
{
  Vector end = vec + pl_vec_size;

  do
    *vec++ &= *vec1++;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_COMPL                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Compl(Vector vec)
{
  Vector end = vec + pl_vec_size;

  do
    *vec = ~(*vec), vec++;
  while (vec < end);
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_ADD_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Add_Vector(Vector vec, Vector vec1)
{
  Vector aux_vec;
  int vec_elem, vec_elem1;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  VECTOR_BEGIN_ENUM(vec1, vec_elem1);

  x = vec_elem + vec_elem1;

  if (x > pl_vec_max_integer)
    goto loop1;

  Vector_Set_Value(vec, x);

  VECTOR_END_ENUM;

loop1:;

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_SUB_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Sub_Vector(Vector vec, Vector vec1)
{
  Vector aux_vec;
  int vec_elem, vec_elem1;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  VECTOR_BEGIN_ENUM(vec1, vec_elem1);

  x = vec_elem - vec_elem1;

  if (x < 0)
    goto loop1;

  Vector_Set_Value(vec, x);
  VECTOR_END_ENUM;

loop1:;

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_MUL_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Mul_Vector(Vector vec, Vector vec1)
{
  Vector aux_vec;
  int vec_elem, vec_elem1;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  VECTOR_BEGIN_ENUM(vec1, vec_elem1);

  x = vec_elem * vec_elem1;

  if (x > pl_vec_max_integer)
    goto loop1;

  Vector_Set_Value(vec, x);

  VECTOR_END_ENUM;

loop1:;

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_DIV_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Div_Vector(Vector vec, Vector vec1)
{
  Vector aux_vec;
  int vec_elem, vec_elem1;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);

  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  if (vec_elem == 0)
    Vector_Set_Value(vec, 0);
  else
    {
      VECTOR_BEGIN_ENUM(vec1, vec_elem1);

      if (vec_elem1 != 0 && vec_elem % vec_elem1 == 0)
	{
	  x = vec_elem / vec_elem1;
	  Vector_Set_Value(vec, x);
	}

      VECTOR_END_ENUM;
    }

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_MOD_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Mod_Vector(Vector vec, Vector vec1)
{
  Vector aux_vec;
  int vec_elem, vec_elem1;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  VECTOR_BEGIN_ENUM(vec1, vec_elem1);

  if (vec_elem1 != 0)
    {
      x = vec_elem % vec_elem1;
      Vector_Set_Value(vec, x);
    }

  VECTOR_END_ENUM;

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_ADD_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Add_Value(Vector vec, int n)
{
  int word_no;
  int bit_no;
  VecWord rem, rem1;
  int i, j;

  if (n >= 0)
    {
      word_no = Word_No(n);
      bit_no = Bit_No(n);

      if (word_no)
	{
	  i = pl_vec_size - 1;
	  j = pl_vec_size - 1 - word_no;

	  while (j >= 0)
	    vec[i--] = vec[j--];

	  while (i >= 0)
	    vec[i--] = 0;
	}

      if (bit_no)
	{
	  rem = 0;
	  for (i = word_no; i < pl_vec_size; i++)
	    {
	      rem1 = vec[i] >> (WORD_SIZE - bit_no);
	      vec[i] = (vec[i] << bit_no) | rem;
	      rem = rem1;
	    }
	}
    }
  else
    {
      word_no = Word_No(-n);
      bit_no = Bit_No(-n);

      if (word_no)
	{
	  i = 0;
	  j = word_no;

	  while (j < pl_vec_size)
	    vec[i++] = vec[j++];

	  while (i < pl_vec_size)
	    vec[i++] = 0;
	}

      if (bit_no)
	{
	  rem = 0;
	  for (i = pl_vec_size - 1 - word_no; i >= 0; i--)
	    {
	      rem1 = vec[i] << (WORD_SIZE - bit_no);
	      vec[i] = (vec[i] >> bit_no) | rem;
	      rem = rem1;
	    }
	}
    }
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_MUL_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Mul_Value(Vector vec, int n)
{
  Vector aux_vec;
  int vec_elem;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  x = vec_elem * n;

  if ((unsigned) x > (unsigned) pl_vec_max_integer)
    return;

  Vector_Set_Value(vec, x);

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_DIV_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Div_Value(Vector vec, int n)
{
  Vector aux_vec;
  int vec_elem;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  if (n == 0)
    return;

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  if (vec_elem % n == 0)
    {
      x = vec_elem / n;
      Vector_Set_Value(vec, x);
    }

  VECTOR_END_ENUM;
}




/*-------------------------------------------------------------------------*
 * PL_VECTOR_MOD_VALUE                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Vector_Mod_Value(Vector vec, int n)
{
  Vector aux_vec;
  int vec_elem;
  int x;

  Vector_Allocate(aux_vec);
  Pl_Vector_Copy(aux_vec, vec);
  Pl_Vector_Empty(vec);

  if (n == 0)
    return;

  VECTOR_BEGIN_ENUM(aux_vec, vec_elem);

  x = vec_elem % n;
  if ((unsigned) x <= (unsigned) pl_vec_max_integer)
    Vector_Set_Value(vec, x);

  VECTOR_END_ENUM;
}




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

  return Vector_Test_Value(range->vec, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_TEST_NULL_INTER                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Range_Test_Null_Inter(Range *range, Range *range1)
{
  int swt, i;

  if (range->min > range1->max || range1->min > range->max)
    return TRUE;

  if (range->min == range1->min || range->min == range1->max ||
      range->max == range1->min || range->max == range1->max)
    return FALSE;

  swt = (Is_Sparse(range) << 1) + Is_Sparse(range1);

  if (swt == 3)			/* Sparse with Sparse */
    return Pl_Vector_Test_Null_Inter(range1->vec, range->vec);


  if ((range->min >= range1->min && range->max >= range1->max) ||
      (range1->min >= range->min && range1->max >= range->max))
    return FALSE;

  if (swt == 0)			/* Interval with Interval */
    return FALSE;

  if (swt == 2)			/* Sparse with Interval */
    return Pl_Range_Test_Null_Inter(range1, range);


  /* Interval with Sparse */
  if (range->min <= range1->min)
    return FALSE;

  for (i = range->min; i <= range->max; i++)
    if (Vector_Test_Value(range1->vec, i))
      return FALSE;

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_COPY                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Copy(Range *range, Range *range1)
{
  range->extra_cstr = range1->extra_cstr;
  range->min = range1->min;
  range->max = range1->max;

  if (Is_Interval(range1))
    range->vec = NULL;
  else
    {
      Vector_Allocate_If_Necessary(range->vec);
      Pl_Vector_Copy(range->vec, range1->vec);
    }
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NB_ELEM                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Nb_Elem(Range *range)
{
  if (Is_Interval(range))	/* here range is not empty */
    return range->max - range->min + 1;


  return Pl_Vector_Nb_Elem(range->vec);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_ITH_ELEM                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Ith_Elem(Range *range, int i)
{
  int n;

  if (Is_Empty(range))
    return -1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      n = range->min + i - 1;

      return n < range->min || n > range->max ? -1 : n;
    }


  return Pl_Vector_Ith_Elem(range->vec, i);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NEXT_AFTER                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Next_After(Range *range, int n)
{
  if (Is_Empty(range))
    return -1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      if (n >= range->max)
	return -1;

      n++;
      if (n < range->min)
	n = range->min;

      return n;
    }


  return Pl_Vector_Next_After(range->vec, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_NEXT_BEFORE                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Range_Next_Before(Range *range, int n)
{
  if (Is_Empty(range))
    return -1;

  if (Is_Interval(range))	/* here range is not empty */
    {				/* 1 <= i  <= nb_elem */
      if (n <= range->min)
	return -1;

      n--;
      if (n > range->max)
	n = range->max;

      return n;
    }


  return Pl_Vector_Next_Before(range->vec, n);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_BECOMES_SPARSE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Becomes_Sparse(Range *range)
{
  Vector_Allocate_If_Necessary(range->vec);

  if (range->min < 0)
    range->min = 0;

  if ((range->extra_cstr = (range->max > pl_vec_max_integer)))
    range->max = pl_vec_max_integer;

  if (Is_Not_Empty(range))
    Pl_Vector_From_Interval(range->vec, range->min, range->max);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_FROM_VECTOR                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_From_Vector(Range *range)
{
  Vector start;
  Vector end;
  int bit;

  start = range->vec - 1;
  end = range->vec + pl_vec_size;

  for (;;)
    if (*++start)
      break;
    else if (start >= end)
      {
	Set_To_Empty(range);
	return;
      }

  for (;;)
    if (*--end)
      break;

  bit = Pl_Least_Significant_Bit(*start);
  range->min = Word_No_And_Bit_No(start - range->vec, bit);

  bit = Pl_Most_Significant_Bit(*end);
  range->max = Word_No_And_Bit_No(end - range->vec, bit);
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
      if ((unsigned) n <= (unsigned) pl_vec_max_integer)
	{
	  Vector_Set_Value(range->vec, n);
	  Pl_Range_From_Vector(range);
	}
      else
	range->extra_cstr = TRUE;

      return;
    }

  if ((unsigned) n > (unsigned) pl_vec_max_integer)
    {
      range->extra_cstr = TRUE;
      return;
    }

  Vector_Set_Value(range->vec, n);
  if (n < range->min || n > range->max)
    Pl_Range_From_Vector(range);
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

  if (range->min == range->max)
    {
      Set_To_Empty(range);
      return;
    }

  if (Is_Interval(range))
    {
      if (n == range->min)
	{
	  range->min++;
	  return;
	}

      if (n == range->max)
	{
	  range->max--;
	  return;
	}

      Pl_Range_Becomes_Sparse(range);
      if ((unsigned) n <= (unsigned) pl_vec_max_integer)
	Vector_Reset_Value(range->vec, n);

      return;
    }


  if ((unsigned) n > (unsigned) pl_vec_max_integer)
    return;


  Vector_Reset_Value(range->vec, n);
  if (n == range->min || n == range->max)
    Pl_Range_From_Vector(range);
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
  Bool extra_cstr;

  if (swt == 0)			/* Interval with Interval */
    {
      if (Is_Not_Empty(range) && Is_Not_Empty(range1) &&
	  range1->min <= range->max + 1 && range->min <= range1->max + 1)
	{
/*       range->extra_cstr=FALSE; */
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

  extra_cstr = range->extra_cstr | range1->extra_cstr;

  if (Is_Empty(range))
    {
      Pl_Range_Copy(range, range1);
      range->extra_cstr = extra_cstr;
      return;
    }

  range->extra_cstr = extra_cstr;

  if (Is_Empty(range1))
    return;

  range->min = math_min(range->min, range1->min);
  range->max = math_max(range->max, range1->max);
  Pl_Vector_Union(range->vec, range1->vec);
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

  if (swt == 0)			/* Interval with Interval */
    {
/*   range->extra_cstr=FALSE; */
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

  range->extra_cstr &= range1->extra_cstr;

  if (Is_Empty(range))
    return;


  if (Is_Empty(range1))
    {
      Set_To_Empty(range);
      return;
    }

  Pl_Vector_Inter(range->vec, range1->vec);
  Pl_Range_From_Vector(range);	/* adjust min and max */
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
	  range->min = 0;
	  range->max = INTERVAL_MAX_INTEGER;
	  return;
	}

      if (range->min <= 0)
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
	  range->min = 0;

	  return;
	}

      Pl_Range_Becomes_Sparse(range);
    }
  /* Sparse */
  range->extra_cstr = TRUE;

  if (Is_Empty(range))
    {
      range->min = 0;
      range->max = pl_vec_max_integer;
      Pl_Vector_Full(range->vec);
    }
  else
    {
      Pl_Vector_Compl(range->vec);
      Pl_Range_From_Vector(range);
    }
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
/*   range->extra_cstr=FALSE; */
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
  Pl_Vector_Add_Vector(range->vec, range1->vec);

  range->min += range1->min;
  range->max += range1->max;
  range->extra_cstr |=
    (range1->extra_cstr | (range->max > pl_vec_max_integer));

  if (range->extra_cstr || range->min < 0)
    Pl_Range_From_Vector(range);
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
/*   range->extra_cstr=FALSE; */
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
  Pl_Vector_Sub_Vector(range->vec, range1->vec);

  range->min -= range1->max;
  range->max -= range1->min;
  range->extra_cstr |= range1->extra_cstr;

  if (range->extra_cstr || range->min < 0)
    Pl_Range_From_Vector(range);
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
      r.vec = NULL;
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }
  /* Sparse with Sparse */
  Pl_Vector_Mul_Vector(range->vec, range1->vec);

  range->extra_cstr |= range1->extra_cstr;
  Pl_Range_From_Vector(range);
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
      r.vec = NULL;
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }
  /* Sparse with Sparse */
  Pl_Vector_Div_Vector(range->vec, range1->vec);

  range->extra_cstr |= range1->extra_cstr;
  Pl_Range_From_Vector(range);
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
      r.vec = NULL;
      Pl_Range_Copy(&r, range1);	/* we cannot modify range1 */
      range1 = &r;
      Pl_Range_Becomes_Sparse(range1);
    }

  /* Sparse with Sparse */
  Pl_Vector_Mod_Vector(range->vec, range1->vec);

  range->extra_cstr |= range1->extra_cstr;
  Pl_Range_From_Vector(range);
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
      range->min += n;
      range->max += n;

      return;
    }
  /* Sparse */
  Pl_Vector_Add_Value(range->vec, n);

  range->min += n;
  range->max += n;

  range->extra_cstr |= (range->max > pl_vec_max_integer);
  if (range->extra_cstr || range->min < 0)
    Pl_Range_From_Vector(range);
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
  Pl_Vector_Mul_Value(range->vec, n);

  range->min = range->min * n;
  range->max = range->max * n;

  range->extra_cstr |= (range->max > pl_vec_max_integer);
  if (range->extra_cstr)
    Pl_Range_From_Vector(range);
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

  if (Is_Interval(range))	/* Interval */
    Pl_Range_Becomes_Sparse(range);
  /* Sparse */
  Pl_Vector_Div_Value(range->vec, n);

  range->min = (range->min + n - 1) / n;
  range->max = range->max / n;

  range->extra_cstr |= (range->max > pl_vec_max_integer);
  if (range->extra_cstr)
    Pl_Range_From_Vector(range);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_MOD_VALUE                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Range_Mod_Value(Range *range, int n)
{
  Range aux;

  if (Is_Empty(range))
    return;

  if (n < 0)
    n = -n;

  if (Is_Interval(range))	/* Interval */
    {
      if (range->min >= 0)
	{
	  if (range->max - range->min + 1 >= n)
	    {
	      range->min = 0;
	      range->max = n - 1;

	      return;
	    }

	  range->min = range->min % n;
	  range->max = range->max % n;

	  if (range->min > range->max)
	    {
	      Range_Init_Interval(&aux, 0, range->max);
	      range->max = n - 1;

	      Pl_Range_Union(range, &aux);
	    }

	  return;
	}

      if (range->max <= 0)
	{
	  if (range->max - range->min + 1 >= n)
	    {
	      range->min = -(n - 1);
	      range->max = 0;

	      return;
	    }

	  range->min = range->min % n;
	  range->max = range->max % n;

	  if (range->min > range->max)	/* Only 0 will remain in the   */
	    {			/* range due to the changeover */
	      /* from Interval to Sparse     */
	      Range_Init_Interval(&aux, -(n - 1), range->max);
	      range->max = 0;

	      Pl_Range_Union(range, &aux);
	    }

	  return;
	}

      /* Here range->min < 0 and range->max > 0 */
      range->min = math_max(range->min, -n + 1);
      range->max = math_min(range->max, n - 1);
      return;
    }
  /* Sparse */
  Pl_Vector_Mod_Value(range->vec, n);

  Pl_Range_From_Vector(range);
}




/*-------------------------------------------------------------------------*
 * PL_RANGE_TO_STRING                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Pl_Range_To_String(Range *range)
{
  int vec_elem;
  int limit1 = -1;
  int limit2;
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

  sprintf(buff, "%s", WRITE_BEGIN_RANGE);

  VECTOR_BEGIN_ENUM(range->vec, vec_elem);
  if (limit1 == -1)
    limit1 = limit2 = vec_elem;
  else if (vec_elem == limit2 + 1)
    limit2 = vec_elem;
  else
    {
      if (limit2 == limit1)
	sprintf(buff + strlen(buff), "%d%s",
		limit1, WRITE_INTERVALS_SEPARATOR);
      else
	sprintf(buff + strlen(buff), "%d%s%d%s",
		limit1, WRITE_LIMITS_SEPARATOR, limit2,
		WRITE_INTERVALS_SEPARATOR);
      limit1 = limit2 = vec_elem;
    }
  VECTOR_END_ENUM;

  if (limit1 != -1)
    {
      if (limit2 == limit1)
	sprintf(buff + strlen(buff), "%d%s", limit1, WRITE_END_RANGE);
      else
	sprintf(buff + strlen(buff), "%d%s%d%s",
		limit1, WRITE_LIMITS_SEPARATOR, limit2, WRITE_END_RANGE);
    }

  if (range->extra_cstr)
    sprintf(buff + strlen(buff), "%s", WRITE_EXTRA_CSTR_SYMBOL);

  return buff;
}
