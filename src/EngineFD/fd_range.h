/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.h                                                      *
 * Descr.: FD Range Implementation - header file                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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

#include "bool.h"

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef unsigned long VecWord;

typedef VecWord *Vector;

typedef struct			/* Ranges are always handled through pointers */
{
  Bool extra_cstr;
  int min;
  int max;
  Vector vec;
}
Range;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#include "fd_hook_range.h"

	  /* Default definitions (if not defined in fd_hook_range.h) */

#ifndef WORD_SIZE
#   define WORD_SIZE               32
#endif



#if WORD_SIZE == 32

#   define WORD_SIZE_BITS          5

#else

#   define WORD_SIZE_BITS          6

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Least_Significant_Bit(VecWord x);

int Most_Significant_Bit(VecWord x);

void Define_Vector_Size(int max_val);

void Vector_From_Interval(Vector vec, int min, int max);

int Vector_Nb_Elem(Vector vec);

int Vector_Ith_Elem(Vector vec, int n);

int Vector_Next_After(Vector vec, int n);

int Vector_Next_Before(Vector vec, int n);

void Vector_Empty(Vector vec);

void Vector_Full(Vector vec);

Bool Vector_Test_Null_Inter(Vector vec, Vector vec1);

void Vector_Copy(Vector vec, Vector vec1);

void Vector_Union(Vector vec, Vector vec1);

void Vector_Inter(Vector vec, Vector vec1);

void Vector_Compl(Vector vec);

void Vector_Add_Vector(Vector vec, Vector vec1);

void Vector_Sub_Vector(Vector vec, Vector vec1);

void Vector_Mul_Vector(Vector vec, Vector vec1);

void Vector_Div_Vector(Vector vec, Vector vec1);

void Vector_Mod_Vector(Vector vec, Vector vec1);

void Vector_Add_Value(Vector vec, int n);

void Vector_Mul_Value(Vector vec, int n);

void Vector_Div_Value(Vector vec, int n);

void Vector_Mod_Value(Vector vec, int n);

Bool Range_Test_Value(Range *range, int n);

Bool Range_Test_Null_Inter(Range *range, Range *range1);

void Range_Copy(Range *range, Range *range1);

int Range_Nb_Elem(Range *range);

int Range_Ith_Elem(Range *range, int n);

int Range_Next_After(Range *range, int n);

int Range_Next_Before(Range *range, int n);

void Range_Set_Value(Range *range, int n);

void Range_Reset_Value(Range *range, int n);

void Range_Becomes_Sparse(Range *range);

void Range_From_Vector(Range *range);

void Range_Union(Range *range, Range *range1);

void Range_Inter(Range *range, Range *range1);

void Range_Compl(Range *range);

void Range_Add_Range(Range *range, Range *range1);

void Range_Sub_Range(Range *range, Range *range1);

void Range_Mul_Range(Range *range, Range *range1);

void Range_Div_Range(Range *range, Range *range1);

void Range_Mod_Range(Range *range, Range *range1);

void Range_Add_Value(Range *range, int n);

void Range_Mul_Value(Range *range, int n);

void Range_Div_Value(Range *range, int n);

void Range_Mod_Value(Range *range, int n);

char *Range_To_String(Range *range);




/*---------------------------------*
 * Vector Management Macros        *
 *---------------------------------*/

#define Word_No_And_Bit_No(w, b)   (((VecWord) (w) << WORD_SIZE_BITS)|\
                                     (VecWord) (b))
#define Word_No(n)                 ((VecWord) (n) >> WORD_SIZE_BITS)
#define Bit_No(n)                  ((n) & (((VecWord) 1 << WORD_SIZE_BITS)-1))



#define Vector_Test_Value(vec, n)  ((vec[Word_No(n)] & ((VecWord) 1 << Bit_No(n))) != 0)




#define Vector_Set_Value(vec, n)   (vec[Word_No(n)] |= ((VecWord) 1 << Bit_No(n)))




#define Vector_Reset_Value(vec, n) (vec[Word_No(n)] &= ~((VecWord) 1 << Bit_No(n)))




#define Vector_Allocate_If_Necessary(vec)	\
  do						\
    {						\
      if (vec == NULL)				\
	Vector_Allocate(vec);			\
    }						\
  while (0)




#define Vector_Allocate(vec)       		\
  do						\
    {						\
      vec = (Vector) RANGE_TOP_STACK;		\
      RANGE_TOP_STACK += vec_size;		\
    }						\
  while (0)




	  /* To enumerate a vector use VECTOR_BEGIN_ENUM / VECTOR_END_ENUM *
	   * macros as follows:                                            *
	   * ...                                                           *
	   * VECTOR_BEGIN_ENUM(the_vector,vec_elem)                        *
	   *    your code (vec_elem contains the current range element)    *
	   * VECTOR_END_ENUM                                               */

#define VECTOR_BEGIN_ENUM(vec, vec_elem)                              	  \
{									  \
  Vector enum_end = vec + vec_size, enum_i = vec;			  \
  int enum_j;								  \
  VecWord enum_word;							  \
									  \
  vec_elem = 0;								  \
  do									  \
    {									  \
      enum_word = *enum_i;						  \
      for (enum_j = 0; enum_j++ < WORD_SIZE; enum_word >>= 1, vec_elem++) \
	{								  \
	  if (enum_word & 1)						  \
	    {


#define VECTOR_END_ENUM                                              	\
	    }								\
	}								\
    }									\
  while (++enum_i < enum_end);						\
}




/*---------------------------------*
 * Range Management Macros         *
 *---------------------------------*/

#define Is_Interval(range)         ((range)->vec == NULL)
#define Is_Sparse(range)           ((range)->vec != NULL)
#define Is_Empty(range)            ((range)->min >  (range)->max)
#define Is_Not_Empty(range)        ((range)->max >= (range)->min)


#define Set_To_Empty(range) (range)->max = (int)(1 << (sizeof(int) * 8 - 1))


#define Range_Init_Interval(range, r_min, r_max)	\
  do							\
    {							\
      (range)->extra_cstr = FALSE;			\
      (range)->min = (r_min);				\
      (range)->max = (r_max);				\
      (range)->vec = NULL;				\
    }							\
  while (0)

