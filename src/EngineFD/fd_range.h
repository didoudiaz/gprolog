/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.h                                                      *
 * Descr.: FD Range Implementation - header file                           *
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


#include "bool.h"
#include "pl_long.h"

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef PlULong VecWord;

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

void Pl_Define_Vector_Size(int max_val);

void Pl_Vector_From_Interval(Vector vec, int min, int max);

int Pl_Vector_Nb_Elem(Vector vec);

int Pl_Vector_Ith_Elem(Vector vec, int n);

int Pl_Vector_Next_After(Vector vec, int n);

int Pl_Vector_Next_Before(Vector vec, int n);

void Pl_Vector_Empty(Vector vec);

void Pl_Vector_Full(Vector vec);

Bool Pl_Vector_Test_Null_Inter(Vector vec, Vector vec1);

void Pl_Vector_Copy(Vector vec, Vector vec1);

void Pl_Vector_Union(Vector vec, Vector vec1);

void Pl_Vector_Inter(Vector vec, Vector vec1);

void Pl_Vector_Compl(Vector vec);

void Pl_Vector_Add_Vector(Vector vec, Vector vec1);

void Pl_Vector_Sub_Vector(Vector vec, Vector vec1);

void Pl_Vector_Mul_Vector(Vector vec, Vector vec1);

void Pl_Vector_Div_Vector(Vector vec, Vector vec1);

void Pl_Vector_Mod_Vector(Vector vec, Vector vec1);

void Pl_Vector_Add_Value(Vector vec, int n);

void Pl_Vector_Mul_Value(Vector vec, int n);

void Pl_Vector_Div_Value(Vector vec, int n);

void Pl_Vector_Mod_Value(Vector vec, int n);

Bool Pl_Range_Test_Value(Range *range, int n);

Bool Pl_Range_Test_Null_Inter(Range *range, Range *range1);

void Pl_Range_Copy(Range *range, Range *range1);

int Pl_Range_Nb_Elem(Range *range);

int Pl_Range_Ith_Elem(Range *range, int n);

int Pl_Range_Next_After(Range *range, int n);

int Pl_Range_Next_Before(Range *range, int n);

void Pl_Range_Set_Value(Range *range, int n);

void Pl_Range_Reset_Value(Range *range, int n);

void Pl_Range_Becomes_Sparse(Range *range);

void Pl_Range_From_Vector(Range *range);

void Pl_Range_Union(Range *range, Range *range1);

void Pl_Range_Inter(Range *range, Range *range1);

void Pl_Range_Compl(Range *range);

void Pl_Range_Add_Range(Range *range, Range *range1);

void Pl_Range_Sub_Range(Range *range, Range *range1);

void Pl_Range_Mul_Range(Range *range, Range *range1);

void Pl_Range_Div_Range(Range *range, Range *range1);

void Pl_Range_Mod_Range(Range *range, Range *range1);

void Pl_Range_Add_Value(Range *range, int n);

void Pl_Range_Mul_Value(Range *range, int n);

void Pl_Range_Div_Value(Range *range, int n);

void Pl_Range_Mod_Value(Range *range, int n);

char *Pl_Range_To_String(Range *range);




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
      RANGE_TOP_STACK += pl_vec_size;		\
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
  Vector enum_end = vec + pl_vec_size, enum_i = vec;			  \
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

