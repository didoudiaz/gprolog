/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_range.h                                                      *
 * Descr.: FD Range Implementation - header file                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2015 Daniel Diaz                                     *
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

#define CHUNK_SIZE (sizeof(Chunk) / sizeof(WamWord))

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int min;
  int max;
  void *prev;
  void *next;
}
Chunk;

typedef struct			/* Ranges are always handled through pointers */
{
  int min;
  int max;
  Chunk *first;
  Chunk *last;
}
Range;



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#if 0
#define GP_FD_POSITIVE_ONLY 					/* if set, use only positive integers (for backwards compatibility) */
#endif

#if 0
#define USE_MALLOC_FOR_CHUNKS
#endif

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
 * Chunk methods                   *
 *---------------------------------*/

Chunk * Pl_Create_Interval_Chunk(int min, int max);

Chunk * Pl_Get_Chunk_For_Value_Recur(Chunk* chunk, int n);

Chunk * Pl_Get_Chunk_For_Value(Range* range, int n);

Bool Pl_Sparse_Test_Value(Range *range, int n);

void Pl_Sparse_Set_Value(Range *range, int n);

void Pl_Sparse_Reset_Value(Range *range, int n);

void Pl_Sparse_Inter(Range *range, Range *range1);

void Pl_Sparse_Union(Range *range, Range *range1);

void Pl_Set_Chunk_At_End_Of_Range(Range *range, Chunk *chunk);

void Pl_Set_Chunk_Before_Chunk(Range *range, Chunk *new_chunk, Chunk *old_chunk);

void Pl_Remove_Interval_Chunk(Range *range, Chunk *chunk);

void Pl_Remove_Interval_Chunk_If_Needed(Range *range, Chunk *chunk);

int Pl_Chunk_Count(Range *range);

void Pl_Sparse_Compl(Range *range);

void Pl_Sparse_Mul_Value(Range *range, int n);

void Pl_Sparse_Div_Value(Range *range, int n);

void Pl_Sparse_Add_Value(Range *range, int n);

void Pl_Sparse_Mod_Value(Range *range, int n);

void Pl_Sparse_Mul_Range(Range *range, Range *range1);

void Pl_Sparse_Div_Range(Range *range, Range *range1);

void Pl_Sparse_Add_Range(Range *range, Range *range1);

void Pl_Sparse_Sub_Range(Range *range, Range *range1);

void Pl_Sparse_Mod_Range(Range *range, Range *range1);

int Pl_Sparse_Nb_Elem(Range *range);

int Pl_Sparse_Ith_Elem(Range *range, int i);

int Pl_Sparse_Next_Before(Range *range, int n);

int Pl_Sparse_Next_After(Range *range, int n);

Bool Pl_Sparse_Interval_Test_Null_Inter(Range *range, Range *range1);

Bool Pl_Sparse_Sparse_Test_Null_Inter(Range *range, Range *range1);

/*---------------------------------*
 * debug methods                   *
 *---------------------------------*/

void Pl_Err_Resource(int atom);

/*---------------------------------*
 * Chunk Management Macros           *
 *---------------------------------*/


#define Chunk_Allocate_If_Necessary(chunk) 	\
  do            				\
    {           				\
      if (chunk == NULL)        		\
	Chunk_Allocate(chunk);     		\
    }           				\
  while (0)

/* Allocate with malloc for unit testing of test_fd_range.c */
/* TODO: update this so that no testing code is in fd_range *
 * (I haven't found a neat way to do this yet)              */
#ifndef USE_MALLOC_FOR_CHUNKS
#define Chunk_Allocate(chunk)			\
  do 						\
    {						\
      chunk = (Chunk*) RANGE_TOP_STACK;		\
      RANGE_TOP_STACK += CHUNK_SIZE;		\
    }						\
  while (0)
#else
#define Chunk_Allocate(chunk)     \
  do            \
    {           \
      chunk = (Chunk*) malloc(sizeof(Chunk)); \
    }           \
  while (0)
#endif

/*---------------------------------*
 * Range Management Macros         *
 *---------------------------------*/

#define Is_Interval(range)         ((range)->first == NULL)
#define Is_Sparse(range)           ((range)->first != NULL)
#define Is_Empty(range)            ((range)->min >  (range)->max)
#define Is_Not_Empty(range)        ((range)->max >= (range)->min)


#define Set_To_Empty(range) 				\
  do							\
    {							\
      (range)->min = 1;					\
      (range)->max = 0;					\
      (range)->first = NULL;				\
      (range)->last = NULL;				\
    }							\
  while (0)


#define Range_Init_Interval(range, r_min, r_max)	\
  do							\
    {							\
      (range)->min = (r_min);				\
      (range)->max = (r_max);				\
      (range)->first = NULL;				\
      (range)->last = NULL;				\
    }							\
  while (0)








