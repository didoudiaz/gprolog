/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_gc_heap.c                                                   *
 * Descr.: WAM garbage collected heap functions                            *
 * Author: Nick Calus and Daniel Diaz                                      *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

#ifdef BOEHM_GC
#include <stdio.h>
#include <stdlib.h>
#include <gc/gc.h>
#include <assert.h>
#include <signal.h>

#include "engine_pl.h"



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

void * pl_gc_bad_alloc=0;
size_t pl_gc_bad_alloc_count=0;
#define PL_GC_BAD_ALLOC_COUNT_MOD (1024*1024)

static int old_gc_no=0;

/*---------------------------------*
 * Function Definitions            *
 *---------------------------------*/

static inline void
pre_collect()
{
  assert(old_gc_no == GC_gc_no);
}

static inline int
post_collect()
{
  if (old_gc_no != GC_gc_no)
    {
      old_gc_no = GC_gc_no;
      Pl_GC_Compact_Trail();
      return 1;
    }
  return 0;
}

/*-------------------------------------------------------------------------*
 * Pl_GC_Init                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_GC_Init()
{
  GC_INIT();
#ifdef DEBUG_MEM_GC_DONT_COLLECT
  GC_disable();
#endif /* DEBUG_MEM_GC_DONT_COLLECT */
  old_gc_no = GC_gc_no;
}

/*-------------------------------------------------------------------------*
 * Pl_GC_Init_Roots                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_GC_Init_Roots()
{
  GC_add_roots(Local_Stack, Local_Stack + Local_Size);
}


/*-------------------------------------------------------------------------*
 * Pl_GC_Register_Trail_Elem                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static inline int
register_trail_elem(WamWord **trail)
{
  if (*trail < Local_Stack || *trail >= Local_Stack + Local_Size)
    {
      return GC_general_register_disappearing_link((void **)trail, *trail);
    }
  return 0;
}

int FC
Pl_GC_Register_Trail_Elem(WamWord **trail)
{
  int result = 0;
  pre_collect();
  result = register_trail_elem(trail);
  if (TR + 1 >= Trail_Stack + Trail_Size)
    GC_gcollect();
  post_collect();
  return result;
}


/*-------------------------------------------------------------------------*
 * Pl_GC_Unregister_Trail_Elem                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
#define unregister_trail_elem Pl_GC_Unregister_Trail_Elem

int FC
Pl_GC_Unregister_Trail_Elem(WamWord **trail)
{
  if (*trail < Local_Stack || *trail >= Local_Stack + Local_Size)
    {
      return GC_unregister_disappearing_link((void **)trail);
    }
  return 0;
}



/*-------------------------------------------------------------------------*
 * Pl_GC_Compact_Trail                                                     *
 *                                                                         *
 * See also Pl_Untrail in wam_inst.c                                       *
 *-------------------------------------------------------------------------*/
static void
move_trail_elem(WamWord **dst, WamWord **src)
{
  int nb;
  WamWord *p;

  assert( *dst <= *src );

  if (Trail_Tag_Of(**src) == TUV)
    {
      if (Trail_Value_Of(**src) != 0)
	{
	  **dst = **src;
	  unregister_trail_elem((WamWord **)*src);
	  nb = register_trail_elem((WamWord **)*dst);
	  assert( nb == 0 );
	  (*dst)++;
	}
      (*src)++;
    }
  else
    {
      nb = Trail_Size_Of(**src);
      while (nb-- > 0)
	{
	  *(*dst)++ = *(*src)++;
	}
    }
}

static void *
compact_trail(void *data)
{
  WamWord *b;
  WamWord *src, *dst;

  src = b = B;
  while (src != LSSA && TRB(src) > Trail_Stack)
    {
      b = src;
      src = BB(src);
    }
  if (b == src)
    b = LSSA;

  dst = src = Trail_Stack;
  while (src < TR)
    {
      move_trail_elem(&dst, &src);
      while (b != LSSA && src >= TRB(b))
	{
	  assert( src == TRB(b) );
	  TRB(b) = dst;
	  if (b == B)
	    b = LSSA;
	  else
	    b = FBB(b);
	}
    }
  assert( src == TR );

  TR = dst;
  *(size_t *)data = src-dst;
  return data;
}

size_t FC
Pl_GC_Compact_Trail()
{
  size_t reduction = 0;
  sigset_t block;
  sigset_t orig;
  sigemptyset(&block);
  sigaddset(&block, SIGINT);
  sigprocmask(SIG_BLOCK, &block, &orig);
  GC_call_with_alloc_lock(&compact_trail, (void *)&reduction);
  sigprocmask(SIG_SETMASK, &orig, NULL);
  return reduction;
}



/*-------------------------------------------------------------------------*
 * Pl_GC_Mem_Alloc                                                         *
 *                                                                         *
 * Allocates memory for the garbage collected heap.                        *
 *-------------------------------------------------------------------------*/
WamWord * FC
Pl_GC_Mem_Alloc(PlULong n)
{
  WamWord *result = 0;
  do
    {
#if DEBUG_MEM_GC_DONT_COLLECT>=2
      WamWord *x;
      result = (WamWord *) malloc(n * sizeof(WamWord));
      if (result != 0) {
	  x = result + n;
	  while (x-- > result) {
	      *x=0;
	  }
      }
#else /* DEBUG_MEM_GC_DONT_COLLECT>=2 */
      pre_collect();
      result = (WamWord *) GC_MALLOC(n * sizeof(WamWord));
      post_collect();
#endif /* DEBUG_MEM_GC_DONT_COLLECT>=2 */
    if (!Tag_Is_REF(result+n-1))
      {
	// Keep the bad memory referenced so it is not collected and reused.
	*(void **)result = pl_gc_bad_alloc;
	pl_gc_bad_alloc = result;
	if (pl_gc_bad_alloc_count++ % PL_GC_BAD_ALLOC_COUNT_MOD == 0)
	  {
	    fprintf(stderr, "Warning: Unusable memory allocated.\n");
	  }
      }
    else
      {
	break;
      }
    }
  while (result != 0);
  assert( Tag_Is_REF(result) );
  assert( result != 0 );
  return result;
}




/*-------------------------------------------------------------------------*
 * Pl_GC_Alloc_Struc                                                       *
 *                                                                         *
 * Allocate the required amount of memory for a given tagged structure.    *
 *                                                                         *
 * next_H:  A pointer to the variable that specifies where to write        *
 *          the content of the struct.                                     *
 * w:       The tagged structure.                                          *
 * returns: A pointer to the allocated memory.                             *
 *-------------------------------------------------------------------------*/
WamWord * FC
Pl_GC_Alloc_Struc(WamWord **next_H, PlULong arity)
{
  WamWord *cur_H;
  cur_H = Pl_GC_Mem_Alloc(arity + 2);
  *cur_H = Tag_STC(cur_H + 1);
  if (next_H != 0)
    {
      *next_H = cur_H + 1;
    }
  return cur_H;
}




/*-------------------------------------------------------------------------*
 * Pl_GC_Alloc_List                                                        *
 *                                                                         *
 * Allocate the required amount of memory for a list.                      *
 *                                                                         *
 * next_H:  A pointer to the variable that specifies where to write        *
 *          the next entry.                                                *
 * returns: A pointer to the allocated memory.                             *
 *-------------------------------------------------------------------------*/
WamWord * FC
Pl_GC_Alloc_List(WamWord **next_H)
{
  WamWord *cur_H;
  cur_H = Pl_GC_Mem_Alloc(3);
  *cur_H = Tag_LST(cur_H + 1);
  if (next_H != 0)
    {
        *next_H = cur_H + 1;
    }
  return cur_H;
}




/*-------------------------------------------------------------------------*
 * Pl_GC_Alloc_Float                                                       *
 *                                                                         *
 * Allocate the required amount of memory for a list.                      *
 *                                                                         *
 * next_H:  A pointer to the variable that specifies where to write        *
 *          the next entry.                                                *
 * returns: A pointer to the allocated memory.                             *
 *-------------------------------------------------------------------------*/
WamWord * FC
Pl_GC_Alloc_Float(WamWord **next_H)
{
  WamWord *cur_H;

#if WORD_SIZE == 32
  cur_H = Pl_GC_Mem_Alloc(3);
#else
  cur_H = Pl_GC_Mem_Alloc(2);
#endif
  *cur_H = Tag_FLT(cur_H + 1);
  if (next_H != 0)
    {
      *next_H = cur_H + 1;
    }
  return cur_H;
}

#endif /* BOEHM_GC */
