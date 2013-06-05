/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_inst.h                                                      *
 * Descr.: WAM garbage collected heap functions - header file              *
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

#ifndef _WAM_GC_HEAP_H
#define _WAM_GC_HEAP_H
#ifdef BOEHM_GC

#include "gp_config.h"
#include "wam_archi.h"

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define GC_is_tag_allowed(tag_mask) (tag_mask == TAG_REF_MASK || tag_mask == TAG_ATM_MASK || tag_mask == TAG_INT_MASK)

#define GC_assert_clean_start_word(start_word)\
      do\
	{\
	  assert( !Tag_Is_LST(start_word) );\
	  assert( !Tag_Is_STC(start_word) );\
	  assert( !Tag_Is_FLT(start_word) );\
	  assert( !Tag_Is_FDV(start_word) );\
	}\
      while (0)

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int FC
Pl_GC_Register_Trail_Elem(WamWord **trail);

int FC
Pl_GC_Unregister_Trail_Elem(WamWord **trail);

size_t FC
Pl_GC_Compact_Trail();

WamWord * FC
Pl_GC_Mem_Alloc(PlULong n_wamwords);


WamWord * FC
Pl_GC_Alloc_Struc(WamWord **next_H, PlULong arity);

WamWord * FC
Pl_GC_Alloc_List(WamWord **next_H);

WamWord * FC
Pl_GC_Alloc_Float(WamWord **next_H);

/*---------------------------------*
 * Auxiliary engine macros         *
 *---------------------------------*/

#endif // BOEHM_GC
#endif // _WAM_GC_HEAP_H
