/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : hash_fct.c                                                      *
 * Descr.: hash function                                                   *
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


#ifndef _HASH_FCT_H
#define _HASH_FCT_H

#include <stdint.h>


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct
{
  int len;
  uint32_t hash;
} HashIncrInfo;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

uint32_t Pl_Hash_Buffer(const void *data, int len);

void Pl_Hash_Incr_Init(HashIncrInfo *hi);

void Pl_Hash_Incr_Buffer(HashIncrInfo *hi, const void *data, int len);

void Pl_Hash_Incr_Int32(HashIncrInfo *hi, uint32_t x);

void Pl_Hash_Incr_Int64(HashIncrInfo *hi, uint64_t x);

void Pl_Hash_Incr_Double(HashIncrInfo *hi, double x);

uint32_t Pl_Hash_Incr_Term(HashIncrInfo *hi);

#endif
