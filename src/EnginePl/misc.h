/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : misc.h                                                          *
 * Descr.: miscellaneous operations - header file                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU Lesser General Public License as published   *
 * by the Free Software Foundation; either version 3, or any later version.*
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU Lesser General Public License*
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdlib.h>

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

char *Pl_Malloc_Check(unsigned size, char *src_file, int src_line);

char *Pl_Calloc_Check(unsigned nb, unsigned size, char *src_file,
		   int src_line);

char *Pl_Realloc_Check(char *ptr, unsigned size, char *src_file, int src_line);

char *Pl_Strdup_Check(char *str, char *src_file, int src_line);

#define Malloc(size)       Pl_Malloc_Check(size, __FILE__, __LINE__)

#define Calloc(nb, size)   Pl_Calloc_Check(nb, size, __FILE__, __LINE__)

#define Realloc(ptr, size) Pl_Realloc_Check(ptr, size, __FILE__, __LINE__)

#define Free(ptr)          free(ptr)

#define Strdup(str)        Pl_Strdup_Check(str, __FILE__, __LINE__)

void Pl_Extend_Table_If_Needed(char **hash_tbl);

void Pl_Extend_Array(char **ptbl, int *nb_elem, int elem_size, Bool bzero);

void Pl_Exit_With_Value(int ret_val);

void Pl_Fatal_Error(char *format, ...);



/* NB: for LSB/MSB the result is undefined if x == 0 */

#ifdef __GNUC__

#define Pl_Least_Significant_Bit(x)   (__builtin_ctzl(x))

#define Pl_Most_Significant_Bit(x)    (WORD_SIZE - 1 - __builtin_clzl(x))

#define Pl_Count_Set_Bits(x)          (__builtin_popcountl(x))


#else /* !__GNUC__ */

#define Pl_Least_Significant_Bit(x)   Pl_LSB(x)

#define Pl_Most_Significant_Bit(x)    Pl_MSB(x)

#define Pl_Count_Set_Bits(x)          Pl_Popcount(x)

int Pl_LSB(long x);

int Pl_MSB(long x);

int Pl_Popcount(long x);

#endif /* !__GNUC__ */
