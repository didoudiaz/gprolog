/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : misc.h                                                          *
 * Descr.: miscellaneous operations - header file                          *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
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

char *Malloc_Check(unsigned size, char *src_file, int src_line);

char *Calloc_Check(unsigned nb, unsigned size, char *src_file,
		   int src_line);

char *Realloc_Check(char *ptr, unsigned size, char *src_file, int src_line);

char *Strdup_Check(char *str, char *src_file, int src_line);

#define Malloc(size)       Malloc_Check(size, __FILE__, __LINE__)

#define Calloc(nb, size)   Calloc_Check(nb, size, __FILE__, __LINE__)

#define Realloc(ptr, size) Realloc_Check(ptr, size, __FILE__, __LINE__)

#define Free(ptr)          free(ptr)

#define Strdup(str)        Strdup_Check(str, __FILE__, __LINE__)

void Extend_Table_If_Needed(char **hash_tbl);

void Exit_With_Value(int ret_val);

void Fatal_Error(char *format, ...);
