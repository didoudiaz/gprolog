/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : misc.c                                                          *
 * Descr.: malloc with checks + other miscellaneous operations             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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

#include "machine.h"

#ifdef USE_DL_MALLOC
#include "dl_malloc.c"

static void __attribute__((constructor)) 
Init_Dl_Malloc(void) {
  mallopt(M_MMAP_THRESHOLD, 0xFFFFFFF);	/* big value to no use mmap */
}
#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "engine_pl.h"

#ifndef NO_USE_LINEDIT
#include "../Linedit/linedit.h"
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define ERR_ALLOC_FAULT            "Memory allocation fault (%s) in %s:%d"

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * MALLOC_CHECK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Malloc_Check(unsigned size, char *src_file, int src_line)
{
  char *m = malloc(size);

  if (m == NULL)
    Fatal_Error(ERR_ALLOC_FAULT, "malloc", src_file, src_line);

  return m;
}




/*-------------------------------------------------------------------------*
 * CALLOC_CHECK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Calloc_Check(unsigned nb, unsigned size, char *src_file, int src_line)
{
  char *m = calloc(nb, size);

  if (m == NULL)
    Fatal_Error(ERR_ALLOC_FAULT, "calloc", src_file, src_line);

  return m;
}




/*-------------------------------------------------------------------------*
 * REALLOC_CHECK                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Realloc_Check(char *ptr, unsigned size, char *src_file, int src_line)
{
  char *m = realloc(ptr, size);

  if (m == NULL)
    Fatal_Error(ERR_ALLOC_FAULT, "realloc", src_file, src_line);

  return m;
}




/*-------------------------------------------------------------------------*
 * STRDUP_CHECK                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
char *
Strdup_Check(char *str, char *src_file, int src_line)
{
  char *s = strdup(str);

  if (s == NULL)
    Fatal_Error(ERR_ALLOC_FAULT, "strdup", src_file, src_line);

  return s;
}




/*-------------------------------------------------------------------------*
 * EXTEND_TABLE_IF_NEEDED                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Extend_Table_If_Needed(char **hash_tbl)
{
  int size = Hash_Table_Size(*hash_tbl);

  if (Hash_Nb_Elements(*hash_tbl) >= size)
    *hash_tbl = Hash_Realloc_Table(*hash_tbl, size * 2);
}




/*-------------------------------------------------------------------------*
 * EXTEND_ARRAY                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Extend_Array(char **ptbl, int *nb_elem, int elem_size, Bool bzero)
{
  int old_nb_elem = *nb_elem;
  int new_nb_elem = old_nb_elem * 2;
  char *new_tbl;

  new_tbl = Realloc(*ptbl, new_nb_elem * elem_size);
  if (bzero)
    memset(new_tbl + (old_nb_elem * elem_size), 0,
	   (new_nb_elem - old_nb_elem) * elem_size);

  *ptbl = new_tbl;
  *nb_elem = new_nb_elem;
}




/*-------------------------------------------------------------------------*
 * EXIT_WITH_VALUE                                                         *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Exit_With_Value(int ret_val)
{
#ifndef NO_USE_LINEDIT
  if (le_hook_exit_process)
    (*le_hook_exit_process)();
#endif

  exit(ret_val);
}




/*-------------------------------------------------------------------------*
 * FATAL_ERROR                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Fatal_Error(char *format, ...)
{
  va_list arg_ptr;
  char buff[1024];

  va_start(arg_ptr, format);
  vsprintf(buff, format, arg_ptr);
  va_end(arg_ptr);

#ifndef NO_USE_LINEDIT
  if (le_hook_message_box)
    (*le_hook_message_box)("Fatal Error", buff, 0);
  else
#endif
    fprintf(stderr, "\nFatal Error: %s\n", buff);

  Exit_With_Value(1);
}


