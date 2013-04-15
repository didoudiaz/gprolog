/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : wam_long.h                                                      *
 * Descr.: Wam long type definition                                        *
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

#ifndef _PL_LONG_H
#define _PL_LONG_H

#include <stdint.h>

#include "gp_config.h"

/* A PlLong can store an address: it is thus an intptr_t (depends on 32/64 bits arch) */

typedef intptr_t PlLong;
typedef uintptr_t PlULong;

#ifdef HAVE_INTTYPES_H

#include <inttypes.h>

#define PL_FMT_d  PRIdPTR
#define PL_FMT_u  PRIuPTR
#define PL_FMT_o  PRIoPTR
#define PL_FMT_x  PRIxPTR

/* Utilities to work on int64_t independently of the 32/64 bits of the arch */

#define FMT64_d   PRId64
#define FMT64_u   PRIu64
#define FMT64_o   PRIo64
#define FMT64_x   PRIx64

#else  /* !HAVE_INTTYPES_H */

#if SIZEOF_LONG == SIZEOF_VOIDP

#  define __PL_FMT_PREFIX "l"

#elif defined(_MSC_VER)

#  define __PL_FMT_PREFIX "I64"

#else

#  define __PL_FMT_PREFIX "ll"

#endif

#define PL_FMT_d  __PL_FMT_PREFIX "d"
#define PL_FMT_u  __PL_FMT_PREFIX "u"
#define PL_FMT_o  __PL_FMT_PREFIX "o"
#define PL_FMT_x  __PL_FMT_PREFIX "x"

/* Utilities to work on int64_t independently of the 32/64 bits of the arch */

#ifdef _MSC_VER

#  define __FMT64_PREFIX  "I64"

#elif WORD_SIZE == 64

#  define __FMT64_PREFIX  "l"

#else

#  define __FMT64_PREFIX  "ll"

#endif

#define FMT64_d   __FMT64_PREFIX "d"
#define FMT64_u   __FMT64_PREFIX "u"
#define FMT64_o   __FMT64_PREFIX "o"
#define FMT64_x   __FMT64_PREFIX "x"

#endif /* !HAVE_INTTYPES_H */


/* --- strtol / strtoul --- */

#if SIZEOF_LONG == SIZEOF_VOIDP

#  define Str_To_PlLong(__str, __end, __base)   strtol (__str, __end, __base)
#  define Str_To_PlULong(__str, __end, __base)  strtoul(__str, __end, __base)

#elif defined(__GNUC__)

#  define Str_To_PlLong(__str, __end, __base)   strtoll (__str, __end, __base)
#  define Str_To_PlULong(__str, __end, __base)  strtoull(__str, __end, __base)

#else  /* MSVC */

#  define Str_To_PlLong(__str, __end, __base)   _strtoi64 (__str, __end, __base)
#  define Str_To_PlULong(__str, __end, __base)  _strtoui64(__str, __end, __base)

#endif

#endif	/* !_PL_LONG_H */
