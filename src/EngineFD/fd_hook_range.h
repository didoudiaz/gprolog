/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint solver                                            *
 * File  : fd_hook_range.h                                                 *
 * Descr.: FD Range Implementation - customizable header file              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define RANGE_TOP_STACK            CS
#if 0
#define INTERVAL_MAX_INTEGER       ((int)(((PlLong)1<<(32-TAG_SIZE-1))-1))	/* only 32 bits (even on 64 bits machine) - ie. 268435455 */
#else /* in practice limit it more to avoid (many) intermediate overflows */
#define INTERVAL_MAX_INTEGER       100000000
#endif

#ifdef GP_FD_POSITIVE_ONLY 					/* if set, use only positive integers (for backwards compatibility) */
#define INTERVAL_MIN_INTEGER       0
#else
#define INTERVAL_MIN_INTEGER       (- INTERVAL_MAX_INTEGER)
#endif



