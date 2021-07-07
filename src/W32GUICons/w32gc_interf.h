/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32gc_interf.c                                                  *
 * Descr.: line editor <--> GUICons interface                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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


#ifndef _W32GC_INTERF_H
#define _W32GC_INTERF_H

#if 1         /* comment if you dont want to activate stack sizes dialog box */
#define GUI_CONSOLE_WITH_STACK_SIZES
#endif

typedef enum {
  QUERY_STACK_GET_NB_OF_STACKS,
  QUERY_STACK_HAS_FIXED_SIZES,
  QUERY_STACK_GET_NAME,
  QUERY_STACK_GET_DESC,
  QUERY_STACK_GET_ENV_VAR_NAME,
  QUERY_STACK_GET_DEFAULT_SIZE,
  QUERY_STACK_GET_SIZE
}QueryStackCmd;

#endif  /* !_W32GC_INTERF_H */
