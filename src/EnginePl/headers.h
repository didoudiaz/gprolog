/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : headers.h                                                       *
 * Descr.: GNU Prolog - general header file (for users)                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2022 Daniel Diaz                                     *
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

#ifndef _GPROLOG_H
#define _GPROLOG_H

#ifdef __cplusplus
extern "C" {
#endif

#include "engine_pl.h"
#include "bips_pl.h"


#ifndef NO_USE_FD_SOLVER

#include "engine_fd.h"
#include "bips_fd.h"

#endif /* NO_USE_FD_SOLVER */

#ifdef __cplusplus
}
#endif

#endif

