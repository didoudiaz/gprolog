/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pl_params.h                                                     *
 * Descr.: parameter header file                                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

#define MAX_OBJECT                 1024

#define START_PRED_TBL_SIZE        4096

#define START_OPER_TBL_SIZE        1024

#define ATOM_SIZE                  16
#define MAX_ATOM                   (1<<ATOM_SIZE)	/* number of elements */

#define NB_OF_X_REGS               256
#define MAX_ARITY                  (NB_OF_X_REGS-1)

/* NB: if NB_OF_X_REGS is changed it is necessary to modify ma2asm but
   also the byte code management */
