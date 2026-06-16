/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam_parser.h                                                    *
 * Descr.: parser - header file                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2026 Daniel Diaz                                     *
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

#include "../EnginePl/bool.h"

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum
{
  END_OF_FILE = -1,		/* EOF, 0..255 to specify a given char */
  ATOM = 256,			/* an atom */
  INTEGER,			/* a Prolog integer corresponding to a PlLong */
  C_INT,			/* a sub type of INTEGER corresponding to a C int */
  FLOAT,			/* a double */
  X_Y,				/* x(X) or y(Y) */
  ADR_OF_X_Y,			/* &(X_Y) (can be provided as result for ANY) */
  F_N,				/* a ATOM / INTEGER */
  MP_N,				/* a [ATOM :] ATOM / INTEGER (optional module qualif) */
  ADR_OF_MP_N,			/* &(MP_N) (can be provided as result for ANY) */
  LABEL,			/* a label */
  ANY,				/* ATOM, INTEGER, FLOAT, F_N, X_Y, &(X_Y), &(MP_N) */
  LIST_INST                     /* a list of instructions */
}
ArgTyp;


#define L1(t1)                 L2(t1, 0)
#define L2(t1, t2)             (((t1) << 16) | (t2))
#define DECODE_L2(a, t1, t2)   t1 = (a) >> 16; t2 = (a) & ((1 << 16) - 1)

typedef double ArgVal;		/* to ensure double alignment */




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

Bool Parse_Wam_File(char *file_name_in, Bool comment);

void Syntax_Error(char *format, ...) ATTR_PRINTF(1);



#define Add_Arg(ptr, type, val)   (*((type *) (ptr)) = (val) , (ptr)++)

#define Test_Arg_Type(ptr, val)   (*(int *) (ptr) == val)

#define Get_Arg(ptr, type, val)   ((val) = *((type *) (ptr)) , (ptr)++)
