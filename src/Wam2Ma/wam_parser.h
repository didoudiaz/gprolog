/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : WAM to mini-assembler translator                                *
 * File  : wam_parser.h                                                    *
 * Descr.: parser - header file                                            *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum
{				/* skip 256 to specify a given char */
  ATOM = 256,			/* an atom */
  INTEGER,			/* an integer */
  FLOAT,			/* a double */
  X_Y,				/* x(X) or y(Y) */
  F_N,				/* a ATOM / INTEGER */
  MP_N,				/* a [ATOM :] ATOM / INTEGER (optional module qualif) */
  LABEL,			/* a label */
  ANY,				/* ATOM or INTEGER or F_N or FLOAT or X_Y */
  LIST_INST                     /* a list of instructions */
}
ArgTyp;


#define L1(t)                  L2(t, 0)
#define L2(t1, t2)             ((t1 << 16) | (t2))
#define DECODE_L2(a, t1, t2)   t1 = (a) >> 16; t2 = (a) & ((1 << 16) - 1)

typedef double ArgVal;		/* to ensure double alignment */




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Parse_Wam_File(char *file_name_in, int comment);

void Syntax_Error(char *s);



#define Add_Arg(ptr, type, val)   (*((type *) (ptr)) = (val) , (ptr)++)

#define Get_Arg(ptr, type, val)   ((val) = *((type *) (ptr)) , (ptr)++)
