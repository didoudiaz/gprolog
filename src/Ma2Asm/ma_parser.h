/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_parser.h                                                     *
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


#include "../EnginePl/pl_long.h"


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum
{
  IDENTIFIER = 256,		/* not a type but a token */
  INTEGER,
  FLOAT,
  MEM,				/* not a token but a type */
  X_REG,
  Y_REG,
  FL_ARRAY,
  FD_ARRAY,
  STRING
}
ArgTyp;




typedef struct
{
  char *name;
  int index;
}
Mem;




typedef struct
{
  ArgTyp type;
  int adr_of;
  union
  {
    char *str_val;		/* for string */
    PlLong int_val;		/* for integer */
    double dbl_val;		/* for double */
    Mem mem;			/* for mem */
    int index;			/* for X() Y() FL() FD() */
  }
  t;
}
ArgInf;




typedef struct
{
  PlLong int_val;
  char *label;
}
SwtInf;




typedef enum
{
  NONE,
  ARRAY_SIZE,
  INITIAL_VALUE
}
VType;



/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Parse_Ma_File(char *file_name_in, int comment);

void Syntax_Error(char *s);
