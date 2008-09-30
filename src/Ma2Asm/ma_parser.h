/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : mini-assembler to assembler translator                          *
 * File  : ma_parser.h                                                     *
 * Descr.: parser - header file                                            *
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
    long int_val;		/* for integer */
    double dbl_val;		/* for double */
    Mem mem;			/* for mem */
    int index;			/* for X() Y() FL() FD() */
  }
  t;
}
ArgInf;




typedef struct
{
  long int_val;
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
