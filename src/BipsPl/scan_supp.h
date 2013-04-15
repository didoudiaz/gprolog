/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : scan_supp.h                                                     *
 * Descr.: scanner support - header file                                   *
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

#define SCAN_BIG_BUFFER            10240




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef enum
{
  TOKEN_VARIABLE,
  TOKEN_INTEGER,
  TOKEN_FLOAT,
  TOKEN_NAME,
  TOKEN_STRING,
  TOKEN_BACK_QUOTED,
  TOKEN_PUNCTUATION,
  TOKEN_IMMEDIAT_OPEN,
  TOKEN_FULL_STOP,
  TOKEN_END_OF_FILE,
  TOKEN_EXTENDED
}
TypTok;




typedef struct
{
  TypTok type;
  char name[SCAN_BIG_BUFFER];	/* for VARIABLE NAME STRING BACK_QUOTED */
  int quoted;                   /* for NAME: was it quoted ?            */
  int punct;			/* for PUNCTUATION                      */
  PlLong int_num;		/* for INTEGER                          */
  double float_num;		/* for FLOAT                            */
  int line;			/* source line of the pl_token          */
  int col;			/* source column of the pl_token        */
}
TokInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef SCAN_SUPP_FILE

TokInf pl_token;

#else

extern TokInf pl_token;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

int Pl_Scan_Peek_Char(StmInf *pstm, Bool convert);



char *Pl_Scan_Token(StmInf *pstm, Bool comma_is_punct);



void Pl_Recover_After_Error(StmInf *pstm);



char *Pl_Scan_Next_Atom(StmInf *pstm);

char *Pl_Scan_Next_Number(StmInf *pstm, Bool integer_only);
