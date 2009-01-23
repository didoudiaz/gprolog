/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : parse_supp.h                                                    *
 * Descr.: parser support - header file                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2009 Daniel Diaz                                     *
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

#define PARSE_END_OF_TERM_DOT      0
#define PARSE_END_OF_TERM_EOF      1




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/


typedef struct			/* Parsed variable information    */
{				/* ------------------------------ */
  char name[MAX_VAR_NAME_LENGTH]; /* variable name                */
  WamWord word;			/* associated WAM word            */
  Bool named;			/* has it a name ?                */
  int nb_of_uses;		/* occurrence counter             */
}
InfVar;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef PARSE_SUPP_FILE

InfVar pl_parse_dico_var[MAX_VAR_IN_TERM];
int pl_parse_nb_var;

#else

extern InfVar pl_parse_dico_var[];
extern int pl_parse_nb_var;

#endif


				/* defined as this to avoid to force the   */
				/* inclusion of parse_supp.o if not needed */
				/* (stream_supp.c uses pl_last_read_line/col) */
int pl_last_read_line;
int pl_last_read_col;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

WamWord Pl_Read_Term(StmInf *pstm, int parse_end_of_term);

WamWord Pl_Read_Atom(StmInf *pstm);

WamWord Pl_Read_Integer(StmInf *pstm);

WamWord Pl_Read_Number(StmInf *pstm);

WamWord Pl_Read_Token(StmInf *pstm);
