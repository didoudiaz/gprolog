/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : atom.h                                                          *
 * Descr.: atom table management - header file                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
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

#if 1
#define OPTIM_1_CHAR_ATOM
#endif




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

	  /* Character Classification */

#define LA                         1	/* layout character      */
#define SC                         2	/* solo character        */
#define QT                         4	/* quote                 */
#define DQ                         8	/* double quote          */
#define BQ                        16	/* back quote            */
#define GR                        32	/* graphic char          */
#define PC                        64	/* punctuation character */
#define DI                       128	/* digit                 */
#define UL                       256	/* underline             */
#define CL                       512	/* capital letter        */
#define SL                      1024	/* small letter          */
#define CM                      2048	/* comment character (%) */
#define EX                      4096	/* extended character    */




#define ATOM_NIL                  1766



	  /* Atom Type */

#define IDENTIFIER_ATOM            0
#define GRAPHIC_ATOM               1
#define SOLO_ATOM                  2
#define OTHER_ATOM                 3




#define Is_Valid_Code(c)           ((unsigned) (c)-1 <256-1)	/* 1<= c <256 */
#define Is_Valid_Byte(c)           ((unsigned) (c) <256)	/* 0=< c <256 */
#define Is_Valid_Atom(a)           ((a)>=0 && (a)<MAX_ATOM && \
                                    atom_tbl[(a)].name!=NULL)



/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/


typedef struct			/* Atom properties                */
{				/* ------------------------------ */
  unsigned length:16;		/* its length (in characters)     */
  unsigned op_mask:4;		/* operator defined for the atom  */
  unsigned type:2;		/* IDENTIFIER GRAPHIC SOLO OTHER  */
  unsigned needs_quote:1;	/* needs ' around it ?            */
  unsigned needs_scan:1;	/* contains ' or control char ?   */
}
AtomProp;




typedef struct			/* Atom information               */
{				/* ------------------------------ */
  char *name;			/* key is <name> (the string)     */
  AtomProp prop;		/* associated properties          */
}
AtomInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef ATOM_FILE

AtomInf atom_tbl[MAX_ATOM];
int nb_atom;

int atom_void;
int atom_curly_brackets;

int atom_false;
int atom_true;

int atom_end_of_file;

#ifndef OPTIM_1_CHAR_ATOM
int atom_char[256];
#endif

    /* int     char_type[256];                    see definition in atom.c */
char char_conv[256];


    /* char    escape_symbol[];                   see definition in atom.c */
    /* char    escape_char  [];                   see definition in atom.c */

#else

extern AtomInf atom_tbl[];
extern int nb_atom;

extern int atom_void;
extern int atom_curly_brackets;

extern int atom_false;
extern int atom_true;

extern int atom_end_of_file;

#ifndef OPTIM_1_CHAR_ATOM
extern int atom_char[];
#endif

extern char char_conv[];
extern int char_type[];

extern char escape_symbol[];
extern char escape_char[];

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Init_Atom(void);

int Create_Allocate_Atom(char *name) FC;

int Create_Atom(char *name) FC;

WamWord Create_Atom_Tagged(char *name) FC;

int Find_Atom(char *name) FC;

int Gen_New_Atom(char *prefix, int hash) FC;

int Find_Next_Atom(int last_atom) FC;



#ifdef OPTIM_1_CHAR_ATOM

#define ATOM_CHAR(c)            ((int) (unsigned char) (c))

#else

#define ATOM_CHAR(c)            (atom_char[(int) (unsigned char) (c)])

#endif
