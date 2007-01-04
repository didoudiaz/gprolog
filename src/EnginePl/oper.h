/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : oper.h                                                          *
 * Descr.: operator table management - header file                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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

#define MAX_PREC                   1200
#define MAX_ARG_OF_FUNCTOR_PREC    999


#define Make_Oper_Key(a, t)        (((unsigned long) (a) << 2) | (t))
#define Atom_Of_Oper(k)            ((unsigned long) (k) >> 2)
#define Type_Of_Oper(k)            ((unsigned long) (k) & 3)




	  /* operator type */

#define PREFIX                     0
#define POSTFIX                    1
#define INFIX                      2




#define Make_Op_Mask(type)         (1<<(type))




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Operator information           */
{				/* ------------------------------ */
  long a_t;			/* key is <atom,operator type>    */
  int prec;			/* precedence of the operator     */
  int left;			/* precedence of the operator lhs */
  int right;			/* precedence of the operator rhs */
}
OperInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef OPER_FILE

char *oper_tbl;

#else

extern char *oper_tbl;

#endif


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Init_Oper(void);

OperInf *Create_Oper(int atom_op, int type, int prec, int left, int right);

OperInf *Lookup_Oper(int atom_op, int type);

OperInf *Lookup_Oper_Any_Type(int atom_op);

OperInf *Delete_Oper(int atom_op, int type);



#define Check_Oper(atom_op, type) \
     (atom_tbl[(atom_op)].prop.op_mask & Make_Op_Mask(type))



#define Check_Oper_Any_Type(atom_op) \
     (atom_tbl[(atom_op)].prop.op_mask)
