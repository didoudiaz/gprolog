/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : oper.h                                                          *
 * Descr.: operator table management - header file                         *
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

#define MAX_PREC                   1200
#define MAX_ARG_OF_FUNCTOR_PREC    999


#define Make_Oper_Key(a, t)        (((PlULong) (a) << 2) | (t))
#define Atom_Of_Oper(k)            ((PlULong) (k) >> 2)
#define Type_Of_Oper(k)            ((PlULong) (k) & 3)




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
  PlLong a_t;			/* key is <atom,operator type>    */
  int prec;			/* precedence of the operator     */
  int left;			/* precedence of the operator lhs */
  int right;			/* precedence of the operator rhs */
}
OperInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef OPER_FILE

char *pl_oper_tbl;

#else

extern char *pl_oper_tbl;

#endif


/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Init_Oper(void);

OperInf *Pl_Create_Oper(int atom_op, int type, int prec, int left, int right);

OperInf *Pl_Lookup_Oper(int atom_op, int type);

OperInf *Pl_Lookup_Oper_Any_Type(int atom_op);

OperInf *Pl_Delete_Oper(int atom_op, int type);



#define Check_Oper(atom_op, type) \
     (pl_atom_tbl[(atom_op)].prop.op_mask & Make_Op_Mask(type))



#define Check_Oper_Any_Type(atom_op) \
     (pl_atom_tbl[(atom_op)].prop.op_mask)
