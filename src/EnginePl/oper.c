/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : oper.c                                                          *
 * Descr.: operator table management                                       *
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


#include <stdio.h>
#include <stdlib.h>


#define OPER_FILE

#include "engine_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define T_FX(p)                    PREFIX, p, 0  , p-1

#define T_FY(p)                    PREFIX,  p, 0  , p

#define T_XF(p)                    POSTFIX, p, p-1, 0

#define T_YF(p)                    POSTFIX, p, p,   0

#define T_XFX(p)                   INFIX,   p, p-1, p-1

#define T_XFY(p)                   INFIX,   p, p-1, p

#define T_YFX(p)                   INFIX,   p, p,   p-1

#define ADD_OPER(prec, type, name) Pl_Create_Oper(Pl_Create_Atom(name), T_##type(prec))




/*-------------------------------------------------------------------------*
 * PL_INIT_OPER                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Oper(void)
{
  pl_oper_tbl = Pl_Hash_Alloc_Table(START_OPER_TBL_SIZE, sizeof(OperInf));

  ADD_OPER(1200, XFX, ":-");
  ADD_OPER(1200, XFX, "-->");

  ADD_OPER(1200, FX, ":-");

  ADD_OPER(1105, XFY, "|");

  ADD_OPER(1100, XFY, ";");

  ADD_OPER(1050, XFY, "->");
  ADD_OPER(1050, XFY, "*->");

  ADD_OPER(1000, XFY, ",");

  ADD_OPER(900, FY, "\\+");

  ADD_OPER(700, XFX, "=");
  ADD_OPER(700, XFX, "\\=");

  ADD_OPER(700, XFX, "==");
  ADD_OPER(700, XFX, "\\==");
  ADD_OPER(700, XFX, "@<");
  ADD_OPER(700, XFX, "@>");
  ADD_OPER(700, XFX, "@=<");
  ADD_OPER(700, XFX, "@>=");

  ADD_OPER(700, XFX, "=..");

  ADD_OPER(700, XFX, "is");
  ADD_OPER(700, XFX, "=:=");
  ADD_OPER(700, XFX, "=\\=");
  ADD_OPER(700, XFX, "<");
  ADD_OPER(700, XFX, "=<");
  ADD_OPER(700, XFX, ">");
  ADD_OPER(700, XFX, ">=");

  ADD_OPER(600, XFY, ":");

  ADD_OPER(500, YFX, "+");
  ADD_OPER(500, YFX, "-");
  ADD_OPER(500, YFX, "/\\");
  ADD_OPER(500, YFX, "\\/");

  ADD_OPER(400, YFX, "*");
  ADD_OPER(400, YFX, "/");
  ADD_OPER(400, YFX, "//");
  ADD_OPER(400, YFX, "rem");
  ADD_OPER(400, YFX, "mod");
  ADD_OPER(400, YFX, "div");
  ADD_OPER(400, YFX, "<<");
  ADD_OPER(400, YFX, ">>");

  ADD_OPER(200, XFX, "**");
  ADD_OPER(200, XFY, "^");

  ADD_OPER(200, FY, "+");
  ADD_OPER(200, FY, "-");

  ADD_OPER(200, FY, "\\");



  /* FD Operators */
  ADD_OPER(750, XFY, "#<=>");
  ADD_OPER(750, XFY, "#\\<=>");
  ADD_OPER(740, XFY, "#==>");
  ADD_OPER(740, XFY, "#\\==>");
  ADD_OPER(730, XFY, "##");
  ADD_OPER(730, YFX, "#\\/");
  ADD_OPER(730, YFX, "#\\\\/");
  ADD_OPER(720, YFX, "#/\\");
  ADD_OPER(720, YFX, "#\\/\\");
  ADD_OPER(710, FY, "#\\");

  ADD_OPER(700, XFX, "#=");
  ADD_OPER(700, XFX, "#\\=");
  ADD_OPER(700, XFX, "#<");
  ADD_OPER(700, XFX, "#=<");
  ADD_OPER(700, XFX, "#>");
  ADD_OPER(700, XFX, "#>=");

  ADD_OPER(700, XFX, "#=#");
  ADD_OPER(700, XFX, "#\\=#");
  ADD_OPER(700, XFX, "#<#");
  ADD_OPER(700, XFX, "#=<#");
  ADD_OPER(700, XFX, "#>#");
  ADD_OPER(700, XFX, "#>=#");
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_OPER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
OperInf *
Pl_Create_Oper(int atom_op, int type, int prec, int left, int right)
{
  OperInf oper_info;
  OperInf *oper;


  Pl_Extend_Table_If_Needed(&pl_oper_tbl);

  oper_info.a_t = Make_Oper_Key(atom_op, type);
  oper_info.prec = prec;
  oper_info.left = left;
  oper_info.right = right;

  oper = (OperInf *) Pl_Hash_Insert(pl_oper_tbl, (char *) &oper_info, TRUE);

  pl_atom_tbl[atom_op].prop.op_mask |= Make_Op_Mask(type);

  return oper;
}




/*-------------------------------------------------------------------------*
 * PL_LOOKUP_OPER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
OperInf *
Pl_Lookup_Oper(int atom_op, int type)
{
  if (!Check_Oper(atom_op, type))
    return NULL;

  return (OperInf *) Pl_Hash_Find(pl_oper_tbl, Make_Oper_Key(atom_op, type));
}




/*-------------------------------------------------------------------------*
 * PL_LOOKUP_OPER_ANY_TYPE                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
OperInf *
Pl_Lookup_Oper_Any_Type(int atom_op)
{
  int op_mask = pl_atom_tbl[atom_op].prop.op_mask;

  if (op_mask & Make_Op_Mask(PREFIX))
    return (OperInf *) Pl_Hash_Find(pl_oper_tbl, Make_Oper_Key(atom_op, PREFIX));

  if (op_mask & Make_Op_Mask(INFIX))
    return (OperInf *) Pl_Hash_Find(pl_oper_tbl, Make_Oper_Key(atom_op, INFIX));

  if (op_mask & Make_Op_Mask(POSTFIX))
    return (OperInf *) Pl_Hash_Find(pl_oper_tbl, Make_Oper_Key(atom_op, POSTFIX));

  return NULL;
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_OPER                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
OperInf *
Pl_Delete_Oper(int atom_op, int type)
{
  PlLong key = Make_Oper_Key(atom_op, type);

  pl_atom_tbl[atom_op].prop.op_mask &= ~Make_Op_Mask(type);

  return (OperInf *) Pl_Hash_Delete(pl_oper_tbl, key);
}
