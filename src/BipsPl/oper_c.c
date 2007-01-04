/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : oper_c.c                                                        *
 * Descr.: operator management - C part                                    *
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

#include <string.h>

#define OBJ_INIT Oper_Initializer

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define FX                         0
#define FY                         1

#define XF                         2
#define YF                         3

#define XFX                        4
#define XFY                        5
#define YFX                        6




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static int atom_specif_tbl[7];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int Detect_Oper_Specif(OperInf *oper);



#define CURRENT_OP_ALT             X2463757272656E745F6F705F616C74

Prolog_Prototype(CURRENT_OP_ALT, 0);




/*-------------------------------------------------------------------------*
 * OPER_INITIALIZER                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Oper_Initializer(void)
{
  char *a[7] = { "fx", "fy", "xf", "yf", "xfx", "xfy", "yfx" };
  int i;

  for (i = 0; i < 7; i++)
    atom_specif_tbl[i] = Create_Atom(a[i]);
}




/*-------------------------------------------------------------------------*
 * OP_3                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Op_3(WamWord prec_word, WamWord specif_word, WamWord oper_word)
{
  int atom_op;
  int prec;
  int atom_specif;
  int i;
  int type, left, right;


  atom_op = Rd_Atom_Check(oper_word);
  prec = Rd_Integer_Check(prec_word);
  if (prec < 0 || prec > MAX_PREC)
    Pl_Err_Domain(domain_operator_priority, prec_word);

  atom_specif = Rd_Atom_Check(specif_word);

  for (i = 0; i < 7 && atom_specif != atom_specif_tbl[i]; i++)
    ;

  switch (i)
    {
    case FX:
    case FY:
      type = PREFIX;
      left = 0;
      right = (i == FX) ? prec - 1 : prec;
      break;

    case XF:
    case YF:
      type = POSTFIX;
      left = (i == XF) ? prec - 1 : prec;
      right = 0;
      break;

    case XFX:
    case XFY:
    case YFX:
      type = INFIX;
      left = (i == XFX || i == XFY) ? prec - 1 : prec;
      right = (i == XFX || i == YFX) ? prec - 1 : prec;
      break;

    default:
      Pl_Err_Domain(domain_operator_specifier, specif_word);
      type = left = right = 0;	/* only for the compiler */
    }

  if (type != PREFIX
      && Check_Oper(atom_op, (type == POSTFIX) ? INFIX : POSTFIX))
    Pl_Err_Permission(permission_operation_create, permission_type_operator,
		      oper_word);

  if (atom_op == ATOM_CHAR(','))
    Pl_Err_Permission(permission_operation_modify,
		      permission_type_operator, oper_word);

  if (prec > 0)
    Create_Oper(atom_op, type, prec, left, right);
  else
    Delete_Oper(atom_op, type);
}




/*-------------------------------------------------------------------------*
 * CURRENT_OP_3                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Op_3(WamWord prec_word, WamWord specif_word, WamWord oper_word)
{
  WamWord word, tag_mask;
  HashScan scan;
  int prec;
  int atom_specif;
  OperInf *oper;
  int atom;
  int op_mask;
  int i;

  DEREF(oper_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK && tag_mask != TAG_ATM_MASK)
    Pl_Err_Type(type_atom, word);
  oper_word = word;


  DEREF(prec_word, word, tag_mask);
  prec = UnTag_INT(word);
  if (tag_mask != TAG_REF_MASK && 
      (tag_mask != TAG_INT_MASK || prec < 0 || prec > MAX_PREC))
    Pl_Err_Domain(domain_operator_priority, word);
  prec_word = word;


  DEREF(specif_word, word, tag_mask);
  if (tag_mask == TAG_ATM_MASK)
    {
      atom_specif = UnTag_ATM(word);

      for (i = 0; i < 7 && atom_specif != atom_specif_tbl[i]; i++)
	;
    }

  if (tag_mask != TAG_REF_MASK && (tag_mask != TAG_ATM_MASK || i >= 7))
    Pl_Err_Domain(domain_operator_specifier, specif_word);
  specif_word = word;


  if (Tag_Mask_Of(oper_word) == TAG_ATM_MASK)
    {
      atom = UnTag_ATM(oper_word);
      op_mask = atom_tbl[atom].prop.op_mask;
      if (op_mask == 0)
	return FALSE;

      for (i = PREFIX; i <= POSTFIX; i++)
	if (op_mask & Make_Op_Mask(i))
	  break;

      op_mask &= ~Make_Op_Mask(i);
      if (op_mask)		/* non deterministic case */
	{
	  A(0) = prec_word;
	  A(1) = specif_word;
	  A(2) = oper_word;
	  A(3) = op_mask;
	  Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0),
			      4);
	}
      oper = Lookup_Oper(atom, i);
    }
  else
    {
      oper = (OperInf *) Hash_First(oper_tbl, &scan);
      if (oper == NULL)
	return FALSE;

      /* non deterministic case */
      A(0) = prec_word;
      A(1) = specif_word;
      A(2) = oper_word;
      A(3) = (WamWord) scan.endt;
      A(4) = (WamWord) scan.cur_t;
      A(5) = (WamWord) scan.cur_p;
      Create_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0), 6);
    }

  return Get_Integer(oper->prec, prec_word) &&
    Get_Atom(Detect_Oper_Specif(oper), specif_word) &&
    Get_Atom(Atom_Of_Oper(oper->a_t), oper_word);
}




/*-------------------------------------------------------------------------*
 * CURRENT_OP_ALT_0                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Current_Op_Alt_0(void)
{
  WamWord prec_word, specif_word, oper_word;
  HashScan scan;
  OperInf *oper;
  int atom;
  int op_mask;
  int i;


  Update_Choice_Point((CodePtr) Prolog_Predicate(CURRENT_OP_ALT, 0), 0);

  prec_word = AB(B, 0);
  specif_word = AB(B, 1);
  oper_word = AB(B, 2);

  if (Tag_Mask_Of(oper_word) == TAG_ATM_MASK)
    {
      atom = UnTag_ATM(oper_word);
      op_mask = AB(B, 3);

      for (i = PREFIX; i <= POSTFIX; i++)
	if (op_mask & Make_Op_Mask(i))
	  break;

      oper = Lookup_Oper(atom, i);

      Delete_Last_Choice_Point();
    }
  else
    {
      scan.endt = (char *) AB(B, 3);
      scan.cur_t = (char *) AB(B, 4);
      scan.cur_p = (char *) AB(B, 5);

      oper = (OperInf *) Hash_Next(&scan);
      if (oper == NULL)
	{
	  Delete_Last_Choice_Point();
	  return FALSE;
	}


#if 0 /* the following data is unchanged */
      AB(B, 0) = prec_word;
      AB(B, 1) = specif_word;
      AB(B, 2) = oper_word;
      AB(B, 3) = (WamWord) scan.endt;
#endif
      AB(B, 4) = (WamWord) scan.cur_t;
      AB(B, 5) = (WamWord) scan.cur_p;
    }

  return Get_Integer(oper->prec, prec_word) &&
    Get_Atom(Detect_Oper_Specif(oper), specif_word) &&
    Get_Atom(Atom_Of_Oper(oper->a_t), oper_word);
}




/*-------------------------------------------------------------------------*
 * DETECT_OPER_SPECIF                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static int
Detect_Oper_Specif(OperInf *oper)
{
  int prec = oper->prec;
  int i;

  switch (Type_Of_Oper(oper->a_t))
    {
    case PREFIX:
      i = (oper->right) < prec ? FX : FY;
      break;

    case POSTFIX:
      i = (oper->left) < prec ? XF : YF;
      break;

    default:			/* i.e INFIX: */
      i = (oper->left) < prec ? ((oper->right) < prec ? XFX : XFY) : YFX;
      break;
    }

  return atom_specif_tbl[i];
}
